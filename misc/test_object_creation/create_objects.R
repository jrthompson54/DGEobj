try({setwd("misc/test_object_creation")})

suppressPackageStartupMessages({
    library(assertthat)
    library(biomaRt)
    library(dplyr)
    library(glue)
    library(stringr)
    library(DGEobj)
    library(DGEobj.utils) #utilized to run workflow
})


# utility to run the workflow to create the example objects
dge_creation_workflow <- function(counts, gene.data, design, qcdata, contrast_list, annotation_file, limit_genes = NULL) {
    result <- initDGEobj(counts, gene.data, design, level = "gene")

    result <- addItem(result,
                      item = qcdata,
                      itemName = "AlignmentQC",
                      itemType = "alignQC")

    # Low intensity Filtering
    result <- lowIntFilter(result,
                           countThreshold = 10,
                           sampleFraction = 0.5)

    # Gene Limitation
    if (!is.null(limit_genes)) {
        keep.genes <- sample(rownames(result$counts), size = limit_genes)
        result <- initDGEobj(counts[rownames(counts) %in% keep.genes, ],
                             gene.data[rownames(gene.data) %in% keep.genes,],
                             design, level = "gene")
    }

    # Annotations
    result <- annotateDGEobj(result, annotation_file)

    # --- To save partial objects for use in the DGEobj.utils vignette
    # saveRDS(result, glue("simple{ifelse(is.null(limit_genes), '', limit_genes)}.RDS"))

    # Protein Coding Filtering
    result <- result[result$geneData$gene_biotype == "protein_coding", ]

    # Normalize
    if (requireNamespace("edgeR", quietly = TRUE)) {
        result <- runEdgeRNorm(result, includePlot = FALSE)
    }

    # Define Model
    formula          <- '~ 0 + ReplicateGroup'
    designMatrixName <- "ReplicateGroupDesign"

    designMatrix <- model.matrix(as.formula(formula),
                                 getItem(result, "design"))
    attr(designMatrix, "formula") <- formula
    colnames(designMatrix) <- make.names(colnames(designMatrix))

    result <- addItem(result,
                      item      = designMatrix,
                      itemName  = designMatrixName,
                      itemType  = "designMatrix",
                      parent    = "design",
                      overwrite = TRUE)

    # runVoom
    result <- runVoom(result,
                      designMatrixName = designMatrixName,
                      mvPlot = FALSE)

    # runContrasts
    result <- runContrasts(result,
                           designMatrixName = designMatrixName,
                           contrastList     = contrast_list,
                           qValue = TRUE,
                           IHW    = TRUE)

    result
}


## Get the source data
## Source: https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE120804
getLocation <- "ftp://ftp.ncbi.nlm.nih.gov/geo/series/GSE120nnn/GSE120804/suppl"

countsFile  <- "GSE120804_counts.txt.gz"
designFile  <- "GSE120804_geo_sample_annotation_edit.csv.gz"
qcFile      <- "GSE120804_qc_metrics.txt.gz"

annotatFile <- "data/GSE120804_metadata.txt"

# acquire raw data
download.file(glue("{getLocation}/{countsFile}"),
              destfile = glue("data/{countsFile}"),
              mode = 'wb')
counts <- read.delim(glue("data/{countsFile}"), stringsAsFactors = FALSE, row.names = 1)

download.file(glue("{getLocation}/{designFile}"),
              destfile = glue("data/{designFile}"),
              mode = 'wb')
design <- read.csv(glue("data/{designFile}"), stringsAsFactors = FALSE)  %>%
    rename(ReplicateGroup = Replicate.group)
rownames(design) <- str_sub(design$raw.file, start = 1, end = 21)

download.file(glue("{getLocation}/{qcFile}"),
              destfile = glue("data/{qcFile}"),
              mode = 'wb')
qcdata <- read.delim(glue("data/{qcFile}"), stringsAsFactors = FALSE)
rownames(qcdata) <- qcdata$Metric
qcdata <- qcdata %>%
    select(-Metric) %>%
    t() %>% as.data.frame()

# Create a DiseaseStatus column by parsing ReplicateGroup
design$DiseaseStatus <- rep("Sham", nrow(design))
idx <- str_detect(design$ReplicateGroup, "BDL")
design$DiseaseStatus[idx] <- "BDL"

# Create an animal# column.  The animal number is encoded in the sample.name column
design$AnimalNum <- str_match(design$Sample.name, "r[0-9]{1,3}")

# get gene information from BioMart
ens.ds      <- "rnorvegicus_gene_ensembl"
ens.mart    <- useMart(biomart = "ensembl", dataset = ens.ds)
ens.columns <- c("ensembl_gene_id", "rgd_symbol", "chromosome_name", "start_position",
                 "end_position", "strand", "gene_biotype", "description")
ens.data    <- getBM(attributes = ens.columns, values = rownames(counts), mart = ens.mart, useCache = F) %>%
    distinct(ensembl_gene_id, .keep_all = T)

# Filter the list to the genes used in the test dataset and format gene info for GenomicRanges
gene.data <- left_join(data.frame(ensembl_gene_id = rownames(counts), stringsAsFactors = F),
                       ens.data,
                       by = "ensembl_gene_id") %>%
    rename(start = start_position, end = end_position) %>%
    mutate(strand = case_when(strand == -1 ~ "-",
                              strand == 1  ~ "+",
                              TRUE         ~ "*"))
rownames(gene.data) <- gene.data$ensembl_gene_id

# Get transcript level data and keep max for each gene, or alternatively, use the cds length
ens.columns     <- c("ensembl_gene_id", "ensembl_transcript_id", "transcript_length")
transcript.data <- getBM(attributes = ens.columns,
                         values     = rownames(counts),
                         mart       = ens.mart,
                         useCache   = F) %>%
    arrange(desc(transcript_length)) %>%
    distinct(ensembl_gene_id, .keep_all = T)

gene.data <- left_join(gene.data,
                       select(transcript.data, ensembl_gene_id, ExonLength = transcript_length))
rownames(gene.data) <- gene.data$ensembl_gene_id
gene.data$ensembl_gene_id <- NULL

# enforce same order as counts
gene.data <- gene.data[rownames(counts), ]

contrast_list  <- list(BDL_vs_Sham = "ReplicateGroupBDL - ReplicateGroupSham",
                       EXT1024_vs_BDL = "ReplicateGroupBDL_EXT.1024  - ReplicateGroupBDL",
                       Nint_vs_BDL = "ReplicateGroupBDL_Nint - ReplicateGroupBDL",
                       Sora_vs_BDL = "ReplicateGroupBDL_Sora - ReplicateGroupBDL"
)


# full size
full.dge <- dge_creation_workflow(counts, gene.data, design, qcdata, contrast_list, annotatFile)
saveRDS(full.dge, "fullObj.RDS")

sm.dge <- dge_creation_workflow(counts, gene.data, design, qcdata, contrast_list, annotatFile, limit_genes = 1000)
saveRDS(sm.dge, 'exampleObj.RDS')
