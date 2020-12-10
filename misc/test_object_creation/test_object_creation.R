setwd("misc/test_object_creation")

library(assertthat)
library(biomaRt)
library(dplyr)
library(glue)
library(stringr)

library(DGEobj)
library(DGE.Tools2)

# Source: https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE120804
getLocation <- "ftp://ftp.ncbi.nlm.nih.gov/geo/series/GSE120nnn/GSE120804/suppl"

countsFile <- "GSE120804_counts.txt.gz"
designFile <- "GSE120804_geo_sample_annotation_edit.csv.gz"

# acquire raw data
download.file(glue("{getLocation}/{countsFile}"),
              destfile = glue("data/{countsFile}"),
              mode = 'wb')
counts <- read.delim(glue("data/{countsFile}"), stringsAsFactors = FALSE, row.names = 1)

download.file(glue("{getLocation}/{designFile}"),
              destfile = designFile,
              mode = 'wb')
design <- read.csv(glue("data/{designFile}"), stringsAsFactors = FALSE)  %>%
    rename(ReplicateGroup = Replicate.group)
rownames(design) <- str_sub(design$raw.file, start = 1, end = 21)


# get gene information from BioMart
ens.ds      <- "rnorvegicus_gene_ensembl"
ens.mart    <- useMart(biomart = "ensembl", dataset = ens.ds)
ens.columns <- c("ensembl_gene_id", "rgd_symbol", "chromosome_name", "start_position",
                 "end_position", "strand", "gene_biotype", "description")
ens.data    <- getBM(attributes = ens.columns, values = rownames(counts), mart = ens.mart) %>%
    distinct(ensembl_gene_id, .keep_all = T)

# properly format gene information for GenomicRanges use
gene.data <- left_join(data.frame(ensembl_gene_id = rownames(counts), stringsAsFactors = F),
                       ens.data,
                       by = "ensembl_gene_id") %>%
    dplyr::rename(start = start_position, end = end_position) %>%
    mutate(strand = case_when(strand == -1 ~ "-",
                              strand == 1  ~ "+",
                              TRUE         ~ "*"))
rownames(gene.data) <- gene.data$ensembl_gene_id

# Create Full DGEobj
my.dge <- initDGEobj(counts, gene.data, design, level = "gene")
my.dge <- annotateDGEobj(my.dge, annotations = "data/GSE120804_metadata.txt")
saveRDS(my.dge, 'testObj1.RDS')

# Create a small DGEobj for package inclusion
sm.counts <- counts[sample(1:NROW(counts), size = 1000), ]
sm.genes  <- gene.data[rownames(sm.counts), ]
sm.dge    <- initDGEobj(sm.counts, sm.genes, design, level = "gene")
sm.dge    <- annotateDGEobj(sm.dge, annotations = "data/GSE120804_metadata.txt")
saveRDS(sm.dge, 'exampleObj.RDS')

# # Low intensity Filtering
#
# fracThreshold <- 0.5
#
# # Low expression filter
# dgeObj <- lowIntFilter(dgeObj,
#                        fpkThreshold = 5,
#                        countThreshold = 10,
#                        sampleFraction = fracThreshold)
