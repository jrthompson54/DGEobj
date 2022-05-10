require(testthat)
require(stringr)
require(DGEobj)

setup_failed <- TRUE

if (requireNamespace('GenomicRanges', quietly = TRUE)) {
    require(GenomicRanges)

    # --- gene level example object
    t_obj <- readRDS(system.file("exampleObj.RDS", package = "DGEobj", mustWork = TRUE))
    t_dim <- dim(t_obj)


    # --- exon level testing object
    exon_data   <- t_obj$geneData
    exon_counts <- t_obj$counts
    rownames(exon_data)   <- c(1:nrow(exon_data))
    rownames(exon_counts) <- rownames(exon_data)
    t_exon_obj <- initDGEobj(primaryAssayData = exon_counts,
                             rowData          = exon_data,
                             colData          = t_obj$design,
                             level            = "exon")


    # --- isoform level testing object
    # (simulated from our test object's data, this is NOT true isoform data)
    isoform_data           <- data.frame('Entry'         = t_obj$geneData$rgd_symbol,
                                         'Protein names' = t_obj$geneData$description,
                                         'Gene Names'    = t_obj$geneData$rgd_symbol,
                                         'Organism'      = 'Rattus norvegicus',
                                         'Length'        = t_obj$geneData$ExonLength)
    intensity              <- scale(t_obj$counts)
    rownames(isoform_data) <- c(1:nrow(isoform_data))
    rownames(intensity)    <- rownames(isoform_data)
    t_isoform_obj          <- initDGEobj(primaryAssayData = intensity,
                                         rowData          = isoform_data,
                                         colData          = t_obj$design,
                                         level            = "isoform")


    # --- protein level testing object
    # (simulated from our test object's data, this is NOT true protein data)
    protein_data           <- data.frame('Protein.IDs'        = paste(t_obj$geneData$rgd_symbol,
                                                                      rev(t_obj$geneData$rgd_symbol),
                                                                      sep = ';'),
                                         'ensembl_peptide_id' = paste(t_obj$geneData$rgd_symbol,
                                                                      rev(t_obj$geneData$rgd_symbol),
                                                                      sep = '|'),
                                         'ensembl_gene_id'    = rownames(t_obj$geneData),
                                         'hgnc_symbol'        = t_obj$geneData$rgd_symbol,
                                         'description'        = t_obj$geneData$description)
    intensity              <- scale(t_obj$counts)
    rownames(protein_data) <- c(1:nrow(protein_data))
    rownames(intensity)    <- rownames(protein_data)
    t_protein_obj          <- initDGEobj(primaryAssayData = intensity,
                                         rowData          = protein_data,
                                         colData          = t_obj$design,
                                         level            = "protein")

    setup_failed <- FALSE
}
