### Function adatToDGEobj ###
#' Function adatToDGEobj
#'
#' Reads a Somalogic .adat file and imports the data into a DGEobj.
#'
#' @author John Thompson, \email{john.thompson@@bms.com}
#' @keywords Somalogic, adat, DGEobj
#'
#' @param adatFile Name of a .adat datafile.
#' @param keepOnlyPasses TRUE uses Somalogic qc data to exclude failed samples
#'   (Default=TRUE)
#' @param sampleNameCol Values of this column should match samples in the
#'   intensity data (Default="ExtIdentifier")
#' @return A DGEobj object with Somalogic data
#'
#' @examples
#'
#'  # MySomaObj is class = DGEobj
#'  MySomaObj <- adatToDGEobj(adatFilename, keepOnlyPasses=TRUE)
#'
#' @import magrittr
#' @importFrom assertthat assert_that
#' @importFrom readat readAdat getSequenceData as.SummarizedExperiment getSampleData
#' @importFrom tibble column_to_rownames
#' @export
adatToDGEobj <- function(adatFile, keepOnlyPasses=TRUE, sampleNameCol="ExtIdentifier"){

    assertthat::assert_that(!missing(adatFile),
                            file.exists(adatFile))

    adat <- readat::readAdat(adatFile, keepOnlyPasses = keepOnlyPasses)

    #need three dataframes to build a DGEobj: proteinData, design, intensities

    proteinData <- readat::getSequenceData(adat) %>%
        as.data.frame() %>%
        tibble::column_to_rownames(var="SeqId")

    #get the intensities by coverting to SE format
    SE <- readat::as.SummarizedExperiment(adat)
    intensities <- SummarizedExperiment::assay(SE, "intensities")  #gene by samples with row/colnames

    design <- readat::getSampleData(adat) %>%
        factor2char()
    rownames(design) <- design[[sampleNameCol]]
    class(design) <- "data.frame"

    somaObj <- initSOMAobj(intensities = intensities,
                           rowData = proteinData,
                           colData = design)

}

factor2char <- function(df){
    #Converts dataframe factor columns to char; leaves other columns alone
    #http://stackoverflow.com/questions/2851015/convert-data-frame-columns-from-factors-to-characters
    i <- sapply(df, is.factor)
    df[i] <- lapply(df[i], as.character)
    return(df)
}
