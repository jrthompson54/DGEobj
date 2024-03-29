#' Add annotations
#'
#' Reads an annotation file containing key/value pairs or a named list and attaches them
#' attributes to a DGEobj. If a file is used, it should be a text file
#' containing key/value pairs separated by an equals sign. The keys argument
#' specifies which keys we want to capture as attributes on the DGEobj.
#'
#' @param dgeObj  A object of class DGEobj created by function initDGEobj()
#' @param annotations Either a character string path to a file with annotations
#'   given as key/value pairs separated by an equal sign, or a named list of
#'   key/value pairs
#' @param keys By default (value = NULL), all keys are read in and applied as
#'   DGEobj attributes.  Use the keys argument to specify a specific list of
#'   keys to read from the file.
#'
#' @return A DGEobj
#'
#' @examples
#'    MyDgeObj <- system.file("miniObj.RDS", package = "DGEobj")
#'
#' \dontrun{
#'    #using a text file file of key=value pairs
#'    annotationFile <- "/location/to/myAnnotations.txt"
#'    MyDgeObj <- annotateDGEobj(MyDgeObj, annotationFile)
#' }
#'
#'    #using a named list of key/values
#'    annotations <- list(Title     = "Rat Liver Slices from Bile Duct Ligation animals",
#'                        Organism  = "Rat",
#'                        GeneModel = "Ensembl.R89")
#'    MyDgeObj <- annotateDGEobj(MyDgeObj, annotations)
#'
#'
#' @import magrittr
#' @importFrom stringr str_remove_all str_locate
#' @importFrom utils read.delim
#'
#' @export
annotateDGEobj <- function(dgeObj, annotations, keys = NULL) {

    if ("character" %in% class(annotations)) {
        assertthat::assert_that(file.exists(annotations),
                                msg = "The file specified does not exist.")

        # Read lines, stripping quotes
        regdat <- utils::read.delim(annotations, sep = "\t",
                                    quote = "\"",
                                    stringsAsFactors = FALSE,
                                    header = FALSE)

        # Just first column
        regdat <- regdat[, 1, drop = FALSE]
        colnames(regdat) <- "pair"

        # Just lines with equals signs, no commented out lines
        regdat <- regdat[grepl("=", regdat$pair) | !grepl("Parameters.", regdat$pair) | grepl("^#", regdat$pair), , drop = FALSE]

        # Loop through the attributes spitting on the first = sign
        regdat$key <- ""
        regdat$value <- ""
        for (i in 1:nrow(regdat)) {
            splitpos <- stringr::str_locate(regdat[i,1], "=")[1]  # Pos of 1st = sign
            regdat$key[i] <- substr(regdat[i,1], 1, (splitpos - 1))
            regdat$value[i] <- substr(regdat[i,1], (splitpos + 1), nchar(regdat[i,1]))
        }

        # After splitting, key without values get the key names inserted as the value. Convert those to empty strings.
        idx <- regdat$key == regdat$value
        regdat$value[idx] <- ""

        # Squeeze spaces out of keys
        regdat$key <- stringr::str_remove_all(regdat$key, " ")

    } else if ("list" %in% class(annotations)) {

        assertthat::assert_that(length(names(annotations)) == length(annotations),
                                msg = "annotations should be a named list of key/value pairs.")

        # if list, turn into a df
        regdat <- data.frame(matrix(ncol = 2, nrow = length(annotations)))
        colnames(regdat) <- c("key", "value")

        regdat <- stats::setNames(data.frame(matrix(ncol = 2, nrow = length(annotations))), c("key", "value"))
        for (i in 1:length(annotations)) {
            regdat$key[i] <- names(annotations)[i]
            regdat$value[i] <- annotations[[i]]
        }
    } else {
        assertthat::assert_that(is.null(class(annotations)),
                                msg = "When annotations is NULL, no attribute gets added to the dgeObj.")
    }

    # Capture/preserve the existing attributes
    MyAttribs <- attributes(dgeObj)
    if (is.null(MyAttribs)) MyAttribs <- list()

    for (i in 1:nrow(regdat))
        if (is.null(keys)) { # Add all keys
            MyAttribs[regdat$key[i]] <- regdat$value[i]
        } else {
            # Just add specified keys
            if (regdat$key[i] %in% keys)
                MyAttribs[regdat$key[i]] <- regdat$value[i]
        }

    # Now attach the attributes to the DGEobj
    attributes(dgeObj) <- MyAttribs
    return(dgeObj)
}
