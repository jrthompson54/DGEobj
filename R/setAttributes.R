### Function setAttributes ###
#' Function setAttributes
#'
#' Set one or more attributes on an object
#'
#' @author John Thompson, \email{john.thompson@@bms.com}
#' @keywords RNA-Seq, DGEobj
#'
#' @param item  Anything you want to attach attrbutes to
#' @param attribs A named list of attribute/value pairs
#'
#' @return the item with new attributes
#'
#' @examples
#'    #assign attributes to a DGEobj
#'    MyAttributes <- list(Platform = "RNA-Seq",
#'            Instrument = "HiSeq",
#'            Vendor = "BMS",
#'            readType = "PE",
#'            readLength = 75,
#'            strandSpecific = TRUE)
#'    MyDGEobj <- setAttributes(MyDGEObj, MyAttributes)
#'
#'    #set attributes on an item inside a DGEobj
#'    MyAttributes <- list(normalized = FALSE,
#'                         LowIntFilter = "FPK >5 in >= 1 group"),
#'    MyDGEObj$data[["counts"]] <- setAttributes(MyDGEObj$data[["counts"]], MyAttributes)
#'
#' @import magrittr assertthat dplyr knitr
#'
#' @export
setAttributes <- function(item, attribs){

    assert_that(!missing(item),
                !missing(attribs),
                class(attribs)[[1]] == "list",
                !is.null(names(attribs))
    )

    for (i in 1:length(attribs))
        attr(item, names(attribs[i])) <- attribs[[i]]
    return(item)
}


