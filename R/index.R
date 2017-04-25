### Function indexType ###
#' Function indexType
#'
#' Return a dataframe containing index entries for the items in dgeObj of
#' the specified type.   Default columns returned are ItemName, ItemType, BaseType,
#' Parent and DateCreated.  Additional attribute columns will be added as specified
#' by the moreAttr argument.  Use names(attributes(dgeObj)) to see the attributes
#' available in the DGEobj.  You should only supply attributes that return
#' character values (not lists or vectors).
#'
#' @author John Thompson, \email{john.thompson@@bms.com}
#' @keywords RNA-Seq
#'
#' @param dgeObj A a DGEobj class object
#' @param type  Name of the type to index
#' @param moreAttr Additional attributes of the DGEobj to capture as columns
#'   in the returned dataframe. You should only supply attributes that return
#'   character values (not lists or vectors).
#'
#' @examples
#'    # Index topTable contrast data
#'    MyIndexDF <- indexType(dgeObj, "topTable")
#'
#' @import assertthat magrittr
#'
#' @export
indexType <- function(dgeObj, type, moreAttr){
    #return a dataframe of metadata about the specified data types
    assert_that(!missing(dgeObj),
                !missing(type),
                class(dgeObj)[[1]] == "DGEobj",
                class(type)[[1]] %in% list("character", "list")
    )

    #get index for defined type items
    idx <- attr(dgeObj, "type") %in% type
    if (sum(idx) >0){
        #Get item attributes
        df <- summarize(dgeObj)
        df %<>% dplyr::filter(ItemType %in% type)
        df$Level <- attr(dgeObj, "level")

        #Get DGEobj attributes (common to all items in DGEobj)
        if (!missing(moreAttr))
            for (i in moreAttr)
                df[i] <- attr(dgeObj, i)

        return(df)
    } else {
        paste("No objects of type", type, "were found in the DGEobj!", sep=" ")
        return(NULL)
    }
}
