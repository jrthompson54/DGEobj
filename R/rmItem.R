#' @export
### rmItem
rmItem <- function(dgeObj, itemName){
    assert_that(class(dgeObj) == "DGEobj",
                !missing(itemName),
                class(itemName) == "character")

    #item not found
    if (!itemName %in% names(dgeObj$data))
        stop(paste(itemName, " does not exist within DGEresult.", sep=""))

    dgeObj$data[itemName] <- NULL
    attr(dgeObj, "type")[[itemName]] <- NULL
    attr(dgeObj, "basetype")[[itemName]] <- NULL
    attr(dgeObj, "dateCreated")[[itemName]] <- NULL
    attr(dgeObj, "funArgs")[[itemName]] <- NULL

    # dgeObj$type[itemName] <- NULL
    # dgeObj$basetype[itemName] <- NULL
    # dgeObj$dateCreated[itemName] <- NULL
    # dgeObj$funArgs[itemName] <- NULL

    return(dgeObj)
}
