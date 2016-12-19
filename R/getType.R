#' @export
#Return all items of a specified type as a list
getType <- function(dgeObj, type){
    #ype can be a single named type or a vector or list of types
    idx <- dgeObj$type %in% type
    if (sum(idx) == 1)
        result <- dgeObj$data[idx][[1]]
    else
        result <- dgeObj$data[idx]

    if (sum(idx) < length(type))
        warning("Some types were not found")

    return(result)
}
