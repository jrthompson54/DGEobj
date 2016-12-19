#' @export
### getItem
getItem <- function(dgeObj, itemName){
    #itemName can be a single element or a vector of item names.
    # Return a single element or a list if more than one element

    idx <- names(dgeObj$data) %in% itemName
    if (sum(idx) == 1)
        result <- dgeObj$data[idx][[1]] #single element
    else {
        result <- dgeObj$data[idx]  #list of elements
        if (length(result) < length (itemName))
            warning("Some requested items were missing!")
    }
    return(result)
}
