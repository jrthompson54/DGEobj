#' @export
### UpdateDGEobjDef
# Add the current (or specified) data definition obj to an existing DGEobj
# }
UpdateObjDef <- function(dgeObj, DGEdef=.DGEobjDef){
    dgeObj$objDef <- DGEdef
    return(dgeObj)
}
