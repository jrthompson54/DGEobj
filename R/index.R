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


### Function annotateDGEobj ###
#' Function annotateDGEobj
#'
#' Read an Omicsoft Registration file and attach attributes to the DGEobj.
#'
#' Omicsoft registration files contain a [SampleSet] section that contains
#' metadata about a project.  These are key/value pairs separated by an equals
#' sign.  The keys parameter specifies which key we want to capture as
#' attributes on the DGEobj.
#'
#' As long as you provide a text file with key/value pairs separated by equals
#' sign, this function should work.
#'
#' @author John Thompson, \email{john.thompson@@bms.com}
#' @keywords RNA-Seq, DGEobj, annotation, attributes
#'
#' @param dgeObj  A class dgeObj created by function initDGEobj
#' @param regfile An omicsoft registration file (text not Excel!)
#' @param keys A list of keys to look for and transfer to the DGEobj
#'
#' @return A DGEobj annotated with attributes.
#'
#' @examples
#'    MyDgeObj <- annotateDGEobj(DGEobj, regfile)
#'
#' @import magrittr assertthat
#'
#' @export
annotateDGEobj <- function(dgeObj, regfile,
                           keys = list("ID",
                                       "BMS_PID",
                                       "Title",
                                       "Description",
                                       "Keywords",
                                       "Business Unit",
                                       "Functional Area",
                                       "Disease",
                                       "Vendor",
                                       "PlatformType",
                                       "Technology",
                                       "LibraryPrep",
                                       "Organism",
                                       "Tissue",
                                       "AlignmentReference",
                                       "GeneModel",
                                       "TBio_Owner",
                                       "TA_Owner",
                                       "LoadData",
                                       "ReadLength",
                                       "ReadType",
                                       "Pipeline",
                                       "AlignmentAlgorithm",
                                       "ScriptID"
                           )){


    # Read lines, stripping quotes
    regdat <- read.delim(regfile, sep="\t",
                         quote = "\"",
                         stringsAsFactors = FALSE)
    #just first column
    regdat <- regdat[,1, drop=FALSE]
    colnames(regdat) <- "pair"
    #just lines with equals signs
    regdat <- filter(regdat, grepl("=", pair), !grepl("Parameters.", pair))
    #now split into key value pairs
    regdat <- strsplit(regdat$pair, "=") %>% as.data.frame() %>% t
    colnames(regdat) <- c("key", "value")

    MyAttribs <- list()
    for (i in 1:nrows(regdat))
        if (regdat$key[i] %in% keys)
            MyAttribs[regdat$key[i]] <- regdat$value[i]

    #now attach the attributes to the DGEobj
    attributes(dgeObj) <- MyAttribs
    return(dgeObj)
}

