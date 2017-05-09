#function in this file relate to annotating DGEobjs and creating a contrast
#database.
#Plans are to split this out into a contrastDB package.

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
#' This function should work as long as you provide a text file with key/value
#' pairs separated by equals sign.
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
#' @import magrittr assertthat stringr
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
    regdat <- dplyr::filter(regdat, str_detect(pair, "="), !str_detect(pair, "Parameters."))
    #now split into key value pairs
    regdat <- strsplit(regdat$pair, "=") %>%
        as.data.frame(stringsAsFactors=FALSE) %>%
        t %>%
        as.data.frame(stringsAsFactors=FALSE)
    colnames(regdat) <- c("key", "value")

    #capture/preserve the existing attributes
    MyAttribs <- attributes(dgeObj)

    for (i in 1:nrow(regdat))
        if (regdat$key[i] %in% keys)
            MyAttribs[regdat$key[i]] <- regdat$value[i]

    #now attach the attributes to the DGEobj
    attributes(dgeObj) <- MyAttribs
    return(dgeObj)
}

### Function indexDGEobj ###
#' Function indexDGEobj
#'
#' Read Retrieve specified attributes from a DGEobj as a vector
#'
#' @author John Thompson, \email{john.thompson@@bms.com}
#' @keywords RNA-Seq, DGEobj, annotation, attributes
#'
#' @param dgeObj  A class dgeObj created by function initDGEobj
#' @param keys A list of attribute keys to retrieve
#'
#' @return A row of DGEobj annotation (vector)
#'
#' @examples
#'    MyDgeObj <- indexDGEobj(DGEobj, regfile)
#'
#' @import magrittr assertthat
#'
#' @export
indexDGEobj <- function(dgeObj,
                        keys = list("ID", #this is all  be a few of the keys implemented in annotateDGEobj
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
                                    "Pipeline"
                        )){

    assert_that(class(dgeObj)[[1]] == "DGEobj",
                class(keys)[[1]] == "list")

    #get attributes
    DGE_attributes <- attributes(dgeObj)
    #remove unwanted attributes
    for (a in names(DGE_attributes))
        if (!a %in% keys)
            DGE_attributes[a] <- NULL

    #keep attributes with character data
    for (i in 1:length(DGE_attributes))
        if(class(DGE_attributes[[i]])[[1]] != "character")
            DGE_attributes[i] <- NULL

    return(DGE_attributes)

}

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
        df <- inventory(dgeObj)
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


### Function buildContrastDB ###
#' Function buildContrastDB
#'
#' Build a constrast database from a set of DGEObjs in a specified input folder.
#' Write the database files to the output folder.  Optionally, skip files
#' already indexed.
#'
#' For addition to the database, DGEobjs should be fully annotated by running
#' annotateDGEobj first and then saved as .RDS files and placed in the inputPath.
#'
#' Each time buildContrastDB is run, it skips RDS files that have already
#' been indexed.  Use rebuild=TRUE to force indexing of all RDS files.
#'
#' @author John Thompson, \email{john.thompson@@bms.com}
#' @keywords RNA-Seq, DGEobj, annotation, attributes
#'
#' @param inputPath folder containing DGEobjects saved in .RDS files
#' @param outputPath folder for the output database tables
#' @param rebuild Force complete rebuilding if TRUE. Default behavior (FALSE)
#' performs an update, only indexing new .RDS files that have not been previously
#' indexed.
#'
#' @return TRUE (success) or FALSE (Failure)
#'
#' @examples
#'    success <- buildContrastDB("~/R/RDS", ~/R/contrastDB")
#'
#' @import magrittr assertthat dplyr
#'
#' @export
buildContrastDB <- function(inputPath, outputPath, rebuild=FALSE){

    .catContrastList <- function(topTableList, contrastData, pid){
        #concatenate topTables with the topTable name added as an identifying column
        for (tname in names(topTableList)){
            key <- paste(tname, pid, sep="_")
            tempTable <- topTableList[[tname]]
            tempTable$key <- key
            contrastData <- rbind(contrastData, tempTable)
        }
    }

    assert_that(file.exists(inputPath),
                file.exists(outputPath))

    #initilize or load ProjectIdx, ContrastIdx, ContrastData dataframes
    if (rebuild == TRUE){
        #initialize
    } else {
        #load from RDS files in outputPath
        projectIdx <- readRDS(file.path(outputPath, "projectIdx.RDS"))
        contrastIdx <- readRDS(file.path(outputPath, "contrastIdx.RDS"))
        contrastData <- readRDS(file.path(outputPath, "contrastData.RDS"))
    }

    errlog <- list()
    failCount <- 0

    # Get a list of .RDS files to process
    rdsPattern <- ".*\\.rds$"
    rdsFiles <- list.files(inputPath, pattern=rdsPattern, ignore.case=TRUE)

    #skip prior indexed files
    if (file.exists(file.path(outputPath, "projects.txt"))){
        projects <- read.delim(file.path(outputPath, "projects.txt"))
        rdsFiles <- setdiff(rdsFiles, projects$filename)
    }

    for (dfile in rdsFiles){
        dgeObj <- readRDS()
        if(class(dgeObj)[[1]] != "DGEobj"){
            errlog$classErr <- paste(dgeObj, "does not contain a DGEobj.", sep=" ")
            failCount <- failCount + 1
        } else { #index this object
            thisContrastIdx <- indexType(dgeObj, "topTable")
            contrastIdx <- rbind(contrastIdx, thisContrastIdx)

            thisProjectIdx <- indexDGEobj(dgeObj)
            projectIdx <- rbind(projectIdx, thisProjectIdx)

            contrastData <- .catContrastList(topTableList=getType(dgeObj, "topTable"),
                                             contrastData=contrastData,
                                             pid = thisProjectIdx$BMS_PID
                                             )
        }
    }

## Loop among a list of DGEobj

## Need to define a key to connect contrastIndex to projectIndex
##      projectIdx$ID the the unique key in projectIdx
##      contrastIdx$ProjectKey = projectIdx$ID
##      contrastIdx$key = paste(contrastName, )
##
## need to define a key to connect contrastIndex to contrastData
##  Project 1toM to Contrasts is 1toM to contrastData
##
## Add queryContrast function (given a list of contrastID, pull out a tall skinny result table.)
##
## Use rbind.fill to merge new rows that might not be in the same order
}
