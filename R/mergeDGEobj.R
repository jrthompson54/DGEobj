### Function mergeDGEobj ###
#' Function mergeDGEobj (DGEobj)
#'
#' Merge two DGEobj.  This will effectively cbind the counts from expt2 onto
#' expt1.  You can choose between inner join and left join when combining the
#' data.  This will **NOT** try to capture the workflows from both objects.  Rather,
#' it will capture just the counts, row and col annotation and merge those items
#' into a new combined DGEobj.  Row (gene) annotation will be taken from the 1st
#' DGEobj.  Sample annotation must have at least one common colname to merged.
#' Only columns present in both datasets will be kept.
#'
#' @author John Thompson, \email{john.thompson@@bms.com}
#' @keywords RNA-Seq, DGEobj
#'
#' @param d1  The first DGEobj (required)
#' @param d2  The second DGEobj (required)
#' @param join The join type to use (default = "inner"). Values of inner and
#' left are supported.
#' @param orig A DGEobj is typically filtered to remove non-expressed genes.  However,
#' a copy of the original (unfiltered) data is still stored.  By default,
#' this function will merge the **original unfiltered** data for these two DGEobjs. Set orig=FALSE
#' to disable this behavior and thus attempt to merge the already filtered data.
#'
#' @return A DGEobj class object with the merged data.
#'
#' @examples
#'    MyCombinedDGEobj <- mergeDGEobj(dgeObj1, dgeObj2, join="inner", orig=TRUE)
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr left_join
#'
#' @export
mergeDGEobj <- function(d1, d2, join="inner", orig=TRUE){

        assertthat::assert_that(!missing(d1),
                    !missing(d2),
                    class(d1)[[1]] == "DGEobj",
                    class(d2)[[1]] == "DGEobj",
                    toupper(join) %in% c("INNER", "LEFT")
                    )

        suffix <- ""
        if (orig == TRUE)
            suffix <- "_orig"

        #Check for same Level in both datasets
        if (attr(d1, "level") != attr(d2, "level")){
            tsmsg(paste("d1 level =", attr(d1, "level")))
            tsmsg(paste("d2 level =", attr(d2, "level")))
            error("d1 and d2 are not the same level.")
        } else {
            level <- attr(d1, "level")
        }

        #get the data we need to combine
        c1 <- getItem(d1, paste("counts", suffix, sep="")) %>% as.data.frame
        g1 <- getItem(d1, paste("geneData", suffix, sep=""))
        design1 <- getItem(d1, paste("design", suffix, sep=""))

        c2 <- getItem(d2, paste("counts", suffix, sep="")) %>% as.data.frame
        design2 <- getItem(d2, paste("design", suffix, sep=""))

        #get the combined genelist first, then merge data from d1, d2
        id1 <- rownames(c1)
        id2 <- rownames(c2)

        switch(toupper(join),
               "INNER" = ids <- id1[id1 %in% id2],
               "LEFT" = ids <- id1
               )
        #ids holds the list of ids for merged data

        #merged gene data (from d1)
        geneData <- g1[rownames(g1) %in% ids,]

        #merge Design data (rbind common columns)
        if (!any(colnames(design1) == colnames(design2)))
            error('No common columns in design data!')
        #rownames in the combined data must be unique
        if(any(rownames(design1) %in% rownames(design2))){
            warning("SampleName clash.  Adding \"_2\" suffix to SampleNames in d2")
            rownames(design2) <- paste(rownames(design2), "_2", sep="")
            colnames(c2) <- paste(colnames(c2), "_2", sep="")
        }

        #filter to common columns and rbind
        idx <- colnames(design1) %in% colnames(design2)
        commonColNames <- colnames(design1)[idx]
        design1 <- design1[,commonColNames]
        design2 <- design2[,commonColNames]
        design <- rbind(design1, design2)

        #merge Count data
        #filter both for commonGenes
        #make rownames a col so we can use dplyr
        c1$.ID <- rownames(c1)
        c2$.ID <- rownames(c2)
        counts <- dplyr::left_join(c1, c2, by=".ID")
        #put the rownames back
        rownames(counts) <- counts$.ID
        counts$.ID = NULL
        counts <- as.matrix(counts)

        #Sort Genes to make sure geneData and counts are properly sorted.
        idx <- sort(rownames(geneData), index.return=TRUE)$ix
        geneData <- geneData[idx,]
        idx <- sort(rownames(counts), index.return=TRUE)$ix
        counts <- counts[idx,]

        #Build the DGEobj
        dgeObj <- initDGEobj(counts=counts,
                             rowData=geneData,
                             colData=design,
                             level=level)

        return(dgeObj)
}
