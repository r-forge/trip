## $Id$

TimeOrderedRecords <- function(x) {
    new("TimeOrderedRecords", TOR.columns=x)
}

getTORnames <- function(obj) obj@TOR.columns

getTimeID <- function(obj) as.data.frame(obj)[, getTORnames(obj)]
