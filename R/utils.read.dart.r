##' Read DarT data in csv format.
##'
##' This is an internal function called by \code{gl.read.dart}.
##' @title Import DarT data as a list.
##' @param filename DarT data in csv format.
##' @param na.strings strings representing NAs.
##' @param skip number of lines before the row of column names. If \code{NULL} it is diagnosed from the file.
##' @param last.metric the name of the last column of preceeding the genetic data
##' @param nrows the DarT format.  If \code{NULL} it is diagnosed from the file.
##' @return a list with elements
##' \item{nrows}{number of rows per loci in DarT genetic data}
##' \item{nind}{number of individuals}
##' \item{nsnp}{number of snps}
##' \item{covmetrics}{non-genetic metrics}
##' \item{gendata}{genetic data}
##' @seealso \code{\link{gl.read.dart}}
##' @author Bernd Gruber (bugs? Post to \url{https://groups.google.com/d/forum/dartr})
##' @export
utils.read.dart <- function(filename,na.strings="-",skip=NULL,last.metric="RepAvg",nrows=NULL) {

  if (is.null(skip)) {
    skip <- max(0,grep("^\\s*\\*",readLines(filename,n=20)))
    warnings("Skipping ",skip," header rows")
  }
  snpraw <- read.csv(filename,na.strings=na.strings,skip=skip,check.names=FALSE)

  if(is.character(last.metric)) {
    lmet <- match(last.metric,colnames(snpraw))
    if(is.na(lmet)) stop("Could not find last data column ", last.metric)
  } else
    lmet <- last.metric

  ind.names <- trimws(colnames(snpraw)[(lmet+1):ncol(snpraw)],which="both")
  if(any(duplicated(ind.names)))
    stop("Non-unique individual identifiers")

  gendata <- snpraw[,lmet+seq_len(ncol(snpraw)-lmet)]


  ## Guess nrows if not provided
  if (is.null(nrows)) {
    nrows <- 3 - max(gendata, na.rm = TRUE)
    if(nrows %in% 1:2)
      warning("nrows not provided. Assuming ",nrows," rows")
    else
      stop("Could not determine nrows")
  }

  covmetrics <-  snpraw[,seq_len(lmet)]

  clone <- sub("\\|.*","",covmetrics$AlleleID)
  spp <- sub("[^-]+-+(\\d{1,3}):.+","\\1",covmetrics$AlleleID)

  covmetrics$clone <- factor(clone)
  covmetrics$uid <- factor(paste(clone, spp,sep="-"))
  nind <- ncol(gendata)
  nsnp <- nrow(covmetrics)/nrows

  list(nrows=nrows,nind=nind,nsnp=nsnp,covmetrics=covmetrics,gendata=gendata)
}
