##' Calculate the OneRatioRef, OneRatioSnp, PICRef, PICSnp, and AvgPIC by locus.
##'
##' The locus metadata supplied by DArT has OneRatioRef, OneRatioSnp,
##' PICRef, PICSnp, and AvgPIC included, but the allelec composition
##' will change when some individuals are removed from the dataset and
##' so the initial statistics will no longer apply. This script
##' recalculates these statistics and places the recalculated values
##' in the appropriate place in the genlight object.
##'
##' @title Calculate AvgPIC
##' @param gl a genlight object
##' @return The modified genlight object
##' @author Arthur Georges (Post to \url{https://groups.google.com/d/forum/dartr})
utils.recalc.avgpic <- function(gl) {

  x <- as.matrix(gl)
  m <- nrow(x)
  n <- ncol(x)
  c0 <- .colSums(x==0L,m,n,na.rm=TRUE)
  c1 <- .colSums(x==1L,m,n,na.rm=TRUE)
  c2 <- .colSums(x==2L,m,n,na.rm=TRUE)
  s <- (c0+c1+c2)
  ZeroRatioRef <- c2/s
  ZeroRatioSnp <- c0/s
  OneRatioRef <- 1-ZeroRatioRef
  OneRatioSnp <- 1-ZeroRatioSnp
  PICRef <- 1 - (OneRatioRef^2 + ZeroRatioRef^2)
  PICSnp <- 1 - (OneRatioSnp^2 + ZeroRatioSnp^2)
  AvgPIC <- (PICRef + PICSnp)/2

  gl@other$loc.metrics$OneRatioRef <- OneRatioRef
  gl@other$loc.metrics$OneRatioSnp <- OneRatioSnp
  gl@other$loc.metrics$PICRef <- PICRef
  gl@other$loc.metrics$PICSnp <- PICSnp
  gl@other$loc.metrics$AVgPIC <- AvgPIC

  gl
}
