##' Calculate the the various allele frequency by locus.
##'
##' Recalculates frequencies of the heterozygous SNP and the
##' homozygous reference and alternate SNP. The locus metadata
##' supplied by DArT does not have MAF included, so it is calculated
##' and added to the metadata.
##'
##'
##' @title Calculate Allele Frequencies
##' @param gl a genlight object
##' @return the modified genlight object
##' @author Arthur Georges (Post to \url{https://groups.google.com/d/forum/dartr})
##' @examples
##' #f <- dartR:::utils.recalc.freq(testset.gl)
utils.recalc.freq <- function(gl) {

  x <- as.matrix(gl)
  m <- nrow(x)
  n <- ncol(x)
  freq <- .colMeans(x,m,n,na.rm=TRUE)/2
  gl@other$loc.metrics$maf <- pmin(freq,1-freq)
  gl@other$loc.metrics$FreqHomRef <- .colMeans(x==0L,m,n,na.rm=TRUE)
  gl@other$loc.metrics$FreqHets <- .colMeans(x==1L,m,n,na.rm=TRUE)
  gl@other$loc.metrics$FreqHomSnp <- .colMeans(x==2L,m,n,na.rm=TRUE)
  gl
}
