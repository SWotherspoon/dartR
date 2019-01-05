##' Filter secondary SNPs from a genlight object
##'
##' SNP datasets generated by DArT include fragments with more than one
##' SNP and record them separately with the same CloneID. These
##' multiple SNP loci within a fragment (secondaries) are likely to be
##' linked, and are often removed.  This script filters out secondary
##' loci, retaining either the loci with greatest repeatability and
##' avgPIC (method="best"), a random loci (method="random") or the
##' first loci encountered (method="first").
##'
##' @param gl a genlight object
##' @param method the method of selecting SNP locus to retain, best, random or first
##' @return The reduced genlight object
##' @author Arthur Georges (Post to \url{https://groups.google.com/d/forum/dartr})
##' @export
##' @examples
##' result <- gl.report.secondaries(testset.gl)
##' result2 <- gl.filter.secondaries(testset.gl)
gl.filter.secondaries <- function(gl, method=c("random","best","first")) {

  method <- match.arg(method)
  index <- switch(method,
                  "best"=order(-gl@other$loc.metrics$RepAvg,-gl@other$loc.metrics$AvgPIC),
                  "random"=sample(nInd(gl)),
                  "first"=seq_len(nInd(gl)))
  keep <- !duplicated(gl@other$loc.metrics$clone[index])[order(index)]

  ## Only subset if necessary
  if(!all(keep)) {
    gl <- gl[,keep]
    gl@other$loc.metrics <- gl@other$loc.metrics[keep, ]
  }
  gl
}
