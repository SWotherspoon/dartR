##' Calculate the observed heterozygosity for each loci.
##'
##' @title Observed Heterozygosity.
##' @param gl a genlight object
##' @return The observed heterozygosity for each loci as a vector.
##' @author Bernd Gruber (bugs? Post to \url{https://groups.google.com/d/forum/dartr})
##' @export
gl.Ho <- function(gl) {
  colMeans(as.matrix(gl)==1L,na.rm=TRUE)
}
