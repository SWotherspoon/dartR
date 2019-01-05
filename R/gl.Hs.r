##' Calculate the expected heterozygosity for each loci.
##'
##'
##' @title Expected Heterozygosity.
##' @param gl a genlight object
##' @return The expected heterozygosity for each loci as a vector.
##' @author Bernd Gruber (bugs? Post to \url{https://groups.google.com/d/forum/dartr})
##' @export
gl.Hs <- function(gl) {
  alf <- colMeans(as.matrix(gl), na.rm = TRUE)/2
  alf*(1-alf)*2
}
