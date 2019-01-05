##' Calculates allele frequencies of the first and second allele for
##' each loci.
##'
##' @title Allelle Frequency
##' @param gl a genlight object
##' @return A dataframe with columns
##' \item{\code{alf1}}{frequency of the reference allele for each loci}
##' \item{\code{alf2}}{frequency of the reference allele for each loci}, alf2
##' @author Bernd Gruber (bugs? Post to \url{https://groups.google.com/d/forum/dartr})
##' @export
gl.alf <- function(gl) {
  alf <- colMeans(as.matrix(gl), na.rm = T)/2
  data.frame(alf1=1-alf, alf2=alf)
}
