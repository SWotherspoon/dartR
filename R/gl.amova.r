##' Performs an AMOVA using genlight data.
##'
##' This script performs an AMOVA based on the genetic distance matrix
##' from stamppNeisD() [package StAMPP] using the amova() function
##' from the package PEGAS for exploring within and between population
##' variation. For detailed information see the help pages for amova
##' and stamppAmova
##' @title Analysis of Molecular Variance
##' @param x a genlight object containing the SNP genotypes and
##'   population grouping
##' @param nperm number of permuations to perform for hypothesis
##'   testing.
##' @return An object of class \code{pegas::amova}.
##' @seealso \code{\link[pegas]{amova}}, \code{\link[StAMPP]{stamppNeisD}}
##' @author Bernd Gruber (bugs? Post to \url{https://groups.google.com/d/forum/dartr})
##' @importFrom StAMPP stamppNeisD
##' @importFrom pegas amova
##' @export
gl.amova <- function(x, nperm=100) {

  dist <- stamppNeisD(x, pop=FALSE)
  pegas::amova(dist~pop,data=data.frame(pop=pop(x)))
}

