##' Converts a genind object to genlight object.
##'
##' Note that \code{gl2gi(gi2gl(x))} may not return the original
##' object due to the ambiguity in choice of the reference alleles.
##' @title Genind to genlight conversion
##' @param gi a genind object
##' @param parallel Switch passed to genlight constructor.
##' @return A genlight object, with all slots filled.
##' @seealso \code{\link{gi2gl}}, \code{\link[adegenet]{genlight}}
##' @author Bernd Gruber (bugs? Post to \url{https://groups.google.com/d/forum/dartr})
##' @export
gi2gl <- function(gi, parallel=TRUE) {
  locna <- unname(gi@loc.n.all)
  new("genlight", gi@tab[,cumsum(c(1,locna[-length(locna)]))], pop = pop(gi), other=gi@other, ploidy=2, loc.names=locNames(gi), ind.names=indNames(gi), parallel=parallel)
}

