##' Remove monomorphic loci, including those with all NAs
##'
##' A DArT dataset will not have monomorphic loci, but they can arise
##' when populations are deleted by assignment or by using the delete
##' option in gl.pop.recode(). Retaining monomorphic loci
##' unnecessarily increases the size of the dataset.
##'
##' @param gl a genlight object
##' @return A genlight object with monomorphic loci removed
##' @author Arthur Georges (Post to \url{https://groups.google.com/d/forum/dartr})
##' @export
##' @examples
##' gl <- gl.filter.monomorphs(testset.gl)
gl.filter.monomorphs <- function (gl) {

  x <- as.matrix(gl)
  keep <- !(colSums(x!=0L,na.rm=TRUE)==0 | colSums(x!=2L,na.rm=TRUE))

  ## Only subset if necessary
  if(!all(keep)) {
    gl <- gl[,keep]
    gl@other$loc.metrics <- gl@other$loc.metrics[keep,]
  }
  gl
}

