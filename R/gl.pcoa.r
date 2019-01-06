##' Perform a PCoA of the genetic data in a genlight object.
##'
##' @title PCoA ordination
##' @param gl a genlight object
##' @param nfactors Number of dimensions to retain in the ordination.
##' @return An object of class glPca containing the eigenvalues, factor scores and factor loadings.
##' @author Arthur Georges (bugs? Post to \url{https://groups.google.com/d/forum/dartr})
##' @export
##' @examples
##' pcoa <- gl.pcoa(testset.gl, nfactors=3)

gl.pcoa <- function(gl, nfactors=5) {

  cl <- match.call()
  x <- as.matrix(gl)
  x <- scale(as.matrix(gl),center=TRUE,scale=rep(2,ncol(x)))
  x[is.na(x)] <- 0
  ## Compute PCA by SVD
  nv <- min(nfactors,nrow(x),ncol(x))
  s <- svd(x,0,nv)
  rank <- sum(s$d^2 > 1.0e-12)
  s$d <- s$d[seq_len(rank)]
  scores <- x%*%s$v
  colnames(scores) <- paste0("PC",1:ncol(s$v))
  colnames(s$v) <- paste0("Axis",1:ncol(s$v))
  if(!is.null(locNames(gl)) & !is.null(alleles(gl)))
    rownames(s$v) <- paste(locNames(gl), alleles(gl), sep = ".")

  structure(
    list(eig=s$d[-length(s$d)]^2/nrow(x),
         scores=scores,
         loadings=s$v,
         call=cl),
    class="glPca")
}

