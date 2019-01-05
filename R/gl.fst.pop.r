##' Calculate a pairwise fst values for populations in a genlight object
##'
##' Calculate pairwise Fst values along with confidence intervals and
##' p-values between populations according to the method proposed by
##' Wright(1949) and updated by Weir and Cockerham (1984).
##'
##' This is a wrapper for \code{stamppFst} function from the \pkg{StAMPP}
##'
##' @param gl a genlight object
##' @param nboots number of bootstraps to perform across loci to generate confidence intervals and p-values
##' @param percent the percentile to calculate the confidence interval around [defalut = 95]
##' @param nclusters the number of proccesor threads or cores to use during calculations.
##' @return A list with components
##' \item{\code{Fsts}}{a matrix of pairwise Fst values between populations}
##' \item{\code{Pvalues}}{a matrix of p-values for each of the pairwise Fst values containined in the 'Fsts' matrix}
##' \item{\code{Bootstraps}}{a dataframe of each Fst value generated during Bootstrapping and the associated confidence intervals}
##' If nboots<2, no bootstrapping is performed and therefore only a matrix of Fst values is returned.
##' @seealso \code{\link[StAMPP]{stamppFst}}
##' @importFrom StAMPP stamppFst
##' @author Bernd Gruber (bugs? Post to \url{https://groups.google.com/d/forum/dartr})
##' @export
##' @examples
##' gl.fst.pop(possums.gl, nboots=1)
gl.fst.pop <- function(gl, nboots=100, percent=95, nclusters=1) {
  stamppFst(gl, nboots=nboots, percent=percent, nclusters = nclusters)
}
