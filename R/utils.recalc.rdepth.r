#' Calculate the read depth by locus
#'
#' The locus metadata supplied by DArT does not have read depth
#' included, so it is calculated and added to the locus metrics by
#' this script. It calculates the read depth as the sum of the average
#' counts for the reference and alterate alleles.
#'
#' @param x a genlight object
#' @return The modified genlight object
#' @author Arthur Georges (Post to \url{https://groups.google.com/d/forum/dartr})
#' @examples
#' #f <- utils.recalc.rdepth(testset.gl)
utils.recalc.rdepth <- function(gl) {
  gl@other$loc.metrics$rdepth <- gl@other$loc.metrics$AvgCountRef + g@other$loc.metrics$AvgCountSnp
  gl
}
