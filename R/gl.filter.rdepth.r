##' Filter loci in a genlight \{adegenet\} object based on counts of
##' sequence tags scored at a locus (read depth)
##'
##' SNP datasets generated by DArT report AvgCountRef and AvgCountSnp
##' as counts of sequence tags for the reference and alternate alleles
##' respectively. These can be added for an index of Read
##' Depth. Filtering on Read Depth can be on the basis of loci with
##' exceptionally low counts, or loci with exceptionally high counts.
##'
##' @title Filter Read Depth
##' @param gl a genlight object
##' @param lower lower read depth threshold
##' @param upper upper read depth threshold
##' @return a genlight object
##' @author Arthur Georges (Post to \url{https://groups.google.com/d/forum/dartr})
##' @examples
##' gl.report.rdepth(testset.gl)
##' result <- gl.filter.rdepth(testset.gl, lower=8, upper=50)
##' @export
gl.filter.rdepth <- function(gl, lower=5, upper=50) {

  gl <- utils.recalc.rdepth(gl)
  keep <- (gl@other$loc.metrics["rdepth"]>=lower & gl@other$loc.metrics["rdepth"]<=upper)

  ## Only subset if necessary
  if(!all(keep)) {
    gl <- gl[,keep]
    gl@other$loc.metrics <- gl@other$loc.metrics[keep,]
  }
  gl
}
