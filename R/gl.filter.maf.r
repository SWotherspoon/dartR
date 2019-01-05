##' Filter loci on the basis of minor allele frequency (MAF)
##'
##' This script calculates the minor allele frequency for each locus and updates the locus
##' metadata for FreqHomRef, FreqHomSnp, FreqHets and MAF (if it exists). It then uses
##' the updated metadata for MAF to filter loci.
##'
##' Note the this filter applies to MAF calculated across all individuals, without regard
##' to population structure. It is a means of removing overall rare alleles. To apply this to
##' single populations, use sepPop and lapply.
##'
##' @param gl a genlight object
##' @param threshold loci with a MAF less than this threshold will be removed
##' @return The reduced genlight dataset
##' @author Arthur Georges (Post to \url{https://groups.google.com/d/forum/dartr})
##' @export
##' @examples
##' f <- gl.filter.maf(testset.gl, threshold=0.05)
gl.filter.maf <- function(gl, threshold=0.01) {

  gl <- utils.recalc.freq(gl)
  # Remove loci with NA count <= 1-threshold
  keep <- gl@other$loc.metrics$maf >= threshold
  if(!all(keep)) {
    gl <- gl[,keep]
    gl@other$loc.metrics <- gl@other$loc.metrics[keep,]
  }
  gl
}
