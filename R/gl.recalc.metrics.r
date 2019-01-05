##' Recalculates the locus metrics of a genlight object
##'
##' If individuals are deleted from a genlight object, the cached
##' locus metrics no longer valid. This function recalculates the locus
##' metrics avgPIC, CallRate, freqHets, freqHomRef, freqHomSnp,
##' OneRatioRef, OneRatioSnp, PICRef and PICSnp.
##' @title Recalculate Locus Metrics
##' @param gl a genlight object
##' @return Returns a genlight object with the recalculated locus metadata.
##' @author Arthur Georges (bugs? Post to \url{https://groups.google.com/d/forum/dartr})
##' @export
gl.recalc.metrics <- function(gl){


  if (is.null(gl@other$loc.metrics)) {
    gl@other$loc.metrics <- data.frame(id=indNames(gl))
  }

  # Recalculate statistics
  gl <- utils.recalc.callrate(gl)
  gl <- utils.recalc.freq(gl)
  gl <- utils.recalc.avgpic(gl)
  gl <- utils.recalc.rdepth(gl)

  gl
}
