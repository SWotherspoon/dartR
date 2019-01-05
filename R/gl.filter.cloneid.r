##' Filter to select only one instance of each clone.
##'
##' @title Filter Clones
##' @param gl a genlight object
##' @return A genlight object containing only unique clones.
##' @author Bernd Gruber (bugs? Post to \url{https://groups.google.com/d/forum/dartr})
##' @export
gl.filter.cloneid <- function(gl) {
  keep <- !duplicated(gl@other$loc.metrics$clone)
  if(!all(keep)) {
    gl <- gl[, keep]
    gl@other$loc.metrics <- gl@other$loc.metrics[keep,]
  }
  gl
}


