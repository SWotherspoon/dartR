#' Calculates the genomic relatedness matrix
#'
#' The G matrix is calculated by centering the allele frequency matrix  of the second allele by substracting 2 times the allefrequency
#'@param gl -- a genlight object
#'@param plotheatmap -- a switch if a heatmap should be shown [Default:TRUE]
#'@param return.imputed switch if loci with imputed data should be returned (see ?A.mat in package rrBLUP)
#'@param ... parameters passed to function A.mat from package rrBLUP
#'@return a genomic relatedness matrix
#'@importFrom stats heatmap cov var
#'@importFrom rrBLUP A.mat
#'@export
#'
#'@examples
#'gl.grm(foxes.gl[1:5,1:10])


gl.grm <- function(gl, plotheatmap=TRUE, return.imputed=FALSE, ...) {
  G <- A.mat(as.matrix(gl)-1,return.imputed = return.imputed, ...)
  if (plotheatmap & return.imputed==FALSE) heatmap(G) else heatmap(G$A)
  G
}
