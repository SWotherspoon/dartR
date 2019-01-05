##' Converts a genlight object to plink format and writes it to a file
##' or connection.
##'
##' @title Convert Genlight to Plink
##' @param gl a genlight object.
##' @param file file or connection to write to.
##' @return Returns an object of class \code{snp} to be used with \pkg{SNPassoc}.
##' @author Bernd Gruber (bugs? Post to \url{https://groups.google.com/d/forum/dartr})
##' @export
gl2plink <- function(gl, file="plink.csv") {
  write.csv(as.matrix(gl), file=file, row.names = TRUE, na = "-9")
}












