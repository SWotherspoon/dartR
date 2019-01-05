#' Calculates the Hamming distance between two DArT trimmed DNA sequences
#'
#' Hamming distance is calculated as the number of base differences between two
#' sequences which can be expressed as a count or a proportion. Typically, it is
#' calculated between two sequences of equal length. In the context of DArT
#' trimmed sequences, which differ in length but which are anchored to the left
#' by the restriction enzyme recognition sequence, it is sensible to compare the
#' two trimmed sequences starting from immediately after the common recognition
#' sequence and terminating at the last base of the shorter sequence.
#'
#' So if the two DNA sequences are of differing length, the longer is truncated. The
#' initial common restriction enzyme recognition sequence is ignored.
#'
#' @param str1 string containing the first sequence
#' @param str2 string containing the second sequence
#' @param r number of bases in the restriction enzyme recognition sequence
#' @return Hamming distance between the two strings
#' @export
#' @author Arthur Georges (bugs? Post to \url{https://groups.google.com/d/forum/dartr})

utils.hamming <- function(str1, str2, r=4) {
  # Make the strings the same length and remove the recognition sequence
  strmin <- min(nchar(str1),nchar(str2))
  x <- charToRaw(substr(str1,r,strmin))
  y <- charToRaw(substr(str2,r,strmin))

  ## Surely this is wrong
  (length(x) - sum(x==y))/strmin
}

