##' Test to see if two populations are fixed at a given locus
##'
##' Compare two percent allele frequencies and report \code{TRUE} if
##' they represent a fixed difference, \code{FALSE} otherwise.
##'
##' A fixed difference at a locus occurs when two populations share no
##' alleles, noting that SNPs are biallelic (ploidy=2).  Tollerance in
##' the definition of a fixed difference is provided by the t
##' parameter. For example, t=0.05 means that SNP allele frequencies of
##' 95,5 and 5,95 percent will be reported as fixed (TRUE).
##'
##' @param s1 percentage SNP allele frequency for the first population
##' @param s2 percentage SNP allele frequency for the second population
##' @param tloc threshold value for tolerance in when a difference is regarded as fixed
##' @seealso \code{\link{gl.fixed.diff}}
##' @return TRUE (fixed difference) or FALSE (alleles shared) or NA (one or both s1 or s2 missing)
##' @author Arthur Georges (bugs? Post to \url{https://groups.google.com/d/forum/dartr})
##' @export
##' @examples
##' is.fixed(s1=100, s2=0, tloc=0)
##' is.fixed(96, 4, tloc=0.05)
is.fixed <- function(s1, s2, tloc=0){

  p <- 100*tloc
  ifelse(is.na(s1) | is.na(s2), NA, (s1 <= p & s2 >= (100-p)) | (s2 <= p & s1 >= (100-p)))
}
