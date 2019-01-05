##' Convert a genlight object to a genind object.
##'
##' Note that \code{gl2gi(gi2gl(x))} may not return the original
##' object due to ordering of major alleles.
##'
##' @title Genlight to genind conversion
##' @param gl a genlight object
##' @return A genind object, with all slots filled.
##' @seealso \code{\link{gi2gl}}, \code{\link[adegenet]{df2genind}}
##' @author Bernd Gruber (bugs? Post to \url{https://groups.google.com/d/forum/dartr})
##' @export
gl2gi <- function(gl) {
  cl <- match.call()
  x <- as.matrix(gl)
  cnms <- paste(rep(colnames(x),each=2),
                ## If no loci allele names, make them up
                if(!is.null(gl@loc.all)) unlist(strsplit(gl@loc.all,"/")) else c("A","T","C","G"),
                sep=".")
  ## Create frequency table (assuming ploidy=2)
  x <- x[,rep(seq_len(ncol(x)),each=2)]
  x[,seq.int(1,ncol(x),2)] <- 2-x[,seq.int(1,ncol(x),2)]
  ## Drop all zero cols (df2genind also drops all zero rows, but might wreck @other)
  keepc <- colSums(x,na.rm=TRUE)>0
  x <- x[,keepc]
  gi <- genind(tab=x,pop=pop(gl),prevcall=cl,ploidy=2,type="codom")
  gi@other <- gl@other
  gi
}


