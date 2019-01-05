##' Report minor allele frequency (MAF) distributions.
##'
##' Displays histograms showing the distribution of minor allele
##' frequency both overall, and for each population.
##' @title Minor Allele Frequency Report
##' @param gl a genelight object
##' @param maf.limit the maximum MAF to show for each population
##' @param ind.limit histograms are not produced for populations with
##'   fewer individuals than this
##' @importFrom graphics layout
##' @author Arthur Georges (Post to \url{https://groups.google.com/d/forum/dartr})
##' @export
gl.report.maf <- function(gl, maf.limit=0.5, ind.limit=5) {

  opar <- par(no.readonly=TRUE)
  on.exit(par(opar))

  gl <- utils.recalc.freq(gl)
  maf <- gl@other$loc.metrics$maf

  layout(matrix(c(1,1,1,2,3,4,5,6,7), 3, 3, byrow = TRUE))
  hist(maf,
       breaks=seq(0,0.5,0.05),
       col=rainbow(10),
       main="Overall",
       xlab="Minor Allele Frequency")

  x <- as.matrix(gl)
  pops <- pop(gl)
  k <- 1
  for(pop in levels(pops)) {
    xp <- x[pops==pop,]
    if(nrow(xp) < ind.limit) next
    xp <- xp[,!(apply(xp==0L,2,all,na.rm=TRUE) | apply(xp==1L,2,all,na.rm=TRUE) | apply(xp==2L,2,all,na.rm=TRUE))]
    freq <- .colMeans(xp,nrow(xp),ncol(xp),na.rm=TRUE)/2
    maf <- pmin(freq,1-freq)
    maf <- maf[maf > 1.0E-10]

    if(k==7)
      layout(matrix(c(1,2,3,4,5,6,7,8,9), 3, 3, byrow = TRUE))

    hist(maf[maf<maf.limit],
         breaks=seq(0,maf.limit,len=10),
         col=rainbow(10),
         main=paste(pop,"\nn =",nrow(xp)),
         xlab="Minor Allele Frequency",
         xlim=c(0,maf.limit))
    k <- k+1
  }
}
