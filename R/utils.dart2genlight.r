##' Converts dart data to an genlight object \code{\link{adegenet}}.
##'
##' This is an internal function called by \code{gl.read.dart} to
##' convert the data read by \code{read.dart} and return a genlight
##' object.
##'
##' The file of individual covariates is searched for the columns
##' \describe{
##' \item{\code{id}}{individual identifiers matching the genetic data in the dart object}
##' \item{\code{pop}}{population identifier for each individual}
##' \item{\code{lat}, \code{lon}}{spatial coordinates (perferably in decimal degrees WGS1984 format)}
##' }
##' Of these, only \code{id} is mandatory.  Covariates other than
##' these contained in the file will be appended to individual
##' metadata dataframe in the genlight object.
##'
##' @title Convert DarT to Genlight
##' @param dart a list created by \code{read.dart}
##' @param ind.metafile name of csv file with metadata for each individual.
##' @return A genlight object with all slots filled.
##' @author Bernd Gruber (bugs? Post to \url{https://groups.google.com/d/forum/dartr})
##' @export
utils.dart2genlight <- function(dart, ind.metafile=NULL) {

  nind <- dart[["nind"]]
  nsnp <- dart[["nsnp"]]
  sraw <- dart[["covmetrics"]]
  nrows <- dart[["nrows"]]

  ## Guess nrows if not provided
  if (is.null(nrows)) {
    nrows <- 3 - max(dart$gendata, na.rm = TRUE)
    if(nrows %in% 1:2)
      warning("DaRT formmat not provided. Assuming ",nrows," row format")
    else
      stop("Could not determine nrows")
  }

  if(!all(c("SNP", "SnpPosition") %in% names(sraw)))
    stop("Could not find SNP or SnpPosition in Dart file. Check the headers!")

  sdata <- dart[["gendata"]]
  sub <- seq.int(nrows,nrow(sdata),nrows)

  pos <- sraw$SnpPosition[sub]
  alleles <- as.character(sraw$SNP)[sub]
  loc.all <- sub(">","/",substr(alleles,nchar(alleles)-2,nchar(alleles)),fixed=TRUE)
  loc.name <- paste(sraw$uid[sub],loc.all,sep="-")
  geninddata <- matrix(0L, nrow=nsnp, ncol=nind)

  if(nrows==2) {
    for(i in seq_len(ncol(sdata)))
      geninddata[,i] <- ifelse(sdata[sub-1,i]==0,2,sdata[sub,i])
  } else {
    for(i in seq_len(ncol(sdata)))
      geninddata[,i] <- ifelse(sdata[,i]==0, 0, 3-sdata[,i])
  }

  gl <- new("genlight",gen=t(geninddata),ploidy=2,ind.names=colnames(sdata),loc.names=loc.name,loc.all=loc.all,position=pos,parallel=F)


  ## Drop unused levels from loci metrics
  gl@other$loc.metrics <- droplevels(sraw[sub,])

  ## Add individual covariates
  if(!is.null(ind.metafile)) {
    ind.cov <- read.csv(ind.metafile, header=T, stringsAsFactors=T)

    if(is.null(ind.cov$id))
      stop("Missing individual covariates data missing identifier (id) column")

    ## Is this necessary?
    ind.cov$id <- trimws(ind.cov$id,which="both")

    if(any(duplicated(ind.cov$id)))
      stop("Non-unique identifiers in individual covariate data")

    ## Subset to individuals for which there is genetic data
    m <- match(colnames(sdata),ind.cov$id)
    if(any(is.na(m)))
      stop("No covariate data for some individuals")
    ind.cov <- ind.cov[m,]

    if(!is.null(ind.cov$pop))
      pop(gl) <- ind.cov$pop
    else
      warning("No population identifier found")

    if(is.null(ind.cov$lon) || is.null(ind.cov$lat))
      warning("No (lat,lon) data found")
    else
      gl@other$latlong <- ind.cov[,c("lat","lon")]

    gl@other$ind.metrics<- ind.cov
  }
  gl
}

