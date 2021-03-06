% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gl.report.ld.r
\name{gl.report.ld}
\alias{gl.report.ld}
\title{Calculates pairwise population based Linkage Disequilibirum across all loci using the specifyied number of cores}
\usage{
gl.report.ld(gi, name = NULL, save = TRUE, nchunks = 2, ncores = 1,
  chunkname = NULL)
}
\arguments{
\item{gi}{a genlight or genind object created (genlight objects are internally converted via \code{\link{gl2gi}} to genind)}

\item{name}{character string for rdata file. If not given genind object name is used}

\item{save}{switch if results are saved in a file}

\item{nchunks}{how many subchunks will be used (the less the faster, but if the routine crashes more bits are lost}

\item{ncores}{how many cores should be used}

\item{chunkname}{the name of the chunks for saving, default is NULL}
}
\value{
returns calculation of pairwise LD across all loci between subpopulation. This functions uses if specified many cores on your computer to speed up. And if save is used can restart (if save=TRUE is used) with the same command starting where it crashed. The final output is a data frame that holds all statistics of pairwise LD between loci. (See ?LD in package genetics for details).
}
\description{
this function is implemented in a parallel fashion to speed up the process. There is also the ability to restart the function if crashed by specifying the chunkfile names or restarting the function exactly in the same way as in the first run. This is implemented as sometimes due to connectivity loss between cores the function my crash half way. Also remove loci with have only missing value before running the function.
}
\author{
Bernd Gruber (bugs? Post to \url{https://groups.google.com/d/forum/dartr})
}
