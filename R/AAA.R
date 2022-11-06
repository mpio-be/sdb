
utils::globalVariables(c('.' , 'CRS' , 'SpatialPointsDataFrame' , 'geometry' , 'getMap' , 'i' , 'multiprocess' , 'spChFIDs' , 'writeOGR'))
NULL

    


#' \strong{s}cidb \strong{d}ata \strong{b}ase interface
#'
#' An interface to \href{http://scidb.mpio.orn.mpg.de}{scidb.mpio.orn.mpg.de} databases
#'
#'
#' @name sdb
#' @docType package
#'
#' @author Mihai Valcu \email{valcu@@orn.mpg.de}
#'
#' @import     data.table
#' @import     RMariaDB RMySQL
#' 
#' @importFrom magrittr %>%
#' @importFrom askpass askpass
#' @importFrom stats rnorm rpois runif
#' @importFrom utils read.table setTxtProgressBar txtProgressBar write.table
#' 
#' 
#' 
#' @examples \dontrun{
#' test_db(pwd = askpass::askpass() ) # make it available for tests
#' }
#' 
NULL


.onAttach <- function(libname, pkgname) {
	dcf <- read.dcf(file=system.file("DESCRIPTION", package=pkgname) )
  msg = paste(pkgname, dcf[, "Version"])
  msg = paste(msg, "This package has been superseded by sdb2 https://github.com/mpio-be/dborn")
	packageStartupMessage()
	


  }
