

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
#'
#' @keywords package


.onLoad <- function(libname, pkgname){
  dcf <- read.dcf(file=system.file("DESCRIPTION", package=pkgname) )
  packageStartupMessage(paste('This is', pkgname, dcf[, "Version"] ))

  if( ! Sys.info()["sysname"] %in% c("Linux", "Windows"))
    packageStartupMessage("sdb might not work under", OS)

	}

