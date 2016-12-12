
#' @export
setClass("postgres_OGR",         representation(dsn = "character") )
#' @export
setClass("OGR_PostgreSQLDriver", representation(dsn = "character", con = "PostgreSQLConnection") )

#' Connect to a database
#'
#' @description     \code{dbcon} returns a database connection. If user and password are not given dbq looks for previously save credentials see(  \code{\link{saveCredentials}} ). \code{\link{closeCon}} closes the connection.
#' @param user      username
#' @param password  password ("" or NA means no password e.g. for localhost)
#' @param database  database to connect to
#' @param path      to credentials file (if different from the default)
#' @param drvNam    one of "MySQL", "postgres" "postgres_OGR". Defaults to MySQL.
#' @param driver    an object returned by \code{driverIni}.
#' @param ...       pass to dbConnect
#' @export
#' @return          a connection object
#' @seealso         \code{\link{saveCredentials}}, \code{\link{dbq} }
driverIni <- function(drvNam) {
  OS = Sys.info()["sysname"]

  if(missing(drvNam) )   drvNam = "MySQL"

   if(drvNam == "MySQL") {
      library(RMySQL)
      drv = dbDriver("MySQL")
      }

   if(drvNam == "postgres") {
      library(RPostgreSQL)
      drv = dbDriver("PostgreSQL")
      }

   if(drvNam == "postgres_OGR") {
      library(RPostgreSQL)
      library(rgdal)
      drv = new("postgres_OGR", dsn = 'PG:')
      }

   return(drv)

    }

#' @rdname driverIni
#' @export
dbcon <- function(user, pwd = "", db = NA, host = "scidb.mpio.orn.mpg.de", path, driver = driverIni("MySQL"), spatial = FALSE, ...) {

  if(!is.na(db) && db == 'aves')   driver = driverIni("postgres")
  if(spatial && db == 'aves' ) driver = driverIni("postgres_OGR")


  if(!missing(user) & !missing(pwd) )
  X = data.frame(user, pwd, db, host, stringsAsFactors = FALSE) else
  X = getCredentials(user = user, host = host, path = path, db = db)
  
  if( nrow(X) == 0 || is.na(X$user) ) stop( "Credentials for user ", dQuote(user), " are not saved!")

  if( is(driver, "MySQLDriver")  ) {
    if(is.na(X$pwd) | X$pwd == "")
         con = dbConnect(driver, user = X$user,                   host = X$host, ...) else
         con = dbConnect(driver, user = X$user, password = X$pwd, host = X$host, ...)

    if(!is.na(db)) dbq(con, paste('USE', db))
    }

  if( is(driver, "PostgreSQLDriver") ) {
    if(is.na(db)) stop("db name is required for this driver!")
    con = dbConnect(driver, user = X$user, password = X$pwd, host = X$host, dbname = X$db, ...)
    }

  if( is(driver, "postgres_OGR") ) {
    if(is.na(db)) stop("db name is required for this driver!")
    dsn = paste0(driver@dsn, 'dbname=', shQuote(db), ' host=', shQuote(host), ' user=', shQuote(X$user), ' password=', shQuote(X$pwd))

    con_pg = dbConnect(dbDriver("PostgreSQL"), user = X$user, password = X$pwd, host = X$host, dbname = X$db, ...)
    con = new("OGR_PostgreSQLDriver",dsn = dsn, con = con_pg)
    }

  
  return(con)

  }

#' @rdname driverIni
#' @export
setGeneric("closeCon", function(con)   standardGeneric("closeCon") )


#' @export
setMethod("closeCon",
          signature  = c(con = "MySQLConnection"),
          definition = function(con) {
      dbDisconnect(con) 
    })     


#' @export
setMethod("closeCon",
          signature  = c(con = "PostgreSQLConnection"),
          definition = function(con) {
      dbDisconnect(con) 
    })     
  

#' @export
setMethod("closeCon",
          signature  = c(con = "OGR_PostgreSQLDriver"),
          definition = function(con) {
        dbDisconnect(con@con) 
    })     
  

