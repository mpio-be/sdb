

#' Connect to a database
#'
#' @description     \code{dbcon} returns a database connection.
#'                   If user and password are not given dbq looks for previously saved credentials
#'                   see(  \code{\link{saveCredentials}} ). \code{\link{closeCon}} closes the connection.
#' 
#' @param user      username
#' @param pwd       password ("" or NA means no password e.g. for localhost)
#' @param db        database to connect to
#' @param host      default to "scidb.mpio.orn.mpg.de"
#' @param path      to credentials file (if different from the default)
#' @param driver    MySQL, MariaDB or mysql_gdal, ... Defaults to MySQL
#' @param ...       pass to dbConnect
#' @param con       a connection made with dbcon or \code{\link{dbConnect} }
#' 
#' @export
#' @return          a connection object
#' @seealso         \code{\link{saveCredentials}}, \code{\link{dbq} }

dbcon <- function(user = Sys.info()['user'], pwd = "", db = NA, host = "scidb.mpio.orn.mpg.de", path, driver = "MySQL" , ...) {

  if(!missing(user) & !missing(pwd) )
  X = data.frame(user, pwd, db, host, stringsAsFactors = FALSE) else
  X = getCredentials(user = user, host = host, path = path)
  if( nrow(X) == 0 || is.na(X$user) ) stop( "Credentials for user ", dQuote(user), " are not saved!")

  if( driver ==  "MySQL" ) {
    if(is.na(X$pwd) | X$pwd == "")
         con = dbConnect( RMySQL::MySQL() , user = X$user,                   host = X$host, ...) else
         con = dbConnect( RMySQL::MySQL() , user = X$user, password = X$pwd, host = X$host, ...)

    if(!is.na(db)) dbExecute(con, paste('USE', db))
    }

  if( driver ==  "MariaDB" ) {
    if(is.na(X$pwd) | X$pwd == "")
         con = dbConnect( RMariaDB::MariaDB() , user = X$user,                   host = X$host, ...) else
         con = dbConnect( RMariaDB::MariaDB() , user = X$user, password = X$pwd, host = X$host, ...)

    if(!is.na(db)) dbExecute(con, paste('USE', db))
    }







  if(driver == 'mysql_gdal') {
    if(missing(db)) stop ('database name is required for mysql_gdal')
    con = paste0('MYSQL:', db ,',user=', X$user, ',host=', X$host, ',password=', X$pwd)
  }


  return(con)

  }

#' @rdname dbcon
#' @export
setGeneric("closeCon", function(con)   standardGeneric("closeCon") )


#' @export
#' @rdname dbcon
setMethod("closeCon",
          signature  = c(con = "MariaDBConnection"),
          definition = function(con) {
      dbDisconnect(con)
    })


#' @export
#' @rdname dbcon
setMethod("closeCon",
          signature  = c(con = "MySQLConnection"),
          definition = function(con) {
      dbDisconnect(con)
    })



