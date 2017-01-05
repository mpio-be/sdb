

#' Connect to a database
#'
#' @description     \code{dbcon} returns a database connection. If user and password are not given dbq looks for previously save credentials see(  \code{\link{saveCredentials}} ). \code{\link{closeCon}} closes the connection.
#' @param user      username
#' @param password  password ("" or NA means no password e.g. for localhost)
#' @param database  database to connect to
#' @param path      to credentials file (if different from the default)
#' @param driver    MySQL or spatial_MySQL, ... Defaults to MySQL.
#' @param ...       pass to dbConnect
#' @export
#' @return          a connection object
#' @seealso         \code{\link{saveCredentials}}, \code{\link{dbq} }

dbcon <- function(user, pwd = "", db = NA, host = "scidb.mpio.orn.mpg.de", path, driver = "MySQL" , ...) {

  if(!missing(user) & !missing(pwd) )
  X = data.frame(user, pwd, db, host, stringsAsFactors = FALSE) else
  X = getCredentials(user = user, host = host, path = path)
  if( nrow(X) == 0 || is.na(X$user) ) stop( "Credentials for user ", dQuote(user), " are not saved!")

  if( driver ==  "MySQL" ) {
    if(is.na(X$pwd) | X$pwd == "")
         con = dbConnect( dbDriver(driver) , user = X$user,                   host = X$host, ...) else
         con = dbConnect( dbDriver(driver) , user = X$user, password = X$pwd, host = X$host, ...)

    if(!is.na(db)) dbq(con, paste('USE', db))
    }

  if(driver == 'spatial_MySQL') {
    if(missing(db)) stop ('database name is required for spatial_MySQL')
    dsn = paste0('MYSQL:', db ,',user=', X$user, ',host=', X$host, ',password=', X$pwd)
    con = suppressWarnings( ogrInfo(dsn) )
  }


  return(con)

  }

#' @rdname dbcon
#' @export
setGeneric("closeCon", function(con)   standardGeneric("closeCon") )


#' @export
setMethod("closeCon",
          signature  = c(con = "MySQLConnection"),
          definition = function(con) {
      dbDisconnect(con) 
    })     



