#' query the database
#'
#' query the database using an user-defined connection or a temp connection based on saved credentials.
#'
#' run an SQL query.
#' @param con a connection object returned by \code{\link{dbcon}}
#' @param q a query string. credentials are storred on disk.
#' @seealso \code{\link{saveCredentials}},\code{\link{dbcon}}
#' @export
#' @return a data.frame or a  Spatial*DataFrame (OGR_PostgreSQLDriver) for a SELECT query, or NULL for non-SELECT queries.
#' @examples
#' # A connection is made and used by dbq
#'  con = dbcon('mihai')
#'  d1 = dbq(con, 'SELECT * from BTatWESTERHOLZ.ADULTS')
#'  d2 = dbq(con, 'SELECT * from BTatWESTERHOLZ.ADULTS', enhance = TRUE)
#' 
#' # A temp. connection is made and closed once the date are retrieved
#' dbq(q = 'select now()', user = 'mihai') %>% str
#' dbq(q = 'select now()', user = 'mihai', enhance = TRUE) %>% str
#' 
#' # no return
#' dbq(user = 'mihai', host = 'localhost', q = 'drop database if exists this_does_not_Exists_ever_123')
#' dbq(con, 'drop database if exists this_does_not_Exists_ever_123')
#' dbDisconnect(con)

setGeneric("dbq", function(con,q, ...)   standardGeneric("dbq") )

#' Enhance a data.table by reference
#' @export
enhanceOutput <- function(d) {
  # find datetime
    datetime_cols <- d[1 : (if(nrow(d) < 5000) nrow(d) else 5000) ][, sapply(.SD, string_is_mysql_date) ]
    datetime_cols <- datetime_cols[which(datetime_cols)] %>% names
    if(length(datetime_cols) > 0)
    d[ , (datetime_cols) := lapply(.SD, as.POSIXct), .SDcols = datetime_cols ]

  # key cols
    kc = c('ID', 'IDmale', 'IDfemale', 'transp', 'transponder',  datetime_cols)
    kc = names(d)[which(names(d) %in% kc )]

    if(length(datetime_cols) > 0)
    setkeyv(d, kc)

 }


#' @export
setMethod("dbq",
          signature  = c(con = "MySQLConnection", q = "character"),
          definition = function(con, q, enhance = FALSE, ...) {
			    o = dbGetQuery(con, q, ...)

          setDT(o)
          if(enhance) enhanceOutput(o)

          if(nrow(o) == 0) o  = NULL  

          return(o)

           }
	)

#' @export
setMethod("dbq",
          signature  = c(con = "PostgreSQLConnection", q = "character"),
          definition = function(con, q, enhance = FALSE, ...) {
			    o = dbGetQuery(con, q, ...)
          setDT(o)
          
          if(nrow(o) == 0) o  = NULL

          return(o)
          }
	)

#' @export
setMethod("dbq",
          signature  = c(con = "OGR_PostgreSQLDriver", q = "character"),
          definition = function(con, q, enhance = FALSE, ...) {

          dbq(con@con, paste('CREATE OR REPLACE VIEW temp AS', q) )
          on.exit( dbq(con@con, 'DROP VIEW temp' ) )

          readOGR(con@dsn, 'temp', verbose = FALSE)

           }
	)

#' @export
setMethod("dbq",
          signature  = c(con = "missing", q = "character"),
          definition = function(q, enhance = FALSE, ...) {
          con = dbcon(...); on.exit(closeCon(con))
          dbq(con, q, enhance = enhance)
          }
  )






















