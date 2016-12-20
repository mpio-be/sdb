#' query the database
#'
#' query the database using an user-defined connection or a temp connection based on saved credentials.
#'
#' run an SQL query.
#' @param con a connection object returned by \code{\link{dbcon}}
#' @param q a query string. credentials are stored on disk.
#' @seealso \code{\link{saveCredentials}},\code{\link{dbcon}}
#' @export
#' @return a data.frame or a  Spatial*DataFrame (OGR_PostgreSQLDriver) for a SELECT query, or NULL for non-SELECT queries.
#' @examples
#' # A connection is made and used by dbq
#'  con = dbcon('mihai', host = 'localhost')
#'  d1 = dbq(con, 'SELECT * from BTatWESTERHOLZ.ADULTS')
#'  d2 = dbq(con, 'SELECT * from BTatWESTERHOLZ.ADULTS', enhance = TRUE)
#' 
#' # A temp. connection is made and closed once the data is retrieved
#' dbq(q = 'select now()', user = 'mihai', host = 'localhost') %>% str
#' dbq(q = 'select now()', user = 'mihai',host = 'localhost',  enhance = TRUE) %>% str
#' 
#' # null return
#' dbq(user = 'mihai', host = 'localhost', q = 'set @c=1')
#' dbq(con, 'set @c=1')
#' 
#' spatial return
#' con = dbcon('mihai', host = 'localhost', db = 'AVES_ranges', driver = 'spatial_MySQL')
#' s = dbq(con, 'ranges_birdlife_v1')
#' 
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
          signature  = c(con = "missing", q = "character"),
          definition = function(q, enhance = FALSE, ...) {
          con = dbcon(...); on.exit(closeCon(con))
          dbq(con, q, enhance = enhance)
          }
  )


#' @export
#' @import rgdal
#' @import sp
setOldClass("ogrinfo")
setMethod("dbq",
          signature  = c(con = "ogrinfo", q = "character"),
          definition = function(con, q, ...) {

          readOGR(con$dsn, q , verbose = FALSE)

           }
  )






















