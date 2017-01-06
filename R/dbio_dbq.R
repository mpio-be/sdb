#' query the database
#'
#' query the database using an user-defined connection or a temp connection based on saved credentials.
#'
#' run an SQL query.
#' @param con a connection object returned by \code{\link{dbcon}}
#' @param q a query string. credentials are stored on disk.
#' @seealso \code{\link{saveCredentials}},\code{\link{dbcon}}
#' @export
#' @return a data.frame or a  Spatial*DataFrame (spatial_MySQL) for a SELECT query, or NULL for non-SELECT queries.
#' @examples
#' # A connection is made and used by dbq
#'  con = dbcon('mihai', host =  '127.0.0.1')
#'  d1 = dbq(con, 'SELECT * from BTatWESTERHOLZ.ADULTS')
#'  d2 = dbq(con, 'SELECT * from BTatWESTERHOLZ.ADULTS', enhance = TRUE)
#'
#' # A temp. connection is made and closed once the data is retrieved
#' dbq(q = 'select now()', user = 'mihai', host =  '127.0.0.1') %>% str
#' dbq(q = 'select now()', user = 'mihai',host =  '127.0.0.1',  enhance = TRUE) %>% str
#'
#' # null return
#' dbq(user = 'mihai', host =  '127.0.0.1', q = 'set @c=1')
#' dbq(con, 'set @c=1')
#' dbDisconnect(con)
#'
#' spatial return
#' con = dbcon('mihai', host =  '127.0.0.1', db = 'tests', driver = 'spatial_MySQL')
#' s = dbq(con, q = 't3')
#' s = dbq(con, q = 'select * from t3 limit 1')
#'
#' con = dbcon('mihai', host =  '127.0.0.1', db = 'AVES_ranges', driver = 'spatial_MySQL')
#' s = dbq(con, q = "select * from breeding_ranges_v1 where scinam = 'Parus major'")


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

#TODO: CHECK and add tests
#' @export
#' @import rgdal
#' @import gdalUtils
#' @import sp
setOldClass("ogrinfo")
setMethod("dbq",
          signature  = c(con = "ogrinfo", q = "character"),
          definition = function(con, q, ...) {

          sqllen = length(str_split(q, ' ', simplify = TRUE))

          if( sqllen == 1)
            o = readOGR(con$dsn, q , verbose = FALSE)

          if( sqllen > 1) { # will go via ogr
            tf = tempfile()
            ogr2ogr(f = 'SQLite',
                   src_datasource_name = con$dsn,
                   dst_datasource_name = tf,
                   dsco = 'SPATIALITE=yes ',
                   dialect = "sqlite",
                   nln = 'mysql_query',
                   sql = q, verbose = FALSE, overwrite = TRUE)

            o = readOGR(tf, 'mysql_query' , verbose = FALSE)
            }

           return(o)

        }
  )






















