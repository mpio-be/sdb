#' query the database
#'
#' query the database using an user-defined connection or a temp connection based on saved credentials.
#'
#' run an SQL query.
#' @param con a connection object returned by \code{\link{dbcon}}
#' @param q a query string. credentials are stored on disk.
#' @seealso \code{\link{saveCredentials}},\code{\link{dbcon}}
#' 
#' @importFrom anytime anytime
#' @importFrom stringr str_detect str_split regex
#' @importFrom sf st_as_sfc st_sf st_set_crs

#' @export
#' @return a data.frame or a  Spatial*DataFrame (spatial_MySQL) for a SELECT query, or NULL for non-SELECT queries.
#' @examples
#' \dontrun{
#' # A connection is made and used by dbq
#'  con = dbcon('testuser', host =  '127.0.0.1', pwd =  askpass::askpass() )
#'  d1 = dbq(con, 'SELECT * from tests.t1')
#'  d2 = dbq(con, 'SELECT * from tests.t1', enhance = TRUE)
#'
#' # A temp. connection is made and closed once the data is retrieved
#' dbq(q = 'select now()', user = 'testuser', host =  '127.0.0.1', pwd =  'cs') 
#' dbq(q = 'select now()', user = 'testuser',host =  '127.0.0.1', pwd =  'cs',  
#' 	enhance = TRUE)%>% print
#'
#' # spatial return
#' s = dbq(con, q = 'select * from tests.t3', geom = 'SHAPE')
#' s = dbq(con, q = 'select SHAPE from tests.t3', geom = 'SHAPE')
#' s = dbq(con, q = 'select * from tests.t2', geom = 'SHAPE')
#' 
#' }


setGeneric("dbq", function(con,q, geom, ...)   standardGeneric("dbq") )

#' Enhance a data.table by reference
#' @param  ... optional parameters to anytime
#' @export
enhanceOutput <- function(d, ...) {
	
	requireNamespace("data.table", quietly=TRUE)
	# find datetime
	n = nrow(d)
	n = if(n < 5000) n else 5000

	datetime_cols <- d[1:n, sapply(.SD, string_is_mysql_date) ]
	datetime_cols <- datetime_cols[which(datetime_cols)] %>% names


	if(length(datetime_cols) > 0)
		d[ , (datetime_cols) := lapply(.SD, anytime, ... ), .SDcols = datetime_cols ]

	# key cols
	kc = c('ID', 'IDmale', 'IDfemale', 'transp', 'transponder',  datetime_cols)
	kc = names(d)[which(names(d) %in% kc )]

	if(length(datetime_cols) > 0)
	setkeyv(d, kc)

	}


#' @export
setMethod("dbq",signature  = c(con = "MariaDBConnection", q = "character"),
		definition = function(con, q, enhance = FALSE, ...) {
		
		o = suppressWarnings( dbGetQuery(con, q, ...) )

		setDT(o)
		if(enhance) enhanceOutput(o)

		if(nrow(o) == 0 & ncol(o) == 0)  o = NULL

		o

		 }
		)

#' @export
setMethod("dbq",signature  = c(con = "missing", q = "character"),
		definition = function(q, enhance = FALSE, ...) {
		
		con = dbcon(...); on.exit(closeCon(con))
		dbq(con, q, enhance = enhance)
		}
		)


#' @export
setMethod("dbq",signature  = c(con = "MariaDBConnection", q = "character", geom = 'character'),
		definition = function(con, q, geom = 'SHAPE', ...) {
		
		# re-shape sql 
		nams  = dbGetQuery(con, paste(q, 'where false')) %>% names
		nams[match(geom, nams) ] <- paste0('ST_AsText(', nams[match(geom, nams) ], ') as ', geom) 
		nams = paste(nams, collapse = ',')

		tab = str_split(q, regex('FROM', ignore_case = TRUE), simplify = TRUE )[2]

		newq = paste("SELECT", nams, 'FROM', tab)

		# get data
		o = suppressWarnings( dbGetQuery(con, newq) )
		setDT(o)
		setnames(o, geom, 'geometry')
		o[, geometry := list(st_as_sfc(geometry)) ]
		o = st_sf(o)


		# get spatial reference 
		crs = try(getCRSfromDB(con, tab), silent = TRUE )

		if( inherits(crs, 'crs'))
			o = st_set_crs(o, crs) else 
			warning('projection could not be retrieved from the database. Set it manually with sf::st_set_crs()')	


		o

		}
		)






















