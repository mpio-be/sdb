#' Connect to a database
#'
#' @description    calls dbWriteTable() on small chunks
#'                
#' @param     con       a MariaDBConnection object
#' @param     name       table name
#' @param     x          data.table
#' @param     append     default to TRUE
#' @param     chunkSize default 1000 rows
#' @param     ...        passed to dbWriteTable
#' @export
#' @examples
#' x = data.table(col1 = rep('a', 1000010), col2 = rnorm(1000010))
#' con = dbcon('mihai', host = '127.0.0.1', db = 'tests')
#' dbq(con, 'CREATE TABLE temp (col1 VARCHAR(50) NULL,col2 FLOAT NULL)' )
#' dbSafeWriteTable(con, 'temp', x)
#' dbq(con, 'DROP TABLE temp')

dbSafeWriteTable <- function(con, name, x, append = TRUE, chunkSize = 1000, ...) {

  n = nrow(x)
  i.to.n = 1:n
  ii = split(i.to.n, ceiling(seq_along(i.to.n)/chunkSize) )

  o = vector(length = length(ii))

  pb = txtProgressBar(max = length(ii), style = 3)
  for(i in 1:length(ii) )   {
   
    z = x[ (ii[[i]]) ]
    
    o[i] = dbWriteTable(conn = con, name = name, value = z, append = TRUE, row.names = FALSE, ...)
    setTxtProgressBar(pb, i)

  }
  rm(pb)
  all(o)




}

