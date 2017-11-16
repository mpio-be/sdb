#' dbWriteTable() on small chunks
#'
#' @description    calls dbWriteTable() on small chunks
#'                
#' @param     con       a *Connection object
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

dbSafeWriteTable <- function(con, name, x, append = TRUE, chunkSize = 1000, verbose = TRUE, ...) {

  n = nrow(x)
  i.to.n = 1:n
  ii = split(i.to.n, ceiling(seq_along(i.to.n)/chunkSize) )

  o = vector(length = length(ii))

  if(verbose) pb = txtProgressBar(max = length(ii), style = 3)
  for(i in 1:length(ii) )   {
   
    z = x[ (ii[[i]]) ]
    
    o[i] = dbWriteTable(conn = con, name = name, value = z, append = TRUE, row.names = FALSE, ...)
    if(verbose) setTxtProgressBar(pb, i)

  }
  
  all(o)

}



#' dbWriteTable safe variant
#'
#' @description  dbWriteTable(tempTable) --> insert into select .. from --> drop tempTable
#'                
#' @param     con       a *Connection object
#' @param     name       table name
#' @param     x          data.table
#' @export
#' @examples
#' x = data.table(f1 = rep('a', 10), f2 = rnorm(10), f3 = 1)
#' con = dbcon('mihai', host = '127.0.0.1', db = 'tests')
#' dbq(con, 'CREATE TABLE temp (f1 VARCHAR(50) NULL,f2 FLOAT NULL)' )

#' dbInsertInto(con, 'temp', x)
#' dbq(con, 'DROP TABLE temp')

dbInsertInto <- function(con, name, x) {


      temp000 = make.db.names(con, as.character(Sys.time() ) )
      o = dbWriteTable(con, temp000, x, row.names = FALSE)
      if(o) message('data.table x saved as ', temp000)


      targetNams = names( dbGetQuery(con, paste('select * from', name, 'limit 0') ) )
      selectTargetNams = intersect(targetNams, names(x))

      
      
      infields = paste(selectTargetNams, collapse = ',')  
      insql =  paste('INSERT INTO', name, '(', infields, ')',
                'SELECT', infields, 'FROM', temp000)

      o = dbExecute(con, insql )
      if(o > 0) {
        return(o)
        message(o, ' rows inserted into ', name)
      }


      dbExecute(con, paste('REPAIR TABLE', name)  )
      
      dbExecute(con, paste('DROP TABLE if exists', temp000)  )

      dbDisconnect(con)

}