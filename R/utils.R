
#' removeDuplicates
#' 
#' @param con    con
#' @param table  table
#' @param key  key. default to 'pk'.
#' 
#' @export
#' @examples \dontrun{
#' con = dbcon('mihai', host = '127.0.0.1', db = 'tests')
#' removeDuplicates(con, 'n1', 'id')
#'}
removeDuplicates <- function(con, table, key = 'pk') {

  t0 = Sys.time()

  n0 = dbq(con, paste("select count(*) n from ", table),enhance= FALSE )$n
  cols = dbq(con, paste("SELECT * FROM", table, "WHERE FALSE" ) )%>% names %>% setdiff(. , key) %>% paste(collapse = '`,`')
  cols = paste0('`', cols, '`')

  dbExecute(con, "DROP TABLE IF EXISTS temp"  )
  dbExecute(con, paste("CREATE TABLE temp like", table)  )
  n1 = dbExecute(con, paste("INSERT INTO temp(", cols, ") SELECT DISTINCT", cols, "FROM", table) )

  o = dbExecute(con, paste("RENAME TABLE", table, "TO temp2, temp TO", table) )
  
  if(n1 <= n0)
   dbExecute(con, "DROP TABLE temp2")

  data.frame(nrows = n0 - n1, mins_run = difftime( Sys.time(), t0) %>% as.numeric)


 }


#' string_is_mysql_date
#' 
#' @param x    A string
#' 
#' @export
string_is_mysql_date <- function(x) {
  o = str_detect(x, pattern = '(\\d{2}|\\d{4})(?:\\-)?([0]{1}\\d{1}|[1]{1}[0-2]{1})(?:\\-)?([0-2]{1}\\d{1}|[3]{1}[0-1]{1})(?:\\s)?([0-1]{1}\\d{1}|[2]{1}[0-3]{1})(?::)?([0-5]{1}\\d{1})(?::)?([0-5]{1}\\d{1})')
  if ( all(is.na(o))  ) o = FALSE else o =  all(o, na.rm= TRUE)
  o
  }


#' Probe a db server 
#' @description Fast probe a db server see if it can be reached through a given port. Default to scidb.mpio.orn.mpg.de and port 3306
#' @param       probe if FALSE, will not probe the db and return ''127.0.0.1'. Default to TRUE. 
#' @param       name  default to scidb.mpio.orn.mpg.de
#' @param       port  default to 3306 (mariadb's default port )
#' @return when reachable return the name of probed server else falls back to localhost (127.0.0.1)
#' @export
#' @examples \dontrun{
#' probeDB()
#' 
#'}
   
probeDB <- function(probe = TRUE, name = 'scidb.mpio.orn.mpg.de', port = 3306) {
    
    if(probe) {


    noBash = inherits(try(system('bash', intern = TRUE, ignore.stderr = TRUE), silent = TRUE) ,'try-error') 

    if(noBash) {
      warning(paste('bash is not available so I cannot probe', name, '; host is  set to localhost.') )
      host = '127.0.0.1'
    } else {

      syscall = paste0("timeout 0.5  bash -c 'cat < /dev/null > /dev/tcp/", name , "/", port, "'")
      ans = suppressWarnings( attributes( system(syscall, intern = TRUE,ignore.stderr = TRUE) )   )
      host = if( is.null(ans) ) name else '127.0.0.1'

      }

    }

   if (name %in% c('localhost', '127.0.0.1') | (!probe) ) host = '127.0.0.1'   

  host
 }





