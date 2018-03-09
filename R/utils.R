
#' removeDuplicates
#' @export
#' @examples \dontrun{
#' con = dbcon('mihai', host = '127.0.0.1', db = 'BTatWESTERHOLZ')
#' removeDuplicates(con, 'ADULTS', 'ad_pk')
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
#' @export
string_is_mysql_date <- function(x) {
    o = str_detect(x, pattern = '(\\d{2}|\\d{4})(?:\\-)?([0]{1}\\d{1}|[1]{1}[0-2]{1})(?:\\-)?([0-2]{1}\\d{1}|[3]{1}[0-1]{1})(?:\\s)?([0-1]{1}\\d{1}|[2]{1}[0-3]{1})(?::)?([0-5]{1}\\d{1})(?::)?([0-5]{1}\\d{1})')
    if ( all(is.na(o))  ) o = FALSE else o =  all(o, na.rm= TRUE)
    o
    }



   
