#' Table comments 
#'
#' @description  Extract table comments to data.table
#'
#' @param     con       a *Connection object. If missing dbq() without a connection argument is called.
#' @param     name       table name
#' @export
#' @examples
#' \dontrun{
#' get_table_comments(con, tableName = '')
#' con = dbcon('mihai', host = '127.0.0.1', db = 'tests')
#' dbq(con, 'CREATE TABLE temp (f1 VARCHAR(50) NULL,f2 FLOAT NULL)' )

#' dbInsertInto(con, 'temp', x)
#' dbq(con, 'DROP TABLE temp')
#' }


get_table_comments <- function(con, tableName) {

    x = dbGetQuery(
      con,
      paste0("SELECT COLUMN_NAME `Column`, COLUMN_COMMENT description FROM  information_schema.COLUMNS
								WHERE TABLE_SCHEMA =", shQuote(db), "AND TABLE_NAME =", shQuote(table))
    ) %>% data.table()




}