
#' string_is_mysql_date
#' @export
string_is_mysql_date <- function(x) {
    o = str_detect(x, pattern = '(\\d{2}|\\d{4})(?:\\-)?([0]{1}\\d{1}|[1]{1}[0-2]{1})(?:\\-)?([0-2]{1}\\d{1}|[3]{1}[0-1]{1})(?:\\s)?([0-1]{1}\\d{1}|[2]{1}[0-3]{1})(?::)?([0-5]{1}\\d{1})(?::)?([0-5]{1}\\d{1})')
    if ( all(is.na(o))  ) o = FALSE else o =  all(o, na.rm= TRUE)
    o
    }


#' @title Test database
#' @description Installs or destroys a mariadb test database
#' @export
#' @note
#' test_db() works independently on other functions in sdb
#'
test_db <- function(user = 'testuser', host =  '127.0.0.1', db = 'tests', pwd, destroy = FALSE) {
      sapply(c('rgdal', 'rworldmap'),
       function(x) suppressPackageStartupMessages(require(x , character.only = TRUE, quietly = TRUE) ) )

      DSN = paste0('MYSQL:',db ,',user=', user, ',host=', host, ',password=', pwd)
      # ogrListLayers(DSN)

      # pwd = readLines('~/.my.cnf')[grep('password', readLines('~/.my.cnf'))]
      # pwd = str_extract(pwd, '[^=]*$') %>% str_trim() %>% str_replace_all("'", "") %>% str_replace_all('"', "")

      con = dbConnect(RMySQL::MySQL(), user = user, password = pwd); on.exit(dbDisconnect(con))

      dbGetQuery(con, paste('DROP DATABASE IF EXISTS', db))

      if(!destroy) {

        dbGetQuery(con, paste('CREATE DATABASE IF NOT EXISTS', db))
        dbGetQuery(con, paste('USE', db))

        #t1 [ a table with major types]
          dbGetQuery(con, "
              CREATE TABLE t1(
                id INT NOT NULL auto_increment PRIMARY KEY,
                n1  int ,
                n2  FLOAT ,
                n3  DOUBLE ,
                n4  BOOLEAN default 1 ,
                n5  ENUM ('x','y','q'),
                v1  varchar (255) ,
                v2  char(2) ,
                dt1 DATE,
                dt2 TIME,
                dt3 DATETIME

              ); ")

           t1 = data.frame(n1 = rpois(100, 2), n2 = rnorm(100), n3 = rnorm(100, 200), n4 = runif(100, 0, 1),
                    n5 = sample(c('x','y','q'), 100, replace = TRUE),
                    v1 = replicate(100, paste( sample(letters, size = runif(1, 1, 20) ), collapse = '') ),
                    v2 = replicate(100, paste(sample(letters, 2), collapse = '')),
                    dt1 = Sys.Date() + rpois(100, 5),
                    dt2 = format(Sys.time() + rnorm(100) , '%H:%M:%S'),
                    dt3 = Sys.time() + rnorm(100)
                    )

          dbWriteTable( con, 't1', t1, row.names = FALSE, overwrite = TRUE )

       #t2 [ spatial table (points) ]
          t2 = SpatialPointsDataFrame(
            cbind( runif(20, -180, 180) , runif(20, -80, 80) ),
            data.frame(id = 1:20),
            proj4string = CRS("+proj=longlat")
            )

          writeOGR(t2, DSN, layer = 't2', driver = 'MySQL', layer_options='ENGINE=Aria', overwrite_layer = TRUE)

       #t3 [ spatial table (polygons) ]
          t3 = getMap()[1:10, 1:3]
          t3  =spChFIDs(t3, as.character(1:nrow(t3) ) )
          writeOGR(t3, DSN, layer = 't3', driver = 'MySQL', layer_options='ENGINE=Aria', overwrite_layer = TRUE)


      }
 }
