


#' @title Test database
#' @description Installs or destroys a mariadb test database
#' 
#' @param  user        default to 'testuser'
#' @param  host        default to '127.0.0.1'
#' @param  db          default to 'tests'
#' @param  pwd         pwd
#' @param  destroy     default to FALSE
#' 
#' @export
#' @note
#' test_db() works independently on other functions in sdb
#'

test_db <- function(user = 'testuser', host =  '127.0.0.1', db = 'tests', pwd, destroy = FALSE) {
  sapply(c('rgdal', 'rworldmap'),
    function(x) require(x , character.only = TRUE) )

  DSN = paste0('MySQL:',db ,',user=', user, ',host=', host, ',password=', pwd)

  # pwd = readLines('~/.my.cnf')[grep('password', readLines('~/.my.cnf'))]
  # pwd = str_extract(pwd, '[^=]*$') %>% str_trim() %>% str_replace_all("'", "") %>% str_replace_all('"', "")

  con = dbConnect(RMariaDB::MariaDB(), user = user, host = host, password = pwd)
  on.exit(dbDisconnect(con))

  if(destroy)
  dbExecute(con, paste('DROP DATABASE IF EXISTS', db))

  dbExecute(con, paste('CREATE DATABASE IF NOT EXISTS', db))
  dbExecute(con, paste('USE', db))

  #T1 [ a table with major types]
    dbExecute(con, "DROP TABLE IF EXISTS T1")
    dbExecute(con, "
        CREATE TABLE T1(
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

      T1 = data.frame(n1 = rpois(100, 2), n2 = rnorm(100), n3 = rnorm(100, 200), n4 = runif(100, 0, 1),
              n5 = sample(c('x','y','q'), 100, replace = TRUE),
              v1 = replicate(100, paste( sample(letters, size = runif(1, 1, 20) ), collapse = '') ),
              v2 = replicate(100, paste(sample(letters, 2), collapse = '')),
              dt1 = Sys.Date() + rpois(100, 5),
              dt2 = format(Sys.time() + rnorm(100) , '%H:%M:%S'),
              dt3 = Sys.time() + rnorm(100)
              )

    dbWriteTable( con, 'T1', T1, row.names = FALSE, overwrite = TRUE )


  # View
  dbExecute(con, 'CREATE OR REPLACE VIEW View_t1 as SELECT * from T1 where n1 = 0')    

  #t2 [ sf POINTS]
    dbExecute(con, "DROP TABLE IF EXISTS t2")
    t2 = st_as_sf(
      data.frame( x = runif(20, -180, 180) , y = runif(20, -80, 80), id = 1:20 ),
      coords = c('x', 'y'),
      crs = st_crs(4326)
      )

    st_write(t2, dsn = DSN, layer = "t2", layer_options = "ENGINE=Aria")


  #t3 [ sf MULTIPOLYGON]
    dbExecute(con, "DROP TABLE IF EXISTS t3")
    t3 = st_read(system.file("shape/nc.shp", package = "sf"))
    st_write(t3, dsn = DSN, layer = "t3", layer_options = "ENGINE=Aria")

      
 }
