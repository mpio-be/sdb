# ====================================================================================
# TEST BATCH for RMySQL/RMariaDB functions against scidb
# User and DB needs to be in place
#   CREATE USER 'testuser'@'%' ;
#   UPDATE mysql.user SET Password=PASSWORD('cs') WHERE User='testuser' AND Host='%' ;
#   GRANT ALL  ON tests.* TO 'testuser'@'%' ;
#   FLUSH PRIVILEGES ;
# ====================================================================================

host     =  'scidb.mpio.orn.mpg.de'
user     =  'testuser'
pwd      =  'cs'
db       =  'tests'
credpath =  tempfile()

test_db(user = user, host = host, db = db, pwd = pwd)




context("RMySQL - sqlAppendTable")

 test_that("dbSafeWriteTable works as expected", {

    x = data.table(a = rep('a', 10), b = rnorm(10) )

    con = RMySQL::dbConnect( dbDriver('MySQL') , user = user, password = pwd, host = host, dbname = db)
    dbExecute(con, 'DROP TABLE IF EXISTS temp')
    dbExecute(con, 'CREATE TABLE temp (a VARCHAR(50) NULL, b FLOAT NULL)' )


    st = sqlAppendTable(con, 'temp', x, row.names = FALSE)
    expect_equal( dbExecute(con, st), 10)
    expect_is( dbReadTable(con, 'temp'), 'data.frame' )

    dbExecute(con, 'DROP TABLE temp')
    dbDisconnect(con)

    })

context("RMariaDB - sqlAppendTable")

 test_that("dbSafeWriteTable works as expected", {

    x = data.table(a = rep('a', 10), b = rnorm(10) )

    con = RMariaDB::dbConnect( dbDriver('MariaDB') , user = user, password = pwd, host = host, dbname = db)
    dbExecute(con, 'DROP TABLE IF EXISTS temp')
    dbExecute(con, 'CREATE TABLE temp (a VARCHAR(50) NULL, b FLOAT NULL)' )

    st = sqlAppendTable(con, 'temp', x, row.names = FALSE)
    expect_equal( dbExecute(con, st), 10)
    expect_is( dbReadTable(con, 'temp'), 'data.frame' )
    dbExecute(con, 'DROP TABLE temp')

    dbDisconnect(con)

    })







# ====================================================================================


test_db(user = user, host = host, db = db, pwd = pwd, destroy = TRUE)
