# ====================================================================================
# TEST BATCH for sdb functions against a localhost db
# User and DB needs to be in place
#   CREATE USER 'testuser'@'%' ;
#   UPDATE mysql.user SET Password=PASSWORD('cs') WHERE User='testuser' AND Host='%' ;
#   GRANT ALL  ON tests.* TO 'testuser'@'%' ;
#   FLUSH PRIVILEGES ;
# ====================================================================================

host     =  '127.0.0.1'
user     =  'testuser'
pwd      =  'cs'
db       =  'tests'
credpath =  tempfile()

test_db(user = user, host = host, db = db, pwd = pwd)

# ====================================================================================


context("Credentials")

 test_that("Save & remove credentials works - using non-default path", {

    expect_true( saveCredentials(user, pwd, host = host, path = credpath) )
    con = dbcon(user, drive = "MySQL", host = host, path = credpath); on.exit(closeCon(con))
    expect_true( inherits(con, "MySQLConnection" ) )
    removeCredentials(path = credpath)

    })

 test_that("Save credentials to testuser to default location", {

    expect_true( saveCredentials(user, pwd, host = host) )

    })

 test_that("credentials file exists", {
    expect_true( saveCredentials(user, pwd, host = host, path = credpath) )
    expect_true( credentialsExist(host, path = credpath))

  })



 test_that("Wrong credentials return error", {

    expect_true( saveCredentials(user, 'wrong pwd', host = host, path = credpath) )

    expect_error ( dbcon('test', host = host,  path = credpath ) )

    expect_true( saveCredentials(user, pwd, host = host, path = credpath) )


    })

context("Connections")

 test_that("connections are established and closed properly", {
  con = dbcon(user, pwd, host = host, drive = "MySQL" )
  expect_true( inherits(con, "MySQLConnection" ) )
  expect_true( closeCon(con ) )
  })

 test_that("when db is given then the default db is active", {
  con = dbcon(user, pwd, host = host, driver = "MySQL", db = db,  path = credpath)
  expect_true( names(dbq(con, 'show tables')) == paste0('Tables_in_', db))
  closeCon(con )
  })


 test_that("default dbcon connects to RMySQL", {
  con = dbcon(user, pwd, host = host,  path = credpath)
  expect_true( class(con) == "MySQLConnection" )
  closeCon(con)
  })

 test_that("spatial_MySQL driver returns an ogrinfo object", {
  expect_error( dbcon(user, pwd, host = host, driver = 'spatial_MySQL',  path = credpath) )

  con = dbcon(user, pwd, host = host, db = db, driver = 'spatial_MySQL',  path = credpath)
  expect_true( class(con) == "ogrinfo" )

  })

context("dbq")

 test_that("dbq returns NULL on non-SELECT and data.table on SELECT", {


    con = dbcon(user=user,host = host, path = credpath) ; on.exit(closeCon(con))

    expect_null( dbq(con, paste('USE', db) ) )
    expect_null( dbq(con, 'DROP TABLE IF EXISTS temp' ) )

    dbWriteTable(con, 'temp', data.table(a = 1:100, b = 'x') )

    expect_true( inherits( dbq(con, "select * from temp"), 'data.table' ))

    expect_null( dbq(con, 'DROP TABLE IF EXISTS temp' ) )

    })

 test_that("dbq can return an enhanced output", {

    con = dbcon(user=user,host = host, pwd = pwd, db = db, path = credpath ); on.exit(closeCon(con))


    dbWriteTable(con, 'temp', data.table(a = seq.POSIXt(Sys.time(), by = 10, length.out = 10), ID = 1), overwrite = TRUE )

    o = dbq(con, "select * from temp", enhance = TRUE)
    expect_is(o, 'data.table'  )
    expect_is(o$a, 'POSIXt'  )

    })

context("spatial")

 test_that("dbq with an ogrinfo con returns a spatial* object ", {
    con = dbcon(user, pwd, host = host, db = db, driver = 'spatial_MySQL',  path = credpath)
    x = dbq(con, q = 't2')
    expect_that(inherits(x, "Spatial"), is_true())

    con = dbcon(user, host = host, db = db, driver = 'spatial_MySQL',  path = credpath)
    x = dbq(con, q = 't3')
    expect_that(inherits(x, "Spatial"), is_true())
    })

 test_that("dbq with an ogrinfo con and an sql query returns a subset", {
    con = dbcon(user, pwd, host = host, db = db, driver = 'spatial_MySQL',  path = credpath)
    x = dbq(con, q = 't2')
    expect_that(inherits(x, "Spatial"), is_true())

    con = dbcon(user, host = host, db = db, driver = 'spatial_MySQL',  path = credpath)
    x = dbq(con, q = 'select * from t3 where scalerank = 1')
    expect_that(inherits(x, "Spatial"), is_true())
    })

context("dbSafeWriteTable")

 test_that("dbSafeWriteTable works as expected", {


    x = data.table(col1 = rep('a', 10010), col2 = rnorm(10010))
    con = dbcon(user=user,host = host,db = db, path = credpath) ; on.exit(closeCon(con))


    dbExecute(con, 'DROP TABLE IF EXISTS temp')

    dbq(con, 'CREATE TABLE temp (col1 VARCHAR(50) NULL,col2 FLOAT NULL)' )
    
    expect_true( dbSafeWriteTable(con, 'temp', x, verbose = FALSE) )
    
    dbExecute(con, 'DROP TABLE temp')



    })


context("dbInsertInto")

 test_that("dbInsertInto works as expected", {


    x = data.table(f1 = rep('a', 10), f2 = rnorm(10), f3 = 1)
    con = dbcon(user=user,host = host,db = db, path = credpath) ; on.exit(closeCon(con))
    dbq(con, 'CREATE TABLE temp (f1 VARCHAR(50) ,f2 FLOAT , f99 INT)' )
    
    expect_equal( dbInsertInto(con, 'temp', x) , 10)


    dbq(con, 'DROP TABLE temp')

    
    })





# ====================================================================================

test_db(user = user, host = host, db = db, pwd = pwd, destroy = TRUE)