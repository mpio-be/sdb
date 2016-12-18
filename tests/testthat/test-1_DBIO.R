host  = if(I_am_in_Seewiesen() )  'scidb.mpio.orn.mpg.de' else 'localhost'
user  = 'test'
pwd   = 'cs'
dbname = 'tests'

context("Credentials")

 test_that("credentials file exists", {

  expect_true( credentialsExist(host))
  
  })

 test_that("Save credentials works", {

  expect_true( saveCredentials(user, pwd, host = host) )
  con = dbcon(user, drive = driverIni("MySQL") ); on.exit(closeCon(con))
  expect_true( inherits(con, "MySQLConnection" ) )

  
  })


 test_that("Wrong credentials return error", {

  expect_true( saveCredentials(user, 'wrong pwd', host = host) )

  expect_error ( dbcon('test', host = host ) )

  expect_true( saveCredentials(user, pwd, host = host) )

  
  })

context("Connections")

 test_that("driverIni returns the correct driver", {

  expect_true( inherits( driverIni("MySQL") , "MySQLDriver") )
  
  expect_true( inherits( driverIni("postgres") , "PostgreSQLDriver") )
  
  expect_true( inherits( driverIni("postgres_OGR") , "postgres_OGR") )

  })

 test_that("connections are established and closed properly", {

  con = dbcon(user, pwd, drive = driverIni("MySQL") )
  expect_true( inherits(con, "MySQLConnection" ) )
  expect_true( closeCon(con ) )


  expect_error(dbcon(user, pwd, drive = driverIni("postgres") ) ) # no dbname
  con = dbcon(user, pwd, dbname, drive = driverIni("postgres") )
  expect_true( inherits(con, "PostgreSQLConnection" ) )
  expect_true( closeCon(con ) )

  con = dbcon(user, pwd, dbname, drive = driverIni("postgres_OGR") )
  expect_true( inherits(con, "OGR_PostgreSQLDriver" ) )
  expect_true( closeCon(con ) )


  })

 test_that("default dbcon connects to MariaDB", {

  con = dbcon(user, pwd)
  expect_true( class(con) == "MySQLConnection" )
  closeCon(con)

  })

context("mysql IO")

 test_that("dbq returns NULL on non-SELECT and data.table on SELECT", {

  con = dbcon(user,host = host, drive = driverIni("MySQL") ); on.exit(closeCon(con))

  expect_null( dbq(con, paste('USE', dbname) ) )
  expect_null( dbq(con, 'DROP TABLE IF EXISTS temp' ) )

  dbWriteTable(con, 'temp', data.table(a = 1:100, b = 'x') )

  expect_true( inherits( dbq(con, "select * from temp"), 'data.table' ))

  })

 test_that("dbq can return an enhanced output", {

  con = dbcon(user,host = host, drive = driverIni("MySQL") ); on.exit(closeCon(con))

  dbq(con, paste('USE', dbname) )

  dbWriteTable(con, 'temp', data.table(a = seq.POSIXt(Sys.time(), by = 10, length.out = 10), ID = 1), overwrite = TRUE )

  o = dbq(con, "select * from temp", enhance = F)

  })












