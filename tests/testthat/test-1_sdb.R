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
    con = dbcon(user, drive = "MariaDB", host = host, path = credpath); on.exit(closeCon(con))
    expect_true( inherits(con, "MariaDBConnection" ) )
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
  con = dbcon(user, pwd, host = host, driver = "MariaDB" )
  expect_true( inherits(con, "MariaDBConnection" ) )
  expect_true( closeCon(con ) )
  })

 test_that("when db is given then the default db is active", {
  con = dbcon(user, pwd, host = host, driver = "MariaDB", db = db,  path = credpath)
  expect_true( names(dbGetQuery(con, 'show tables')) == paste0('Tables_in_', db))
  closeCon(con )
  })


 test_that("default dbcon connects to MariaDB", {
  con = dbcon(user, pwd, host = host,  path = credpath)
  expect_true( class(con) == "MariaDBConnection" )
  closeCon(con)
  })

context("dbq")


 test_that("dbq can return an enhanced output", {

    con = dbcon(user=user,host = host, pwd = pwd, db = db, path = credpath ); on.exit(closeCon(con))


    dbWriteTable(con, 'temp', data.table(a = seq.POSIXt(Sys.time(), by = 10, length.out = 10), ID = 1), overwrite = TRUE )

    o = dbq(con, "select * from temp", enhance = TRUE)
    expect_is(o, 'data.table'  )
    expect_is(o$a, 'POSIXt'  )

    })



# ====================================================================================

# test_db(user = user, host = host, db = db, pwd = pwd, destroy = TRUE)