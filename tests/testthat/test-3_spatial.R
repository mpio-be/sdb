
host     =  '127.0.0.1'
user     =  'testuser'
pwd      =  'cs'
db       =  'tests'
credpath =  tempfile()
saveCredentials(user, pwd, host = host, path = credpath)

test_db(user = user, host = host, db = db, pwd = pwd)



context("spatial")

 test_that("dbq with an mysql-gdal driver returns a  DNS string ", {
    con = dbcon(user, pwd, host = host, db = db, driver = 'mysql_gdal',  path = credpath)
    expect_that(inherits(con, "character"), is_true())
    

    })


 test_that("dbq returns a sf object when geom is given", {
    con = dbcon(user, pwd, host = host, db = db, path = credpath)
    
    s = dbq(con, q = 'select * from tests.t3', geom = 'SHAPE')

    expect_that(inherits(s, "sf"), is_true())

    dbDisconnect(con)

    })






test_db(user = user, host = host, db = db, pwd = pwd, destroy = TRUE)