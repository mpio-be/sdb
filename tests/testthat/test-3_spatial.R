
host     =  '127.0.0.1'
user     =  'testuser'
pwd      =  'cs'
db       =  'tests'
credpath =  tempfile()
saveCredentials(user, pwd, host = host, path = credpath)

test_db(user = user, host = host, db = db, pwd = pwd)
con = dbcon(user, pwd, host = host, db = db, path = credpath)


context("spatial")

 test_that("dbq with an mysql-gdal driver returns a  DNS string ", {
	con = dbcon(user, pwd, host = host, db = db, driver = 'mysql_gdal',  path = credpath)
	expect_true(inherits(con, "character") )
	

	})


 test_that("spatial dbq returns a sf object when geom is given", {
	
	s = dbq(con, 'select SHAPE from tests.t3', geom = 'SHAPE')
	expect_true(inherits(s, "sf") )

	}) 

 test_that("spatial dbq fails for select * FROM queries", {
	
	expect_error( dbq(con, 'select * from tests.t3', geom = 'SHAPE') )

	}) 


 test_that("spatial dbq play with JOINS", {
	
	expect_warning(dbq(con, q = 'SELECT t3.OGR_FID, t3.SHAPE, t1.n2 FROM 
						t3 JOIN t1 ON t3.OGR_FID = t1.n1 
							WHERE n1 < 4', geom = 'SHAPE') )

	}) 






dbDisconnect(con)
#test_db(user = user, host = host, db = db, pwd = pwd, destroy = TRUE)