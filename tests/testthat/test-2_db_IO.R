
host     =  '127.0.0.1'
user     =  'testuser'
pwd      =  'cs'
db       =  'tests'
credpath =  tempfile()
saveCredentials(user, pwd, host = host, path = credpath)

test_db(user = user, host = host, db = db, pwd = pwd)


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


context("remove duplicates")

 test_that("removeDuplicates do", {

    x = data.table(f1 = rep('a', 10), f2 = rnorm(10), f3 = 1)
    x = rbind(x,x)
    x[, id := 1:.N]

    con = dbcon(user=user,host = host,db = db, path = credpath) ; on.exit(closeCon(con))

    dbWriteTable(con,'temp_test', x, row.names = FALSE)

    expect_is( removeDuplicates(con, 'temp_test', 'id'), 'data.frame')


    dbq(con, 'DROP TABLE temp_test')
    
    })









test_db(user = user, host = host, db = db, pwd = pwd, destroy = TRUE)