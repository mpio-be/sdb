
#' mysqldump
#'
#' @param db      db
#' @param tables  tables are given as a "tableName1 tableName2".
#' @param user    user
#' @param pwd       pwd
#' @param host      default to '127.0.0.1'
#' @param filenam   filenam. If missing then constructed from Sys.time()
#' @param dir       saving location on disk.
#' @param dryrun    when TRUE return call only. default to FALSE.
#' @param compress  when TRUE archive the sql output. default to TRUE.
#' @param ...       further arguments to mysqldump (e.g. --no-data --no-create-db)
#'
#' @return    the file path to the sql file
#' @export
#'
#' @examples
#' \dontrun{
#' fp = mysqldump('tests',  user = 'testuser', dir = tempdir() , dryrun = TRUE)
#' mysqldump('tests', 't1 t2', 'testuser' , dir = tempdir() )
#' mysqldump('tests', 't1', 'testuser' , dir = tempdir(), compress = FALSE )
#' }
#'
#'
mysqldump <- function(db,tables,user, pwd, host = '127.0.0.1', filenam, dir = getwd(), dryrun = FALSE, compress=TRUE, ...) {

	if(missing(filenam) & !missing(tables) ) {
	ntables = strsplit(tables, ' ')[[1]] %>% length 
	if(ntables == 1 ) 
		fname = paste(db, tables, sep = '.')
	if(ntables > 1 ) 
		fname = paste(db, paste0(ntables, 'tables'), sep = '_')
	}

	if(missing(filenam) & missing(tables) ) {
	fname = db
	}


	if(!compress)
	filenam = paste0(fname, '.sql') else
	filenam = paste0(fname, '.sql.gz')
		

	filepath = paste(dir, filenam, sep = .Platform$file.sep)

	if(missing(pwd))
	crd = getCredentials(user = user, host = host) else
	crd = data.frame(user = user, pwd = pwd, host = host)

	syscall = paste0('mysqldump        --host=',      crd$host,
				' --user=' ,     crd$user,
				' --password=' , crd$pwd,
				' --databases ',  db,
				if(!missing(tables))   paste(' --tables ', paste(tables, collapse = " ") ) else NULL ,
				' --routines ',
				if(!compress) paste0(' --result-file=', filepath) else NULL,
				' --verbose ', ...)

	if(compress)
		syscall = paste0(syscall, " | gzip >", filepath)

	if(dryrun)  cat(syscall, '\n-------')

	if(!dryrun)	system(syscall, wait = TRUE)

	cat('Output file:', filepath, '\n')
	cat('File size:', file.size(filepath), '\n')

	return(filepath)
 }



#' mysqldump_host
#' 
#' @param host    default to '127.0.0.1'
#' @param user    user
#' @param pwd     pwd
#' @param dir     saving location on disk.
#' @param exclude  db-s to exclude default to c('mysql', 'information_schema', 'performance_schema')
#' @export

#' @examples
#' \dontrun{
#' mysqldump_host('127.0.0.1',  'mihai', dir = '~/Desktop' )
#' 
#' }

mysqldump_host <- function(host = '127.0.0.1', user, pwd, dir, exclude = c('tests', 'information_schema', 'performance_schema') ) {
	
	# INI
		started.at=proc.time()
		if(missing(pwd))
		crd = getCredentials(user = user, host = host) else
		crd = data.frame(user = user, pwd = pwd, host = host)

		con = dbcon(user=user, host = host); on.exit(closeCon(con))

		dbExecute(con, " SET GLOBAL max_connections = 300;")

	# table listing
		x = dbq(con, 'SELECT TABLE_SCHEMA db, TABLE_NAME tab, TABLE_TYPE TYPE, 
							TABLE_ROWS nrows FROM information_schema.`TABLES`')
		x = x[! db %in% exclude]

	# prepare dir locations and tables paths 
		maindir = paste0(dir, '/backup_', host, '_', format(Sys.time(), "%m-%d-%y-%HH"))

		stopifnot( ! dir.exists(maindir))
		dir.create(maindir, recursive = TRUE)

		z = unique(x[, .(db)])
		z[, path := paste0(maindir, '/', db)]
		z[, dir.create(path), by = path]

		# prepare tables paths 	
		x[, path := paste0(maindir, '/', db) ]

	# RUN
		doFuture::registerDoFuture()
		future::plan(future::multiprocess)


		foreach( i = 1:nrow(x) )  %dopar% {
				
				x[i, mysqldump(db = db, tables = tab, host = host, user = user, pwd = crd$pwd, dir = path) ]
				}
	
	# CHECK
		x[, path := paste0(path,'/', db, '.', tab, '.sql.gz')]
		x[ , fsize := file.size(path)/1e+6, by = path ]

	# write output
	 fwrite(x, file = paste0(maindir, '/mysqldump_out.txt'))
	 cat(paste('time taken = ', timetaken(started.at) ), file = paste0(maindir, '/mysqldump_log.txt') )	
	 cat(paste('finished at = ',  Sys.time() ), file = paste0(maindir, '/mysqldump_log.txt'), append = TRUE , sep = "\n")	

	 cat("Finished in",timetaken(started.at),"\n")

}




#' mysqlrestore
#'
#' restore sql file locally
#' @param file    sql or sql.gz file
#' @param db      database name
#' @param user    user, default to 'root'
#' @param host    default to  '127.0.0.1'
#' @param dryrun only print the mysql cli call and exit
#' @export
#'
#' @examples
#' \dontrun{
#' fp =  mysqldump(db = 'tests', table='tests', user='testuser' , dir = tempdir(), compress = FALSE )
#' mysqlrestore(file = fp, db = 'tests', user = 'testuser')
#' 
#' fp =  mysqldump(db = 'tests', table='tests', user='testuser' , dir = tempdir(), compress = TRUE )
#' mysqlrestore(file = fp, db = 'tests', user = 'testuser')
#' 
#' }
#'
#'
mysqlrestore <- function(file, db, user , host =  '127.0.0.1', dryrun = FALSE) {

	con = dbcon(user = user, host = host); 	on.exit(closeCon(con))


	if( !missing(db) )
	DBI::dbExecute(con, paste('CREATE DATABASE IF NOT EXISTS', db))

	crd = getCredentials(user = user, host = host)
	crd$user = paste('-u', crd$user)
	if(!is.na(crd$pwd))
		crd$pwd = paste0('-p', crd$pwd) else
		crd$pwd = ''

	mysqlCall = 	paste('mysql  --max-allowed-packet 2GB --net_buffer_length=1000000', paste0('-h', host),  crd$user , crd$pwd, db)

	if(tools::file_ext(file) == 'sql')
		syscall = paste(mysqlCall, '<', file )


	if(tools::file_ext(file) == 'gz')
		syscall = paste('gunzip -c', shQuote(file), "|", mysqlCall)

	if(dryrun)
		cat('\n----------\n', syscall, '\n----------\n')
	
	if(!dryrun)		
	system(syscall, wait = TRUE)

	}



#' mysqlrestore_host
#'
#' restore an entire db system or several db-s
#' @param dir      a directory containing all the sql files.
#' @param wipe     drop all non-system db-s before restore. default to FALSE
#' @param ...      further options passed to mysqlrestore
#' @export
#' 
#' @importFrom foreach foreach %dopar% 
#' @importFrom future  plan 
#' @importFrom doFuture registerDoFuture  
#' 
#' @examples
#' \dontrun{
#'  mysqldump_host('127.0.0.1',  'mihai', dir = '/home/mihai/Desktop' )
#'  mysqlrestore_host(dir = '~/Desktop/backup_127.0.0.1_09-11-19-12H',user = 'mihai', wipe = TRUE)
#' }
#'
#'
mysqlrestore_host <- function(dir, wipe = FALSE, ... ) {

	# INI
		started.at=proc.time()

		# fetch mysqldump_out.txt
		x = fread(paste0(dir, '/mysqldump_out.txt'))
		ok = all(names(x) == c("db", "tab", "TYPE", "nrows", "path", "fsize"))
		if(!ok) stop(dQuote(dir), ' does not contain a valid backup!')


		con = dbcon(user=user, host = host); on.exit(closeCon(con))

		DBI::dbExecute(con, " SET GLOBAL max_connections = 300;")


	# WIPE
		if(wipe){
			z = dbq(con, "SELECT DISTINCT TABLE_SCHEMA db FROM information_schema.`TABLES` 
								WHERE TABLE_SCHEMA 
								NOT IN ('mysql', 'information_schema', 'performance_schema')")
			z[, DBI::dbExecute(con, paste('DROP DATABASE', db) ), by = 1:nrow(z)]

		}


	# RUN
		doFuture::registerDoFuture()
		future::plan(future::multiprocess)



		foreach( i = 1:nrow(x) )  %dopar% {

			x[i, mysqlrestore(file = path, db = db, ... ) ]

			}


	 cat("Finished in",timetaken(started.at),"\n")


 }


































