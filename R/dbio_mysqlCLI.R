
#' mysqldump
#'
#' @param db        db
#' @param tables    tables are given as a "tableName1 tableName2".
#' @param user      user
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
				' --default-character-set=utf8mb4 --verbose', ...)

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
#' @param host      default to '127.0.0.1'
#' @param user      user
#' @param pwd       pwd
#' @param dir       saving location on disk.
#' @param exclude   db-s to exclude default to c('mysql', 'information_schema', 'performance_schema')
#' @param parallel  default to TRUE
#' @export

#' @examples
#' \dontrun{
#' sdb::mysqldump_host('127.0.0.1',  user = 'mihai', dir = '~/Desktop', parallel = FALSE )
#' 
#' }

mysqldump_host <- function(host = '127.0.0.1', user, pwd, dir, exclude = c('mysql', 'information_schema', 'performance_schema'), parallel = TRUE ) {
	
	# INI
		started.at=proc.time()
		if(missing(pwd))
		crd = getCredentials(user = user, host = host) else
		crd = data.frame(user = user, pwd = pwd, host = host)

		con = dbcon(user=user, host = host); on.exit(closeCon(con))


		if(parallel) {
			DBI::dbExecute(con, "SET GLOBAL max_connections = 300;")
			doFuture::registerDoFuture()
			future::plan(future::multiprocess)
			}

		# table listing
		x = dbq(con, 'SELECT TABLE_SCHEMA db, TABLE_NAME tab, TABLE_TYPE TYPE, 
							TABLE_ROWS nrows FROM information_schema.`TABLES`')
		x = x[! db %in% exclude]

		# prepare dir locations 
		maindir = paste0(dir, '/backup_', host, '_', format(Sys.time(), "%m-%d-%y-%HH"))
		if(dir.exists(maindir)) stop(dir, " directory exists!")

		dir.create(maindir, recursive = TRUE)
		dir.create(paste0(maindir, '/DATA'), recursive = TRUE)
		dir.create(paste0(maindir, '/VIEWS'), recursive = TRUE)
		dir.create(paste0(maindir, '/mysql'), recursive = TRUE)
		

		z = unique(x[, .(db)])
		z[, path := paste0(maindir, '/DATA/', db)]
		z[, dir.create(path), by = path]

		# prepare tables paths 	
		x[, path := paste0(maindir, '/DATA/', db) ]
		x[TYPE == 'VIEW', path := paste0(maindir, '/VIEWS') ]

	# DUMP data
		b = x[TYPE == 'BASE TABLE']

		foreach( i = 1:nrow(b) )  %dopar% {
				
				b[i, mysqldump(db = db, tables = tab, host = host, user = user, pwd = crd$pwd, dir = path) ]
				}
	
	# DUMP views	
		v = x[TYPE == 'VIEW']
		v[, mysqldump(db = db, tables = tab, host = host, user = user, pwd = crd$pwd, dir = path), by = 1:nrow(v) ]

	# DUMP mysql.users
		
		mysqldump(db = 'mysql', tables = 'user', host = host, user = user, pwd = crd$pwd, dir = paste0(maindir, '/mysql') )

	# FEEDBACK

		msg = paste(timetaken(started.at), 
			  system(paste('du -h --max-depth=0', maindir) , intern = TRUE) , 
			  system(paste('find',  maindir , '-type f | wc -l') , intern = TRUE) %>% paste(., 'files'), sep = '\n' )


		pushoverr::pushover(msg, user = 'ucaxcbwj94h7tthuuax89soav2i2yk', app = 'a6geyo6f85gkca8w3y6dmopq8nhew4')

	#return path 
		
		maindir
	

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
#' @param dir       a directory containing all the sql files.
#' @param host      default to localhost
#' @param user      user
#' @param pwd       pwd
#' @param wipe      drop all non-system db-s before restore. default to FALSE
#' @param mysqldb   restore mysql db?. default to FALSE
#' @param parallel  default to TRUE
#' @param ...       further options passed to mysqlrestore
#' @export
#' 
#' @importFrom foreach foreach %dopar% 
#' @importFrom future  plan 
#' @importFrom doFuture registerDoFuture  
#' 
#' @examples
#' \dontrun{
#'  fp = sdb::mysqldump_host('127.0.0.1',  'mihai', dir = '/home/mihai/Desktop', parallel = FALSE )
#'  sdb::mysqlrestore_host(dir = fp ,user = 'mihai', wipe = TRUE, mysqldb = TRUE)
#' }
#'
#'
mysqlrestore_host <- function(dir, host = '127.0.0.1', user ,pwd, wipe = FALSE, mysqldb = FALSE, parallel = TRUE ) {

	# INI
		started.at=proc.time()
		if(missing(pwd))
		crd = getCredentials(user = user, host = host) else
		crd = data.frame(user = user, pwd = pwd, host = host)

		con = dbcon(user=user, host = host); on.exit(closeCon(con))


		if(parallel) {
			DBI::dbExecute(con, "SET GLOBAL max_connections = 300;")
			doFuture::registerDoFuture()
			future::plan(future::multiprocess)
			}

		# db-s
		o = data.table(maindirs = list.dirs(dir, full.names = FALSE, recursive = FALSE) )
		if(!all( o$maindirs == c('DATA', 'mysql', 'VIEWS') ) ) stop('invalid backup directory.')

		# DATA dump file listing
		d = data.table(table_dumps = list.files(paste0(dir, '/DATA'), full.names = TRUE, recursive = TRUE) )
		d[, db := dirname(table_dumps) %>% basename]
	
		# VIEWs file listing
		v = data.table(views = list.files(paste0(dir, '/VIEWS'), full.names = TRUE, recursive = TRUE) )
		v[, db := str_split(basename(views) , '\\.', simplify = TRUE)[1]  , by = 1:nrow(v)]
		
		# mysqldb user file listing
		myusers = list.files(paste0(dir, '/mysql'), full.names = TRUE, recursive = TRUE)

	



	# WIPE
		if(wipe){
			z = dbq(con, "SELECT DISTINCT TABLE_SCHEMA db FROM information_schema.`TABLES` 
								WHERE TABLE_SCHEMA 
								NOT IN ('mysql', 'information_schema', 'performance_schema')")
			z[, DBI::dbExecute(con, paste('DROP DATABASE', db) ), by = 1:nrow(z)]

		}

	# RESTORE DATABASES
		x = unique(o[, .(db)])
		x[, DBI::dbExecute(con, paste('CREATE DATABASE IF NOT EXISTS', db)), by = db]
		

	# Restore DATA

		foreach( i = 1:nrow(d) )  %dopar% {

			d[i, mysqlrestore(file = table_dumps, db = db, host = host, user = user) ]

			}

	# Restore VIEWS		
		v[, mysqlrestore(file = views, db = db, host = host, user = user), by = 1:nrow(v) ]		
	
	# Restore USERS		
		mysqlrestore(file = myusers, db = 'mysql', host = host, user = user)
		DBI::dbExecute(con, 'FLUSH PRIVILEGES')


	# FEEDBACK

		msg = timetaken(started.at) 

		pushoverr::pushover(msg, user = 'ucaxcbwj94h7tthuuax89soav2i2yk', app = 'ax5vtoj4uduec4idegic4swux1ut7c')



 }


































