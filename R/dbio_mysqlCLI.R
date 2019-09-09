
#' mysqldump
#'
#' @param db      db
#' @param tables  tables are given as a "tableName1 tableName2".
#' @param user    user
#' @param pwd     pwd
#' @param host    default to '127.0.0.1'
#' @param filenam filenam. If missing then constructed from Sys.time()
#' @param dir     saving location on disk.
#' @param dryrun  when TRUE return call only
#' @param ...     further arguments to mysqldump (e.g. --no-data --no-create-db)
#'
#' @return    the file path to the sql file
#' @export
#'
#' @examples
#' \dontrun{
#' fp = mysqldump('BTatWESTERHOLZ',  user = 'mihai', dir = tempdir() , dryrun = TRUE)
#' mysqldump('BTatWESTERHOLZ', 'ADULTS BREEDING', 'mihai' , dir = tempdir() )
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

		con = dbcon('mihai', host = host); on.exit(closeCon(con))

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
#' fp = mysqldump('tests', user = 'mihai' , dir = tempdir(), host = '127.0.0.1' )
#' mysqlrestore(file = fp, db = 'tests', user = 'mihai')
#' 

#' }
#'
#'
mysqlrestore <- function(file, db, user , host =  '127.0.0.1', dryrun = FALSE) {

	con = dbcon(user = user, host = host); 	on.exit(closeCon(con))


	if( !missing(db) )
	dbExecute(con, paste('CREATE DATABASE IF NOT EXISTS', db))

	crd = getCredentials(user = user, host = host)
	crd$user = paste('-u', crd$user)
	if(!is.na(crd$pwd))
		crd$pwd = paste0('-p', crd$pwd) else
		crd$pwd = ''

	mysqlCall = 	paste('mysql  --max-allowed-packet 4GB --net_buffer_length=1000000', paste0('-h', host),  crd$user , crd$pwd, db)

	if(tools::file_ext(file) == 'sql')
		syscall = paste(mysqlCall, '<', file )


	if(tools::file_ext(file) == 'gz')
		syscall = paste('gunzip -c', shQuote(file), "|", mysqlCall)

	if(dryrun)
		cat('\n----------\n', syscall, '\n----------\n')
	
	if(!dryrun)		
	system(syscall, wait = TRUE)

	}



#' mysqlrestore_host (TODO)
#'
#' restore an entire db system or several db-s
#' @param dir      a directory containing all the sql files (see mysqlrestore).
#' @param filetype default to .sql.gz
#' @param exclude  db-s to exclude default to c('mysql', 'information_schema', 'performance_schema')
#' @param progress progress file, default to '/tmp/monitor_progress.txt'. use tail -f /tmp/monitor_progress.txt
#' @param ...      further options passed to mysqlrestore
#' @export
#' 
#' @importFrom foreach foreach %dopar% 
#' @importFrom future  plan 
#' @importFrom doFuture registerDoFuture  
#' 
#' @examples
#' \dontrun{
#' mysqlrestore_host('..../scidb_backup_5.01.17/',user = 'mihai', host =  '127.0.0.1', verbose = TRUE)
#' }
#'
#'
mysqlrestore_host <- function(dir, filetype = ".sql.gz",
										exclude = c('mysql', 'information_schema', 'performance_schema'),
										progress = '/tmp/monitor_progress.txt', ...) {

	on.exit( file.remove(progress) )

	registerDoFuture()
	plan(multiprocess)

	# restore
	sqlfiles = list.files(dir, pattern = filetype, full.names = TRUE, recursive = TRUE)

		# TODO: change to doFuture
		foreach( i = 1:length(sqlfiles) )  %dopar% {
			fi = sqlfiles[i]
			dbi = basename(dirname(fi))
			write.table(data.frame(i, basename(fi)), file = progress, append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)

			if(!dbi%in%exclude)
				mysqlrestore(file = fi, db = dbi, ...)

			}

 }


































