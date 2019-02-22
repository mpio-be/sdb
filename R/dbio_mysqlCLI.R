

#' dumps tables between databases (wrapper to mysqldump and mysql CLI)
#'
#' my_remote2local uses mysql CLI and mysqldump to copy tables from remote to localhost
#'
#' @return              NULL or the call itself if call_only = TRUE
#' 
#' @param db            database name
#' @param tables        tables are given as a "tableName1 tableName2".
#' @param remoteUser    remoteUser
#' @param remoteHost    remoteHost
#' @param localHost     localHost Default to '127.0.0.1'
#' @param localUser     localUser. Default to 'root'
#' @param map      		max_allowed_packet in GB (see https://dev.mysql.com/doc/refman/5.5/en/mysql-command-options.html)
#' @param call_only     Default to FALSE
#' @param no_data       Default to FALSE
#' 
#' @note           		non-table objects (views, procedures, etc) are exported by default.
#' @export
#' @examples \dontrun{
#' my_remote2local("dbnam", "table", "user")
#' sysCall = my_remote2local("dbnam", "table", "user", call_only = TRUE)
#' system(paste ("ssh user_at_host", sysCall) )
#'}
my_remote2local <- function(db,	tables,	remoteUser,
	remoteHost = 'scidb.mpio.orn.mpg.de',
	localHost  =  '127.0.0.1', localUser  = 'root',
	map        = 1,	
	call_only  = FALSE,
	no_data    = FALSE	) {

	localhost = getCredentials(user = localUser, host = localHost)
	remote    = getCredentials(user = remoteUser, host = remoteHost)

	localCon = dbcon(user = localhost$user, pwd = localhost$pwd, host = localhost$host)
	on.exit(closeCon(localCon))

	#DB INI
	dbq(localCon, paste('CREATE DATABASE IF NOT EXISTS', db )  )


	mysqldump = paste0('mysqldump --host=',      remote$host,
								' --user=' ,     remote$user,
								' --password=' , remote$pwd,
								' --databases ',  db,
	if(!missing(tables))   paste(' --tables ',     paste(tables, collapse = " ") ) else NULL ,
								' --routines',
								' --verbose')

	if(no_data)	mysqldump = paste(mysqldump,'--no-data')


	if( is.na(localhost$pwd) )
	mysql = paste0('mysql --host=',localHost ,' --user=', localhost$user ,' --database=', db, ' --max_allowed_packet=',map,'GB ') else
	mysql = paste0('mysql --host=', localHost,' --user=', localhost$user ,' --password=', localhost$pwd ,' --database=', db, ' --max_allowed_packet=',map,'GB ')

	call = paste(mysqldump, mysql, sep = "|")

	if(call_only) call else

	system( call )

	}

#' mysqldump
#'
#' @param db      db
#' @param tables  tables are given as a "tableName1 tableName2".
#' @param user    user
#' @param pwd     pwd
#' @param host    default to 'scidb.mpio.orn.mpg.de'
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
#' fp = mysqldump('BTatWESTERHOLZ',  user = 'mihai', dir = tempdir() )
#' mysqldump('BTatWESTERHOLZ', 'ADULTS BREEDING', 'mihai' , dir = tempdir() )
#' }
#'
#'
mysqldump <- function(db,tables,user, pwd, host = 'scidb.mpio.orn.mpg.de', filenam, dir = getwd(), dryrun = FALSE, ...) {

  if(missing(filenam))
    filenam = Sys.time() %>%
    		as.character %>%
    		make.names %>%
    		gsub("X", "", .) %>%
    		paste0('dbdump_', ., '.sql')

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
	            ' --result-file=', filepath,
	            ' --verbose ', ...)

	if(dryrun)  cat(syscall, '\n-------')

	if(!dryrun)	system(syscall, wait = TRUE)

	cat('Output file:', filepath, '\n')
	cat('File size:', file.size(filepath), '\n')

	return(filepath)
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
#' db = 'BTatWESTERHOLZ'
#' fp = mysqldump(db, 'ADULTS BREEDING', 'mihai' , dir = tempdir() )
#' mysqlrestore(file = fp, db = db)
#'
#' fp = mysqldump(db, user = 'mihai' , dir = tempdir() )
#' mysqlrestore(file = fp, db = db)
#' }
#'
#'
mysqlrestore <- function(file, db, user = 'root', host =  '127.0.0.1', dryrun = FALSE) {

	con = dbcon(user = user, host = host); 	on.exit(closeCon(con))


	if( !missing(db) )
	dbq(con, paste('CREATE DATABASE IF NOT EXISTS', db))

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



#' mysqlrestoreSITE
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
#' mysqlrestoreSITE('..../scidb_backup_5.01.17/',user = 'mihai', host =  '127.0.0.1', verbose = TRUE)
#' }
#'
#'
mysqlrestoreSITE <- function(dir, filetype = ".sql.gz",
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


































