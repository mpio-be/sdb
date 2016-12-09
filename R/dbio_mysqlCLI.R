

#' dumps tables between databases (wrapper to mysqldump and mysql CLI)
#'
#' my_remote2local uses mysql CLI and mysqldump to copy tables from remote to localhost
#'
#' @return       NULL or the call itself if call_only = TRUE
#' @param db     database name
#' @param tables   tables are given as a "tableName1 tableName2".
#' @param map    max_allowed_packet in GB (see https://dev.mysql.com/doc/refman/5.5/en/mysql-command-options.html)
#' @note        non-table objects (views, procedures, etc) are exported by default.
#' @export
#' @examples \dontrun{
#' my_remote2local("dbnam", "table", "user")
#' sysCall = my_remote2local("dbnam", "table", "user", call_only = TRUE)
#' system(paste ("ssh user_at_host", sysCall) )
#'}
my_remote2local <- function(db,	tables,	remoteUser,
	remoteHost = 'scidb.mpio.orn.mpg.de',
	localHost  = 'localhost',localUser  = 'root',
	map        = 1,	call_only  = FALSE,
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
#' @param db     db
#' @param tables tables are given as a "tableName1 tableName2".
#' @param user    user
#' @param host    default to 'scidb.mpio.orn.mpg.de'
#' @param call 	 show mysql call
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
mysqldump <- function(db,tables,user,host = 'scidb.mpio.orn.mpg.de', filenam, dir = getwd(), call = FALSE) {

  if(missing(filenam))
    filenam = Sys.time() %>%
    		as.character %>%
    		make.names %>%
    		gsub("X", "", .) %>%
    		paste0('dbdump_', ., '.sql')

	filepath = paste(dir, filenam, sep = .Platform$file.sep)

	crd = getCredentials(user = user, host = host)

	syscall = paste0('mysqldump        --host=',      crd$host,
	            ' --user=' ,     crd$user,
	            ' --password=' , crd$pwd,
	            ' --databases ',  db,
	            if(!missing(tables))   paste(' --tables ', paste(tables, collapse = " ") ) else NULL ,
	            ' --routines ',
	            ' --result-file=', filepath,
	            ' --verbose')

	if(call) cat(syscall, '\n-------')

	system(syscall, wait = TRUE)

	cat('Output file:', filepath, '\n')
	cat('File size:', file.size(filepath), '\n')

	return(filepath)
 }


#' mysqlrestore
#'
#' restore sql file locally
#' @param user    user, default to 'root'
#' @export
#'
#' @examples
#' \dontrun{
#' db = 'BTatWESTERHOLZ'
#' fp = mysqldump(db, 'ADULTS BREEDING', 'mihai' , dir = tempdir() )
#' mysqlrestore(filepath = fp, db = db)
#'
#' fp = mysqldump(db, user = 'mihai' , dir = tempdir() )
#' mysqlrestore(filepath = fp, db = db)
#' }
#'
#'
mysqlrestore <- function(db, user = 'root', filepath, call = FALSE) {
	host = 'localhost'
	con = dbcon(user = user, host = host)

	dbq(con, paste('CREATE DATABASE IF NOT EXISTS', db))

	crd = getCredentials(user = user, host = host)
	crd$user = paste('-u', crd$user)
	if(!is.na(crd$pwd))
		crd$pwd = paste0('-p', crd$pwd) else
		crd$pwd = ''

	syscall = paste('mysql',  crd$user , crd$pwd, db, '<', filepath )
	if(call) cat(syscall, '\n-------')

	system(syscall, wait = TRUE)

	}





































