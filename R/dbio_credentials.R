
#' manage database credentials
#'
#' manage database credentials for easier database access
#'
#' saveCredentials saves the specified credentials to a default location unless you specify a custom path. This information can be used by \code{\link{dbcon}} and \code{\link{dbq}} to connect to and query the database.
#' Currently, you can store credentials for different hosts and for different users within a host.
#' \code{removeCredentials} removes the credentials file.
#' credentialsExist checks if credentials are saved for a specified host.
#' @return \code{credentialsExist},\code{removeCredentials}, and \code{saveCredentials} return \code{TRUE} if successful
#' @export
#' @aliases credentialsExist removeCredentials
#' @seealso \code{\link{dbcon}},\code{\link{dbq}}
#' @section Warning:
#' Credentials are stored in plain text in a hidden file in your home directory. Passwords are saved obfuscated. The obfuscation is settings dependent so it is not possible to use the same credentials file on another machine.

#' @examples
#' saveCredentials(user = 'user_a', pwd = 'pwd_a', host = 'localhost')
#' saveCredentials('user_b', 'pass_b', host = 'localhost')
#' removeCredentials()
saveCredentials   <- function(user, pwd, host , db, path) {
  if(missing(path)) path = credentialsPath()
  if(missing(db)) db = NA

  d = data.frame(user, pwd, db, host, stringsAsFactors = FALSE)
  if(  file.exists(path) )  {
      e = read.table(path, sep = '@', header = TRUE, stringsAsFactors = FALSE)
      e$pwd = .maskpwd(e$pwd, path = path, mask = FALSE)
      d = rbind(d, e )
      d =  d[!duplicated( paste(d$user,d$host, d$db)), ]
      }
  d$pwd = .maskpwd(d$pwd, path = path, mask = TRUE)

  write.table(d, file = path, append = FALSE, sep = "@", row.names = FALSE)
  if(Sys.info()["sysname"] == "Windows") system(paste('attrib +h', path) )
   return(TRUE)
  }

#' @rdname saveCredentials
#' @export 
credentialsExist  <- function(host , path) {
  if(missing(path)) path = credentialsPath()

  if( ! file.exists(path) ) return(FALSE)
  if( file.exists(path ) )  {
    x =read.table(path, sep = '@', header = TRUE)
    if(host%in%x$host) return(TRUE) else
        return(FALSE)
    }
   }

#' @rdname saveCredentials
#' @export 
removeCredentials <- function(path ) {
	 if(missing(path)) path = credentialsPath()
     sapply(rnorm(100), function(x) write(x, file = path) )
     file.remove(path)
	 }

#' @rdname saveCredentials
#' @export 
getCredentials    <- function(user, db, host, path) {
    if(missing(path)) path = credentialsPath()

    if( !credentialsExist(host, path) ) stop('There are no credentials saved for ', host, ' on ', dirname(path) )

    x = read.table(path, sep = '@', header = TRUE , stringsAsFactors = FALSE)

    x = x[x$host == host, ]

    if(!missing(db))   x = x[x$db %in% db, ]
    if(!missing(user)) x = x[x$user == user, ]

    if(nrow(x) > 1) {
      xc = x
      xc$pwd = "*****"
      warning( "For the given arguments multiple credentials are returned.\nOnly the 1st line will be retained." )
      print(xc)
      x = x[1, ]
      }

     x$pwd = .maskpwd(x$pwd, path = path, mask = FALSE)

     return(invisible(x))

    }

#' @rdname saveCredentials
#' @export 
credentialsPath <- function() {
  file = if(Sys.info()["sysname"] == "Windows") ".sdb." else ".sdb"
  paste( path.expand("~"), file , sep = .Platform$file.sep )
  }

.maskpwd <- function(pwd, path, mask = TRUE) {
  s = as.numeric(factor(strsplit(path, "")[[1]]))
  a = c(LETTERS, letters, unique(s), " ")
  b = a[length(a):1]
  a = paste(a, collapse = "")
  b = paste(b, collapse = "")
  if(mask) chartr(a, b, pwd) else
  chartr(b, a, pwd)
  }