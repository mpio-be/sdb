% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dbio_credentials.R
\name{saveCredentials}
\alias{saveCredentials}
\alias{credentialsExist}
\alias{removeCredentials}
\alias{getCredentials}
\alias{credentialsPath}
\title{manage database credentials}
\usage{
saveCredentials(user, pwd, host, db, path)

credentialsExist(host, path)

removeCredentials(path)

getCredentials(user, db, host, path, show = FALSE)

credentialsPath()
}
\arguments{
\item{user}{user}

\item{pwd}{pwd}

\item{host}{host}

\item{db}{db}

\item{path}{when missing s detected by credentialsPath ()}

\item{show}{default to FALSE}
}
\value{
\code{credentialsExist},\code{removeCredentials}, and \code{saveCredentials} return \code{TRUE} if successful
}
\description{
manage database credentials for for different hosts and for different users within a host.
}
\details{
saveCredentials saves the specified credentials to a default location unless you specify a custom path. This information can be used by \code{\link{dbcon}} and \code{\link{dbq}} to connect to and query the database.
Currently, you can store credentials for different hosts and for different users within a host.
\code{removeCredentials} removes the credentials file.
credentialsExist checks if credentials are saved for a specified host.
}
\section{Warning}{

Credentials are stored in plain text in a hidden file in your home directory.
}

\examples{
saveCredentials(user = 'user_a', pwd = 'pwd_a', host =  '127.0.0.1')
saveCredentials('user_b', 'pass_b', host =  '127.0.0.1')
removeCredentials()

}
\seealso{
\code{\link{dbcon}},\code{\link{dbq}}
}
