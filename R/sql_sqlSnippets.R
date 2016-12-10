#' SQL Snippets
#'
#' @param query        An sql query
#' @param description  description
#' @param host         default to "localhost"
#' @param user         database user
#' @param f            A file
#' @param id           snip ID
#' @param kw           keyword
#'
#' @return             snipSave returns the id of the saved snippet.
#'
#' @export
#'
snipSave         <- function(query, description='', user, host = "localhost") {

	con = dbcon(user, host = host); on.exit(dbDisconnect(con))

	cat("--> Testing snippet ... \n")
	#test
	dbq(con, query) %>% rm


	cat("\n--> Saving snippet ", paste0( sdb:::headQuery(query)  , "... by ", user) )
	v = paste("INSERT INTO DBLOG.snippets (query, author, description) VALUES(",
			paste(shQuote(query),shQuote(user), shQuote(description), sep = ",")   ,");")


	dbq(con, v)

	id = dbq(con, 'SELECT max(ID) id from DBLOG.snippets')%>%as.numeric

	cat( paste(" as  ID: ", id), "\n")

	return(id)

 }

#' @rdname snipSave
#' @export
snipFetch        <- function(id, user, host = "localhost") {

	con = dbcon(user, host = host); on.exit(dbDisconnect(con))

	x = dbq(con, paste('SELECT * from DBLOG.snippets where ID = ', id) )
	
	o = paste('-- ID:', x$ID, '\n', 
				'-- author:', x$author,'\n', 
				'-- description:', x$description, '\n',
				'-- SQL:\n', x$query, '\n'
			)

	o

	}



#' @rdname snipSave
#' @export
snipDrop         <- function(id, user, host = "localhost") {
	con = dbcon(user, host = host); on.exit(dbDisconnect(con))
	dbq(con,  paste('DELETE FROM DBLOG.snippets where ID = ', id, 'and author = ', shQuote(user) ) )
	
	}


#' @rdname snipSave
#' @export
snipSearch  <- function(kw, user, host = "localhost") {

	con = dbcon(user, host = host); on.exit(dbDisconnect(con))

	if(missing(kw) )  res = dbq(con, 'SELECT * from DBLOG.snippets')
	if(!missing(kw))  res = dbq(con, paste0('SELECT * from DBLOG.snippets where query like "%', kw, '%"') )

	prt = res; prt$query = unlist(lapply(res$query, headQuery))

	print(prt)
	return(invisible(res))
	}

#' @rdname snipSave
headQuery        <- function(z, nchar = 50) {
	gsub("^\\s+", "", gsub("\\s+", " ", gsub("\n", " ", substring(z, 1, nchar)), perl = TRUE), perl = TRUE )
	}

