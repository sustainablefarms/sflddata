#' @title SQL Helper Function: Apply Query multiple times, substituting the '?' in the query
#' @description Fetch queries using wildcard values given in vallist.
#' @param qry Is SQL query created containing a '?' to indicate where [vallist] values should be placed.
#' For JDBC is must be a character text, for ODBC [qry] can be an object created by [DBI::dbSendQuery()].
#' @param vallist A list of values to replace the '?' in [qry].
#' @param con A DBI connection
#' @return A data frame of all results bound together (bound using rbind)
#' @details For ODBC the function dbBind and dbFetch are used. 
#' For JDBC dbBind doesn't work (at time of writing) so the replacement of '?' is performed before creating an SQL query object.
#' When [qry] is a character object, the new line characters are ignored.
#' @export
lapplyquery <- function(qry, vallist, con = NULL){
  if (("character" %in% class(qry))) {
    data <- lapplyquery_nodbBind(qry, vallist, con)
    return(data)
  } else if (dbIsValid(qry)){ #should test if it is ODBC query, but probably can't - can't establish the ODBC connections anymore
    data <- lapplyquery_odbc(qry, vallist)
    return(data)
  } else {
    stop("No method written for applying this query type with each value.")
  }
}
lapplyquery_odbc <- function(qry, vallist){
  stopifnot(dbIsValid(qry)) #for example if query has expired - already been run etc
  data_l <- lapply(vallist,
         function(x){
           nqry <- DBI::dbBind(qry, x)
           out <- DBI::dbFetch(nqry)
           return(out)
         })
  DBI::dbClearResult(qry)
  stopifnot(length(vallist) == length(data_l))
  data <- do.call(rbind, data_l)
  return(data)
}

lapplyquery_nodbBind <- function(qry, vallist, con){
  stopifnot(grepl("\\?", qry)) #make sure the wildcard character is in there
  data_l <- lapply(vallist, function(x){
    qrystatement <- gsub("\n", " ", qry)
    qrystatement <- gsub("\\?", x, qrystatement)
    out <- DBI::dbGetQuery(con, qrystatement)
    return(out)
  })
  stopifnot(length(vallist) == length(data_l))
  data <- do.call(rbind, data_l)
  return(data)
}
  