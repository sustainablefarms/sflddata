#' @title SQL Helper Function: Apply Query multiple times, substituting the '?' in the query
#' @description Uses dbBind to fetch queries using wildcard values given in vallist.
#' @return A data frame of all results bound together (bound using rbind)
lapplyquery <- function(qry, vallist){
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