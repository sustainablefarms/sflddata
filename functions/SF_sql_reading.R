#' @title Extract table from connection to Sustainable Farms SQL database
#' @description Extraction using the column order given in the database does not always work. I suspect this is due to the issue https://github.com/r-dbi/odbc/issues/112.
#' @param cols A list of column names to extract
#' @param tablename The name of table to extract columns from
#' @param schema The schema containing the table
#' @param con A connection to the SQL database. Created, for example, by `DBI::dbConnect`
#' @param last Column names to put last when querying the database
#' @param tryperm TRUE means try different permutations of the column ordering until table successfully extracted.
#' @param verbose If TRUE prints each permutation of column names before it is tried.
#' @importFrom arrangements ipermutations
#' @importFrom odbc dbGetQuery
library(arrangements)

getSFtable <- function(tablename, schema, con, last = NULL, tryperm = TRUE, verbose = tryperm){
  # get column names for tables
  allcols <- dbGetQuery(con, "SELECT * FROM INFORMATION_SCHEMA.COLUMNS")
  cols <- allcols[(allcols$TABLE_NAME == tablename) & (allcols$TABLE_SCHEMA == schema),
                  "COLUMN_NAME"]
  
  # put last columns last
  cols_ro <- c(setdiff(cols[, 1], last), last)
  
  out <- NULL
  if (tryperm){  #try extractions
    iperm <- ipermutations(cols_ro, replace = FALSE)
    while(is.null(out)) {
      cols_ro2 <- cols_ro[iperm$getnext(1)]
      if (verbose) { print(paste("Trying ", paste(cols_ro2, collapse = ", "))) }
      tryCatch(out <- colsfromtbl(cols_ro2, tablename, schema, con),
               error = function(e) {
                 if(!grepl("Invalid Descriptor Index", e)){stop(e)}
               },
               warning = function(w) w
      )
    }
    if (verbose) {print("Success.")}
  }
  else {# just go with reordered extractions
    out <- colsfromtbl(cols_ro, tablename, schema, con)
  }
  
  stopifnot(!is.null(out))
  #order output columns in same order as in SCHEMA
  return(out[, cols[, 1]])
}

#' @describeIn getSFtable Extract specific columns from a table in the SQL database
colsfromtbl <- function(cols, tablename, schema, con){
  # make table+schema string name
  tblescheme <- paste0("[", schema, "].[", tablename, "]")
  sqlquerystr <- paste('select',
                       paste(shQuote(cols, type = "cmd"), collapse = ", "),
                       'from',
                       tblescheme)
  out <- dbGetQuery(con, sqlquerystr)
  return(out)
}