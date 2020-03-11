#' @title Extract table from connection to Sustainable Farms SQL database
#' @description Extraction using the column order given in the database does not always work. I suspect this is due to the issue https://github.com/r-dbi/odbc/issues/112.
#' @param cols A list of column names to extract
#' @param tablename The name of table to extract columns from
#' @param schema The schema containing the table
#' @param con A connection to the SQL database. Created, for example, by `DBI::dbConnect`
#' @param last Column names to put last when querying the database
#' @param tryperm TRUE means try different permutations of the column ordering until table successfully extracted.
#' @param verbose If TRUE prints each permutation of column names before it is tried.
#' @param n Passed to dbGetQuery. Maximum number of records to retrieve per fetch. Use n = -1 or n = Inf to retrieve all pending records. Some implementations may recognize other special values.
#' @param params Query parameters to pass to dbBind() via dbGetQuery. See dbBind() for details.
#' @importFrom arrangements ipermutations
#' @importFrom odbc dbGetQuery
#' @details Best guesses so far is that all columns of data type 'varchar' must be read last (credit to https://github.com/r-dbi/odbc/issues/86#issuecomment-314492486).
#' The bug report here https://github.com/r-dbi/odbc/issues/10 also suggests 'varbinary' types should be places last.
#' The names of columns of these types are appeneded to \code{last} in \code{getSFtable}. 
#' 
#' @examples 
#' out <- getSFtable("tblPost-fireAcaciaBasal", "VicBirdsVegetation", con, last = "VisitCode2")


getSFtable <- function(tablename, schema, con, last = NULL, tryperm = FALSE, verbose = tryperm, n = -1, params = NULL, ...){
  # get column names for tables
  allcols <- dbGetQuery(con, "SELECT * FROM INFORMATION_SCHEMA.COLUMNS")
  colprops <- allcols[(allcols$TABLE_NAME == tablename) & (allcols$TABLE_SCHEMA == schema), ]
  cols <- colprops$COLUMN_NAME
  
  # make varchar and varbinary columns last
  last <- c(last, colprops$COLUMN_NAME[colprops$DATA_TYPE == "varchar"])
  last <- c(last, colprops$COLUMN_NAME[colprops$DATA_TYPE == "varbinary"])
  
  # put last columns last
  cols_ro <- c(setdiff(cols, last), last)
  
  out <- NULL
  if (tryperm){  #try extractions
    iperm <- arrangements::ipermutations(cols_ro, replace = FALSE)
    while(is.null(out)) {
      cols_ro2 <- cols_ro[iperm$getnext(1)]
      # Sys.sleep(5)
      if (verbose) { print(paste("Trying: ", paste(cols_ro2, collapse = ", "))) }
      tryCatch(out <- colsfromtbl(cols_ro2, tablename, schema, con,  n = -1, params = NULL, ...),
               error = function(e) {
                 if(!grepl("Invalid Descriptor Index", e)){stop(e)}
               },
               warning = function(w) w
      )
    }
    if (verbose) {print("Success.")}
  }
  else {# just go with reordered extractions
    out <- colsfromtbl(cols_ro, tablename, schema, con,  n = -1, params = NULL, ...)
  }
  
  stopifnot(!is.null(out))
  #order output columns in same order as in SCHEMA
  return(out[, cols])
}

#' @describeIn getSFtable Extract specific columns from a table in the SQL database
colsfromtbl <- function(cols, tablename, schema, con, n = -1, params = NULL, ...){
  # make table+schema string name
  tblescheme <- paste0("[", schema, "].[", tablename, "]")
  sqlquerystr <- paste('select',
                       paste(shQuote(cols, type = "cmd"), collapse = ", "),
                       'from',
                       tblescheme)
  out <- dbGetQuery(con, sqlquerystr, n = n, params = params, ...)
  # tryCatch(out <- dbFetch(cursor),
  #          error = function(e) warning(e))
  # dbClearResult(cursor)
  return(out)
}
