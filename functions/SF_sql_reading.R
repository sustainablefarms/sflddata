#' @title Extract table from connection to Sustainable Farms SQL database
#' @description Extraction using the column order given in the database does not always work. I suspect this is due to the issue https://github.com/r-dbi/odbc/issues/112.
#' @param cols A list of column names to extract
#' @param tablename The name of table to extract columns from
#' @param schema The schema containing the table
#' @param con A connection to the SQL database. Created, for example, by `DBI::dbConnect`
#' @param last Column names to put last when querying the database
#' @param tryperm TRUE means try different permutations of the column ordering until table successfully extracted.
#' @importFrom arrangements ipermutations
library(arrangements)

getSFtable <- function(tablename, schema, con, last = NULL, tryperm = TRUE){
  # get column names for tables
  allcols <- dbGetQuery(con, "SELECT * FROM INFORMATION_SCHEMA.COLUMNS")
  cols <- allcols %>% 
    dplyr::filter(TABLE_NAME == tablename,
                  TABLE_SCHEMA == schema) %>%
    dplyr::select(COLUMN_NAME)
  
  # put last columns last
  cols_ro <- c(setdiff(cols[, 1], last), last)
  
  out <- NULL
  if (tryperm){ 
    iperm <- ipermutations(cols_ro, replace = FALSE)
    while(is.null(out)) {
      cols_ro2 <- cols_ro[iperm$getnext(1)]
      print(paste("Trying ", paste(cols_ro2, collapse = ", ")))
      tryCatch(out <- colsfromtbl(cols_ro2, tablename, schema, con),
               error = function(e) {
                 if(!grepl("Invalid Descriptor Index", e)){stop(e)}
               },
               warning = function(w) w
      )
    }
    print("Success.")
  }
  else {
    out <- colsfromtbl(cols_ro, tablename, schema, con)
  }
  
  stopifnot(!is.null(out))
  #order output columns in same order as in SCHEMA
  return(out[, cols[, 1]])
}


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

getSFtable_last <- function(tablename, schema, con, last = NULL){
  # get column names for tables
  allcols <- dbGetQuery(con, "SELECT * FROM INFORMATION_SCHEMA.COLUMNS")
  cols <- allcols %>% 
    dplyr::filter(TABLE_NAME == tablename,
                  TABLE_SCHEMA == schema) %>%
    dplyr::select(COLUMN_NAME)
 
  # put VisitCode last
  cols_ro <- c(setdiff(cols[, 1], last), last)
  out <- colsfromtbl(cols_ro, tablename, schema, con)
  
  #order output columns in same order as in SCHEMA
  return(out[, cols[, 1]])
}


getSFtable_permorder <- function(tablename, schema, con){
  # get column names for tables
  allcols <- dbGetQuery(con, "SELECT * FROM INFORMATION_SCHEMA.COLUMNS")
  cols <- allcols %>% 
    dplyr::filter(TABLE_NAME == tablename,
                  TABLE_SCHEMA == schema) %>%
    dplyr::select(COLUMN_NAME)
  
  # try pulling out table by permuting order
  out <- NULL
  iperm <- ipermutations(cols[, 1], replace = FALSE)
  while(is.null(out)) {
    cols_ro <- cols[iperm$getnext(1), ]
    print(paste("Trying ", paste(cols_ro, collapse = ", ")))
    tryCatch(out <- colsfromtbl(cols_ro, tablename, schema, con),
             error = function(e) {
               if(!grepl("Invalid Descriptor Index", e)){stop(e)}
             },
             warning = function(w) w
    )
  }
  print("Success.")
  
  #order output columns correctly
  return(out[, cols[, 1]])
}