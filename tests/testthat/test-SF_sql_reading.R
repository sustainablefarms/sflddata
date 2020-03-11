context("Tests of SF SQL data base reading")
library(testthat)

stop("Must open connection manually (for security reasons)")


# get schema and table names
table_schema_df <- dbGetQuery(con,
                              'SELECT * FROM INFORMATION_SCHEMA.TABLES')


test_that("Tables with varbinary types can be read",{
  filt <- colnames %>%
    filter(DATA_TYPE == "varbinary")
  out <- getSFtable(filt$TABLE_NAME[1], filt$TABLE_SCHEMA[1], con, n = 3)
  expect_s3_class(out, "data.frame")
  out <- getSFtable(filt$TABLE_NAME[2], filt$TABLE_SCHEMA[2], con, n = 3)
  expect_s3_class(out, "data.frame")
})

test_that("Tables with varchar types can be read", {
  filt <- colnames %>%
    filter(DATA_TYPE == "varchar")
  out <- getSFtable(filt$TABLE_NAME[15], filt$TABLE_SCHEMA[15], con, n = 3)
  expect_s3_class(out, "data.frame")
})

test_that("Tables with nvarchar types can be read", {
  filt <- colnames %>%
    filter(DATA_TYPE == "nvarchar")
  out <- getSFtable(filt$TABLE_NAME[1], filt$TABLE_SCHEMA[1], con, n = 3)
  expect_s3_class(out, "data.frame")
})

### Try reading ALL tables
for (i in 1:nrow(table_schema_df)){
  tryCatch(out <- getSFtable(table_schema_df$TABLE_NAME[i], table_schema_df$TABLE_SCHEMA[i], con,
                             verbose = FALSE, n = 3),
           error = function(e) paste("In ",
                                     table_schema_df$TABLE_NAME[i],
                                     "of", 
                                     table_schema_df$TABLE_SCHEMA[i],
                                     ":",
                                     e)
  )
}
