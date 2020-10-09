context("Tests of SF SQL data base reading")

skip_if_offline()

source("~/dANU/ComputerRelated/AccessLindenmayerSQL.R")
# get all colummn properties
colnames <- dbGetQuery(con,
                       "SELECT * FROM INFORMATION_SCHEMA.COLUMNS")


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

test_that("All but 6 tables can be read", {
# get schema and table names
  table_schema_df <- dbGetQuery(con,
                                'SELECT * FROM INFORMATION_SCHEMA.TABLES')
  failed <- list()
  for (i in 1:nrow(table_schema_df)){
    out <- NULL
    tryCatch(out <- getSFtable(table_schema_df$TABLE_NAME[i], table_schema_df$TABLE_SCHEMA[i], con,
                               verbose = FALSE, n = 3),
             error = function(e) print(paste("In ",
                                       table_schema_df$TABLE_NAME[i],
                                       "of", 
                                       table_schema_df$TABLE_SCHEMA[i],
                                       ":",
                                       e))
    )
    if (is.null(out)){ failed <- c(failed, i) }
  }

  knownerrors <- table_schema_df$TABLE_NAME %in% c("v_LTERN_SpotlightData",
                                              "v_LTERN_SpotlightDataWithVisit",
                                              "v_SpotlightData",
                                              "tblVegetationData(FieldDataEntry)",
                                              "tblVegetationPlotData(FieldEntry)",
                                              "tblVegetationPlotData(InFieldEntry)")
  expect_equal(sum(knownerrors), 6)
  expect_equal(failed, knownerrors)
})

## 3 tables error because of binding, 3 more error because of equal signs in the column names
# getSFtable("v_LTERN_SpotlightData", "ltern", con,  verbose = FALSE, n = 3) # something about binding
# getSFtable("v_LTERN_SpotlightDataWithVisit", "ltern", con,  verbose = FALSE, n = 3) #something about binding errors
# getSFtable("v_SpotlightData", "dbo", con,  verbose = FALSE, n = 3) #something about binding errors
# 
# getSFtable("tblVegetationData(FieldDataEntry)", "Nanangroe", con,  verbose = FALSE, n = 3) #something about '=' signs
# getSFtable("tblVegetationPlotData(FieldEntry)", "Restoration", con,  verbose = FALSE, n = 3) #something about '=' signs
# getSFtable("tblVegetationPlotData(InFieldEntry)", "bbmp", con,  verbose = FALSE, n = 3) #something about '=' signs

# a <- colnames %>%
#   filter(TABLE_NAME == "tblVegetationData(FieldDataEntry)") %>%
#   select(COLUMN_NAME)
# showNonASCII(a[, 1])
# out <- dbGetQuery(con,'select VegetationData2ID, [OS (=10m) %Cover]
#            from [Nanangroe].[tblVegetationData(FieldDataEntry)]')

DBI::dbDisconnect(con)