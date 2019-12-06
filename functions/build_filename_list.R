#' @title Create list of Filenames
#' @param server server url
#' @param directory directory of data within server
#' @param prefix start of filename
#' @param middle middle of filename
#' @param suffix end of filename
#' @param type_extension The character string representing file type
#' @param dirsep Character used to combine server and directory
#' @param namesep Characer used to combine parts of the filenames
#' @details server and directory are concatenated using dirsep. prefix, middle, suffix and type_extension are concatenated using namesep separator.
#' Uses \code{paste}.
#' @examples
#' server <- "http://dapds00.nci.org.au/thredds/dodsC/ub8/au/OzWALD" #http://dapds00.nci.org.au/thredds/catalog/ub8/au/OzWALD/catalog.html
#' directory <- "8day/GPP"
#' prefix <- "OzWALD"
#' middle <- "GPP"
#' suffix <- 2000:2018
#' type_extension <- "nc"
#' build_filename_list("http://dapds00.nci.org.au/thredds/dodsC/ub8/au/OzWALD",
#'       "8day/GPP",
#'       "OzWALD",
#'       "GPP",
#'       2000:2018,
#'       "nc")

build_filename_list <- function(server, directory = "",
                                prefix = "", middle = "", suffix = "", type_extension = "",
                                dirsep = "/", namesep = "."){
  dirname <- paste(server, directory, sep = dirsep)
  filename <- paste(prefix, middle, suffix, type_extension, sep = namesep)
  return(paste(dirname, filename, sep = dirsep))
}
