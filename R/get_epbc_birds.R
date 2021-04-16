# library(rvest)
#' @title Downloads the EPBC Lists of Species
#' @description Converts the lists shown at https://www.environment.gov.au/cgi-bin/sprat/public/publicthreatenedlist.pl to a table.
#' @param destfile The file to save the epbc lists to.
#' @details 
#' Requires the packages rvest and xml2. Worked on March 23, 2021.
#' @return A data frame of the listed species with columns for Scientific Name, Common Name, Fauna Type, Conservation Status, and a few others columns present on the website.
#' The cell values have been processed as little as possible. So, for example, a 'Common Name' for Pseudocheirus occidentalis is Western Ringtail Possum, Ngwayir, Womp, Woder, Ngoor, Ngoolangit.
#' @examples
#' constatuses <- download_epbc()
#' destfile = paste0("./private/data/raw/EPBClist_", Sys.Date(), ".tsv")
#' write.table(constatuses, destfile, sep = "\t")
download_epbc <- function(){
  epbc_list_page <- "https://www.environment.gov.au/cgi-bin/sprat/public/publicthreatenedlist.pl"
  xmlofpage <- xml2::read_html(epbc_list_page)
  tbl <- xmlofpage %>%
    rvest::html_nodes("#threatlist") %>%
    rvest::html_table() %>%
    `[[`(1)
  colnames(tbl)[[1]] <- "Genus, species (subspecies, population)"
  colnames(tbl)[[2]] <- "Common Name"
  colnames(tbl)[[3]] <- "Effective"
  colnames(tbl)[[4]] <- "Profile Available"
  colnames(tbl)[[5]] <- "Other Information Available"
  colnames(tbl)[[6]] <- "Photo.NotParsed"

  # remove interspersed column names
  tbl <- tbl[tbl[, 1] != "Genus, species (subspecies, population)", ]

  # turn subheaders into extra columns
  headerrows <- which(tbl[, 1] == tbl[, 2])
  headers <- tbl[headerrows, 1]
  faunatype <- gsub(" .*", "", headers)
  conservationstatus <- gsub(".*that are ", "", headers)
  conservationstatus <- gsub("\n\t.*", "", conservationstatus)
  
  rowsforidxs <- lapply(1:(length(headerrows) - 1),
         function(idx){
           rowsforidx <- seq.int(headerrows[idx], headerrows[idx + 1] - 1)
           return(rowsforidx)
         }
         )
  for (idx in 1:length(rowsforidxs)){
    tbl[rowsforidxs[[idx]], "Fauna Type"] <- faunatype[[idx]]
    tbl[rowsforidxs[[idx]], "Conservation Status"] <- conservationstatus[[idx]]
  }
  
  # remove the header rows
  tbl <- tbl[-headerrows, ]
  return(tbl)
}

