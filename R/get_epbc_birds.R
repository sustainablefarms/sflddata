# library(rvest)
epbc_list_page <- "https://www.environment.gov.au/cgi-bin/sprat/public/publicthreatenedlist.pl?wanted=fauna#birds_extinct"
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
  tbl[rowsforidxs[[idx]], "FaunaType"] <- faunatype[[idx]]
  tbl[rowsforidxs[[idx]], "ConservationStatus"] <- conservationstatus[[idx]]
}

# remove the header rows
tbl <- tbl[-headerrows, ]

write.table(tbl, paste0("./private/data/raw/EPBClist_", Sys.Date(), ".tsv"), sep = "\t")
