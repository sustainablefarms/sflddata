# import and processing of angry birds data (SWS)

library(readxl)

birds_raw <- as.data.frame(
  read_excel(
    "./raw_data/LongTermStudies_BirdDataExtractions_19-03-2019.xlsx",
    sheet = "SWS"
))

species <- sort(unique(birds_raw$CommonName))

# reorganise into a visit (row) by species (col) matrix
bird_list <- lapply(
  split(
    birds_raw[, c("CommonName", "SumAbundance")],
    # birds_raw$SurveyVisitId
    paste0(birds_raw$SiteCode, "_", birds_raw$SurveyYear)
  ),
  function(a, spp){
    summed_a <- do.call(rbind, lapply(
      split(a, a$CommonName),
      function(b){
        out <- b[1, ]
        out$SumAbundance <- sum(b$SumAbundance)
        return(out)
      }
    ))
    result <- merge(spp, summed_a, by = "CommonName", all = TRUE)
    result$SumAbundance[which(is.na(result$SumAbundance))] <- 0
    return(result$SumAbundance)
  },
  spp = data.frame(
    CommonName = species,
    stringsAsFactors = FALSE
  )
)
bird_matrix <- as.data.frame(do.call(rbind, bird_list))
colnames(bird_matrix) <- species
bird_matrix <- bird_matrix[, which(species != "Nil")]
# apply(bird_matrix, 2, sum)


# traits
traits <- as.data.frame(
  read_excel(
    "./raw_data/Ikin_SWS_Bird_Traits_updatedApril2017.xlsx",
    sheet = "Ikin_SWS_Bird_Traits"
))

species_df <- data.frame(
  common_name = colnames(bird_matrix),
  column = seq_len(ncol(bird_matrix)),
  n = apply(bird_matrix, 2, sum),
  stringsAsFactors = FALSE
)

species_df <- merge(species_df, traits,
  by.x = "common_name",
  by.y = "species",
  all.x = TRUE,
  all.y = FALSE)

species_df <- species_df[-which(species_df$diet == "Vertebrates"), ]
species_df <- species_df[-which(species_df$substrate == "Water"), ]
species_df <- species_df[which(species_df$common_name != "Australian Reed Warbler"), ]
# bird_richness_matrix <- bird_matrix[, species_df$column]
# species_df <- species_df[which(species_df$n > 100), ]
bird_matrix <- bird_matrix[, species_df$column]
saveRDS(species_df, "./clean_data/sws_traits.rds")


# extract visit-level attributes
visit_list <- lapply(
  split(
    birds_raw[, 1:17],
    # birds_raw$SurveyVisitId
    paste0(birds_raw$SiteCode, "_", birds_raw$SurveyYear)
  ),
  function(a){a[1, ]}
)
visit_df <- as.data.frame(
  do.call(rbind, visit_list),
  stringsAsFactors = FALSE
)

rm(bird_list, visit_list) # cleaning
bird_data <- cbind(visit_df, bird_matrix)


# extract site data
sites_raw <- as.data.frame(
  read_excel(
    "./raw_data/LongTermStudies_SiteTableData_22-03-2019.xlsx",
    sheet = "SWS"
))
sites_simple <- sites_raw[, c(
  "SiteCode", "GrowthType",
  "latitude", "longitude", "elevation",
  "Rmnnt/PltngsSize (ha)", "NativeVegArea500mRadius"
)]
for(i in 3:7){sites_simple[, i] <- as.numeric(sites_simple[, i])}

growth <- rep(NA, nrow(sites_simple))
growth[which(sites_simple$GrowthType == "planting")] <- 1
growth[which(sites_simple$GrowthType == "natural regrowth")] <- 2
growth[which(sites_simple$GrowthType == "coppiced regrowth")] <- 2
growth[which(sites_simple$GrowthType == "oldgrowth")] <- 3
sites_simple$GrowthType <- factor(
  growth,
  levels = c(1:3),
  labels = c("planting", "regrowth", "oldgrowth")
)
rm(growth)

# add veg data
sites_veg <- read.csv("./raw_data/sws_mean_veg_structure.csv",
  stringsAsFactors = FALSE)[, -1]
colnames(sites_veg)[1] <- "SiteCode"

# merge & export
sites_simple <- merge(sites_simple, sites_veg, by = "SiteCode", all = TRUE)

bird_data <- merge(sites_simple, bird_data, by = "SiteCode", all = TRUE)

birds_only <- bird_data[,
  c(which(colnames(bird_data) == "Apostlebird"):ncol(bird_data))
  ]
saveRDS(birds_only, "./clean_data/sws_bird_richness.rds")


birds_small <- birds_only[,
  colnames(birds_only) %in% species_df$common_name[which(species_df$n > 100)]
]
saveRDS(
  birds_small,
  "./clean_data/sws_birds.rds")

saveRDS(bird_data[, 1:28], "./clean_data/sws_sites.rds")
