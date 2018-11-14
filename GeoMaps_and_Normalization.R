#================================================================

### CLEAN LOCAL AUTHORITIES NAMES

clean_UK_local_authorities_names <- function(text)
{

  # This function cleans the UK local authorities names
  # This is important to match the normalization information with the data

  library(stringi)

  text <- stri_trans_tolower(text)
  text <- stri_replace_all_fixed(text, " ?", "")
  text <- stri_replace_all_fixed(text, "-", " ")
  text <- stri_replace_all_fixed(text, "city", "")
  text <- stri_replace_all_fixed(text, "of", "")
  text <- stri_replace_all_fixed(text, "county", "")
  text <- stri_replace_all_fixed(text, "councils", "")
  text <- stri_replace_all_fixed(text, "council", "")
  text <- stri_replace_all_fixed(text, ",", "")
  text <- stri_replace_all_fixed(text, "|", "")
  text <- stri_replace_all_fixed(text, ".", "")
  text <- gsub("\\(", "",text)
  text <- gsub("\\)", "",text)
  text <- gsub("[[:digit:]]+","", text)
  text <- stri_replace_all_fixed(text, "london corporation", "london")
  text <- stri_replace_all_fixed(text, "isle  anglesey", "anglesey")
  text <- stri_replace_all_fixed(text, "south bucks", "south buckinghamshire")
  text <- stri_replace_all_fixed(text, "kingston upon hull", "hull")
  text <- stri_replace_all_fixed(text, "comhairle nan eilean siar western isles", "na h eileanan siar")
  text <- stri_replace_all_fixed(text, "na h eileanan siar", "comhairle nan eilean siar western isles")

  # FSA data                          Map data
  # Adur and Worthing        ==>   adur, worthing"
  # babergh and mid suffolk  ==>   babergh, mid suffolk
  # rother and wealden       ==>   rother, wealden

  text <- stri_trim(text)

  return(text)
}
#================================================================

# number of restaurants per local authority from FSA
# https://data.food.gov.uk/catalog/datasets/069c7353-4fdd-4b4f-9c13-ec525753fb2c
restaurants_per_local_authority_file <- "resources/2016-17-enforcement-data-food-hygiene.csv" # 2016/2017 Enforcement Data - Food Hygiene
# restaurants_per_local_authority_file <- "resources/2017-18-enforcement-data-food-hygiene.csv" # 2017/2018 Enforcement Data - Food Hygiene

# data,frame with local authorities information
normalization_per_local_authority.df <- read.csv(file = restaurants_per_local_authority_file, header = TRUE, sep=",")

# select only the Local authority name and TotalEstablishments.IncludingNotYetRated.Outside.
original_list_of_variables <- c("LAName","TotalEstablishments.IncludingNotYetRated.Outside.")
normalization_per_local_authority.df <- subset(normalization_per_local_authority.df, select = original_list_of_variables)

# rename column names
list_of_variables  <- c("LAName","TotalEstablishments")
names(normalization_per_local_authority.df) <- list_of_variables

# The geographical differences plots will be normalized with the TotalEstablishments variable

# create a new column with the LAName cleaned
normalization_per_local_authority.df$District <- as.character(clean_UK_local_authorities_names(normalization_per_local_authority.df$LAName))

# Now load the UK map of local authorities
# https://blog.exploratory.io/making-maps-for-uk-countries-and-local-authorities-areas-in-r-b7d222939597

library(ggmap)
library(rgdal)
library(spdplyr)
library(sp)

# load UK local authorities map
uk_county_shapefiles   <- readOGR(dsn = "resources/UK_Local_Authority_2016", layer = "Local_Authority_Districts_December_2016_Super_Generalised_Clipped_Boundaries_in_the_UK")
wgs.84                 <- "+proj=longlat +datum=WGS84"
uk_county_shapefiles   <- spTransform(uk_county_shapefiles, CRS(wgs.84)) # Convert to WGS84 format
shape.df               <- fortify(uk_county_shapefiles)

# data.frame for plotting number of restaurants per local authority
library(dplyr)
normalization_per_local_authority.df <- left_join(normalization_per_local_authority.df,
                                                  data.frame(objectid = uk_county_shapefiles$objectid,
                                                             long     = uk_county_shapefiles$long,
                                                             lat      = uk_county_shapefiles$lat,
                                                             lad16nm  = uk_county_shapefiles$lad16nm,
                                                             District = as.character(clean_UK_local_authorities_names(as.character(uk_county_shapefiles$lad16nm)))),
                                                  by = "District")

# Some rows don't have Latitute and longitude. Drop them
labelled.df.geo <- labelled.df[!is.na(labelled.df$latitude),]

# Letting R know that these are specifically spatial coordinates
sp <- SpatialPoints(labelled.df.geo[,c("longitude", "latitude")])
# To ensure the same coordinate system
proj4string(sp) <- "+proj=longlat +datum=WGS84"

# Match coordinates to each uk region in uk shapefile
local_authority <- over(sp, uk_county_shapefiles)
labelled.df.geo <- cbind(labelled.df.geo, local_authority)

# Some rows don't have local authority name. Drop them
labelled.df.geo <- labelled.df.geo[!is.na(labelled.df.geo$lad16nm),]

# create a new column with the lad16nm cleaned
labelled.df.geo$District <- clean_UK_local_authorities_names(labelled.df.geo$lad16nm)

# Shift needed to match map with actual local authority
labelled.df.geo$objectid <- as.character(labelled.df.geo$objectid)
labelled.df.geo$objectid <- as.character(as.numeric(labelled.df.geo$objectid) + 1)

# In order to get the normalization information the lad16nm_clean columns from labelled.df.geo and normalization_per_local_authority.df have to be matched


#
