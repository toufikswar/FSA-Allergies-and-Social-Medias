library(ggmap)

# https://blog.exploratory.io/making-maps-for-uk-countries-and-local-authorities-areas-in-r-b7d222939597

library(rgdal)
library(spdplyr)
library(sp)

API_KEY = "AIzaSyCzk2FoBaNzkPoGUcRzfzbVN0YOqq2S_aA"

uk_county_shapefiles   <- readOGR(dsn = "UK_Local_Authority_2016", layer = "Local_Authority_Districts_December_2016_Super_Generalised_Clipped_Boundaries_in_the_UK")
wgs.84 <- "+proj=longlat +datum=WGS84"
uk_county_shapefiles   <- spTransform(uk_county_shapefiles, CRS(wgs.84)) # Convert to WGS84 format
shape.df <- fortify(uk_county_shapefiles)

labelled.df.geo <- labelled.df[!is.na(labelled.df$latitude),] # Some rows don't have Latitute and longitude. Drop them
require(sp)
local_authority <- over( labelled.df.geo[,c("longitude","latitude")], uk_county_shapefiles ) # doesn't work
labelled.df.geo <- cbind( labelled.df.geo , local_authority )


UK <- geocode("United Kingdom", source = "dsk")
UKrefmap <- get_map(location = c(lon = UK$lon, lat = UK$lat),
                    maptype = "terrain", color="bw", zoom = 5 )

london <- get_map(location = c(lon = -0.129, lat = 51.51),
                  maptype = "terrain", color="bw", zoom = 10 )

UK_map <- ggmap(UKrefmap, extent='device', legend="bottomleft") +
  # geom_polygon(data = shape.df, aes(x = long, y=lat, group=group), 
  #              fill="blue", alpha=0.2) +
  geom_path(data=shape.df, aes(x=long, y=lat, group=group), 
            color="gray50", size=0.3) +
  geom_point(data = all_allergens.norm.df.t14, aes(x = longitude, y = latitude, colour = Allergen),
             size = 0.5, alpha = 0.5)
UK_map 
register_google(key = key)