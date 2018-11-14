load(image_analysis)

# output file where the plots are saved
out.dir <- file.path(paste(output_dir,"/",sep=""))

# normalization factor
norm_factor <- 100

labelled.df.geo <- subset(labelled.df.geo, source != "News")

# putting the data in a format for choropleth map plotting
library(dplyr)

# All isues of stream 1
labelled.df.geo.stream1_summary <- labelled.df.geo %>%
  group_by(objectid, long, lat, District, lad16nm) %>%
  summarise(count_allergy_enquiries = sum(allergy_enquiries),
            count_food_labelling    = sum(food_labelling),
            count_mild_reaction     = sum(mild_reaction),
            count_severe_reaction   = sum(severe_reaction))

# join the normalization information
labelled.df.geo.stream1_summary <- left_join(labelled.df.geo.stream1_summary,
                                             normalization_per_local_authority.df[,c("District","TotalEstablishments")], "District")
labelled.df.geo.stream1_summary$objectid   <- as.character(labelled.df.geo.stream1_summary$objectid)

# Normalizing the counts
labelled.df.geo.stream1_summary$norm_count_allergy_enquiries <- norm_factor*labelled.df.geo.stream1_summary$count_allergy_enquiries / labelled.df.geo.stream1_summary$TotalEstablishments
labelled.df.geo.stream1_summary$norm_count_food_labelling    <- norm_factor*labelled.df.geo.stream1_summary$count_food_labelling    / labelled.df.geo.stream1_summary$TotalEstablishments
labelled.df.geo.stream1_summary$norm_count_mild_reaction     <- norm_factor*labelled.df.geo.stream1_summary$count_mild_reaction     / labelled.df.geo.stream1_summary$TotalEstablishments
labelled.df.geo.stream1_summary$norm_count_severe_reaction   <- norm_factor*labelled.df.geo.stream1_summary$count_severe_reaction   / labelled.df.geo.stream1_summary$TotalEstablishments

# Joing stream 1 issues with map polygons
names_to_join    <- c("objectid","District","TotalEstablishments",
                      "count_allergy_enquiries","norm_count_allergy_enquiries",
                      "count_food_labelling",   "norm_count_food_labelling",
                      "count_mild_reaction",    "norm_count_mild_reaction",
                      "count_severe_reaction",  "norm_count_severe_reaction")
shape.df.stream1 <- left_join(shape.df,labelled.df.geo.stream1_summary[,names_to_join],by = c("id" = "objectid"))

# fff <- !is.na(labelled.df.geo.stream1_summary$count) & labelled.df.geo.stream1_summary$count > 0
# yyy <- labelled.df.geo.stream1_summary[fff,]
# print(yyy)
# print(nrow(yyy))
# print(nrow(labelled.df.geo.stream1_summary))
#
# print("count         # restaurants        norm_count     District")
# for(i in 1:nrow(yyy)) {
#   print(paste(yyy$count[i],"       ",yyy$TotalEstablishments[i],"     ",round(yyy$norm_count[i],5),"    ",as.character(yyy$lad16nm[i]),sep=""))
# }

library(ggmap)
library(scales)

# load the maps extracted from google
load("resources/Static_maps.RData")
UKrefmap <- UKrefmap_terrain_bw_zoom5_nolables

# get the latitude/longitude ranges for the plots
lon_range <- range(labelled.df.geo$long)
lat_range <- range(labelled.df.geo$lat)

scale_lon <- c(0.4,0.05)
delta     <- lon_range[2] - lon_range[1]
for(i in 1:2) lon_range[i] <- lon_range[i] + (-1)^i*delta*scale_lon[i]

scale_lat <- c(0.05,0.05)
delta     <- lat_range[2] - lat_range[1]
for(i in 1:2) lat_range[i] <- lat_range[i] + (-1)^i*delta*scale_lat[i]

# Number of businesses per local authority
number_of_businesses.geo <- ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_path(data = shape.df, aes(x=long, y=lat, group=group),color="gray50", size=0.3) +
  geom_polygon(data = shape.df.stream1, aes(x=long, y=lat, group=group, fill=TotalEstablishments), color = "black", size=0.2) +
  scale_x_continuous(limits = lon_range, expand = c(0,0)) +
  scale_y_continuous(limits = lat_range, expand = c(0,0)) +
  coord_map() +
  scale_fill_distiller(name = "# businesses",type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n = 5)) +
  theme_nothing(legend=TRUE) +
  labs(title="Number of businesses", fill="")
number_of_businesses.geo

ggsave("number_of_businesses_map.png", plot = last_plot(), device = NULL, path = out.dir,
       width = 15, height = 15, units = "cm",
       dpi = 300)

# Allergy enquiries plots
selection_allergy_enquiries <- shape.df.stream1$count_allergy_enquiries > 0
allergy_enquiries.summary.geo.raw <- ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_path(data = shape.df, aes(x=long, y=lat, group=group),color="gray50", size=0.3) +
  geom_polygon(data = shape.df.stream1[selection_allergy_enquiries,], aes(x=long, y=lat, group=group, fill=count_allergy_enquiries), color = "black", size=0.2) +
  scale_x_continuous(limits = lon_range, expand = c(0,0)) +
  scale_y_continuous(limits = lat_range, expand = c(0,0)) +
  coord_map() +
  scale_fill_distiller(name = "raw mentions",type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n = 5)) +
  theme_nothing(legend=TRUE) +
  labs(title="Allergy enquiries raw mentions", fill="")
allergy_enquiries.summary.geo.raw

ggsave("allergy_enquiries_map_raw.png", plot = last_plot(), device = NULL, path = out.dir,
       width = 15, height = 15, units = "cm",
       dpi = 300)

allergy_enquiries.summary.geo.norm <- ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_path(data = shape.df, aes(x=long, y=lat, group=group),color="gray50", size=0.3) +
  geom_polygon(data = shape.df.stream1[selection_allergy_enquiries,], aes(x=long, y=lat, group=group, fill=norm_count_allergy_enquiries), color = "black", size=0.2) +
  scale_x_continuous(limits = lon_range, expand = c(0,0)) +
  scale_y_continuous(limits = lat_range, expand = c(0,0)) +
  coord_map() +
  scale_fill_distiller(name = "norm. mentions",type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n = 5)) +
  theme_nothing(legend=TRUE) +
  labs(title=paste("Allergy enquiries mentions / ", norm_factor, " restaurants"), fill="")
allergy_enquiries.summary.geo.norm

ggsave("allergy_enquiries_map_norm.png", plot = last_plot(), device = NULL, path = out.dir,
       width = 15, height = 15, units = "cm",
       dpi = 300)

# Food labelling plots
selection_food_labelling <- shape.df.stream1$count_food_labelling > 0
food_labelling.summary.geo.raw <- ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_path(data = shape.df, aes(x=long, y=lat, group=group),color="gray50", size=0.3) +
  geom_polygon(data = shape.df.stream1[selection_food_labelling,], aes(x=long, y=lat, group=group, fill=count_food_labelling), color = "black", size=0.2) +
  scale_x_continuous(limits = lon_range, expand = c(0,0)) +
  scale_y_continuous(limits = lat_range, expand = c(0,0)) +
  coord_map() +
  scale_fill_distiller(name = "raw mentions",type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n = 5)) +
  theme_nothing(legend=TRUE) +
  labs(title="Food labelling raw mentions", fill="")
food_labelling.summary.geo.raw

ggsave("food_labelling_map_raw.png", plot = last_plot(), device = NULL, path = out.dir,
       width = 15, height = 15, units = "cm",
       dpi = 300)

food_labelling.summary.geo.norm <- ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_path(data = shape.df, aes(x=long, y=lat, group=group),color="gray50", size=0.3) +
  geom_polygon(data = shape.df.stream1[selection_food_labelling,], aes(x=long, y=lat, group=group, fill=norm_count_food_labelling), color = "black", size=0.2) +
  scale_x_continuous(limits = lon_range, expand = c(0,0)) +
  scale_y_continuous(limits = lat_range, expand = c(0,0)) +
  coord_map() +
  scale_fill_distiller(name = "norm. mentions",type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n = 5)) +
  theme_nothing(legend=TRUE) +
  labs(title=paste("Food labelling mentions / ", norm_factor, " restaurants"), fill="")
food_labelling.summary.geo.norm

ggsave("food_labelling_map_norm.png", plot = last_plot(), device = NULL, path = out.dir,
       width = 15, height = 15, units = "cm",
       dpi = 300)

# Mild reactions plots
selection_mild_reaction <- shape.df.stream1$count_mild_reaction > 0
mild_reaction.summary.geo.raw <- ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_path(data = shape.df, aes(x=long, y=lat, group=group),color="gray50", size=0.3) +
  geom_polygon(data = shape.df.stream1[selection_mild_reaction,], aes(x=long, y=lat, group=group, fill=count_mild_reaction), color = "black", size=0.2) +
  scale_x_continuous(limits = lon_range, expand = c(0,0)) +
  scale_y_continuous(limits = lat_range, expand = c(0,0)) +
  coord_map() +
  scale_fill_distiller(name = "raw mentions",type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n = 5)) +
  theme_nothing(legend=TRUE) +
  labs(title="Mild reaction raw mentions", fill="")
mild_reaction.summary.geo.raw

ggsave("mild_reaction_map_raw.png", plot = last_plot(), device = NULL, path = out.dir,
       width = 15, height = 15, units = "cm",
       dpi = 300)

mild_reaction.summary.geo.norm <- ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_path(data = shape.df, aes(x=long, y=lat, group=group),color="gray50", size=0.3) +
  geom_polygon(data = shape.df.stream1[selection_mild_reaction,], aes(x=long, y=lat, group=group, fill=norm_count_mild_reaction), color = "black", size=0.2) +
  scale_x_continuous(limits = lon_range, expand = c(0,0)) +
  scale_y_continuous(limits = lat_range, expand = c(0,0)) +
  coord_map() +
  scale_fill_distiller(name = "norm. mentions",type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n = 5)) +
  theme_nothing(legend=TRUE) +
  labs(title=paste("Mild reaction mentions / ", norm_factor, " restaurants"), fill="")
mild_reaction.summary.geo.norm

ggsave("mild_reaction_map_norm.png", plot = last_plot(), device = NULL, path = out.dir,
       width = 15, height = 15, units = "cm",
       dpi = 300)

# Severe reactions plots
selection_severe_reaction <- shape.df.stream1$count_severe_reaction > 0
severe_reaction.summary.geo.raw <- ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_path(data = shape.df, aes(x=long, y=lat, group=group),color="gray50", size=0.3) +
  geom_polygon(data = shape.df.stream1[selection_severe_reaction,], aes(x=long, y=lat, group=group, fill=count_severe_reaction), color = "black", size=0.2) +
  scale_x_continuous(limits = lon_range, expand = c(0,0)) +
  scale_y_continuous(limits = lat_range, expand = c(0,0)) +
  coord_map() +
  scale_fill_distiller(name = "raw mentions",type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n = 5)) +
  theme_nothing(legend=TRUE) +
  labs(title="Severe reaction raw mentions", fill="")
severe_reaction.summary.geo.raw

ggsave("severe_reaction_map_raw.png", plot = last_plot(), device = NULL, path = out.dir,
       width = 15, height = 15, units = "cm",
       dpi = 300)

severe_reaction.summary.geo.norm <- ggmap(UKrefmap, extent='device', legend="bottomleft") +
  geom_path(data = shape.df, aes(x=long, y=lat, group=group),color="gray50", size=0.3) +
  geom_polygon(data = shape.df.stream1[selection_severe_reaction,], aes(x=long, y=lat, group=group, fill=norm_count_severe_reaction), color = "black", size=0.2) +
  scale_x_continuous(limits = lon_range, expand = c(0,0)) +
  scale_y_continuous(limits = lat_range, expand = c(0,0)) +
  coord_map() +
  scale_fill_distiller(name = "norm. mentions",type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n = 5)) +
  theme_nothing(legend=TRUE) +
  labs(title=paste("Severe reaction mentions / ", norm_factor, " restaurants"), fill="")
severe_reaction.summary.geo.norm

ggsave("severe_reaction_map_norm.png", plot = last_plot(), device = NULL, path = out.dir,
       width = 15, height = 15, units = "cm",
       dpi = 300)

#
