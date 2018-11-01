### COUNT OF 14 ALLERGENS AND OTHER ALLERGENS
load("Tweet_allergens.RData")
library(quanteda)
library(tidyr)

# Lookup 14 allergens in content.dfm 
fourteen_allergens_dict.dfm <- dfm_lookup(content.dfm, fourteen_allergens.dict)
fourteen_allergens.df <- convert(fourteen_allergens_dict.dfm, "data.frame")
colnames(fourteen_allergens.df)[1] <- "id"
fourteen.allergen.names <- colnames(fourteen_allergens.df)[-1]
# Normalized to one mention per document
fourteen_allergens.df.norm <- data.frame(id = fourteen_allergens.df$id, ifelse(fourteen_allergens.df[,fourteen.allergen.names] > 0, 1, 0))

# Lookup other allergens in content.dfm
other_allergens_dict.dfm <- dfm_lookup(content.dfm, other_allergens.dict)
other_allergens.df <- convert(other_allergens_dict.dfm, "data.frame")
colnames(other_allergens.df)[1] <- "id"
other.allergen.names <- colnames(other_allergens.df)[-1]
# Normalized to one mention per document
other_allergens.df.norm <- data.frame(id = other_allergens.df$id, ifelse(other_allergens.df[,other.allergen.names] > 0, 1, 0))

# Merge labelled tweets with metadata.df
library(dplyr)
fourteen_allergens.df <- left_join(fourteen_allergens.df, metadata.df[, c("id", "source", "latitude", "longitude","date","users","hashtags", "sentiment class")], "id")
fourteen_allergens.df.norm <- left_join(fourteen_allergens.df.norm, metadata.df[, c("id", "source", "latitude", "longitude","date","users","hashtags", "sentiment class")], "id")
other_allergens.df <- left_join(other_allergens.df, metadata.df[, c("id", "source", "latitude", "longitude","date","users","hashtags","sentiment class")], "id")
other_allergens.df.norm <- left_join(other_allergens.df.norm, metadata.df[, c("id", "source", "latitude", "longitude","date","users","hashtags","sentiment class")], "id")


library(tidyr)
library(forcats)
fourteen.df.long <- fourteen_allergens.df
fourteen.df.long <- gather(fourteen.df.long, Allergen, "Mentions", fourteen.allergen.names, factor_key = TRUE)
fourteen.df.long$category <- "Fourteen_Allergens"
fourteen.df.long$class <- "raw_counts"
fourteen.df.norm.long <- fourteen_allergens.df.norm
fourteen.df.norm.long <- gather(fourteen.df.norm.long, Allergen, "Mentions", fourteen.allergen.names, factor_key = TRUE)
fourteen.df.norm.long$category <- "Fourteen_Allergens"
fourteen.df.norm.long$class <- "per_document"

other_allergens.df.long <- other_allergens.df
other_allergens.df.long <- gather(other_allergens.df.long, Allergen, "Mentions", other.allergen.names, factor_key = TRUE)
other_allergens.df.long$category <- "Other_Allergens"
other_allergens.df.long$class <- "raw_counts"
other_allergens.df.norm.long <- other_allergens.df.norm
other_allergens.df.norm.long <- gather(other_allergens.df.norm.long, Allergen, "Mentions", other.allergen.names, factor_key = TRUE)
other_allergens.df.norm.long$category <- "Other_Allergens"
other_allergens.df.norm.long$class <- "per_document"

all_allergens.df <- rbind(fourteen.df.long, other_allergens.df.long)
all_allergens.df <- all_allergens.df[all_allergens.df$Mentions!=0,] # remove rows with no mentions
all_allergens.df$date <- as.Date(all_allergens.df$date, format= "%Y-%m-%d")

all_allergens.norm.df <- rbind(fourteen.df.norm.long, other_allergens.df.norm.long)
all_allergens.norm.df$date <- as.Date(all_allergens.norm.df$date, format= "%Y-%m-%d")
#all_allergens.norm.df <- all_allergens.norm.df[all_allergens.norm.df$Mentions!=0,] # to remove zeros. When binning (ie. by month) dont perform this and keep zeros in.
all_allergens.norm.df$Month <- as.Date(cut(all_allergens.norm.df$date, breaks = "month"))
names(all_allergens.norm.df)[names(all_allergens.norm.df) == "sentiment class"] <- "sentiment_class" #rename sentiment class to a single string
all_allergens.norm.df$sentiment_class[all_allergens.norm.df$sentiment_class %in% c("not_evaluable", "processing")] <- "neutral" # collapse not evaluable and procesing to neutral


fourteen.bysource.df <- fourteen.df.long %>%
  group_by(source, Allergen) %>%
  summarise(count=sum(Mentions))

fourteen.bysource.norm.df <- fourteen.df.norm.long %>%
  group_by(source, Allergen) %>%
  summarise(count=sum(Mentions))

other.bysource.df <- other_allergens.df.long %>%
  group_by(source, Allergen) %>%
  summarise(count=sum(Mentions))

other.bysource.norm.df <- other_allergens.df.norm.long %>%
  group_by(source, Allergen) %>%
  summarise(count=sum(Mentions))

library(ggplot2)
fourteen.bysource <- ggplot(fourteen.bysource.df, 
                            aes(x = fct_reorder(Allergen, count), y= count, fill = source)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  scale_fill_brewer(palette="Spectral") +
  xlab("Allergen")+
  ylab("Mentions") +
  ggtitle("Raw Mentions of the 14 Allergens")+
  coord_flip() 
fourteen.bysource

fourteen.bysource.norm <- ggplot(fourteen.bysource.norm.df, 
                            aes(x = fct_reorder(Allergen, count), y= count, fill = source)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  scale_fill_brewer(palette="Spectral") +
  xlab("Allergen")+
  ylab("Mentions") +
  ggtitle("Mentions of the 14 Allergens Normalized by Document")+
  coord_flip() 
fourteen.bysource.norm

other.bysource <- ggplot(other.bysource.df, 
                            aes(x = fct_reorder(Allergen, count), y= count, fill = source)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  scale_fill_brewer(palette="Spectral") +
  xlab("Allergen")+
  ylab("Mentions") +
  ggtitle("Raw Mentions of Other Allergens") +
  coord_flip() 
other.bysource

other.bysource.norm <- ggplot(other.bysource.norm.df, 
                            aes(x = fct_reorder(Allergen, count), y= count, fill = source)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  scale_fill_brewer(palette="Spectral") +
  xlab("Allergen")+
  ylab("Mentions") +
  ggtitle("Mentions of Other Allergens Normalized by Document")+
  coord_flip() 
other.bysource.norm

library(ggpubr)
bysource.14panel <- ggarrange(fourteen.bysource, fourteen.bysource.norm, 

                           ncol = 2, nrow = 1)

bysource.otherpanel <- ggarrange(
                              other.bysource, other.bysource.norm,
                              ncol = 2, nrow = 1)

bysource.panel <- ggarrange(fourteen.bysource, fourteen.bysource.norm, 
                            other.bysource, other.bysource.norm,
                            ncol = 2, nrow = 2)
bysource.panel


library(scales)
library(ggrepel)

all_allergens.norm.df.t14 <- subset(all_allergens.norm.df, source == "Twitter" & Allergen %in% fourteen.allergen.names)

by.date.twitter.14 <- ggplot(all_allergens.norm.df.t14, aes(x = Month, y = Mentions, colour = Allergen), group = Allergen)+
  stat_summary(fun.y = sum, # adds up all observations for the month
               geom = "line") +
  theme_minimal()+
  theme(legend.position="bottom")+
  facet_grid(sentiment_class~.)
by.date.twitter.14

all_allergens.norm.df.t.other <- subset(all_allergens.norm.df, source == "Twitter" & Allergen %in% other.allergen.names)

by.date.twitter.other <- ggplot(all_allergens.norm.df.t.other, aes(x = Month, y = Mentions, colour = Allergen), group = Allergen)+
  stat_summary(fun.y = sum, # adds up all observations for the month
               geom = "line") +
  theme_minimal()+
  theme(legend.position="bottom")+
  facet_grid(sentiment_class~.)
by.date.twitter.other

