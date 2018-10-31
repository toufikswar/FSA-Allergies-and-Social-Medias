### COUNT OF 14 ALLERGENS AND OTHER ALLERGENS
load("Tweet_allergens.RData")
library(quanteda)
library(tidyr)

# Lookup 14 allergens in content.dfm 
fourteen_allergens_dict.dfm <- dfm_lookup(content.dfm, fourteen_allergens.dict, nomatch = "_unmatched")
fourteen_allergens.df <- convert(fourteen_allergens_dict.dfm, "data.frame")
colnames(fourteen_allergens.df)[1] <- "id"
# Normalized to one mention per document
fourteen_allergens.df.norm <- data.frame(id = fourteen_allergens.df$id, ifelse(fourteen_allergens.df[,2:15] > 0, 1, 0))

# Lookup other allergens in content.dfm
other_allergens_dict.dfm <- dfm_lookup(content.dfm, other_allergens.dict, nomatch = "_unmatched")
other_allergens.df <- convert(other_allergens_dict.dfm, "data.frame")
colnames(other_allergens.df)[1] <- "id"
# Normalized to one mention per document
other_allergens.df.norm <- data.frame(id = other_allergens.df$id, ifelse(other_allergens.df[,2:25] > 0, 1, 0))

# Merge labelled tweets with metadata.df
library(dplyr)
fourteen_allergens.df <- left_join(fourteen_allergens.df, metadata.df[, c("id", "source", "latitude", "longitude","date")], "id")
fourteen_allergens.df.norm <- left_join(fourteen_allergens.df.norm, metadata.df[, c("id", "source", "latitude", "longitude","date")], "id")
other_allergens.df <- left_join(other_allergens.df, metadata.df[, c("id", "source", "latitude", "longitude","date")], "id")
other_allergens.df.norm <- left_join(other_allergens.df.norm, metadata.df[, c("id", "source", "latitude", "longitude","date")], "id")


library(tidyr)
library(forcats)
fourteen.df.long <- fourteen_allergens.df[,-match("_unmatched", colnames(fourteen_allergens.df))]
fourteen.df.long <- gather(fourteen.df.long, Allergen, "Mentions", 2:15, factor_key = TRUE)
fourteen.df.long$category <- "Fourteen_Allergens"
fourteen.df.long$class <- "raw_counts"
fourteen.df.norm.long <- fourteen_allergens.df.norm
fourteen.df.norm.long <- gather(fourteen.df.norm.long, Allergen, "Mentions", 2:15, factor_key = TRUE)
fourteen.df.norm.long$category <- "Fourteen_Allergens"
fourteen.df.norm.long$class <- "per_document"

other_allergens.df.long <- other_allergens.df[,-match("_unmatched", colnames(other_allergens.df))]
other_allergens.df.long <- gather(other_allergens.df.long, Allergen, "Mentions", 2:25, factor_key = TRUE)
other_allergens.df.long$category <- "Other_Allergens"
other_allergens.df.long$class <- "raw_counts"
other_allergens.df.norm.long <- other_allergens.df.norm
other_allergens.df.norm.long <- gather(other_allergens.df.norm.long, Allergen, "Mentions", 2:25, factor_key = TRUE)
other_allergens.df.norm.long$category <- "Other_Allergens"
other_allergens.df.norm.long$class <- "per_document"


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
bysource.panel <- ggarrange(fourteen.bysource, fourteen.bysource.norm, 
                            other.bysource, other.bysource.norm,
                           ncol = 2, nrow = 2)
bysource.panel

