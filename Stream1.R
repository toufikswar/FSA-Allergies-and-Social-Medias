### Stream 1: Supporting Local Authorities
load("Tweet_allergens.RData")
library(quanteda)
library(tidyr)
source("utils.R")

# Allergy enquiries:
allergy_enquiries.df.norm <- from_corpus_to_lookup_dataframe(content.corpus,allergy_enquiries.dict)
allergy_enquiries.names   <- colnames(allergy_enquiries.df.norm)[-1]
# Convert from 0/1 to FALSE/TRUE
allergy_enquiries.df.norm <- data.frame(id = allergy_enquiries.df.norm$id, ifelse(allergy_enquiries.df.norm[,allergy_enquiries.names] == 0, FALSE, TRUE))
# Combine the queries
allergy_enquiries.df.norm$allergy_enquiries <- ifelse(allergy_enquiries.df.norm[,"allergy"] &
                                                      allergy_enquiries.df.norm[,"info"]    &
                                                      (allergy_enquiries.df.norm[,"request"] | allergy_enquiries.df.norm[,"response"]) &
                                                      allergy_enquiries.df.norm[,"restaurant"],TRUE,FALSE)
allergy_enquiries.df.norm <- allergy_enquiries.df.norm[,-match(allergy_enquiries.names,names(allergy_enquiries.df.norm))]
# print(head(allergy_enquiries.df.norm,10))


# # Food labelling:
#
# Reporting reactions:
reaction_report.df.norm <- from_corpus_to_lookup_dataframe(content.corpus,reaction_report.dict)
reaction_report.names   <- colnames(reaction_report.df.norm)[-1]
# Convert from 0/1 to FALSE/TRUE
reaction_report.df.norm <- data.frame(id = reaction_report.df.norm$id, ifelse(reaction_report.df.norm[,reaction_report.names] == 0, FALSE, TRUE))
# Combine the queries
reaction_report.df.norm$mild_reaction <- ifelse(reaction_report.df.norm[,"symptons"] & reaction_report.df.norm[,"ingestion"] &
                                                !reaction_report.df.norm[,"severe"]
                                                ,TRUE,FALSE)
reaction_report.df.norm$severe_reaction <- ifelse(reaction_report.df.norm[,"symptons"] & reaction_report.df.norm[,"ingestion"] &
                                                  reaction_report.df.norm[,"severe"]
                                                  ,TRUE,FALSE)
reaction_report.df.norm <- reaction_report.df.norm[,-match(reaction_report.names,names(reaction_report.df.norm))]
# print(head(reaction_report.df.norm,10))

support_local_authorities.df <- data.frame(id                = allergy_enquiries.df.norm$id,
                                           allergy_enquiries = allergy_enquiries.df.norm$allergy_enquiries,
                                           mild_reaction     = reaction_report.df.norm$mild_reaction,
                                           severe_reaction   = reaction_report.df.norm$severe_reaction)
# print(head(support_local_authorities.df,10))

# Merge labelled tweets with other features from data.df
library(dplyr)

support_local_authorities.df$id <- as.character(support_local_authorities.df$id)
support_local_authorities.df    <- left_join(support_local_authorities.df, data.df[,merging_names], "id")
# print(head(support_local_authorities.df,10))

# library(forcats)
# fourteen.df.norm.long <- fourteen_allergens.df.norm
# fourteen.df.norm.long <- gather(fourteen.df.norm.long, Allergen, "Mentions", fourteen.allergen.names, factor_key = TRUE)
#
# other_allergens.df.norm.long <- other_allergens.df.norm
# other_allergens.df.norm.long <- gather(other_allergens.df.norm.long, Allergen, "Mentions", other.allergen.names, factor_key = TRUE)
#
# all_allergens.norm.df <- rbind(fourteen.df.norm.long, other_allergens.df.norm.long)
# all_allergens.norm.df$date <- as.Date(all_allergens.norm.df$date, format= "%Y-%m-%d")
# all_allergens.norm.df <- all_allergens.norm.df[all_allergens.norm.df$Mentions!=0,] # to remove zeros. When binning (ie. by month) dont perform this and keep zeros in.
# all_allergens.norm.df$Month <- as.Date(cut(all_allergens.norm.df$date, breaks = "month"))
# all_allergens.norm.df$Week <- as.Date(cut(all_allergens.norm.df$date, breaks = "week"))
# names(all_allergens.norm.df)[names(all_allergens.norm.df) == "sentiment class"] <- "sentiment_class" #rename sentiment class to a single string
# all_allergens.norm.df$sentiment_class[all_allergens.norm.df$sentiment_class %in% c("not_evaluable", "processing")] <- "neutral" # collapse not evaluable and procesing to neutral
#
# fourteen.bysource.norm.df <- fourteen.df.norm.long %>%
#   group_by(source, Allergen) %>%
#   summarise(count=sum(Mentions))
#
# other.bysource.norm.df <- other_allergens.df.norm.long %>%
#   group_by(source, Allergen) %>%
#   summarise(count=sum(Mentions))
#
# library(ggplot2)
#
# fourteen.bysource.norm <- ggplot(fourteen.bysource.norm.df,
#                             aes(x = fct_reorder(Allergen, count), y= count, fill = source)) +
#   geom_bar(stat = "identity") +
#   theme_minimal() +
#   scale_fill_brewer(palette="Spectral") +
#   xlab("Allergen")+
#   ylab("Mentions") +
#   ggtitle("Mentions of the 14 Allergens Normalized by Document")+
#   coord_flip()
# fourteen.bysource.norm
#
# other.bysource.norm <- ggplot(other.bysource.norm.df,
#                             aes(x = fct_reorder(Allergen, count), y= count, fill = source)) +
#   geom_bar(stat = "identity") +
#   theme_minimal() +
#   scale_fill_brewer(palette="Spectral") +
#   xlab("Allergen")+
#   ylab("Mentions") +
#   ggtitle("Mentions of Other Allergens Normalized by Document")+
#   coord_flip()
# other.bysource.norm
#
# library(ggpubr)
# bysource.panel <- ggarrange(fourteen.bysource.norm,
#                             other.bysource.norm,
#                             ncol = 1, nrow = 2)
# bysource.panel
#
#
# library(scales)
# library(ggrepel)
#
# all_allergens.norm.df.t14 <- subset(all_allergens.norm.df, source == "Twitter" & Allergen %in% fourteen.allergen.names)
#
# by.date.twitter.14.month <- ggplot(all_allergens.norm.df.t14, aes(x = Month, y = Mentions, colour = Allergen), group = Allergen)+
#   stat_summary(fun.y = sum, # adds up all observations for the month
#                geom = "line") +
#   theme_minimal()+
#   theme(legend.position="bottom")+
#   facet_grid(sentiment_class~.)
# by.date.twitter.14.month
#
# by.date.twitter.14.week <- ggplot(all_allergens.norm.df.t14, aes(x = Week, y = Mentions, colour = Allergen), group = Allergen)+
#   stat_summary(fun.y = sum, # adds up all observations for the week
#                geom = "line") +
#   theme_minimal()+
#   theme(legend.position="bottom")+
#   facet_grid(sentiment_class~.)
# by.date.twitter.14.week
#
# all_allergens.norm.df.t.other <- subset(all_allergens.norm.df, source == "Twitter" & Allergen %in% other.allergen.names)
#
# by.date.twitter.other <- ggplot(all_allergens.norm.df.t.other, aes(x = Month, y = Mentions, colour = Allergen), group = Allergen)+
#   stat_summary(fun.y = sum, # adds up all observations for the month
#                geom = "line") +
#   theme_minimal()+
#   theme(legend.position="bottom")+
#   facet_grid(sentiment_class~.)
# by.date.twitter.other
