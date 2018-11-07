load("Waterfall.RData")

# Static plots

library(tidyr)
# Long Dataframe needed for certain types of plots. 
labelled.df.long <- gather(labelled.df, Allergen, "Mentions", c(fourteen.allergen.names,other.allergen.names), factor_key = TRUE)

allergen.bysource.df <- labelled.df.long %>%
  group_by(source, Allergen) %>%
  summarise(count=sum(Mentions))

library(ggplot2)
library(forcats)

fourteen.bysource <- ggplot(subset(allergen.bysource.df, Allergen %in% fourteen.allergen.names),
                                 aes(x = fct_reorder2(Allergen, source, count, .desc = FALSE), y= count, fill = source)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  scale_fill_brewer(palette="Spectral") +
  xlab("Allergen")+
  ylab("Mentions") +
  ggtitle("Mentions of the 14 Allergens by Source")+
  coord_flip()
fourteen.bysource

other.bysource <- ggplot(subset(allergen.bysource.df, 
                                Allergen %in% other.allergen.names),
                              aes(x = fct_reorder2(Allergen, source, count, .desc = FALSE), y= count, fill = source)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  scale_fill_brewer(palette="Spectral") +
  xlab("Allergen") +
  ylab("Mentions") +
  ggtitle("Mentions of Other Allergens by Source") +
  coord_flip()
other.bysource


library(scales)
library(ggrepel)

all_allergens.norm.df.t14 <- subset(labelled.df.long, 
                                    source == "Twitter" & 
                                      Allergen %in% fourteen.allergen.names)

by.month.twitter.14 <- ggplot(all_allergens.norm.df.t14, 
                                   aes(x = Month, y = Mentions, colour = Allergen), 
                                   group = Allergen) +
  stat_summary(fun.y = sum, # adds up all observations for the month
               geom = "line") +
  theme_minimal()+
  theme(legend.position="bottom")+
  facet_grid(sentiment_class~.)
by.month.twitter.14

by.week.twitter.14 <- ggplot(all_allergens.norm.df.t14, 
                                  aes(x = Week, y = Mentions, colour = Allergen), 
                                  group = Allergen) +
  stat_summary(fun.y = sum, # adds up all observations for the week
               geom = "line") +
  theme_minimal() +
  theme(legend.position="bottom") +
  facet_grid(sentiment_class~.) +
  theme(strip.text.y = element_text(angle = 0))
by.week.twitter.14 

all_allergens.norm.df.t.other <- subset(labelled.df.long, source == "Twitter" & Allergen %in% other.allergen.names)

by.month.twitter.other <- ggplot(all_allergens.norm.df.t.other, aes(x = Month, y = Mentions, colour = Allergen), group = Allergen)+
  stat_summary(fun.y = sum, # adds up all observations for the month
               geom = "line") +
  theme_minimal()+
  theme(legend.position="bottom") +
  facet_grid(sentiment_class~.) +
  theme(strip.text.y = element_text(angle = 0))
by.month.twitter.other

by.week.twitter.other <- ggplot(all_allergens.norm.df.t.other, aes(x = Week, y = Mentions, colour = Allergen), group = Allergen)+
  stat_summary(fun.y = sum, # adds up all observations for the month
               geom = "line") +
  theme_minimal()+
  theme(legend.position="bottom") +
  facet_grid(sentiment_class~.) +
  theme(strip.text.y = element_text(angle = 0))
by.week.twitter.other

## Intersections

enquiries_source_react.bar <- ggplot(labelled.df, aes(x = source, y = allergy_enquiries, fill = sentiment_class))+
  stat_summary(fun.y = sum, # adds up all observations for the month
               geom = "bar") +
  theme_minimal() +
  labs(x= "Source", y="Allergen Enquiries", fill = "Sentiment Class") +
  scale_fill_manual(values = c("#DF3309","grey80","#0D83E6"))+
  ggtitle("Allergen Enquiries by Source and Sentiment Class")
enquiries_source_react.bar

labelling_source_react.bar <- ggplot(labelled.df, aes(x = source, y = food_labelling, fill = sentiment_class))+
  stat_summary(fun.y = sum, # adds up all observations for the month
               geom = "bar") +
  theme_minimal() +
  labs(x= "Source", y="Mentions Flagged for Food Labelling", fill = "Sentiment Class") +
  scale_fill_manual(values = c("#DF3309","grey80","#0D83E6"))+
  ggtitle("Food Labelling Mentions by Source and Sentiment Class")
labelling_source_react.bar


int_14allergen_react <- ggplot(subset(labelled.df.long, Mentions > 0 & Allergen %in% fourteen.allergen.names), 
                               aes(x = Week, y = reactions_report, fill = reactions_report))+
  stat_summary(fun.y = sum, # adds up all observations for the month
               geom = "bar") +
  scale_fill_manual(values = c("grey90","yellow","red"))+
  labs(x="Month", y="Mentions", fill="Class of Reported Reaction")+
  scale_x_date(breaks = "month")+
  ggtitle("Reactions to 14 Allergen Mentions Over Time (All Sources)")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  theme(axis.text.y = element_blank())+
  theme(panel.grid.minor = element_blank())+
  theme(strip.text.y = element_text(angle = 0))+
  theme(legend.position="bottom")+
  facet_grid(Allergen~.)
int_14allergen_react

allergen.react.df <- subset(labelled.df.long, Allergen %in% fourteen.allergen.names) %>%
  group_by(Allergen, Month, reactions_report) %>%
  summarise(Count= sum(Mentions))

allergen.react.bubble <- ggplot(allergen.react.df, aes(x = Month, y = fct_reorder(Allergen, Count), 
                                                size = ifelse(Count == 0, NA, Count), 
                                                colour = reactions_report))+
  geom_point()+
  scale_size_area(max_size = 10)+
  scale_colour_manual(values = c("grey90","yellow","red"))+
  labs(x="Month", y="Allergen", size="Number of Mentions", 
       col="Class of Reported Reaction")+
  scale_x_date(breaks = "month")+
  ggtitle("Reactions to 14 Allergen Mentions Over Time (All Sources)")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  theme(panel.grid.minor = element_blank())
allergen.react.bubble

allergen.react.labelling.df <- subset(labelled.df.long, Allergen %in% fourteen.allergen.names & food_labelling > 0) %>%
  group_by(Allergen, Month, sentiment_class, reactions_report) %>%
  summarise(Count= sum(Mentions))

allergen.react.labelling.bubble <- ggplot(allergen.react.labelling.df, aes(x = Month, y = fct_reorder(Allergen, Count), 
                                                       size = ifelse(Count == 0, NA, Count), 
                                                       colour = reactions_report))+
  geom_point()+
  scale_size_area(max_size = 10)+
  scale_colour_manual(values = c("grey90","yellow","red"))+
  labs(x="Month", y="Allergen", size="Number of Mentions", 
       col="Class of Reported Reaction")+
  scale_x_date(breaks = "month")+
  ggtitle("Reactions to 14 Allergen Mentions Over Time (All Sources, Flagged under Food Labelling)")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  theme(panel.grid.minor = element_blank())+
  theme(strip.text.y = element_text(angle = 0))+
  facet_grid(sentiment_class~.)
allergen.react.labelling.bubble

allergen.react.inquiries.df <- subset(labelled.df.long, Allergen %in% fourteen.allergen.names & allergy_enquiries > 0) %>%
  group_by(Allergen, Month, reactions_report) %>%
  summarise(Count= sum(Mentions))

allergen.react.inquiries.bubble <- ggplot(allergen.react.labelling.df, aes(x = Month, y = fct_reorder(Allergen, Count), 
                                                                           size = ifelse(Count == 0, NA, Count), 
                                                                           colour = reactions_report))+
  geom_point()+
  scale_size_area(max_size = 10)+
  scale_colour_manual(values = c("grey90","yellow","red"))+
  labs(x="Month", y="Allergen", size="Number of Mentions", 
       col="Class of Reported Reaction")+
  scale_x_date(breaks = "month")+
  ggtitle("Reactions to 14 Allergen Mentions Over Time (All Sources, Flagged under Allergy Enquiries)")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  theme(panel.grid.minor = element_blank())
allergen.react.inquiries.bubble


int_otherallergen_react <- ggplot(subset(labelled.df.long, Mentions > 0 & Allergen %in% other.allergen.names), 
                               aes(x = Week, y = reactions_report, fill = reactions_report))+
  stat_summary(fun.y = sum, # adds up all observations for the month
               geom = "bar") +
  theme_minimal() +
  labs(x="Month", y="Mentions", fill="Class of Reported Reaction")+
  scale_fill_manual(values = c("grey90","yellow","red"))+
  ggtitle("Reported Reactions to Other Allergens Over Time (All Sources)")+
  scale_x_date(breaks = "month")+
  facet_grid(Allergen~.)+
  theme(strip.text.y = element_text(angle = 0))+
  theme(axis.text.y = element_blank())+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                 size = 12, hjust = 1))+
  theme(legend.position="bottom")+
  theme(panel.grid.minor = element_blank())
int_otherallergen_react

Int_enquiry_reaction <- ggplot(subset(labelled.df, allergy_enquiries > 0 ), aes(x = Week, y = allergy_enquiries, fill = reactions_report))+
  stat_summary(fun.y = sum, # adds up all observations for the month
               geom = "bar") +
  theme_minimal()+
  labs(x="Month", y="Allergy Enquiries", fill="Class of Reported Reaction")+
  theme(legend.position="bottom")+
  scale_x_date(breaks = "month")+
  scale_fill_manual(values = c("grey90","yellow","red"))+
  ggtitle("Allergen Inquiries Over Time by Sentiment Class (All Sources)")+
  facet_grid(sentiment_class~.)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  theme(strip.text.y = element_text(angle = 0))+
  theme(panel.grid.minor = element_blank())
Int_enquiry_reaction

labelling_reaction <- ggplot(subset(labelled.df, food_labelling > 0 ), aes(x = Week, y = food_labelling, fill = reactions_report))+
  stat_summary(fun.y = sum, # adds up all observations for the month
               geom = "bar") +
  theme_minimal()+
  labs(x="Month", y="Mentions Flagged for Food Labelling", fill="Class of Reported Reaction")+
  theme(legend.position="bottom")+
  scale_x_date(breaks = "month")+
  scale_fill_manual(values = c("grey90","yellow","red"))+
  ggtitle("Food Labelling Over Time by Sentiment Class (All Sources)")+
  facet_grid(sentiment_class~.)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  theme(strip.text.y = element_text(angle = 0))+
  theme(panel.grid.minor = element_blank())
labelling_reaction

