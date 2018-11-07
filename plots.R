source("Waterfall.RData")

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
                                 aes(x = fct_reorder(Allergen, count), y= count, fill = source)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  scale_fill_brewer(palette="Spectral") +
  xlab("Allergen")+
  ylab("Mentions") +
  ggtitle("Mentions of the 14 Allergens Normalized by Document")+
  coord_flip()
fourteen.bysource.norm

other.bysource <- ggplot(allergen.bysource.df, Allergen %in% other.allergen.names,
                              aes(x = fct_reorder(Allergen, count), y= count, fill = source)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  scale_fill_brewer(palette="Spectral") +
  xlab("Allergen")+
  ylab("Mentions") +
  ggtitle("Mentions of Other Allergens Normalized by Document")+
  coord_flip()
other.bysource


library(scales)
library(ggrepel)

all_allergens.norm.df.t14 <- subset(labelled.df.long, source == "Twitter" & Allergen %in% fourteen.allergen.names)

by.date.twitter.14.month <- ggplot(all_allergens.norm.df.t14, aes(x = Month, y = Mentions, colour = Allergen), group = Allergen) +
  stat_summary(fun.y = sum, # adds up all observations for the month
               geom = "line") +
  geom_point(size = 0.1) 
theme_minimal()+
  theme(legend.position="bottom")+
  facet_grid(sentiment_class~.)
by.date.twitter.14.month

by.date.twitter.14.week <- ggplot(all_allergens.norm.df.t14, aes(x = Week, y = Mentions, colour = Allergen), group = Allergen) +
  stat_summary(fun.y = sum, # adds up all observations for the week
               geom = "line") +
  theme_minimal() +
  theme(legend.position="bottom")+
  facet_grid(sentiment_class~.)
by.date.twitter.14.week

all_allergens.norm.df.t.other <- subset(labelled.df.long, source == "Twitter" & Allergen %in% other.allergen.names)

by.date.twitter.other <- ggplot(all_allergens.norm.df.t.other, aes(x = Month, y = Mentions, colour = Allergen), group = Allergen)+
  stat_summary(fun.y = sum, # adds up all observations for the month
               geom = "line") +
  theme_minimal()+
  theme(legend.position="bottom")+
  facet_grid(sentiment_class~.)
by.date.twitter.other


## Intersections

int_enq_source_react.bar <- ggplot(labelled.df, aes(x = source, y = allergy_enquiries, fill = sentiment_class))+
  stat_summary(fun.y = sum, # adds up all observations for the month
               geom = "bar") +
  theme_minimal() +
  ylab("Enquiries") +
  scale_fill_manual(values = c("red","grey80","green"))+
  ggtitle("Allergen Enquiries by Source")
int_enq_source_react.bar

int_14allergen_react <- ggplot(subset(labelled.df.long, Mentions > 0 & Allergen %in% fourteen.allergen.names), 
                               aes(x = Week, y = reactions_report, fill = reactions_report))+
  stat_summary(fun.y = sum, # adds up all observations for the month
               geom = "bar") +
  theme_minimal() +
  ylab("Mentions") +
  xlab("Week")+
  scale_fill_manual(values = c("grey90","yellow","red"))+
  ggtitle("Reported Reactions Over Time")+
  facet_grid(Allergen~.)+
  theme(strip.text.y = element_text(angle = 0))
int_14allergen_react

Int_enquiry_reaction <- ggplot(subset(labelled.df, allergy_enquiries > 0 ), aes(x = Week, y = allergy_enquiries, fill = reactions_report))+
  stat_summary(fun.y = sum, # adds up all observations for the month
               geom = "bar") +
  theme_minimal()+
  xlab("Week")+
  ylab("Allergy Enquiries") +
  theme(legend.position="bottom")+
  scale_fill_manual(values = c("grey90","yellow","red"))+
  ggtitle("Allergen Inquiries Over Time")+
  facet_grid(sentiment_class~.)+
  theme(strip.text.y = element_text(angle = 0))
Int_enquiry_reaction