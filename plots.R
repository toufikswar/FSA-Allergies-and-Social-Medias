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
  group_by(Allergen, Month, sentiment_class, reactions_report) %>%
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
  theme(panel.grid.minor = element_blank())+
  theme(strip.text.y = element_text(angle = 0))+
  facet_grid(sentiment_class~.)
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
  group_by(Allergen, Month, sentiment_class, reactions_report) %>%
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
  theme(strip.text.y = element_text(angle = 0))+
  theme(panel.grid.minor = element_blank())+
  facet_grid(sentiment_class~.)
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


load("Tweet_allergens.RData")
library(quanteda)
library(tidyr)
source("utils.R")

# Lookup 14 allergens and other aleergens in content.dfm
fourteen_allergens.df.norm <- from_corpus_to_lookup_dataframe(content.corpus, fourteen_allergens.dict)
other_allergens.df.norm <- from_corpus_to_lookup_dataframe(content.corpus, other_allergens.dict)

# to create a Dictance Matrix Computation for 14 allergens and other allergens
fourteen_allergens.df.norm.subset <- fourteen_allergens.df.norm[2:15]
fourteen_allergens.df.norm.subset.transpose <- t(fourteen_allergens.df.norm.subset) 
dist(fourteen_allergens.df.norm.subset.transpose)

other_allergens.df.norm.subset <- other_allergens.df.norm[2:26]
other_allergens.df.norm.subset.transpose <- t(other_allergens.df.norm.subset)
dist(other_allergens.df.norm.subset.transpose)

# Hierarchical Cluster Analysis for 14 allergens and other allergens
hc_fourteen_allergens <- hclust(dist(fourteen_allergens.df.norm.subset.transpose))
plot(hc_fourteen_allergens)

hc_other_allergens <- hclust(dist(other_allergens.df.norm.subset.transpose))
plot(hc_other_allergens)

# creating percentage matrix for heatmap 1 for fourteen allergens

column_names = colnames(fourteen_allergens.df.norm.subset)
number_of_rows = nrow(fourteen_allergens.df.norm.subset)
number_of_cols = ncol(fourteen_allergens.df.norm.subset)
mat = matrix(list(), nrow=number_of_cols, ncol=number_of_cols)


for (i in 1:(number_of_cols)){
  for (j in i:number_of_cols){
    temp = table(fourteen_allergens.df.norm.subset[,i] + fourteen_allergens.df.norm.subset[,j])
    count_one = table(fourteen_allergens.df.norm.subset[,j])
    number_of_positive = as.vector(count_one[names(count_one) ==1])
    count_of_both_present = as.vector(temp[names(temp) ==2]) # 1,1 - are cases where both are present
    percentage_of_2 = 100*count_of_both_present/number_of_positive
    print(c(column_names[j],column_names[i], percentage_of_2))
    mat[i, j] = percentage_of_2
    mat[j, i] = percentage_of_2
  }
}

percentage_mat = data.frame(mat)
colnames(percentage_mat) = column_names
rownames(percentage_mat) = column_names
percentage_mat <- data.matrix(percentage_mat, rownames.force = NA)

library(reshape2)
melted_cormat_per_14 <- melt(percentage_mat) 
head(melted_cormat_per_14)

# plot heatmap 1 for 14 allergens

library(ggplot2)
ggplot(data = melted_cormat_per_14, aes(x=Var1, y=Var2, fill=value)) + geom_tile()


get_upper_tri <- function(percentage_mat){   # to get lower triangle part of the heatmap
  percentage_mat[lower.tri(percentage_mat)]<- NA
  return(percentage_mat)
}
upper_tri <- get_upper_tri(percentage_mat)

melted_cormat_per_14 <- melt(upper_tri, na.rm = TRUE)  # Melt the correlation matrix

ggplot(data = melted_cormat_per_14, aes(Var1, Var2, fill = value))+  # Heatmap 1 for 14 Allergens
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "white", high = "red", mid = "white", 
                       midpoint = 0, limit = c(0,60), space = "Lab", 
                       name="Percentage") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()


# creating 14x14 matrix for heatmap 2 for fourteen allergens

for (i in 1:(number_of_cols)){
  for (j in i:number_of_cols){
    temp = table(fourteen_allergens.df.norm.subset[,i] + fourteen_allergens.df.norm.subset[,j])
    count_one = table(fourteen_allergens.df.norm.subset[,i])
    number_of_positive = as.vector(count_one[names(count_one) ==1])
    count_of_both_present = as.vector(temp[names(temp) ==2]) # 1,1 - are cases where both are present
    percentage_of_2 = 100*count_of_both_present/number_of_positive
    print(c(column_names[i],column_names[j], percentage_of_2))
    mat[i, j] = percentage_of_2
    mat[j, i] = percentage_of_2
  }
}

percentage_mat = data.frame(mat)
colnames(percentage_mat) = column_names
rownames(percentage_mat) = column_names
percentage_mat <- data.matrix(percentage_mat, rownames.force = NA)

library(reshape2)
melted_cormat_per_14 <- melt(percentage_mat) 
head(melted_cormat_per_14)

#plot heatmap 2 for 14 allergens

library(ggplot2)
ggplot(data = melted_cormat_per_14, aes(x=Var1, y=Var2, fill=value)) + geom_tile()


get_upper_tri <- function(percentage_mat){   # to get upper triangle part of the heatmap
  percentage_mat[upper.tri(percentage_mat)]<- NA
  return(percentage_mat)
}

upper_tri <- get_upper_tri(percentage_mat)

melted_cormat_per_14 <- melt(upper_tri, na.rm = TRUE)  # Melt the correlation matrix

ggplot(data = melted_cormat_per_14, aes(Var1, Var2, fill = value))+  # plot heatmap 2 for 14 Allergens
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "white", high = "red", mid = "white", 
                       midpoint = 0, limit = c(0,60), space = "Lab", 
                       name="Percentage") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()






