### COUNT OF 14 ALLERGENS AND OTHER ALLERGENS
load("Tweet_allergens.RData")
library(quanteda)
library(tidyr)
source("utils.R")

# Lookup 14 allergens in content.dfm
fourteen_allergens.df.norm <- from_corpus_to_lookup_dataframe(content.corpus, fourteen_allergens.dict)
fourteen.allergen.names <- colnames(fourteen_allergens.df.norm)[-1]
# Lookup other allergens in content.dfm
other_allergens.df.norm <- from_corpus_to_lookup_dataframe(content.corpus, other_allergens.dict)
other.allergen.names <- colnames(other_allergens.df.norm)[-1]

# Merge labelled tweets with other features from data.df
library(dplyr)
fourteen_allergens.df.norm$id <- as.character(fourteen_allergens.df.norm$id)
other_allergens.df.norm$id    <- as.character(other_allergens.df.norm$id)

fourteen_allergens.df.norm <- left_join(fourteen_allergens.df.norm, data.df[, retained_metadata], "id")
other_allergens.df.norm    <- left_join(other_allergens.df.norm,    data.df[, retained_metadata], "id")

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



library(forcats)
fourteen.df.norm.long <- fourteen_allergens.df.norm
fourteen.df.norm.long <- gather(fourteen.df.norm.long, Allergen, "Mentions", fourteen.allergen.names, factor_key = TRUE)

other_allergens.df.norm.long <- other_allergens.df.norm
other_allergens.df.norm.long <- gather(other_allergens.df.norm.long, Allergen, "Mentions", other.allergen.names, factor_key = TRUE)

all_allergens.norm.df <- rbind(fourteen.df.norm.long, other_allergens.df.norm.long)
all_allergens.norm.df$date <- as.Date(all_allergens.norm.df$date, format= "%Y-%m-%d")
all_allergens.norm.df <- all_allergens.norm.df[all_allergens.norm.df$Mentions!=0,] # to remove zeros. When binning (ie. by month) dont perform this and keep zeros in.
all_allergens.norm.df$Month <- as.Date(cut(all_allergens.norm.df$date, breaks = "month"))
all_allergens.norm.df$Week <- as.Date(cut(all_allergens.norm.df$date, breaks = "week"))
names(all_allergens.norm.df)[names(all_allergens.norm.df) == "sentiment class"] <- "sentiment_class" #rename sentiment class to a single string
all_allergens.norm.df$sentiment_class[all_allergens.norm.df$sentiment_class %in% c("not_evaluable", "processing")] <- "neutral" # collapse not evaluable and procesing to neutral

fourteen.bysource.norm.df <- fourteen.df.norm.long %>%
  group_by(source, Allergen) %>%
  summarise(count=sum(Mentions))

other.bysource.norm.df <- other_allergens.df.norm.long %>%
  group_by(source, Allergen) %>%
  summarise(count=sum(Mentions))

library(ggplot2)

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
bysource.panel <- ggarrange(fourteen.bysource.norm,
                            other.bysource.norm,
                            ncol = 1, nrow = 2)
bysource.panel


library(scales)
library(ggrepel)

all_allergens.norm.df.t14 <- subset(all_allergens.norm.df, source == "Twitter" & Allergen %in% fourteen.allergen.names)

by.date.twitter.14.month <- ggplot(all_allergens.norm.df.t14, aes(x = Month, y = Mentions, colour = Allergen), group = Allergen)+
  stat_summary(fun.y = sum, # adds up all observations for the month
               geom = "line") +
  theme_minimal()+
  theme(legend.position="bottom")+
  facet_grid(sentiment_class~.)
by.date.twitter.14.month

by.date.twitter.14.week <- ggplot(all_allergens.norm.df.t14, aes(x = Week, y = Mentions, colour = Allergen), group = Allergen)+
  stat_summary(fun.y = sum, # adds up all observations for the week
               geom = "line") +
  theme_minimal()+
  theme(legend.position="bottom")+
  facet_grid(sentiment_class~.)
by.date.twitter.14.week

all_allergens.norm.df.t.other <- subset(all_allergens.norm.df, source == "Twitter" & Allergen %in% other.allergen.names)

by.date.twitter.other <- ggplot(all_allergens.norm.df.t.other, aes(x = Month, y = Mentions, colour = Allergen), group = Allergen)+
  stat_summary(fun.y = sum, # adds up all observations for the month
               geom = "line") +
  theme_minimal()+
  theme(legend.position="bottom")+
  facet_grid(sentiment_class~.)
by.date.twitter.other
