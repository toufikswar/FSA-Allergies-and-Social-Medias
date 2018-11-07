load("Tweet_allergens.RData")
library(quanteda)
library(tidyr)
source("utils.R")

labelled.df <- content.df

# Gobal Execution time:
start_time  <- Sys.time()

### Stream 1: Supporting Local Authorities
start_time1 <- Sys.time()
# Allergy enquiries:
allergy_enquiries.df.norm <- from_corpus_to_lookup_dataframe(content.corpus,allergy_enquiries.dict)
allergy_enquiries.names   <- colnames(allergy_enquiries.df.norm)[-1]
# Combine the queries
labelled.df$allergy_enquiries <- ifelse(allergy_enquiries.df.norm[,"allergy"] &
                                        allergy_enquiries.df.norm[,"info"]    &
                                        (allergy_enquiries.df.norm[,"request"] | allergy_enquiries.df.norm[,"response"]) &
                                        allergy_enquiries.df.norm[,"restaurant"],1,0)
allergy.enquiries.names       <- c("allergy_enquiries")

end_time1 <- Sys.time()
allergy_enquiries_time <- as.difftime(end_time1 - start_time1, units = "secs")

# Food labelling:
start_time1 <- Sys.time()
food_labelling.df.norm <- from_corpus_to_lookup_dataframe(content.corpus, food_labelling.dict)
food_labelling.names <- colnames(food_labelling.df.norm)
# Combine the queries
labelled.df$food_labelling <- ifelse(food_labelling.df.norm[,"consumer"] & 
                                       food_labelling.df.norm[,"issue"] &
                                       food_labelling.df.norm[,"labelling"] 
                                     |
                                       food_labelling.df.norm[,"incorrect"] &
                                       food_labelling.df.norm[,"allergy"] &
                                       food_labelling.df.norm[,"labelling"] 
                                     |
                                       food_labelling.df.norm[,"consumer"] &
                                       food_labelling.df.norm[,"allergy"] &
                                       food_labelling.df.norm[,"labelling"] ,1,0)

end_time1 <- Sys.time()
food_labelling_time <- as.difftime(end_time1 - start_time1, units = "secs")


# Reporting reactions:
start_time1 <- Sys.time()
reaction_report.df.norm <- from_corpus_to_lookup_dataframe(content.corpus,reaction_report.dict)
reaction_report.names   <- colnames(reaction_report.df.norm)[-1]
# Combine the queries
mild_reaction   <- ifelse(reaction_report.df.norm[,"symptons"] & reaction_report.df.norm[,"ingestion"] &
                          !reaction_report.df.norm[,"severe"]
                          ,1,0)
severe_reaction <- ifelse(reaction_report.df.norm[,"symptons"] & reaction_report.df.norm[,"ingestion"] &
                          reaction_report.df.norm[,"severe"]
                          ,1,0)
labelled.df$reactions_report <- rep("No-report",nrow(labelled.df))
labelled.df$reactions_report[mild_reaction == 1 & severe_reaction == 0] <- "Mild-reaction"
labelled.df$reactions_report[mild_reaction == 0 & severe_reaction == 1] <- "Severe-reaction"
labelled.df$reactions_report <- factor(labelled.df$reactions_report, levels = c("No-report","Mild-reaction", "Severe-reaction"))
reaction.report.names <- c("reactions_report")

end_time1 <- Sys.time()
reaction_report_time <- as.difftime(end_time1 - start_time1, units = "secs")

### Stream 2: 14 allergens
# 14 allergens
start_time1 <- Sys.time()
fourteen_allergens.df.norm <- from_corpus_to_lookup_dataframe(content.corpus, fourteen_allergens.dict)
fourteen.allergen.names    <- colnames(fourteen_allergens.df.norm)[-1]
labelled.df                <- cbind(labelled.df,fourteen_allergens.df.norm[,fourteen.allergen.names])
end_time1 <- Sys.time()
fourteen_allergens_time <- as.difftime(end_time1 - start_time1, units = "secs")

# other allergens
start_time1 <- Sys.time()
other_allergens.df.norm <- from_corpus_to_lookup_dataframe(content.corpus, other_allergens.dict)
other.allergen.names    <- colnames(other_allergens.df.norm)[-1]
labelled.df             <- cbind(labelled.df,other_allergens.df.norm[,other.allergen.names])
end_time1 <- Sys.time()
other_allergens_time <- as.difftime(end_time1 - start_time1, units = "secs")

# Merge labelled tweets with other features from data.df
library(dplyr)

labelled.df <- left_join(labelled.df, data.df[,retained_metadata], "id")
labelled.df$date <- as.Date(labelled.df$date, format= "%Y-%m-%d")
labelled.df$Month <- as.Date(cut(labelled.df$date, breaks = "month"))
labelled.df$Week <- as.Date(cut(labelled.df$date, breaks = "week"))
names(labelled.df)[names(labelled.df) == "sentiment class"] <- "sentiment_class" #rename sentiment class to a single string
labelled.df$sentiment_class[labelled.df$sentiment_class %in% c("not_evaluable", "processing")] <- "neutral" # collapse not evaluable and procesing to neutral

save.image(file = "Waterfall.RData")

end_time <- Sys.time()
global_time <- as.difftime(end_time - start_time, units = "secs")

cat("\n\n")
print(paste("Preprocessed data records = ",nrow(labelled.df),sep=""))
print(paste("Labelled data.frame variables:",sep=""))
print(colnames(labelled.df))
cat("\n\n")

cat("\n\n")
the_time_unit <- get_time_units(allergy_enquiries_time)
print(paste("Allergy enquiries labelling time:    ",round(as.numeric(allergy_enquiries_time,  units=the_time_unit),5), " ",the_time_unit,sep=""))
the_time_unit <- get_time_units(food_labelling_time)
print(paste("Food Labelling labelling time:    ",round(as.numeric(food_labelling_time,  units=the_time_unit),5), " ",the_time_unit,sep=""))
the_time_unit <- get_time_units(reaction_report_time)
print(paste("Reporting reactions labelling time:  ",round(as.numeric(reaction_report_time,    units=the_time_unit),5), " ",the_time_unit,sep=""))
the_time_unit <- get_time_units(fourteen_allergens_time)
print(paste("14 allergens labelling time:         ",round(as.numeric(fourteen_allergens_time, units=the_time_unit),5), " ",the_time_unit,sep=""))
the_time_unit <- get_time_units(other_allergens_time)
print(paste("Other allergens labelling time:      ",round(as.numeric(other_allergens_time,    units=the_time_unit),5), " ",the_time_unit,sep=""))
the_time_unit <- get_time_units(global_time)
print(paste("Total labelling time:                ",round(as.numeric(global_time,             units=the_time_unit),5), " ",the_time_unit,sep=""))
cat("\n\n")


#



