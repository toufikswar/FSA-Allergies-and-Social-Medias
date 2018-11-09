load("Tweet_allergens.RData")
library(quanteda)
library(tidyr)

labelled.df <- content.df

# Gobal Execution time:
start_time  <- Sys.time()

### Stream 1: Supporting Local Authorities
start_time1 <- Sys.time()
supporting_local_authorities.df.norm <- from_corpus_to_lookup_dataframe(content.corpus,supporting_local_authorities.dict)
supporting_local_authorities.names   <- colnames(supporting_local_authorities.df.norm)[-1]
labelled.df                          <- cbind(labelled.df,supporting_local_authorities.df.norm[,supporting_local_authorities.names])

# Allergy enquiries query combination:
# Looking for [allergy AND info AND (request OR response) AND restaurant]
labelled.df$allergy_enquiries <- ifelse(labelled.df[,"allergy"] &
                                        labelled.df[,"info"]    &
                                        (labelled.df[,"request"] | labelled.df[,"response"]) &
                                        labelled.df[,"restaurant"],1,0)
allergy.enquiries.names       <- c("allergy_enquiries")

# Food labelling query combination:
# Looking for [(consurmer AND issue AND labelling) OR (incorrect AND allergy AND labelling) OR (consumer AND allergy AND labelling)]
labelled.df$food_labelling <- ifelse((labelled.df[,"consumer"] &
                                      labelled.df[,"issue"] &
                                      labelled.df[,"labelling"])
                                      |
                                     (labelled.df[,"incorrect"] &
                                      labelled.df[,"allergy"] &
                                      labelled.df[,"labelling"])
                                      |
                                     (labelled.df[,"consumer"] &
                                      labelled.df[,"allergy"] &
                                      labelled.df[,"labelling"]) ,1,0)


# Reporting reactions query combination:
# Looking for [allergy & symptons & ingestion AND NOT severe]
mild_reaction   <- ifelse(labelled.df[,"allergy"] & labelled.df[,"symptons"] & labelled.df[,"ingestion"] &
                          !labelled.df[,"severe"]
                          ,1,0)
# Looking for [allergy & symptons & ingestion AND severe]
severe_reaction <- ifelse(labelled.df[,"allergy"] & labelled.df[,"symptons"] & labelled.df[,"ingestion"] &
                          labelled.df[,"severe"]
                          ,1,0)
labelled.df$reactions_report <- rep("No-report",nrow(labelled.df))
labelled.df$reactions_report[mild_reaction == 1 & severe_reaction == 0] <- "Mild-reaction"
labelled.df$reactions_report[mild_reaction == 0 & severe_reaction == 1] <- "Severe-reaction"
labelled.df$reactions_report <- factor(labelled.df$reactions_report, levels = c("No-report","Mild-reaction", "Severe-reaction"))
reaction.report.names <- c("reactions_report")

end_time1 <- Sys.time()
supporting_local_authorities_time <- as.difftime(end_time1 - start_time1, units = "secs")

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
the_time_unit <- get_time_units(supporting_local_authorities_time)
print(paste("Supporting local authorities labelling time:  ",round(as.numeric(supporting_local_authorities_time,  units=the_time_unit),5), " ",the_time_unit,sep=""))
the_time_unit <- get_time_units(fourteen_allergens_time)
print(paste("14 allergens labelling time:                  ",round(as.numeric(fourteen_allergens_time, units=the_time_unit),5), " ",the_time_unit,sep=""))
the_time_unit <- get_time_units(other_allergens_time)
print(paste("Other allergens labelling time:               ",round(as.numeric(other_allergens_time,    units=the_time_unit),5), " ",the_time_unit,sep=""))
the_time_unit <- get_time_units(global_time)
print(paste("Total labelling time:                         ",round(as.numeric(global_time,             units=the_time_unit),5), " ",the_time_unit,sep=""))
cat("\n\n")


#
