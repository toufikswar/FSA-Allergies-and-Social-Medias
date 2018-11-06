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

save.image(file = "Waterfall.RData")

end_time <- Sys.time()
gobal_time <- as.difftime(end_time - start_time, units = "secs")

cat("\n\n")
print(paste("Preprocessed data records = ",nrow(labelled.df),sep=""))
print(paste("Labelled data.frame variables:",sep=""))
print(colnames(labelled.df))
cat("\n\n")

cat("\n\n")
print(paste("Allergy enquiries labelling time:    ",round(as.numeric(allergy_enquiries_time,  units="secs"),5), " secs",sep=""))
print(paste("Reporting reactions labelling time:  ",round(as.numeric(reaction_report_time,    units="secs"),5), " secs",sep=""))
print(paste("14 allergens labelling time:         ",round(as.numeric(fourteen_allergens_time, units="secs"),5), " secs",sep=""))
print(paste("Other allergens labelling time:      ",round(as.numeric(other_allergens_time,    units="secs"),5), " secs",sep=""))
print(paste("Total labelling time:                ",round(as.numeric(gobal_time,              units="secs"),5), " secs",sep=""))
cat("\n\n")


#
