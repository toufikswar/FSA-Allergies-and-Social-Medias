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
allergy_enquiries_time <- end_time1 - start_time1

# Food labelling:



# Reporting reactions:
start_time1 <- Sys.time()
reaction_report.df.norm <- from_corpus_to_lookup_dataframe(content.corpus,reaction_report.dict)
reaction_report.names   <- colnames(reaction_report.df.norm)[-1]
# Combine the queries
labelled.df$mild_reaction   <- ifelse(reaction_report.df.norm[,"symptons"] & reaction_report.df.norm[,"ingestion"] &
                                      !reaction_report.df.norm[,"severe"]
                                      ,1,0)
labelled.df$severe_reaction <- ifelse(reaction_report.df.norm[,"symptons"] & reaction_report.df.norm[,"ingestion"] &
                                      reaction_report.df.norm[,"severe"]
                                      ,1,0)
reaction.report.names       <- c("mild_reaction","severe_reaction")

end_time1 <- Sys.time()
reaction_report_time <- end_time1 - start_time1

### Stream 2: 14 allergens
# 14 allergens
start_time1 <- Sys.time()
fourteen_allergens.df.norm <- from_corpus_to_lookup_dataframe(content.corpus, fourteen_allergens.dict)
fourteen.allergen.names    <- colnames(fourteen_allergens.df.norm)[-1]
labelled.df                <- cbind(labelled.df,fourteen_allergens.df.norm[,fourteen.allergen.names])
end_time1 <- Sys.time()
fourteen_allergens_time <- end_time1 - start_time1

# other allergens
start_time1 <- Sys.time()
other_allergens.df.norm <- from_corpus_to_lookup_dataframe(content.corpus, other_allergens.dict)
other.allergen.names    <- colnames(other_allergens.df.norm)[-1]
labelled.df             <- cbind(labelled.df,other_allergens.df.norm[,other.allergen.names])
end_time1 <- Sys.time()
other_allergens_time <- end_time1 - start_time1

# Merge labelled tweets with other features from data.df
library(dplyr)

labelled.df <- left_join(labelled.df, data.df[,retained_metadata], "id")

end_time <- Sys.time()
gobal_time <- end_time - start_time

cat("\n\n")
print(paste("Preprocessed data records = ",nrow(labelled.df),sep=""))
print(paste("Labelled data.frame variables:",sep=""))
print(colnames(labelled.df))
cat("\n\n")

cat("\n\n")
print(paste("Allergy enquiries labelling time:    ",round(allergy_enquiries_time,5),  " secs",sep=""))
print(paste("Reporting reactions labelling time:  ",round(reaction_report_time,5),    " secs",sep=""))
print(paste("14 allergens labelling time:         ",round(fourteen_allergens_time,5), " secs",sep=""))
print(paste("Other allergens labelling time:      ",round(other_allergens_time,5),    " secs",sep=""))
print(paste("Total labelling time:                ",round(gobal_time,5),              " secs",sep=""))
cat("\n\n")

save.image(file = "Waterfall.RData")


#
