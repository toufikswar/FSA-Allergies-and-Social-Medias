### Parent Script for Finding Public Mentions of Allergens in Tweet Data

# Set working directory to current folder
# setwd(dirname(sys.frame(1)$ofile))


### ======================INITIALIZATION========================== ###
# Load libraries and source utils.R.
# quanteda library must loaded before 'utils.R' is
# sourced to ensure allergen dictionaries are created properly

## for testing
start_time  <- Sys.time()
start_time1 <- Sys.time()

library(stringi)
library(quanteda)
library(magrittr)

# Read in Functions stored in utils.R
source("utils.R")

# Path where the data is located
dataDir   = "Data/"
# list for file names
filenames = c("20161029-20171127.xlsx",
              "20171128-20180926.xlsx")
sheet_name = "Sheet1"

# paste the data path with filename
filenames = paste(dataDir,filenames,sep="")

cat("\n\n")
cat(paste("Start loading the data","\n",sep=""))
# verbose variable
verbose = TRUE
data.df <- load_list_of_xlsx_files(filenames,sheet_name,verbose)
cat(paste("N records = ",nrow(data.df),"\n",sep=""))

# flag to run preprocessing test
do_test_preprocessing <- FALSE

end_time1 <- Sys.time()
cat("\n\n")
atimediff <- as.difftime(end_time1 - start_time1, units = "secs")
the_time_unit <- get_time_units(atimediff)
cat(paste("Loading data time:  ",round(as.numeric(atimediff,units=the_time_unit),5)," ",the_time_unit,"\n",sep=""))
cat(paste("Finished loading the data","\n",sep=""))
cat("\n\n")

### =============LOAD DICTIONARIES================= ###
# (will only create if library(quanteda) is loaded correctly)

cat("\n\n")
cat(paste("Start loading dictionaries","\n",sep=""))

start_time1 <- Sys.time()
# Stream 1: Supporting local authorities
supporting_local_authorities_dict_filename <- "dictionaries/supporting_local_authorities_dictionary.csv"
supporting_local_authorities.dict <- get_dictionary_from_file(supporting_local_authorities_dict_filename)
cat("\n\n")
cat(paste("Supporting local authorities dictionary","\n",sep=""))
print(supporting_local_authorities.dict)
cat("\n\n")

# Stream 2: 14 allergen list:
# 14 allergen dictionary:
fourteen_allergens_dict_filename <- "dictionaries/fourteen_allergens_dictionary.csv"
fourteen_allergens.dict <- get_dictionary_from_file(fourteen_allergens_dict_filename)
cat("\n\n")
cat(paste("14 allergens dictionary","\n",sep=""))
print(fourteen_allergens.dict)
cat("\n\n")

# Other allergen dictionary:
other_allergens_dict_filename <- "dictionaries/other_allergens_dictionary.csv"
other_allergens.dict <- get_dictionary_from_file(other_allergens_dict_filename)
cat("\n\n")
cat(paste("other allergens dictionary","\n",sep=""))
print(other_allergens.dict)
cat("\n\n")

end_time1 <- Sys.time()
atimediff <- as.difftime(end_time1 - start_time1, units = "secs")
the_time_unit <- get_time_units(atimediff)
cat(paste("Loading dictionary time:  ",round(as.numeric(atimediff,units=the_time_unit),5)," ",the_time_unit,"\n",sep=""))
cat(paste("Finished loading dictionaries","\n",sep=""))
cat("\n\n")

## columns names from the original data to be merged with the streams labellings
retained_metadata <- c("id", "latitude", "longitude","date","sentiment","sentiment class","topics")

### =============PREPROCESSING & TEXT CLEANING================= ###

# Set time for beginning of text pre-preprocessing

cat("\n\n")
cat(paste("Start 1st data preprocessing","\n",sep=""))

start_time1 <- Sys.time()

# Lets drop some spurious columns
columns_to_drop <- c("search",
                     "language",
                     "main emotion",
                     "emotions",
                     "image tags",
                     "tags",
                     "parent source identifier",
                     "user longitude",
                     "user latitude",
                     "city",
                     "user city",
                     "no. of likes",
                     "no. of comments",
                     "no. of engagements")

data.df <- data.df[,-match(columns_to_drop,names(data.df))]
#print(names(data.df))

# Sorting data by date in increasing order
data.df <- data.df[order(as.Date(data.df$date, format=c("%Y/%m/%d","h:m:s"))),]

# Subset dataframe with only 'id' and 'content' columns : content.df
content.df <- subset(data.df, select=c("id", "content","source"))
content.df$original_content <- content.df$content
#content.df <- content.df[1:1000,]

#Convert to lowercase
content.df$content <- stri_trans_tolower(content.df$content)

# Remove Retweets
content.df <- content.df[-grep("^rt", content.df$content),]

# Remove duplicate original tweets
content.df <- content.df[!duplicated(content.df$content),]

# Convert tweets text to ascii format
content.df$content <- iconv(content.df$content, from = "latin1", to = "ascii", sub = "byte")

library(dplyr)

# Extract usernames to new column
content.df$users <- stri_extract_all_regex(content.df$content, "@\\w+")
# Merge usernames to data.df
data.df <- left_join(data.df, content.df[,c("id","users")], "id")

# Extract hashtags to new column
content.df$hashtags <- stri_extract_all_regex(content.df$content, "#\\w+")
# Merge hashtags to data.df
data.df <- left_join(data.df, content.df[,c("id","hashtags")], "id")

#Remove Usernames starting with @, Emoticons (<xx> tags) and HTML entities (e.g. &amp;)
content.df$content <- gsub("@\\w+|<\\w+>|&.*;","", content.df$content)

# #Replace % by percent
content.df$content <- stri_replace_all_fixed(content.df$content, "%", " percent ")
#Replace ~ by whitespace
content.df$content <- stri_replace_all_fixed(content.df$content, "~", " ")

# expand acronyms
acronym_key        <- read.csv("resources/acronyms.csv", header=FALSE,col.names = c("abv","repl"))  # acronyms map
abv  <- paste("\\b",acronym_key$abv,"\\b",sep="")
repl <- paste(acronym_key$repl,sep="")
# content.df$content <- replace_abbreviation(content.df$content, acronym_key)
content.df$content <- stri_replace_all_regex(content.df$content, abv, repl, vectorize_all=FALSE)

#Remove all punctuation
content.df$content <- stri_replace_all(content.df$content, "", regex = "[[:punct:]]")

# Expand the english contractions
english_contraction_key  <- read.csv("resources/english_contractions.csv", header=FALSE,col.names = c("contraction","expansion"))  # acronyms map
contraction <- paste("\\b",english_contraction_key$contraction,"\\b",sep="")
expansion   <- paste(" ",english_contraction_key$expansion," ",sep="")
content.df$content <- stri_replace_all_regex(content.df$content, contraction, expansion, vectorize_all=FALSE)

# Remove http, url links that have been collapsed into words
content.df$content <- stri_replace_all_fixed(content.df$content, "=", "")
content.df$content <- gsub("url\\w+|http\\w+", "", content.df$content)

#Replace ~ and . by whitespace
content.df$content <- stri_replace_all_fixed(content.df$content, ".", " ")

# string surroundings whitespace
content.df$content <- stri_trim(content.df$content)
end_time1 <- Sys.time()
atimediff <- as.difftime(end_time1 - start_time1, units = "secs")
the_time_unit <- get_time_units(atimediff)
cat(paste("1st preprocessing time:  ",round(as.numeric(atimediff,units=the_time_unit),5)," ",the_time_unit,"\n",sep=""))
cat(paste("Finished 1st data preprocessing","\n",sep=""))
cat("\n\n")

start_time1 <- Sys.time()

# Stemming & Stopword Removal
cat("\n\n")
cat(paste("Start Stemming & Stopword Removal","\n",sep=""))
cat("Depending on the size of the Data this step can take 5-10 minutes. Be patient please.")
cat(" Distributing jobs to available CPU cores...\n")
# words_to_remove   <- stopwords("english") # list of engish stop words
# Emojis emoji_dictionary from (https://raw.githubusercontent.com/lyons7/emojidictionary/master/emoji_dictionary.csv)
#emoticons         <- read.csv("resources/emoji_dictionary.csv", header = TRUE) # emojis emoji_dictionary

library(parallel)
instance <- makeCluster(detectCores()) # Start a local cluster with the cores available
clusterEvalQ(instance, {
  library(quanteda)
})

content.df$content <- parLapply(instance, content.df$content,
  function(i) {
    i %>%
    tokens() %>%
    tokens_remove(stopwords("english")) %>%
    tokens_wordstem() %>%
    paste(collapse = " ")
  }
)
stopCluster(instance)
rm(instance)

end_time1 <- Sys.time()
atimediff <- as.difftime(end_time1 - start_time1, units = "secs")
the_time_unit <- get_time_units(atimediff)
cat(paste("Stemming & Stopword Removal time:  ",round(as.numeric(atimediff,units=the_time_unit),5)," ",the_time_unit,"\n",sep=""))
cat(paste("Finished Stemming & Stopword Removal","\n",sep=""))
cat("\n\n")

# Removing duplicates
content.df <- content.df[!duplicated(content.df$content),]

# Running test to compare the oirignal and preprocessed texts
# of a randomly selected set of records
if(do_test_preprocessing) {
  n.test.records = 500
  test_text_preprocessing(content.df,n.test.records)
}

file_out <- "Tweet_allergens.RData"
cat("\n\n")
cat(paste("Saving image of RData to ", file_out,"...", "\n",sep=""))
start_time1 <- Sys.time()
# Collapse tokenized words to character vectors
content.df$content <- as.character(content.df$content)  # This may interfere with tokenization for stream 1 - be aware
# Create a corpus including id for identifier
content.corpus <- corpus(content.df, docid_field = "id", text_field = "content") # Username and Hashtag metadata is retained
save.image(file = file_out)

end_time1 <- Sys.time()
atimediff <- as.difftime(end_time1 - start_time1, units = "secs")
the_time_unit <- get_time_units(atimediff)
cat(paste("Image save time:  ",round(as.numeric(atimediff,units=the_time_unit),5)," ",the_time_unit,"\n",sep=""))
cat(paste("Finished! ","\n",sep=""))
cat("\n\n")

end_time <- Sys.time()

atimediff <- as.difftime(end_time - start_time, units = "secs")
the_time_unit <- get_time_units(atimediff)
cat("\n\n")
cat(paste("Total execution time:     ",round(as.numeric(atimediff,units=the_time_unit),5)," ",the_time_unit,"\n",sep=""))
cat("\n\n")
cat(paste("Number of unique entires cleaned: ",nrow(content.df),"\n",sep=""))
cat("\n\n")


#
