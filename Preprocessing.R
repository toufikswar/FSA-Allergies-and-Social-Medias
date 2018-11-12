### Data loading, cleaning and pre-processing script


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

sheet_name <- configuration$sheet_name
filenames  <- paste(configuration$input_dir,"/",configuration$input_file_list,sep="")

cat("\n\n")
cat(paste("Start loading the data","\n",sep=""))
# verbose variable
verbose <- TRUE
data.df <- load_list_of_xlsx_files(filenames,sheet_name,verbose)
cat(paste("Total number of records in the data = ",nrow(data.df),"\n",sep=""))

# flag to run preprocessing test
do_test_preprocessing <- FALSE

end_time1 <- Sys.time()
cat("\n\n")
atimediff <- as.difftime(end_time1 - start_time1, units = "secs")
the_time_unit <- get_time_units(atimediff)
cat(paste("Loading data time:  ",round(as.numeric(atimediff,units=the_time_unit),5)," ",the_time_unit,"\n",sep=""))
cat(paste("Finished loading the data","\n",sep=""))
cat("\n\n")

### =============TEXT CLEANING & PREPROCESSING================= ###

# Set time for beginning of text pre-preprocessing

cat("\n\n")
cat(paste("Start 1st data preprocessing","\n",sep=""))

start_time1 <- Sys.time()

# List of spurious columns to drop
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

# In this part of the pre-processing some generic string operations are applied

# Sort data by date in increasing order
data.df <- data.df[order(as.Date(data.df$date, format=c("%Y/%m/%d","h:m:s"))),]

# Subset dataframe with only 'id', 'content' and source columns : content.df
content.df <- subset(data.df, select=c("id", "content","source"))
# copy copy original content previous to pre-processing
content.df$original_content <- content.df$content
# content.df <- content.df[1:1000,]

#Convert content text to lowercase
content.df$content <- stri_trans_tolower(content.df$content)

# Remove Retweets (RT) ==> records starting with the rt text
content.df <- content.df[-grep("^rt", content.df$content),]

# Remove duplicated records
content.df <- content.df[!duplicated(content.df$content),]

# Convert content text from latin1 to ascii format
content.df$content <- iconv(content.df$content, from = "latin1", to = "ascii", sub = "byte")

# Extract usernames (words starting with @) and put them into a new column: users column
content.df$users <- stri_extract_all_regex(content.df$content, "@\\w+")

# Extract hashtags from content text and put them into a new column: hashtags
content.df$hashtags <- stri_extract_all_regex(content.df$content, "#\\w+")

#Remove Usernames, Emoticons (<xx> tags) and HTML entities (e.g. &amp;)
content.df$content <- gsub("@\\w+|<\\w+>|&.*;","", content.df$content)

#Replace % symbol by the word percent
content.df$content <- stri_replace_all_fixed(content.df$content, "%", " percent ")
#Replace ~ symbol by a white-space
content.df$content <- stri_replace_all_fixed(content.df$content, "~", " ")

# expand acronyms
# uses a dictionary with several acronyms and expand them (e.g. asap ==> as soon as possible)
acronym_key        <- read.csv("resources/acronyms.csv", header=FALSE,col.names = c("abv","repl"))  # acronyms map
abv  <- paste("\\b",acronym_key$abv,"\\b",sep="")
repl <- paste(acronym_key$repl,sep="")
content.df$content <- stri_replace_all_regex(content.df$content, abv, repl, vectorize_all=FALSE)

#Remove all punctuation
content.df$content <- stri_replace_all(content.df$content, "", regex = "[[:punct:]]")

# Expand the english contractions (e.g. he's ==> he is)
english_contraction_key  <- read.csv("resources/english_contractions.csv", header=FALSE,col.names = c("contraction","expansion"))  # acronyms map
contraction <- paste("\\b",english_contraction_key$contraction,"\\b",sep="")
expansion   <- paste(" ",english_contraction_key$expansion," ",sep="")
content.df$content <- stri_replace_all_regex(content.df$content, contraction, expansion, vectorize_all=FALSE)

# Remove http, url links that have been collapsed into words
content.df$content <- stri_replace_all_fixed(content.df$content, "=", "")
content.df$content <- gsub("url\\w+|http\\w+", "", content.df$content)

#Replace ~ and . by whitespace
content.df$content <- stri_replace_all_fixed(content.df$content, ".", " ")

# remove white-spaces at the begining and the end of the content text
content.df$content <- stri_trim(content.df$content)

end_time1 <- Sys.time()
atimediff <- as.difftime(end_time1 - start_time1, units = "secs")
the_time_unit <- get_time_units(atimediff)
cat(paste("1st preprocessing time:  ",round(as.numeric(atimediff,units=the_time_unit),5)," ",the_time_unit,"\n",sep=""))
cat(paste("Finished 1st data preprocessing","\n",sep=""))
cat("\n\n")

start_time1 <- Sys.time()

# In this 2nd part of the data two operations will be applied
# - stop word removal: will remove a set of stop words from the english language (e.g. is, a, you, ...)
# - word stemming: a rule-based algorithm that converts inflected forms of words into their base forms (stems)
#
# This part of pre-processing is a bit computationally intensive. Will run this steps in parallel
# using the cores available.
library(parallel)
Ncores <- detectCores() # detect the number of cores available
cat("\n\n")
cat(paste("Start Stopword Removal & Stemming","\n",sep=""))
cat("Depending on the size of the Data this step can take 5-10 minutes. Be patient please.")
cat(" Distributing jobs to ",Ncores," available CPU cores ...\n")
# Emojis emoji_dictionary from (https://raw.githubusercontent.com/lyons7/emojidictionary/master/emoji_dictionary.csv)
#emoticons         <- read.csv("resources/emoji_dictionary.csv", header = TRUE) # emojis emoji_dictionary

# Start a local cluster with the cores available
instance <- makeCluster(Ncores)
clusterEvalQ(instance, {
  library(quanteda)
})

# parallelized application of stop-word removal and stemming
content.df$content <- parLapply(instance, content.df$content,
  function(i) {
    i %>%
    tokens() %>%
    tokens_remove(stopwords("english")) %>%
    tokens_wordstem() %>%
    paste(collapse = " ")
  }
)
# Stop the local cluster instance
stopCluster(instance)
rm(instance)

end_time1 <- Sys.time()
atimediff <- as.difftime(end_time1 - start_time1, units = "secs")
the_time_unit <- get_time_units(atimediff)
cat(paste("Stemming & Stopword Removal time:  ",round(as.numeric(atimediff,units=the_time_unit),5)," ",the_time_unit,"\n",sep=""))
cat(paste("Finished Stemming & Stopword Removal","\n",sep=""))
cat("\n\n")

# Final removal of records duplicates
content.df <- content.df[!duplicated(content.df$content),]

# Collapse tokenized words to character vector
content.df$content <- as.character(content.df$content)  # This may interfere with tokenization for stream 1 - be aware

# Testing structure
if(do_test_preprocessing) {
  # Run a comparison between the oirignal and preprocessed texts
  # for a randomly selected set of records

  n.test.records = 500 # number of random records to printout
  test_text_preprocessing(content.df,n.test.records)
}

# Save the environment into an image file
file_out <- image_preprocessing
cat("\n\n")
cat(paste("Saving image of RData to ", file_out,"...", "\n",sep=""))
start_time1 <- Sys.time()
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
