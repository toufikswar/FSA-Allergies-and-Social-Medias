## Parent Script for Finding Public Mentions of Allergens in Tweet Data

## Set working directory to current folder
#setwd(dirname(sys.frame(1)$ofile))
# (set you workdir)


### Initialization

## Load libraries for preprocessing
library(stringi)
# library(qdap)
library(quanteda)
library(magrittr)

## for testing
start_time  <- Sys.time()
start_time1 <- Sys.time()

# Read in Functions stored in utils.R
source("utils.R")

# Path where the data is located
dataDir   = "Data/"
# list for file names
filenames = c("20161029-20171127.xlsx",
              "20171128-20180926.xlsx")

# paste the data path with filename
filenames = paste(dataDir,filenames,sep="")

# verbose variable
verbose = TRUE
data.df <- load_list_of_xlsx_files(filenames,verbose)
cat(paste("N records = ",nrow(data.df),"\n",sep=""))



### Now some preprocessing

# Lets drop some spureous columns
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

# Subset dataframe with only 'id' and 'content' columns : content.df
content.df <- subset(data.df, select=c("id", "content","source"))
content.df <- content.df[1:1000,]

# Subset dataframe containing metadata only
metadata.df <- data.df[ , ! colnames(data.df) %in% c("content") ]

end_time1 <- Sys.time()
cat("\n\n")
print(paste("Loading time:       ",round(end_time1 - start_time1,5)," secs",sep=""))

# Start text pre-preprocessing

start_time1 <- Sys.time()
#Convert to lowercase
content.df$content <- stri_trans_tolower(content.df$content)

# Convert tweets text to ascii format
content.df$content <- iconv(content.df$content, from = "latin1", to = "ascii", sub = "byte")

#Remove Usernames starting with @, & rt
content.df$content <- gsub("@\\w+|^rt |<\\w+>","", content.df$content)

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
print(paste("1st preprocessing:  ",round(end_time1 - start_time1,5)," secs",sep=""))

start_time1 <- Sys.time()

# stemming & stopword + emojis removing

# list of engish stop words
words_to_remove   <- stopwords("english")
# Emojis emoji_dictionary from (https://raw.githubusercontent.com/lyons7/emojidictionary/master/emoji_dictionary.csv)
#emoticons         <- read.csv("resources/emoji_dictionary.csv", header = TRUE) # emojis emoji_dictionary

library(parallel)
instance <- makeCluster(detectCores()) # Start a local cluster with the cores available
clusterEvalQ(instance, {
  library(quanteda)
  words_to_remove   <- stopwords("english")
})

content.df$content <- parLapply(instance, content.df$content,
  function(i) {
    i %>%
    tokens() %>%
    tokens_wordstem() %>%
    tokens_remove(words_to_remove) %>%
    paste(collapse = " ")
  }
)
stopCluster(instance)
end_time1 <- Sys.time()
print(paste("2nd preprocessing:  ",round(end_time1 - start_time1,5)," secs (stemming & stopwords removal)",sep=""))

end_time <- Sys.time()

print(paste("Execution time:     ",round(end_time - start_time,5)," secs (all processes)",sep=""))
cat("\n\n")
print(paste("Number of tweets processed: ",nrow(content.df),sep=""))
cat("\n\n")

# Running test to compare the oirignal and preprocessed texts
# of a randomly selected set of records
n.test.records = 500
test_text_preprocessing(data.df,content.df,n.test.records)

###
