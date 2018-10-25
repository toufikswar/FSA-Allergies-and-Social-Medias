library(stringi)
library(qdap)
library(quanteda)
library(magrittr)

# Set working directory to current folder
#setwd(dirname(sys.frame(1)$ofile))

# setwd("~/Google Drive/S2DS/FSA-Virtual-Oct18/")

start_time  <- Sys.time()

start_time1 <- Sys.time()

source("utils.R")

# path where the data is located
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

# Now some preprocessing

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
print(names(data.df))

# Subset dataframe with only 'id' and 'content' columns : content.df
content.df <- subset(data.df, select=c("id", "content"))
content.df <- content.df[1:1000,]

# Subset dataframe containing metadata only
metadata.df <- data.df[ , ! colnames(data.df) %in% c("content") ]

end_time1 <- Sys.time()
print(paste("Loading time:       ",round(end_time1 - start_time1,5)," secs",sep=""))

# Start text pre-preprocessing
#Convert to lowercase
start_time1 <- Sys.time()
content.df$content <- stri_trans_tolower(content.df$content)

#Expand acronyms
start_time1 <- Sys.time()
acronym_key        <- read.csv("acronyms.csv", header=FALSE,col.names = c("abv","repl"))  # acronyms map
content.df$content <- replace_abbreviation(content.df$content, acronym_key)

#Remove Usernames starting with @, rt, #
content.df$content <- gsub("@\\w+|#\\w+|^rt ","", content.df$content)

#Replace ~ by whitespace
content.df$content <- stri_replace_all_fixed(content.df$content, "~", " ")

#Remove all punctuation
content.df$content <- stri_replace_all(content.df$content, "", regex = "[[:punct:]]")

#Remove http, url links that have been collapsed into words
content.df$content <- stri_replace_all_fixed(content.df$content, "=", "")
content.df$content <- gsub("url\\w+|http\\w+", "", content.df$content)

# string surroundings whitespace
content.df$content <- stri_trim(content.df$content)
end_time1 <- Sys.time()
print(paste("1st preprocessing:  ",round(end_time1 - start_time1,5)," secs",sep=""))

start_time1 <- Sys.time()
# stemming & stopword removing
content.df$content <- lapply(content.df$content,
  function(i) {
    i %>%
    tokens() %>%
    tokens_wordstem() %>%
    tokens_remove(stopwords("english")) %>%
    paste(collapse = " ")
  }
)
end_time1 <- Sys.time()
print(paste("2nd preprocessing:  ",round(end_time1 - start_time1,5)," secs",sep=""))

# # Test structure for Printing out 100 randomly selected tweets after preprocessing
# set.seed(1)
# sub_sample <- sample(1:nrow(content.df),100,replace=FALSE)
#
# for(i in sub_sample) {
#   cat("\n")
#   cat(paste("Printing tweet ",i,"\n",sep=""))
#   cat(paste(as.character(content.df$content[i],"\n",sep="")))
#   cat("\n")
#   cat("\n")
# }

end_time <- Sys.time()

print(paste("Execution time:     ",round(end_time - start_time,5)," secs",sep=""))

###
