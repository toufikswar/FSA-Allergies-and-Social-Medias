# Set working directory to current folder
#setwd(dirname(sys.frame(1)$ofile)) 

setwd("~/Google Drive/S2DS/FSA-Virtual-Oct18/")

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
content.df2 <- content.df
# Subset dataframe containing metadata only
metadata.df <- data.df[ , ! colnames(data.df) %in% c("content") ]


library(stringi)

#Convert to lowercase
content.df$content <- stri_trans_tolower(test.df$content) 

library(qdap)
#Expand acronyms
acronym_key <- read.csv("acronyms.csv", header=FALSE,col.names = c("abv","repl"))
content.df <- replace_abbreviation(content.df, acronym_key)

#Remove Usernames starting with @ & rt
content.df$content <- gsub("@\\w+ *","", content.df$content)
content.df$content <- gsub("^rt ", "", content.df$content)

#Remove all punctuation
content.df$content <- stri_replace_all(content.df$content, "", regex = "[[:punct:]]")

#Remove http links that have been collapsed into words
content.df$content <- gsub("http\\w+","", content.df$content)








library(quanteda)








