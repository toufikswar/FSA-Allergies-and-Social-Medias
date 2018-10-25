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

# Subset dataframe containing metadata only
metadata.df <- data.df[ , ! colnames(data.df) %in% c("content") ]


library(stringi)

#small test dataset
test.df <- content.df[1:100,]

#Convert to lowercase
test.df$content <- stri_trans_tolower(test.df$content) 

#Remove Usernames starting with @ & rt
test.df$content <- gsub("@\\w+ *","", test.df$content)
test.df$content <- gsub("^rt ", "", test.df$content)

#Remove all punctuation
test.df$content <- stri_replace_all(test.df$content, "", regex = "[[:punct:]]")

#Remove http links that have been collapsed into words
test.df$content <- gsub("http\\w+","", test.df$content)

library(quanteda)








