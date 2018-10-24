# Set working directory to current folder
setwd(dirname(sys.frame(1)$ofile)) 

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


# Create a sub dataframe with only 'id' and 'content' columns : content.df

content.df <- subset(data.df, select=c("id", "content"))
