setwd("/home/aperez/Documentos_importantes/Cosas_Importantes/Reconversion_Profesional/S2DS_Virtual_2018/FSA_project/Code/FSA-Virtual-Oct18/")

source("LoadingData.R")

# path where the data is located
dataDir   = "Data/"
# list for file names
filenames = c("20161029-20171127.xlsx",
              "20171128-20180926.xlsx")

# paste the data path with filename
filenames = paste(dataDir,filenames,sep="")

# verbose variable
verbose = TRUE
df <- load_list_of_xlsx_files(filenames,verbose)
cat(paste("N records = ",nrow(df),"\n",sep=""))

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

df <- df[,-match(columns_to_drop,names(df))]
print(names(df))

