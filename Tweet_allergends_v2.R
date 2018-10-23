setwd("/home/aperez/Documentos_importantes/Cosas_Importantes/Reconversion_Profesional/S2DS_Virtual_2018/FSA_project/MyCode")

source("LoadingData.R")

# path where the data is located
dataDir   = "../Data/"
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
#columns_to_drop = vector()
#df <- df[,-columns_to_drop]
