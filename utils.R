# Utilities and Dictionaries

## FUNCTIONS:

### LOADING DATA:

library(readxl)          # library to read xlsx files (excel)

#================================================================
load_list_of_xlsx_files <- function(filenames,
                                    sheet_name="Sheet1",
                                    verbose=FALSE)
{

  # reads the data from a list of files into a single data.frame

  nfiles    = length(filenames)  # number of filenames in list
  all_data  = list()             # empty list with all data.frames per file
  col_names = vector()           # empty vector with the columns names
  for(i in 1:nfiles) {

    if(i == 1) {
      # only the 1st file has the columns names
      all_data[[i]] = read_excel(filenames[i],sheet=sheet_name,col_names=TRUE)
      # save column names
      col_names     = names(all_data[[i]])

      if(verbose) {
        # print info about the column names of this data
        cat(paste("Data column names (from 1st file):","\n",sep=""))
        print(col_names)
        cat("\n")
      }

    }
    else {
      # extract the rest of the files without column names
      all_data[[i]] = read_excel(filenames[i],sheet=sheet_name,col_names=FALSE)
      # set the same column names as the 1st file
      colnames(all_data[[i]]) <- col_names
    }

    if(verbose) {
      # report on number of records on each dataset on list
      cat(paste("Extracting data from file: ",filenames[i],"\n",sep=""))
      cat(paste("N records in this files = ",nrow(all_data[[i]]),"\n\n",sep=""))
    }
  }

  # concatenate all the data.frames into a single one
  if(verbose) cat(paste("Merging all the data into a single data.frame","\n\n",sep=""))
  df <- do.call(rbind.data.frame, all_data)

  return(df)

}
#================================================================

### TESTING ORIGINAL TWEET VS PROCESSED TWEET

test_text_preprocessing <- function(df,n.test.records)
{

  # Compares the original and preprocessed text for a random sampling of n.test.records

  n.test = n.test.records

  # The number of test records cannot be higher than the number of records
  nrecords = nrow(df)
  if(n.test > nrecords) n.test = nrecords

  # Set seed for test reproductivility
  set.seed(1)
  sub_sample <- sample(1:nrecords,n.test,replace=FALSE)

  for(i in sub_sample) {
    cat("\n")
    cat(paste("Printing tweet id=",df$id[i],", (source = ",df$source[i],")\n",sep=""))
    cat(paste("Original     record:     ",as.character(df$original_content[i],"\n",sep="")))
    cat("\n")
    cat("\n")
    cat(paste("Preprocessed record:     ",as.character(df$content[i],"\n",sep="")))
    cat("\n")
    cat("\n")
    cat("\n")
  }

}
#================================================================
get_dictionary_from_file <- function(dict_filename)
{

  # build a dictionary from a cvs file

  # Vector with the dictionary groupping names
  the_names <- vector()
  # The output dictionary
  myList <- list()

  # opening the file with the dictionary in csv format
  con <- file(dict_filename,"r")
  # file line counter
  counter_line <- 0
  # groupping line counter
  counter_groupping <- 0
  # read line by line
  while(TRUE) {
    # get the line
    line <- readLines(con,n=1)
    if(length(line) == 0) {
      # Exit if end of file
      break
    }
    # Counting the lines already read
    counter_line <- counter_line + 1
    # exclude empty lines
    if(line == "") next
    # exclude comment lines (starting with #)
    if(startsWith(line,"#")) next

    # counter the number of groupping in the file
    counter_groupping <- counter_groupping + 1
    # the elements in a line a comma separated
    # get a vector with the different elements
    vec_line <- unlist(strsplit(line,","))

    # the 1st element is the groupping name
    the_names <- c(the_names,vec_line[1])
    # the rest of the elements are the dictionary
    myList[[counter_groupping]] <- vec_line[-1]

  }
  close(con)

  # Set the groupping names
  names(myList) <- the_names

  # Stemming the dictionary
  myList <- stem_dictionary(myList)

  return(dictionary(myList))

}
#==================================================================

## This function takes a dictionary in list format and
## perform stemming on its terms
## Returns the stemmed dictionary

stem_dictionary <- function(myList)
{

  library(quanteda)

  # loop over the dictionary lists
  for(i in 1:length(myList)) {
    # loop over the list elements
    for(j in 1:length(myList[[i]])) {
      term <- myList[[i]][j]
      # separate the string into words
      words <- unlist(strsplit(term," "))
      words <- words[words != ""]

      # loop over the words
      for(k in 1:length(words)) {
        # check if word ends with *
        endsWithStart <- endsWith(words[k],"*")
        # remove the * at the end of the word
        if(endsWithStart) words[k] <- substr(words[k],1,nchar(words[k])-1)

        # stem the word
        words[k] <- paste(tokens_wordstem(tokens(words[k])),collapse="")
        # reattach the * and the end of the word
        if(endsWithStart) words[k] <- paste(c(words[k],"*"),collapse="")
      }
      # combine all the words into a single string
      term <- paste(words,collapse=" ")
      myList[[i]][j] <- term
    }
    # remove duplicates
    myList[[i]] <- unique(myList[[i]])
    # order alphabetically
    myList[[i]] <- sort(myList[[i]],decreasing=FALSE)
  }

  return(myList)

}
#==================================================================

## Function that takes a <corpus> and a <dictonary>
## Looks up the content their content to a DFM
## Returns a normalized Data Frame (on one mention per document)

from_corpus_to_lookup_dataframe <- function(data.corpus, dict)
{
  data.dfm <- dfm(data.corpus, tolower = FALSE, verbose = TRUE, dictionary=dict)
  processed.df <- convert(data.dfm, "data.frame")
  colnames(processed.df)[1] <- "id"
  processed.df.names <- colnames(processed.df)[-1]
  #Normalization to one mention per document
  norm.df <- data.frame(id = processed.df$id, ifelse(processed.df[,processed.df.names] > 0, 1, 0))
  return (norm.df)
}
#================================================================
get_time_units = function(timediff)
{

  timediff <- as.numeric(timediff,units="secs")

  amin  <- 60
  ahour <- 60*amin
  aday  <- 24*ahour

  the_time_unit = "secs"

  if(timediff/amin > 1) {
    the_time_unit = "mins"
  } else if(timediff/ahour > 1) {
    the_time_unit = "hours"
  } else if(timediff/aday > 1) {
    the_time_unit = "days"
  }

  return(the_time_unit)

}
#================================================================


#
