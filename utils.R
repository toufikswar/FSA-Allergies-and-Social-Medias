# Utilities and Dictionaries

## FUNCTIONS:

### LOADING DATA:

library(readxl)          # library to read xlsx files (excel)

#================================================================
load_list_of_xlsx_files = function(filenames,verbose=FALSE)
{

  # reads the data from a list of files into a single data.frame

  nfiles    = length(filenames)  # number of filenames in list
  all_data  = list()             # empty list with all data.frames per file
  col_names = vector()           # empty vector with the columns names
  for(i in 1:nfiles) {

    if(i == 1) {
      # only the 1st file has the columns names
      all_data[[i]] = read_excel(filenames[i],sheet="Sheet1",col_names=TRUE)
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
      all_data[[i]] = read_excel(filenames[i],sheet="Sheet1",col_names=FALSE)
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

test_text_preprocessing = function(original.data,preprocessed.data,n.test.records)
{

  # Compares the original and preprocessed text for a random sampling of n.test.records

  n.test = n.test.records

  # The number of test records cannot be higher than the number of records
  nrecords = nrow(preprocessed.data)
  if(n.test > nrecords) n.test = nrecords

  # Set seed for test reproductivility
  set.seed(1)
  sub_sample <- sample(1:nrecords,n.test,replace=FALSE)

  for(i in sub_sample) {
    cat("\n")
    cat(paste("Printing tweet ",i,", (source = ",original.data$source[i],")\n",sep=""))
    cat(paste("Original     record:     ",as.character(original.data$content[i],    "\n",sep="")))
    cat("\n")
    cat("\n")
    cat(paste("Preprocessed record:     ",as.character(preprocessed.data$content[i],"\n",sep="")))
    cat("\n")
    cat("\n")
    cat("\n")
  }

}
#================================================================
get_dictionary_from_file = function(dict_filename)
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

  return(dictionary(myList))

}
#================================================================

## DICTIONARIES (will only create if library(quanteda) is loaded correctly)

# # Allergy enquiries dictionary:
# allergy_enquiries_dict_filename <- "dictionaries/allergy_enquiries_dictionary.csv"
# allergy_enquiries.dict <- get_dictionary_from_file(allergy_enquiries_dict_filename)
# # Food labelling:
#
# # Reporting reactions:
# reaction_report_dict_filename <- "dictionaries/reaction_report_dictionary.csv"
# reaction_report.dict <- get_dictionary_from_file(reaction_report_dict_filename)

fourteen_allergens_dict_filename <- "dictionaries/fourteen_allergens_dictionary.csv"
fourteen_allergens.dict <- get_dictionary_from_file(fourteen_allergens_dict_filename)

other_allergens_dict_filename <- "dictionaries/other_allergens_dictionary.csv"
other_allergens.dict <- get_dictionary_from_file(other_allergens_dict_filename)




#
