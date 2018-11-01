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





## DICTIONARIES (will only create if library(quanteda) is loaded correctly)

fourteen_allergens.dict <- dictionary(list(celery = c("celery*", "celeri"),
                                           cereals_contain_gluten = c("gluten*", "wheat*", "rye*", "barley*", "barli*","oat*"),
                                           crustaceans = c("crab*", "prawn*", "lobster*", "lobstr*",  "scampi*",  "shrimp*", "shrimp paste", "shrimppaste", "crayfish*", "cray fish*", "crustacean*"),
                                           eggs = c("egg*"),
                                           fish = c("fish*", "salmon*", "tuna*"),
                                           lupin = c("lupin*"),
                                           milk = c("milk*", "cream*"),
                                           molluscs = c("mollusc*", "mussel*", "oyster*", "clams*", "scallop*", "snail*", "squid*"),
                                           mustard = c("mustard*"),
                                           tree_nuts = c("nut*" ,"almond*", "hazelnut*", "walnut*", "brazil nut*", "brazilnut*", "cashew*", "pecan*", "pistachio*", "macadamia nut*", "macadamianut*", "queensland nut*", "queenslandnut*", "tree nut*", "treenut*"),
                                           peanuts = c("peanut*", "pea nut*"),
                                           sesame_seeds = c("sesame*","sesame seed*", "sesameseed*"),
                                           soybeans = c("soya*", "soybean*", "soy bean*", "soy*"),
                                           sulphur_dioxide_and_sulphites = c("sulphur dioxide*", "sulphurdioxide*", "sulphit*", "sulphite*"))
)


other_allergens.dict <- dictionary(list(corn = c("corn*", "maize*",  "sweetcorn*"),
                                        meat = c("meat*", "pork*", "beef*", "lamb*"),
                                        gelatine = c("gelatine*"), 
                                        seed = c("seed*", "sunflower*", "poppy*", "seed oil*"),
                                        spice = c("spice*", "coriander*", "garlic*", "cinnamon*", "chilli*"),
                                        fruit = c("fruit*", "apple*", "carrot*", "peach*", "plum*", "tomato*", "banana*", "strawberry*" ),
                                        vegetable = c("vegetable*"),
                                        latex = c("latex*"),
                                        carrageenan = c("carrageenan*", "seaweed*", "sea weed*"),
                                        coconut = c("coconut*"),
                                        chestnut = c("chestnut*", "chest nut*"),
                                        pinenut = c("pinenut*", "pine nut*"),
                                        fungus = c("fungus*", "quorn*", "yeast*", "mushroom*"),
                                        kiwi = c("kiwi*"),
                                        peas = c("peas*"),
                                        chickpeas = c("chickpeas*", "chick peas*"),
                                        poppy_seeds = c("poppy seed*", "poppyseed*"),
                                        pepper = c("pepper*", "bell pepper*"),
                                        pumpkin = c("pumpkin*", "squash*", "punkin*"),
                                        kidney_beans = c("kidney bean*", "kidneybean*"),
                                        garlic = c("garlic*"),
                                        banana = c("banana*"),
                                        lentils = c("lentil*"),
                                        buckwheat = c("buckwheat*"),
                                        edible_insects = c("edible instect*", "edible bug*", "cricket*", "grasshopper*", "mealworm*",  "buffalo worm*", "buffaloworm*"))
)
