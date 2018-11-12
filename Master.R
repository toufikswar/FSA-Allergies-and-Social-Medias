### Master Script for Finding Public Mentions of Allergens in Social Media Data

source("utils.R")

# Data structure with all the configuration variables
configuration = list(input_dir               = "",
                     input_file_list         = vector(),
                     sheet_name              = "",
                     project_name            = "",
                     rerun_data_processing   = FALSE,
                     do_static_plots         = FALSE,
                     output_report           = FALSE,
                     launch_shiny_dash_board = FALSE)

output_dir <- "output_files/"
image_preprocessing <- ""
image_analysis      <- ""

allergies_and_social_media <- function(config_file)
{

  # Read the configuration file. Use deparse and assign functions to change the configuration global variable value
  configuration_tmp <- deparse(substitute(configuration))
  configuration     <- read_config_file(config_file,configuration)
  assign(configuration_tmp,configuration,pos=parent.frame())

  # Check for reasonable inputs
  if(length(configuration$input_file_list) == 0) {
    stop("Input file list has no elements.\n  Need to specify it with the flag input_file_list: followed with a list of file names separated by white-spaces.")
  } else if(configuration$sheet_name == "") {
    stop("Sheed name not specified.\n  Need to specify it with the flag sheet_name: followed with the sheet name.")
  } else if(configuration$project_name == "") {
    stop("Project name not specified.\n  Need to specify it with the flag project_name: followed with the project name.")
  } else if(configuration$output_report & !configuration$do_static_plots) {
    stop("Produce output report is set to yes, but do static plots is set to no.\n  If produce output report is set to yes then do static plots has to be set to yes as well.")
  }

  print_config(configuration)

  # Create the output directory. Use deparse and assign functions to change the configuration global variable value
  output_dir_tmp <- deparse(substitute(output_dir))
  output_dir     <- paste(output_dir,"/",configuration$project_name,"/",sep="")
  assign(output_dir_tmp,output_dir,pos=parent.frame())
  dir.create(output_dir,showWarnings = FALSE)

  # Set preprocessing image file name. Use deparse and assign functions to change the configuration global variable value
  image_preprocessing_tmp <- deparse(substitute(image_preprocessing))
  image_preprocessing     <- paste(output_dir,"/Preprocessing.RData",sep="")
  assign(image_preprocessing_tmp,image_preprocessing,pos=parent.frame())
  # Set analysis image file name. Use deparse and assign functions to change the configuration global variable value
  image_analysis_tmp <- deparse(substitute(image_analysis))
  image_analysis     <- paste(output_dir,"/Waterfall.RData",sep="")
  assign(image_analysis_tmp,image_analysis,pos=parent.frame())

  # data pre-processing and lebelling step
  file_exists_preprocessing <- file.exists(image_preprocessing) # checks if preprocessing image file exists
  file_exists_analysis      <- file.exists(image_analysis)      # checks if analysis      image file exists
  file_exists               <- file_exists_preprocessing & file_exists_analysis
  if(!file_exists | configuration$rerun_data_processing) {
    # if either of the output image files (preprocessing analysis analysis) don't exits or the user request rerun_data_processing, then run preprocessing + labelling
    if(!file_exists) {
      cat("\n\n")
      cat(paste("Preprocessing image file ",image_preprocessing," doesn't exists. Running preprocessing and labelling to generate it.","\n",sep=""))
      cat("\n\n")
    } else if(file_exists & configuration$rerun_data_processing) {
      cat("\n\n")
      cat(paste("Preprocessing image file ",image_preprocessing," exists, but user specified rerun_data_processing = yes. Re-running preprocessing and labelling.","\n",sep=""))
      cat("\n\n")
    }

    source("Preprocessing.R")  # Data pre-processing
    source("Waterfall.R")      # Data labelling
  } else {
    cat("\n\n")
    cat(paste("Preprocessing image file ",image_preprocessing," exists and user specified rerun_data_processing = no. Passing to next pipe-line step.","\n",sep=""))
    cat("\n\n")
  }

  # IPORTANT NOTE: this part of the code not final
  # Here we should put some code for plots and stuff
  # if(configuration$do_static_plots) {
  #   source("plots.R")
  # }
  #
  # if(configuration$output_report) {
  #   # some stuff
  # }
  # if(configuration$launch_shiny_dash_board) {
  #   # some stuff
  # }

}

# IPORTANT NOTE: this part of the code not final.
# This is a stupid way of running the Master.R code using the config file
config_file <- "config_files/config_Data_set_1.txt"
# config_file <- "config_files/config_Data_set_2.txt"
allergies_and_social_media(config_file)



#
