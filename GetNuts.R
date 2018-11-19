### Master Script for Finding Public Mentions of Allergens in Social Media Data

source("scripts/utils.R")

cat("\n")
cat("\n")
cat("\n")
cat("       ################################ GetNuts 1.0 ##################################")
cat("\n")

cat(c("       ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,",
"       ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,",
"       ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,",
"       ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,",
"       ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,***/((#########((/**,,,,,,,,,,,,,,,,,,",
"       ,,,,,,#####((((###################((((//**,,.           .,,/(########(/*,,,,,,,",
"       ,,,,,,##                                                           .*/###*,,,,,",
"       ,,,,,,/#.        ,*//////*//*/*                                        (#,,,,,,",
"       ,,,,,,,#/     *///////////*/*////*        .....               //**/     ,#*,,,,",
"       ,,,,,,,##   ./////*/((/*,/(///(/.     ,*//////////*          /////(.    (#,,,,,",
"       ,,,,,,,(#   /////*(,                 //*////((///////*      /////       ##,,,,,",
"       ,,,,,,,(#. ,///////    *////////// ////////*   *//////*(///////////* .#(,,,,,,,",
"       ,,,,,,,/#, ./*/////    ./////**/// ////////**///*////* (//////(((/(  ,#/,,,,,,,",
"       ,,,,,,,/#,  ///////,    ,.../*/*// ,*//*//     .,       ,//////(      /#*,,,,,,,",
"       ,,,,,,,/#*  .///////***,***//////   *//////*///*/////  //////(,     .(#,,,,,,,,",
"       ,,,,,,,*#/    ////////*//*/////      ,(//////////    ,*/////(      *#(,,,,,,,,,",
"       ,,,,,,,,#(.     .*((((((((((*              ..,...     .*(((/(,      ##*,,,,,,,,",
"       ,,,,,,,,##*            .*((####################((*,.        .      .##,,,,,,,,,",
"       ,,,,,,,,*##    .,(############################%%%%%%#####/,        (#/,,,,,,,,,",
"       ,,,,,,,,,##,,########%#######################%,    /%########(,   .#(,,,,,,,,,,",
"       ,,,,,,,,,(###%%%%%#.    *%%#%%%%%%%###%%%%%%%&.    %#####%%%%%%%#((#//,,,,,,,,,",
"       ,,,,,,,(###%              /%%     %##%/   /       ,&(       #%###(//*,,,,,,,,,,",
"       ,,,,,,(####%,     .(#,     %%     #%*/     //.        *.       ,%######//,,,,,,",
"       ,,,,,*#####%#     #%,     (%      #%*/     /%,    (%%#     */(%%%######/*,,,,,,",
"       ,,,,,*######%.    #%#%     .%     #%*/     (%     %##%*         *%#####(,,,,,,,",
"       ,,,,,*(#####%/    ,%##      #     %%%%.    (%     ##%%%#(*     %#####*,,,,,,,,,",
"       ,,,,,,*/#####%     %##%*  .,(*            .%/    ,%##%*   .     ,%####*,,,,,,,,",
"       ,,,,,,,*/(###%,   .#%##%%%###%%.        .#%%/.   (%#%#        .#%###*,,,,,,,,,,",
"       ,,,,,,,,,,*//#%%#################%%%%%%%#########%####%%&%%&%%###(,,,,,,,,,,,,,",
"       ,,,,,,,,,,,,,*///(###########################################/,,,,,,,,,,,,,,,,,",
"       ,,,,,,,,,,,,,,,,,*/////(###############################(/*,,,,,,,,,,,,,,,,,,,,,",
"       ,,,,,,,,,,,,,,,,,,,,,,,,,***///////////(((/////**,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,",
"       ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,",
"       ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,",
"       ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,",
"       ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,",
"       ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,"
), sep = "\n")
cat("       ###############################################################################")
cat("\n")
cat("       Authors:\n")
cat("         A. Newman\n")
cat("         A. Perez Perez\n")
cat("         O. Pervane\n")
cat("         T. Swar\n")
cat("       ###############################################################################")
cat("\n")
cat("\n")
cat("\n")


# Detect operating system
OS <- as.character(Sys.info()["sysname"])
if(OS == "Linux") {
  cat(paste("\n","It looks like you are running Linux","\n"))
} else if(OS == "Darwin") {
  cat(paste("\n","It looks like you are running macOS","\n"))
} else if(OS == "Windows") {
  cat(paste("\n","It looks like you are running Windows","\n"))
}

# Data structure with all the configuration variables
configuration = list(input_dir                = "",
                     input_file_list          = vector(),
                     sheet_name               = "",
                     project_name             = "",
                     static_plot_format       = "png",
                     rerun_data_preprocessing = FALSE,
                     rerun_data_labelling     = FALSE,
                     do_static_plots          = FALSE,
                     launch_shiny_dash_board  = FALSE)

output_dir <- "output_files"
# If output_dir doesn't exist, create it.
if(!dir.exists(file.path(output_dir))) dir.create(file.path(output_dir))

plots_output_dir <- ""

possible_static_plots_formats <- c("eps","ps","pdf","jpeg","tif","png","bmp","svg")

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
    stop("Input file list has no elements.
         \n  You must specify one or more files (separated by white spaces) in the config file after 'input_file_list:'.")
  } else if(configuration$sheet_name == "") {
    stop("Sheet name not specified.
         \n  Please specify the sheet name of the excel data file under 'sheet_name:' in the config gile")
  } else if(configuration$project_name == "") {
    stop("Project name not specified.
         \n  Please specify a project name under 'project_name:' in the config file.")
  }

  # check if static plots format is one of the predefined formats
  if(configuration$do_static_plots & !(configuration$static_plot_format %in% possible_static_plots_formats)) {
    all_formats <- paste(possible_static_plots_formats,collapse = ", ")
    stop(paste("Specified static plot output filetype ", configuration$static_plot_format,
               " is not a supported filetype.  (Plot output filetype can be one of ",all_formats,").",sep=""))
  }

  print_config(configuration)

  # Create the output directory. Use deparse and assign functions to change the configuration global variable value
  output_dir_tmp <- deparse(substitute(output_dir))
  output_dir     <- paste(output_dir,"/",configuration$project_name,"/",sep="")
  assign(output_dir_tmp,output_dir,pos=parent.frame())
  if(!dir.exists(file.path(output_dir))) dir.create(output_dir,showWarnings = FALSE)

  # Create the plots output directory. Use deparse and assign functions to change the configuration global variable value
  plots_output_dir_tmp <- deparse(substitute(plots_output_dir))
  plots_output_dir     <- paste(output_dir,"plots/",sep="")
  assign(plots_output_dir_tmp,plots_output_dir,pos=parent.frame())
  if(!dir.exists(file.path(plots_output_dir))) dir.create(plots_output_dir,showWarnings = FALSE)

  # Set preprocessing image file name. Use deparse and assign functions to change the configuration global variable value
  image_preprocessing_tmp <- deparse(substitute(image_preprocessing))
  image_preprocessing     <- paste(output_dir,"Preprocessing.RData",sep="")
  assign(image_preprocessing_tmp,image_preprocessing,pos=parent.frame())
  # Set analysis image file name. Use deparse and assign functions to change the configuration global variable value
  image_analysis_tmp <- deparse(substitute(image_analysis))
  image_analysis     <- paste(output_dir,"Waterfall.RData",sep="")
  assign(image_analysis_tmp,image_analysis,pos=parent.frame())

  # data pre-processing
  file_exists_preprocessing <- file.exists(image_preprocessing) # checks if preprocessing image file exists
  generated_labelling       <- FALSE
  if(!file_exists_preprocessing | configuration$rerun_data_preprocessing) {
    # if output image for preprocessing don't exits or the user request rerun_data_preprocessing, then run preprocessing + labelling
    if(!file_exists_preprocessing) {
      cat("\n")
      cat(paste("Preprocessing .RData image file ",image_preprocessing," doesn't exist. Running preprocessing and labelling to generate it.","\n",sep=""))
      cat("\n")
    } else if(file_exists_preprocessing & configuration$rerun_data_preprocessing) {
      cat("\n")
      cat(paste("Preprocessing image file ",image_preprocessing," exists, but rerun_data_processing is set to 'yes'. Re-running preprocessing and labelling.","\n",sep=""))
      cat("\n")
    }

    source("scripts/Preprocessing.R")  # Data pre-processing
    source("scripts/Waterfall.R")      # Data labelling
    generated_labelling <- TRUE
  } else {
    cat("\n")
    cat(paste("Preprocessing .RData image file ",image_preprocessing," exists and rerun_data_preprocessing is set to 'no'. Passing to next pipe-line step.","\n",sep=""))
    cat("\n")
  }

  file_exists_analysis <- file.exists(image_analysis) # checks if analysis image file exists
  if(!file_exists_analysis | (configuration$rerun_data_labelling & !generated_labelling)) {
    # if output image for labelling don't exits or the user request rerun_data_labelling, then run labelling only
    if(!file_exists_analysis) {
      cat("\n")
      cat(paste("Labelling .RData image file ",image_analysis," doesn't exist. Running labelling to generate it.","\n",sep=""))
      cat("\n")
    } else if(file_exists_analysis & (configuration$rerun_data_labelling & !generated_labelling)) {
      cat("\n")
      cat(paste("Labelling .RData image file ",image_analysis," exists, but rerun_data_labelling is set to 'yes'. Re-running labelling.","\n",sep=""))
      cat("\n")
    }

    source("scripts/Waterfall.R")      # Data labelling
  } else if(!generated_labelling) {
    cat("\n")
    cat(paste("Labelling .RData image file ",image_analysis," exists and rerun_data_labelling is set to 'no'. Passing to next pipe-line step.","\n",sep=""))
    cat("\n")
  }

  # set the format of static plots
  output_format <<- configuration$static_plot_format

  # Static plotting
  if(configuration$do_static_plots) {
    # Producing static plots

    source("scripts/plots.R")
    source("scripts/Static_Geoplots.R")
  }

  if(configuration$launch_shiny_dash_board) {
    # Launching shiny dashboard
    # source("scripts/Geoplotting_private.R")
    # source("scripts/Shiny_dashboard.R")
    library(shiny)
    runApp("Dashboard")
  }

}

# IPORTANT NOTE: this part of the code not final.
# This is a stupid way of running the Master.R code using the config file
config_file <- "config_files/config_Data_set_1.txt"
# config_file <- "config_files/config_Data_set_2.txt"
allergies_and_social_media(config_file)


#
