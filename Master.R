### Master Script for Finding Public Mentions of Allergens in Social Media Data

source("utils.R")

allergies_and_social_media <- function(config_file)
{

  # Data structure with all the configuration variables
  configuration = list(input_dir               = "",
                       input_file_list         = vector(),
                       sheet_name              = "",
                       project_name            = "",
                       rerun_data_processing   = FALSE,
                       do_static_plots         = FALSE,
                       output_report           = FALSE,
                       launch_shiny_dash_board = FALSE)

  # read the configuration file
  configuration <- read_config_file(config_file,configuration)

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

  # # running data loading and pre-processing
  # pre_processing(configuration$input_dir,
  #                configuration$input_file_list,
  #                configuration$sheet_name,
  #                configuration$preprocess_output_img)
  #
  # # running data labeling
  # data_labeling(configuration$preprocess_output_img,
  #               configuration$labeling_output_img)
  #
  # if(configuration$do_static_plots) {
  #   static_plotting(configuration$labeling_output_img)
  # }

}

config_file <- "input_config_file.txt"
allergies_and_social_media(config_file)

#
