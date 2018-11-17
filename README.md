# GetNuts

**GetNuts** is a pipeline that takes data tables (`.xlsx`) pulled from Pulsar and can output a report (in either pdf or gitbook html format), a series of static plots, as well as an interactive shiny dashboard. This pipeline is tested to work on Windows, macOS and Linux.

To use **GetNuts**, download or clone the repo to a folder on your computer, then create a folder `/Data` where pulsar `.xlsx` data can be placed. **Important:** if using multiple excel files, it is important that only the first file has header information in the table.

**GetNuts** analysis can be modularized by selecting the desired outputs in config files, which enables separation of project and analysis.  An example config file can be seen in the `/config_files` directory.

## Installation

**Dependencies:**

* [readxl](https://github.com/tidyverse/readxl)
* [quanteda](https://github.com/quanteda/quanteda)
* [magrittr](https://github.com/tidyverse/magrittr)
* [tidyr](https://cran.r-project.org/web/packages/tidyr/index.html)
* [ggplot2](https://ggplot2.tidyverse.org/)
* [ggpubr](http://www.sthda.com/english/rpkgs/ggpubr/)
* [bookdown](https://github.com/rstudio/bookdown)
* [tinytex](https://yihui.name/tinytex/)
* [leaflet](https://rstudio.github.io/leaflet/)
* [rgdal](https://cran.r-project.org/web/packages/rgdal/index.html)
* [sp](https://cran.r-project.org/web/packages/sp/index.html)
* [spdplyr](https://cran.r-project.org/web/packages/spdplyr/index.html)
* [ggmap](https://github.com/dkahle/ggmap)

**Prior to running the Pipeline install:**

```r
install.packages(c(
  "readxl","stringi","quanteda","magrittr",
  "tidyr","ggplot2","ggpubr","bookdown","tinytex",
  "shiny","leaflet","sp","rgdal","spdplyr"))
```

and

The latest ggmap must be installed from github:

```r
if(!requireNamespace("devtools"))
install.packages("devtools")
devtools::install_github("dkahle/ggmap", ref = "tidyup")
```

And configured for use with a valid Google Maps API key: https://developers.google.com/maps/ . Add your valid API key to the config file under 'API_KEY'.


## Configuration

To analyze data, create a folder `/Data` in the repo parent folder and place Pulsar `.xlsx` data there.

To perform an analysis, the user must specify a `config.txt` file to run with GetNuts. Config files are stored in `/config_files` and specify parameters of the analysis to be performed. These include:

* `input_file_dir:` where the data has been stored (Default is `Data`)
* `input_file_list:` The files in the `input_file_dir` to be analyzed. **Important:** the first file should have the header information, all files after should not have header information
* `sheet_name:` sheet of the excel file to take the data from (Default is `Sheet1`)
* `project_name:` Name given to analysis. This will create a folder of this name in `/output_files/` where all output from this analysis will be found.
* `rerun_data_preprocessing:` ("yes" or "no") Data Preprocessing, which involves stopword removal and stemming is the most computationally expensive process in the pipeline. If set to "yes", pipeline will be re-run from the beginning. (Default: "no")
* `rerun_data_labelling:` ("yes" or "no") Data labelling refers to when the preprocessed data is checked against the supplied dictionaries (in `/Dictionaries`). If a change is made to a dictionary, the analysis does not need to be re-run from the beginning, it can begin here if set to "yes". (Default: no)

For the outputs, the user can also specify in the config file what is desired and in which format.
* `do_static_plots:` Output static plots to `/output_files/plots` What are the desired outputs (Report, Dashboard or Static plots)
* Which format the Static plots are to be in

output files include the report, the .RData, and static plots, and can be found in `output_files/$projectID/`
