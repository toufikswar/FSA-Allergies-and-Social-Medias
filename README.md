# Allergater

**Allergater** is a pipeline that takes data tables (`.xlsx`) pulled from Pulsar and can output a report (in either pdf or gitbook html format), a series of static plots, as well as an interactive shiny dashboard. This pipeline is currently only tested to work on linux or macOS.

To use **Allergater**, download or clone the repo to a folder on your computer, then create a folder `/Data` where pulsar `.xlsx` data can be placed. **Important:** if using multiple excel files, it is important that only the first file has header information in the table.

**Allergator** analysis can be modularized by selecting the desired outputs in config files, which enables separation of project and analysis.  An example config file can be seen in the `/config_files` directory.

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
* [ggmap](https://github.com/dkahle/ggmap)

**Prior to running the Pipeline install:**

```r
install.packages(c(
  "readxl","stringi","quanteda","magrittr",
  "tidyr","ggplot2","ggpubr","bookdown","tinytex",
  "shiny","leaflet","sp","rgdal"))
```

and

The latest ggmap must be installed from github:

```r
if(!requireNamespace("devtools"))
install.packages("devtools")
devtools::install_github("dkahle/ggmap", ref = "tidyup")
```

And configured for use with a valid Google Maps API key: https://developers.google.com/maps/ . Add your valid API key to the config file under 'API_KEY'.

For PDF Generation (TinyTex), run the command in the R console:
```r
tinytex::install_tinytex()
```

## Outputs

Output files include the report, the .RData, and static plots, and can be found in `output_files/$projectID/`
