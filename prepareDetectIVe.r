options(repos = c(CRAN = "https://mran.revolutionanalytics.com/snapshot/2022-04-07"),
        shiny.launch.browser = .rs.invokeShinyWindowExternal,
        rlib_downstream_check = FALSE,
        lib="glyphicon")


install.packages("checkpoint")
install.packages("vctrs")
install.packages("shiny")


####Packages on local drive (fast)####
library(checkpoint)
checkpoint("2022-04-07", scanForPackages = F,scan_now = F,
           checkpoint_location = "C:/R_Checkpoint")




GUIPackages <-
  c(
    "plotly",
    "shinyBS",
    "shinyalert",
    "shinybusy",
    "shinycustomloader",
    "shinyWidgets",
    "shinydashboard",
    "shinydashboardPlus",
    "shinyjs",
    "shiny",
    "rstudioapi",
    "tryCatchLog",
    "esquisse",
    "scales",
    "gtools"
  )

supportPackages <- c(
  "dplyr",
  "tidyverse",
  "stringi",
  "vroom",
  "openxlsx",
  "gWidgets2",
  "matrixStats",
  "checkmate",
  "outliers",
  "purrrlyr",
  "ggsci",
  "ggthemes",
  "factoextra",
  "ggplot2",
  "patchwork",
  "cluster",
  "factoextra",
  "ggalt",
  "gtools",
  "latex2exp",
  "cowplot",
  "scales",
  "svglite",
  "Cairo",
  "gridExtra",
  "gWidgets2tcltk",
  "ggpubr")


sapply(GUIPackages, function(x) {if(!require(x,character.only = T)) install.packages(x)})
sapply(supportPackages, function(x) {if(!require(x,character.only = T)) install.packages(x)})



####Newest release####
VersionEphysWSI = '2.1.7'
####Newest release####
path <- rstudioapi::getSourceEditorContext()$path
path <- paste(strsplit(path,"/")[[1]][-length(strsplit(path,"/")[[1]])], collapse = "/")

if(!require(ephys.WSI)) install.packages(
  paste0(path,"/","ephys.WSI_",VersionEphysWSI,".zip"),
  repos = NULL,
  type = "win.binary"
)
