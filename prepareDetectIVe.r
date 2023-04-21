# Unneeded in Rinno Fork
# install.packages("vctrs")
# install.packages("shiny")
# install.packages("rstudioapi")
#
#
#
# GUIPackages <-
#   c(
#     "plotly",
#     "shinyBS",
#     "shinyalert",
#     "shinybusy",
#     "shinycustomloader",
#     "shinyWidgets",
#     "shinydashboard",
#     "shinydashboardPlus",
#     "shinyjs",
#     "shiny",
#     "rstudioapi",
#     "tryCatchLog",
#     "esquisse",
#     "scales",
#     "gtools"
#   )
#
# install.packages("dplyr")
# install.packages("tidyverse")
# install.packages("stringi")
# install.packages("vroom")
# install.packages("openxlsx")
# install.packages("gWidgets2")
# install.packages("matrixStats")
# install.packages("checkmate")
# install.packages("outliers")
# install.packages("purrrlyr")
# install.packages("ggsci")
# install.packages("ggthemes")
# install.packages("factoextra")
# install.packages("ggplot2")
# install.packages("patchwork")
# install.packages("cluster")
# install.packages("factoextra")
# install.packages("ggalt")
# install.packages("gtools")
# install.packages("latex2exp")
# install.packages("cowplot")
# install.packages("scales")
# install.packages("svglite")
# install.packages("Cairo")
# install.packages("gridExtra")
# install.packages("gWidgets2tcltk")
# install.packages("ggpubr")
#
#
# supportPackages <- c(
#   "dplyr",
#   "tidyverse",
#   "stringi",
#   "vroom",
#   "openxlsx",
#   "gWidgets2",
#   "matrixStats",
#   "checkmate",
#   "outliers",
#   "purrrlyr",
#   "ggsci",
#   "ggthemes",
#   "factoextra",
#   "ggplot2",
#   "patchwork",
#   "cluster",
#   "factoextra",
#   "ggalt",
#   "gtools",
#   "latex2exp",
#   "cowplot",
#   "scales",
#   "svglite",
#   "Cairo",
#   "gridExtra",
#   "gWidgets2tcltk",
#   "ggpubr")
#
#
# sapply(GUIPackages, function(x) {library(x)})
# sapply(supportPackages, function(x) {library(x)})
#
#
#
#
#
# ####Newest release####
# VersionEphysWSI = '2.1.8'
# ####Newest release####
# path <- rstudioapi::getSourceEditorContext()$path
# path <- paste(strsplit(path,"/")[[1]][-length(strsplit(path,"/")[[1]])], collapse = "/")
#
# if(!require(ephys.WSI)) install.packages(
#   paste0(path,"/","ephys.WSI_",VersionEphysWSI,".zip"),
#   repos = NULL,
#   type = "win.binary"
# )
