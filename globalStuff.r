rm(list = ls())
options(
        rlib_downstream_check = FALSE,
        lib="glyphicon")

if("ephys.WSI" %in%  .packages()) detach("package:ephys.WSI", unload = TRUE)



####Load the most needed packages to run####

library(plotly)
library(shinyBS)
library(shinyalert)
library(shinybusy)
library(shinycustomloader)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(shiny)
library(rstudioapi)
library(tryCatchLog)
library(esquisse)
library(scales)
library(gtools)
library(ephys.WSI)
library(dplyr)
library(tidyverse)
library(stringi)
library(vroom)
library(openxlsx)
# library(gWidgets2)
library(gWidgets2tcltk)
library(matrixStats)
library(checkmate)
library(outliers)
library(purrrlyr)
library(ggsci)
library(ggthemes)
library(factoextra)
library(ggplot2)
library(patchwork)
library(cluster)
library(factoextra)
library(ggalt)
library(gtools)
library(latex2exp)
library(cowplot)
library(scales)
library(svglite)
library(Cairo)
library(gridExtra)
library(ggpubr)
library(showtext)
library(ephys.WSI)

options("guiToolkit"="tcltk")
guiToolkit("tcltk")

###Envirs###
data_storage_envir <- data_storage_envir
data_envir <- data_envir
settings_envir <- settings_envir
###Envirs###

####Support Packages List####
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
  "showtext",
  "ggpubr")

#####Global Variables#####
isDataImported <- FALSE
colorPalette <- c("#000000", "#0072B2", "#009E73",
                  "#D55E00", "#56B4E9", "#F0E442",
                  "#CC79A7", "#999999", "#E69F00")
colorChoices <- list(Boxplot = colorPalette,
                     Ratioplot = colorPalette,
                     SingleTraces = colorPalette,
                     MatTraces = colorPalette,
                     MedianTraces = colorPalette,
                     PPlot = colorPalette,
                     StitchedPlot = colorPalette)
colorSelected <- list(Boxplot = colorPalette,
                      Ratioplot = colorPalette,
                      SingleTraces = colorPalette,
                      MatTraces = colorPalette,
                      MedianTraces = colorPalette,
                      PPlot = colorPalette,
                      StitchedPlot = colorPalette)

IV_columns <- c(
  "None",
  "CurrentDensity[pA/pF]",
  "smoothed_CurrentDensity",
  "normalized_CurrentDensity",
  "fitted_normalized_CurrentDensity",
  "normalized_slopeConductance"
)


axisChoices <- function(name) {

  if(invalid(name)) return()

  return(switch(
    name,
    "None" = {
      NULL
    },
    "IV" = {
      IV_plot_axes
    },
    "IV no Cross" = {
      IV_plot_axes_wo_AxisCross
    },
    "IV-Analysis" = {
      IV_plot_axes_analysis
    },

    "P-Values" = {
      p_plot_axes
    },
    "P-Plot" = {
      c(IV_plot_axes,
        p_plot_axes)
    },
    "P-Plot-Analysis" = {
      c(IV_plot_axes_analysis,
        p_plot_axes_analysis)
    },
    "Complete-Plot" = {
      c(IV_plot_axes,
        p_plot_axes)
    },
    "Complete-Plot-Analysis" = {
      c(IV_plot_axes_analysis,
        p_plot_axes_analysis)
    },
    {
      input$Used_Axis
    }
  ))
}

themeChoices <- function(name) {
  return(switch(
    name,
    "Boxplot" = {
      theme_chris_boxplot
    },
    "IV" = {
      theme_chris_IV
    },
    "IV-Analysis" = {
      theme_chris_IV_analysis
    },
    "P-Plot" = {
      c(theme_chris_IV, theme_chris_P_values)
    },
    "P-Plot-Analysis" = {
      c(theme_chris_IV_analysis,
        theme_chris_P_values_analysis)
    },
    "Complete-Plot" = {
      c(theme_chris_IV, theme_chris_P_values)
    },
    "Complete-Plot-Analysis" = {
      c(theme_chris_IV_analysis,
        theme_chris_P_values_analysis)
    }
  ))
}



plot_list <- list(Boxplot = NULL,
                  Ratioplot = NULL,
                  SingleTraces = NULL,
                  MatTraces = NULL,
                  MedianTraces = NULL,
                  PPlot = NULL,
                  StitchedPlot = NULL)

plot_list_woLegend <- list(Boxplot = NULL,
                           Ratioplot = NULL,
                           SingleTraces = NULL,
                           MatTraces = NULL,
                           MedianTraces = NULL,
                           PPlot = NULL,
                           StitchedPlot = NULL)

legend_list <- list(Boxplot = NULL,
                    Ratioplot = NULL,
                    SingleTraces = NULL,
                    MatTraces = NULL,
                    MedianTraces = NULL,
                    PPlot = NULL,
                    StitchedPlot = NULL)

dimensions_list <- list(Boxplot = NULL,
                        Ratioplot = NULL,
                        SingleTraces = NULL,
                        MatTraces = NULL,
                        MedianTraces = NULL,
                        PPlot = NULL,
                        StitchedPlot = NULL)



nameGenListNouns <-
  c(
    "Soup",
    "King",
    "Lip",
    "Activity",
    "Answer",
    "Test",
    "Zipper",
    "Theory",
    "Ladybug",
    "Initiative",
    "Lady",
    "Beer",
    "Cast",
    "Table",
    "Calculator",
    "Detail",
    "Studio",
    "Throne",
    "Guitar",
    "Glass",
    "Animal"
  )

nameGenListAdj <-
  c(
    "Fanatical",
    "Incredible",
    "Acidic",
    "Obscene",
    "Sneaky",
    "Spectacular",
    "Nebulous",
    "Heady",
    "Legal",
    "Itchy",
    "Productive",
    "Plant",
    "Pastoral",
    "Flagrant",
    "Unsuitable",
    "Absurd",
    "Jaded",
    "Unkempt",
    "Embarrassed",
    "Hanging",
    "Breezy"
  )


showtext_auto()
font_data <- font_files()
Fonts <- c()
# Loop through the data and add the "Regular" fonts
for (i in seq_len(nrow(font_data))) {
  if (font_data$face[i] == "Regular") {
    font_add(font_data$family[i], regular = font_data$file[i])
    Fonts <- c(Fonts, font_data$family[i])
  }
}

if ("Arial" %in% Fonts) {
  # Move "Arial" to the first element
  Fonts <- c("Arial", Fonts[Fonts != "Arial"])
}
