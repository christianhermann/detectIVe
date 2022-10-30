#### Factories and GUI Elements####
solidHeaderBoxFactory <-
  function(color, ...) {
    return(function(...) {
      box(
        ...,
        status = color,
        collapsible = TRUE,
        solidHeader = TRUE
      )
    })
  }
solidHeaderBoxes <-
  lapply(
    c(
      Blue = "primary",
      Green = "success",
      LightBlue = "info",
      Orange = "warning",
      red = "danger"
    ),
    solidHeaderBoxFactory
  )

leftLabeledFactory <- function(widget, ...) {
  return(function(...) {
    tags$div(id = "inline", widget(...))
  })
}

leftLabeldWidgets <- lapply(
  c(
    numericInput = numericInput,
    textInput = textInput,
    selectInput = selectInput
  ),
  leftLabeledFactory
)


saveBoxFactory <-
  function(name) {
    return(
      function(...) {
        solidHeaderBoxes$LightBlue(
          title = "Save Plot",
          width = 12,
          fluidRow(
            column(
              width = 7,
              textInput(paste0(name, "saveName"), "Filename", placeholder = "Toller Bildername")
            ),
            column(
              width = 5,
              selectizeInput(
                paste0(name, "Filetype"),
                "Filetype",
                choices = list("png", "tiff", "svg", "pdf", "emf")
              )
            )
          ),
          fluidRow(
            column(
              width = 4,
              numericInput(
                paste0(name, "Save_width"),
                "Width",
                value = 80,
                min = 0,
                max = 10000,
                step = 0.1
              )
            ),
            column(
              width = 4,
              numericInput(
                paste0(name, "Save_height"),
                "Height",
                value = 80,
                min = 0,
                max = 10000,
                step = 0.1
              )
            ),
            column(
              width = 4,
              div(
                style = "margin-top: 25px;",
                actionButton(paste0(name, "Save_plot"), "Save Plot", icon = icon("download", lib="glyphicon"))
              )
            )
          ),
          ...
        )
      }
    )
  }

saveBox <- lapply(
  c(
    Boxplot = "Boxplot_",
    Ratioplot = "Ratioplot_",
    SingleTraces = "SingleTraces_",
    MatTraces = "MatTraces_",
    MedianTraces = "MedianTraces_",
    PPlot = "PPlot_",
    StitchedPlot = "StitchedPlot_"
  ),
  saveBoxFactory
)

colorBoxFactory <- function(name) {
  return(
    function(...) {
      solidHeaderBoxes$LightBlue(
        title = "Colors",
        width = 12,
        fluidPage(fluidRow(
          column(
            width = 6,
            selectizeInput(
              inputId = paste0(name, "colors"),
              "Plot Colors",
              choices = colorChoices,
              selected = colorSelected,
              multiple = T,
              options = list(create = TRUE)
            )
          ),
          column(
            width = 6,
            colorPicker(
              inputId = paste0(name, "colorPicker"),
              label = "Color List:",
              choices = list(
                "Standard" = c(
                  "#000000",
                  "#56B4E9",
                  "#009E73",
                  "#F0E442",
                  "#0072B2",
                  "#D55E00",
                  "#CC79A7",
                  "#999999",
                  "#E69F00"
                ),
                "Blues" = brewer_pal(palette = "Blues")(9),
                "Reds" = brewer_pal(palette = "Reds")(9),
                "Greens" = brewer_pal(palette = "Greens")(9),
                "Greys" = brewer_pal(palette = "Greys")(9),
                "Purples" = brewer_pal(palette = "Purples")(9),
                "Reds" = brewer_pal(palette = "Reds")(9),
                "Yellow Orange" = brewer_pal(palette = "YlOrBr")(9)
              )
            )
          )
        )),
        ...
      )
    }
  )
}
colorBox <- lapply(
  c(
    Boxplot = "Boxplot_",
    Ratioplot = "Ratioplot_",
    SingleTraces = "SingleTraces_",
    MatTraces = "MatTraces_",
    MedianTraces = "MedianTraces_",
    PPlot = "PPlot_",
    StitchedPlot = "StitchedPlot_"
  ),
  colorBoxFactory
)


seriesBoxFactory <-
  function(name) {
    return(
      function(...) {
        solidHeaderBoxes$LightBlue(
          title = "Data",
          width = 12,
          fluidPage(
            fluidRow(
              selectizeInput(
                paste0(name, "Series"),
                "Series",
                choices = list(),
                multiple = TRUE
              )
            ),
            ...,
            fluidRow(selectizeInput(
              paste0(name, "SummaryOutlier"),
              "Outlier Summary",
              choices = c("None", "Outlier"),
              "None"
            )),
            fluidRow(
              selectizeInput(paste0(name, "IVOutlier"),
                             "Outlier IV",
                             choices = IV_columns,
                             "None"
              )
            )
          )
        )
      }
    )
  }

seriesBox <- lapply(
  c(
    Boxplot = "Boxplot_",
    Ratioplot = "Ratioplot_",
    SingleTraces = "SingleTraces_",
    MatTraces = "MatTraces_",
    MedianTraces = "MedianTraces_",
    PPlot = "PPlot_",
    StitchedPlot = "StitchedPlot_"
  ),
  seriesBoxFactory
)

updatePlotBoxFactory <-
  function(name) {
    return(
      function(...) {
        solidHeaderBoxes$LightBlue(
          title = "Plot Updates",
          width = 12,
          fluidPage(fluidRow(
            checkboxInput(
              paste0(name, "PreviewPlot"),
              label = "Preview Plot",
              value = FALSE
            ),
            div(
              style = "margin-top: 25px;",
              actionButton(paste0(name, "UpdatePlot"), "Update Plot", icon = icon("refresh", lib="glyphicon"))
            )
          ))
        )
      }
    )
  }

updatePlotBox <- lapply(
  c(
    Boxplot = "Boxplot_",
    Ratioplot = "Ratioplot_",
    SingleTraces = "SingleTraces_",
    MatTraces = "MatTraces_",
    MedianTraces = "MedianTraces_",
    PPlot = "PPlot_",
    StitchedPlot = "StitchedPlot_"
  ),
  updatePlotBoxFactory
)

plotPreviewFactory <-
  function(name) {
    return(
      function(...) {
        solidHeaderBoxes$LightBlue(
          style='overflow-x: scroll;height:500px;overflow-y: scroll;',
          title = "Preview",
          width = 12,
          fluidPage(fluidRow(
            imageOutput(paste0(name, "PreviewPlot"),height = "500px"),
          ))
        )
      }
    )
  }

plotPreviewBox <- lapply(
  c(
    Boxplot = "Boxplot_",
    Ratioplot = "Ratioplot_",
    SingleTraces = "SingleTraces_",
    MatTraces = "MatTraces_",
    MedianTraces = "MedianTraces_",
    PPlot = "PPlot_",
    StitchedPlot = "StitchedPlot_"
  ),
  plotPreviewFactory
)

plotModalsFactory <- function(name) {
  return(
    function(...) {
      fluidRow(
        bsModal(paste0(name, "Modal"), "Plot", paste0(name, "UpdatePlot"),
                size = "large",
                actionButton(paste0(name, "Plotly"), "Interactive"),
                actionButton(paste0(name, "Modal_save_Plot"), "Quick Save Plot"),
                actionButton(paste0(name, "Modal_save_Plot_R"), "Save Plot as .rData"),
                imageOutput(paste0(name, "Plot")),
                tags$head(tags$style(".modal-body {height: 700px;overflow-y: auto;};"))
        ),
        bsModal(paste0(name, "Plotly_Modal"), "Plotly", paste0(name, "Plotly"),
                size = "large",
                fluidRow(plotlyOutput(paste0(name, "Plotly_Plot")))
        )
      )
    }
  )
}

plotModals <- lapply(
  c(
    Boxplot = "Boxplot_",
    Ratioplot = "Ratioplot_",
    SingleTraces = "SingleTraces_",
    MatTraces = "MatTraces_",
    MedianTraces = "MedianTraces_",
    PPlot = "PPlot_",
    StitchedPlot = "StitchedPlot_"
  ),
  plotModalsFactory
)



traceLabelsFactory <-
  function(names) {
    return(
      function(...) {
        selectizeInput(
          paste0("Names_", names[1]),
          paste0(names[2]," Labels"),
          choices = c(),
          multiple = T,
          options = list(create = T)
        )
      }
    )
  }

traceLabels <- lapply(
  list(
    Boxplot = list("Boxplot", "Box"),
    Ratioplot = list("Ratioplot", "Box"),
    SingleTraces = list("SingleTraces", "Trace"),
    MatTraces = list("MatTraces", "Trace"),
    MedianTraces = list("MedianTraces", "Trace"),
    PPlot = list("PPlot", "Trace"),
    StitchedPlot = list("StitchedPlot", "Trace")
  ),
  traceLabelsFactory
)
#
# checkboxTestsFactory <-
#   function(inputs) {
#     return(
#       function(...) {
#         verticalLayout(
#           checkboxInput(
#             paste0("Paired_SigTest_",inputs),
#             "Wilcoxon matched pairs signed-rank test",
#             value = F
#           ),
#           if(!is.null(inputs[2])){
#           checkboxInput(paste0("Paired_SigTest_", inputs[1], "_Intern"), "Wilcoxon matched pairs internal paired", value = F)},
#           checkboxInput(paste0("Kruskal_Wallis_", inputs[1]), "Kruskal Wallis test", value = F)
#         )
#       }
#     )
#   }

checkboxTestsFactory <-
  function(inputs) {
    return(
      function(...) {
        verticalLayout(
        div(style = "margin-top: -10px; margin-bottom: 15px; font-size:15px; text-align:center", HTML("<strong> Tests </strong>")),
        if(!is.na(inputs[2])){
          radioButtons(
            paste0("Significance_", inputs[1]),
            label = NULL,
            choiceNames =  list(
              "Mann-Whitney U",
              "Wilcoxon matched pairs signed-rank test",
              "Wilcoxon matched pairs internal paired",
              "Kruskal Wallis test"
            ),
            choiceValues = list(
              "Mann-Whitney U",
              "Paired_SigTest",
              "Paired_SigTest_Intern",
              "Kruskal_Wallis"
            )
          )
        }else{
          radioButtons(
          paste0("Significance_", inputs[1]),
          label = NULL,
          choiceNames =  list(
            "Mann-Whitney U",
            "Wilcoxon matched signed-rank test (paired)",
            "Kruskal Wallis test"
          ),
          choiceValues = list(
            "Mann-Whitney U",
            "Paired_SigTest",
            "Kruskal_Wallis"
          )
        )
        }
)
      }
    )
  }

checkboxTests<- lapply(
  list(
    Boxplot = c("Boxplot",T),
    Ratioplot = c("Ratioplot",T),
    SingleTraces = "SingleTraces",
    MatTraces = "MatTraces",
    MedianTraces = "MedianTraces",
    PPlot = "PPlot",
    StitchedPlot = "StitchedPlot"
  ),
  checkboxTestsFactory
)

sliderFontSizeFactory <-
  function(name) {
    return(
      function(...) {
        sliderInput(
          paste0("Fontsize_", name),
          "Font Size",
          1,
          50,
          value = 12,
          step = 0.1
        )
      }
    )
  }

sliderFontSize<- lapply(
  c(
    Boxplot = "Boxplot",
    Ratioplot = "Ratioplot",
    SingleTraces = "SingleTraces",
    MatTraces = "MatTraces",
    MedianTraces = "MedianTraces",
    PPlot = "PPlot",
    StitchedPlot = "StitchedPlot"
  ),
  sliderFontSizeFactory
)

sliderPValueSizeFactory <-
  function(name) {
    return(
      function(...) {
        sliderInput(
          paste0("P-Value_Size_", name),
          "Rel. P-Value Font Size",
          0,
          2,
          value = 1,
          step = 0.01
        )
      }
    )
  }

sliderPValueSize<- lapply(
  c(
    Boxplot = "Boxplot",
    Ratioplot = "Ratioplot",
    SingleTraces = "SingleTraces",
    MatTraces = "MatTraces",
    MedianTraces = "MedianTraces",
    PPlot = "PPlot",
    StitchedPlot = "StitchedPlot"
  ),
  sliderPValueSizeFactory
)

sliderLineSizeFactory <-
  function(name) {
    return(
      function(...) {
        sliderInput(
          paste0("Line_Size_", name),
          "Line Size",
          0,
          2,
          value = 1,
          step = 0.1
        )
      }
    )
  }

sliderLineSize<- lapply(
  c(
    Boxplot = "Boxplot",
    Ratioplot = "Ratioplot",
    SingleTraces = "SingleTraces",
    MatTraces = "MatTraces",
    MedianTraces = "MedianTraces",
    PPlot = "PPlot",
    StitchedPlot = "StitchedPlot"
  ),
  sliderLineSizeFactory
)

numericInputYlimsFactory <-
  function(name) {
    return(
      function(...) {
        fluidRow(column(
          6,
          numericInput(paste0(name,"_YlimMin"), "Ylim Min:", "", -100000, 100000, 0.1)
        ), column(
          6,
          numericInput(paste0(name,"_YlimMax"), "Ylim Max:", "", -100000, 100000, 0.1)
        ))
      }
    )
  }

numericInputYlims<- lapply(
  c(
    Boxplot = "Boxplot",
    Ratioplot = "Ratioplot",
    SingleTraces = "SingleTraces",
    MatTraces = "MatTraces",
    MedianTraces = "MedianTraces",
    PPlot = "PPlot",
    StitchedPlot = "StitchedPlot"
  ),
  numericInputYlimsFactory
)


selectizeThemeFactory <-
  function(names) {
    return(
      function(...) {
        selectizeInput(
          paste0(names[1],"_Used_theme"),
          "Theme",
          choices = list(
            "Boxplot",
            "IV",
            "IV-Analysis",
            "P-Plot",
            "P-Plot-Analysis",
            "Complete-Plot",
            "Complete-Plot-Analysis"
          ),
          selected = names[2]
        )
      }
    )
  }

selectizeTheme <- lapply(
  list(
    Boxplot = list("Boxplot", "Boxplot"),
    Ratioplot = list("Ratioplot", "Boxplot"),
    SingleTraces = list("SingleTraces", "IV"),
    MatTraces = list("MatTraces", "IV"),
    MedianTraces = list("MedianTraces", "IV"),
    PPlot = list("PPlot", "P-Plot"),
    StitchedPlot = list("StitchedPlot", "Complete-Plot")
  ),
  selectizeThemeFactory
)


selectizeYLabFactory <-
  function(names) {
    return(
      function(...) {
        selectizeInput(
          paste0(names[1],"_Used_yLabel"),
          "Y-Axis Label",
          choices = list("Boxplot", "Ratio", "IV", "Slope", "Complete-Plot"),
          options = list(create = TRUE),
          selected = names[2]
        )
      }
    )
  }

selectizeYLab <- lapply(
  list(
    Boxplot = list("Boxplot", "Boxplot"),
    Ratioplot = list("Ratioplot", "Ratio"),
    SingleTraces = list("SingleTraces", "IV"),
    MatTraces = list("MatTraces", "IV"),
    MedianTraces = list("MedianTraces", "IV"),
    PPlot = list("PPlot", "Slope"),
    StitchedPlot = list("StitchedPlot", "Complete-Plot")
  ),
  selectizeYLabFactory
)

selectizeUsedAxisFactory <-
  function(names) {
    return(function(...) {
      selectizeInput(
        paste0(names[1], "_Used_Axis"),
        "X-Axis",
        choices = list(
          "None",
          "IV",
          "IV no Cross",
          "IV-Analysis",
          "P-Values",
          "P-Plot",
          "P-Plot-Analysis",
          "Complete-Plot",
          "Complete-Plot-Analysis"
        ),
        selected = names[2]
      )
    })
  }

selectizeUsedAxis <- lapply(
  list(
    Boxplot = list("Boxplot", "Boxplot"),
    Ratioplot = list("Ratioplot", "Boxplot"),
    SingleTraces = list("SingleTraces", "IV"),
    MatTraces = list("MatTraces", "IV"),
    MedianTraces = list("MedianTraces", "IV"),
    PPlot = list("PPlot", "P-Plot"),
    StitchedPlot = list("StitchedPlot", "Complete-Plot")
  ),
  selectizeUsedAxisFactory
)


selectizeFontFactory <-
  function(name) {
    return(
      function(...) {
        selectizeInput(paste0("Font_",name), "Font", choices = Fonts)
      }
    )
  }

selectizeFont<- lapply(
  c(
    Boxplot = "Boxplot",
    Ratioplot = "Ratioplot",
    SingleTraces = "SingleTraces",
    MatTraces = "MatTraces",
    MedianTraces = "MedianTraces",
    PPlot = "PPlot",
    StitchedPlot = "StitchedPlot"
  ),
  selectizeFontFactory
)

prettySwitchSplitPlotFactory <-
  function(name) {
    return(
      function(...) {
        prettySwitch(
          paste0("split_plot_",name),
          "Split Plot",
          fill = TRUE,
          status = "primary"
        )      }
    )
  }

prettySwitchSplitPlot<- lapply(
  c(
    Boxplot = "Boxplot",
    Ratioplot = "Ratioplot",
    SingleTraces = "SingleTraces",
    MatTraces = "MatTraces",
    MedianTraces = "MedianTraces",
    PPlot = "PPlot",
    StitchedPlot = "StitchedPlot"
  ),
  prettySwitchSplitPlotFactory
)

radioButtonDeviationFactory <-function(name) {
  return(
    function(...) {
      radioButtons(
        paste0("Deviation_",name),
        "Deviations",
        choices = list(
          "SD (Standard Deviation)" = "SD",
          "MAD (Median Absolute Deviation)" = "MAD"
        ),
        selected = "SD"
      )
    }
  )
}

radioButtonDeviation<- lapply(
  c(
    Boxplot = "Boxplot",
    Ratioplot = "Ratioplot",
    SingleTraces = "SingleTraces",
    MatTraces = "MatTraces",
    MedianTraces = "MedianTraces",
    PPlot = "PPlot",
    StitchedPlot = "StitchedPlot"
  ),
  radioButtonDeviationFactory
)

textInputShadowDirectionFactory <-function(name) {
  return(
    function(...) {
      radioButtons(
        paste0("SD_MAD_Shadow_",name),
        "Shadow",
        value = "+,-,+,-",
        width = NULL,
      )
    }
  )
}

textInputShadowDirection<- lapply(
  c(
    Boxplot = "Boxplot",
    Ratioplot = "Ratioplot",
    SingleTraces = "SingleTraces",
    MatTraces = "MatTraces",
    MedianTraces = "MedianTraces",
    PPlot = "PPlot",
    StitchedPlot = "StitchedPlot"
  ),
  textInputShadowDirectionFactory
)

checkboxSeperateLegendFactory <-function(name) {
  return(
    function(...) {
      checkboxInput(
        paste0("legend_seperate_",name),
        "Seperate legend",
        T
      )
    }
  )
}

checkboxSeperateLegend <- lapply(
  c(
    Boxplot = "Boxplot",
    Ratioplot = "Ratioplot",
    SingleTraces = "SingleTraces",
    MatTraces = "MatTraces",
    MedianTraces = "MedianTraces",
    PPlot = "PPlot",
    StitchedPlot = "StitchedPlot"
  ),
  checkboxSeperateLegendFactory
)

selectizeSelectColumnFactory <-
  function(name) {
    return(
      function(...) {
        selectizeInput(paste0("Selected_Column_",name),
                       "Selected Column",
                       choices = IV_columns,
                       "None")
      }
    )
  }

selectizeSelectColumn<- lapply(
  c(
    Boxplot = "Boxplot",
    Ratioplot = "Ratioplot",
    SingleTraces = "SingleTraces",
    MatTraces = "MatTraces",
    MedianTraces = "MedianTraces",
    PPlot = "PPlot",
    StitchedPlot1 = "StitchedPlot1",
    StitchedPlot2 = "StitchedPlot2",
    StitchedPlot3 = "StitchedPlot3"
  ),
  selectizeSelectColumnFactory
)

radioTextSDMADShadowFactory <-function(name) {
  return(
    function(...) {
      verticalLayout(
        radioButtons(
          paste0("Deviations_", name),
          label = "Deviations",
          choices = list(
            "SD (Standard Deviation)" = "SD",
            "MAD (Median Absolute Deviation)" = "MAD"
          ),
          selected = "SD"
        ),
        textInput(
          paste0("SD_MAD_Shadow_", name),
          "Shadow",
          value = "+,-,+,-",
          width = NULL
        )
      )
    }
  )
}

radioTextSDMADShadow <- lapply(
  c(
    Boxplot = "Boxplot",
    Ratioplot = "Ratioplot",
    SingleTraces = "SingleTraces",
    MatTraces = "MatTraces",
    MedianTraces = "MedianTraces",
    PPlot = "PPlot",
    StitchedPlot = "StitchedPlot"
  ),
  radioTextSDMADShadowFactory
)

numericInputSpacerPlotsFactory <-function(name) {
  return(
    function(...) {
      numericInput(
        paste0("Spacer_Plots_", name),
        label = ("Spacer Plots"),
        value = 0
      )
    }
  )
}

numericInputSpacerPlots <- lapply(
  c(
    Boxplot = "Boxplot",
    Ratioplot = "Ratioplot",
    SingleTraces = "SingleTraces",
    MatTraces = "MatTraces",
    MedianTraces = "MedianTraces",
    PPlot = "PPlot",
    StitchedPlot = "StitchedPlot"
  ),
  numericInputSpacerPlotsFactory
)

sliderRatioFactory <-function(name) {
  return(
    function(...) {
      sliderInput(
        paste0("PlotRatio_", name),
        label = ("Ratio Traces to P-Plot"),
        value = 2,
        step = 0.1,
        min = 0,
        max = 4
      )
    }
  )
}

sliderRatio <- lapply(
  c(
    Boxplot = "Boxplot",
    Ratioplot = "Ratioplot",
    SingleTraces = "SingleTraces",
    MatTraces = "MatTraces",
    MedianTraces = "MedianTraces",
    PPlot = "PPlot",
    StitchedPlot = "StitchedPlot"
  ),
  sliderRatioFactory
)

checkboxSetDimensionsFactory <-function(name) {
  return(
    function(...) {
      checkboxInput(
        paste0("SetDimensions_",name),
        "Set Dimensions",
        F
      )
    }
  )
}

checkboxSetDimensions <- lapply(
  c(
    Boxplot = "Boxplot",
    Ratioplot = "Ratioplot",
    SingleTraces = "SingleTraces",
    MatTraces = "MatTraces",
    MedianTraces = "MedianTraces",
    PPlot = "PPlot",
    StitchedPlot = "StitchedPlot"
  ),
  checkboxSetDimensionsFactory
)

#####




##### Sidebar######
sidebar <- dashboardSidebar(
  width = 200,
#  sidebarSearchForm(label = "Search...", "searchText", "searchButton"),
  sidebarMenu(
    id = "tabs",
    menuItem("Whats New", icon = icon("envelope", lib="glyphicon"), tabName = "News"),
    menuItem(
      "Data",
      icon = icon("floppy-disk", lib="glyphicon"),
      menuSubItem("Import/Export", tabName = "ImportExport"),
      menuSubItem("View", tabName = "View")
    ),
    menuItem(
      "Plots",
      icon = icon("picture", lib="glyphicon"),
      menuSubItem("Boxplots", tabName = "BoxPlots"),
      menuSubItem("Ratioplots", tabName = "Ratioplots"),
      menuSubItem("Single Traces", tabName = "SingleTraces"),
      menuSubItem("Matrix Traces", tabName = "MatTraces"),
      menuSubItem("Median Traces", tabName = "MedianTraces"),
      menuSubItem("P-Plot", tabName = "P-Plot"),
      menuSubItem("Stitched Plot", tabName = "StitchedPlot")
    ),
    menuItem(
      "Settings",
      icon = icon("wrench",  lib = "glyphicon"),
      menuSubItem("Data Import", tabName = "SettingsDataImport"),
      menuSubItem("Packages", tabName = "SettingsPackages"),
      menuSubItem("Other", tabName = "OtherSettings")
    ),
    menuItem(
      "About", icon = icon("info-sign", lib="glyphicon"), tabName = "About")
  )
)
#####

##### Body#####
body <- dashboardBody(
  tags$head(
    tags$style(
      type = "text/css",
      "#inline label{ display: table-cell; text-align: center; vertical-align: middle; }
                #inline .form-group { display: table-row;}
      .modal-lg { width: 90%;  }"
    )
  ),
  tags$head(tags$style(HTML("div > .selectize-dropdown  {min-width: 250px;}"))),
  useShinyjs(),
  tabItems(
    tabItem(
      "News",
      h3("DetectIVe 2.0 is here!"),
      h4("New backend, new GUI, new everything!"),
      h4("Faster, more stable, new functionality, better error messages!"),
      h4("You will love it!")
    ),
    ##### Import/Export Data#####
    tabItem(
      "ImportExport",
      fluidRow(
        solidHeaderBoxes$LightBlue(
          title = "Import Data",
          width = 5,
          actionButton("Check_Data", " Check Data Names", icon = icon("search", lib="glyphicon")),
          actionButton("Quick_Import_Data", "Quick Import Data", icon = icon("upload", lib="glyphicon")),
          actionButton("Import_Data", "Import Data", icon = icon("floppy-open", lib="glyphicon")),
          actionButton("Load_Rdata", " Load Rdata", icon = icon("open", lib="glyphicon") ),
          hr(),
          withLoader(
            textOutput("StatusDataImport"),
            proxy.height = 20,
            type = "html",
            loader = "loader7"
          )
        ),
        solidHeaderBoxes$LightBlue(
          title = "Export Data",
          width = 4,
          actionButton("Save_asc", "Save IVs", icon = icon("list-alt", lib="glyphicon")),
          actionButton("Save_xlsx", "Save Summary", icon = icon("book", lib="glyphicon")),
          actionButton("Save_rData", "Save Workspace", icon = icon("save", lib="glyphicon"))
        ),
        solidHeaderBoxes$LightBlue(
          title = "Workspace",
          width = 3,
          actionButton("ChangeWorkspace", "Change Workspace!", icon = icon("folder-open", lib="glyphicon")),
          hr(),
          textOutput("Workspace")
        ),
        bsModal("CheckDataModal", "Check Data", "Check_Data", size = "large",
                p("Are there the right amount of .asc files:"),
                verbatimTextOutput("checkReport1"),
                p("Are the names of the .asc files identical to the summary name:"),
                verbatimTextOutput("checkReport2"),
                verbatimTextOutput("checkReport3"))
      )
    ),
    #####
    ##### Boxplots#####
    tabItem(
      "BoxPlots",
      fluidRow(
        column(
          4,
          saveBox$Boxplot(id = "myBox")
        ),
        column(
          6,
          colorBox$Boxplot()
        ),
        column(
          2,
          updatePlotBox$Boxplot()
        )
      ),
      fluidRow(
        column(
          4,
          seriesBox$Boxplot(
            fluidRow(
              traceLabels$Boxplot(),
              checkboxInput("Include_Bef_Boxplot", label = "Include Before Values", value = TRUE),
              numericInput("Boxplot_AddUp",
                           "Add Spacer Boxes until:",
                           value = NULL
              )
            )
          )
        ),
        column(
          8,
          solidHeaderBoxes$LightBlue(
            title = "Settings",
            width = 12,
            fluidRow(
              column(
                2,
                div(style = "margin-top: -10px; margin-bottom: 15px; font-size:15px; text-align:center", HTML("<strong> Composition </strong>")),
                checkboxGroupInput(
                  "Boxplot_Extras_CD",
                  label = NULL,
                  choiceNames = list(
                    "Errorbars",
                    "Jitter",
                    "Violins",
                    "Observations",
                    "P-Values",
                    "Significances",
                    "Median"
                  ),
                  choiceValues = list(
                    "Errorbars",
                    "Jitter",
                    "Violins",
                    "Observations",
                    "P-Values",
                    "Significances",
                    "Median"
                  ),
                  selected = list(
                    "Errorbars",
                    "Jitter",
                    "Observations",
                    "P-Values",
                    "Significances",
                    "Median"
                  )
                ),
                checkboxTests$Boxplot()
              ),
              column(
                style = "border-right: 1px solid black; border-left: 1px solid black",
                3,
                div(style = "margin-top: -10px; margin-bottom: 15px; font-size:15px; text-align:center", HTML("<strong> Sizes </strong>")),
                sliderFontSize$Boxplot(),
                sliderPValueSize$Boxplot(),
                sliderLineSize$Boxplot(),
                sliderInput(
                  "Jitter_Size_CD",
                  "Jitter Size",
                  0,
                  2,
                  value = 1,
                  step = 0.1
                )
              ),
              column(
                3,
                div(style = "margin-top: -10px; margin-bottom: 15px; font-size:15px; text-align:center", HTML("<strong> P-Values </strong>")),
                radioButtons(
                  "placement_type_CD",
                  label = ("Placement"),
                  choices = list("Absolute" = "absolute", "Relative" = "relative"),
                  selected = "absolute"
                ),
                fluidRow(column(
                  6,
                  numericInput(
                    "relative_placement_scale_obs_CD",
                    "Dist. N",
                    0,
                    5,
                    value = 1.5,
                    step = 0.01
                  )
                )),
                fluidRow(
                  column(
                    6,
                    numericInput(
                      "relative_placement_scale_sig_CD",
                      "Dist. Sign. Outward",
                      0,
                      5,
                      value = 1.2,
                      step = 0.01
                    )
                  ),
                  column(
                    6,
                    numericInput(
                      "relative_placement_scale_sig_CD_Min",
                      "Dist. Sign. Inward",
                      0,
                      4,
                      value = 1.5,
                      step = 0.01
                    )
                  )
                ),
                sliderInput(
                  "distance_p_value_stars_CD",
                  "distance p-value/stars",
                  0,
                  3,
                  value = 1.15,
                  step = 0.01
                )
              ),
              column(
                style = "border-left: 1px solid black",
                4,
                div(style = "margin-top: -10px; margin-bottom: 15px; font-size:15px; text-align:center", HTML("<strong> Other </strong>")),
                numericInputYlims$Boxplot(),
                fluidRow(
                  column(
                    6,
                    numericInput(
                      "Boxplot_X_Axis_Angle",
                      "X-Axis Label Angle",
                      0,
                      360,
                      value = 90,
                      step = 1
                    )
                  ),
                  column(
                    6,
                    numericInput(
                      "Alpha_Extra_CD",
                      "Jitter Transparency",
                      0,
                      1,
                      value = 0.5,
                      step = 0.01
                    )
                  )
                ),
                selectizeTheme$Boxplot(),
                selectizeYLab$Boxplot(),
                selectizeFont$Boxplot(),
                checkboxSetDimensions$Boxplot()
              )
            )
          )
        )
      ),
      fluidRow(
        column(6,
               plotPreviewBox$Boxplot())
      ),
      plotModals$Boxplot()

    ),
    #####Ratioplots#####
    tabItem(
      "Ratioplots",
      fluidRow(
        column(
          4,
          saveBox$Ratioplot(id = "myBox")
        ),
        column(
          6,
          colorBox$Ratioplot()
        ),
        column(
          2,
          updatePlotBox$Ratioplot()
        )
      ),
      fluidRow(
        column(
          4,
          seriesBox$Ratioplot(
            fluidRow(
              traceLabels$Ratioplot(),
              checkboxInput("Include_Bef_Ratioplot", label = "Include Before Values", value = TRUE),
              numericInput("Ratioplot_AddUp",
                           "Add Spacer Boxes until:",
                           value = NULL
              )
            )
          )
        ),
        column(
          8,
          solidHeaderBoxes$LightBlue(
            title = "Settings",
            width = 12,
            fluidRow(
              column(
                2,
                div(style = "margin-top: -10px; margin-bottom: 15px; font-size:15px; text-align:center", HTML("<strong> Composition </strong>")),
                checkboxGroupInput(
                  "Ratioplot_Extras_CD",
                  label = NULL,
                  choiceNames = list(
                    "Errorbars",
                    "Jitter",
                    "Violins",
                    "Observations",
                    "P-Values",
                    "Significances",
                    "Median"
                  ),
                  choiceValues = list(
                    "Errorbars",
                    "Jitter",
                    "Violins",
                    "Observations",
                    "P-Values",
                    "Significances",
                    "Median"
                  ),
                  selected = list(
                    "Errorbars",
                    "Jitter",
                    "Observations",
                    "P-Values",
                    "Significances",
                    "Median"
                  )
                ),
                checkboxTests$Ratioplot()

              ),
              column(
                style = "border-right: 1px solid black; border-left: 1px solid black",
                3,
                div(style = "margin-top: -10px; margin-bottom: 15px; font-size:15px; text-align:center", HTML("<strong> Sizes </strong>")),
                sliderFontSize$Ratioplot(),
                sliderPValueSize$Ratioplot(),
                sliderLineSize$Ratioplot(),
                sliderInput(
                  "Jitter_Size_Ratioplot",
                  "Jitter Size",
                  0,
                  2,
                  value = 1,
                  step = 0.1
                )
              ),
              column(
                3,
                div(style = "margin-top: -10px; margin-bottom: 15px; font-size:15px; text-align:center", HTML("<strong> P-Values </strong>")),
                radioButtons(
                  "placement_type_Ratioplot",
                  label = ("Placement"),
                  choices = list("Absolute" = "absolute", "Relative" = "relative"),
                  selected = "absolute"
                ),
                fluidRow(column(
                  6,
                  numericInput(
                    "relative_placement_scale_obs_Ratioplot",
                    "Dist. N",
                    0,
                    5,
                    value = 1.5,
                    step = 0.01
                  )
                )),
                fluidRow(
                  column(
                    6,
                    numericInput(
                      "relative_placement_scale_sig_Ratioplot",
                      "Dist. Sign. Outward",
                      0,
                      5,
                      value = 1.2,
                      step = 0.01
                    )
                  ),
                  column(
                    6,
                    numericInput(
                      "relative_placement_scale_sig_Ratioplot_Min",
                      "Dist. Sign. Inward",
                      0,
                      4,
                      value = 1.5,
                      step = 0.01
                    )
                  )
                ),
                sliderInput(
                  "distance_p_value_stars_Ratioplot",
                  "distance p-value/stars",
                  0,
                  3,
                  value = 1.15,
                  step = 0.01
                )
              ),
              column(
                style = "border-left: 1px solid black",
                4,
                div(style = "margin-top: -10px; margin-bottom: 15px; font-size:15px; text-align:center", HTML("<strong> Other </strong>")),
                numericInputYlims$Ratioplot(),
                fluidRow(
                  column(
                    6,
                    numericInput(
                      "Ratioplot_X_Axis_Angle",
                      "X-Axis Label Angle",
                      0,
                      360,
                      value = 90,
                      step = 1
                    )
                  ),
                  column(
                    6,
                    numericInput(
                      "Alpha_Extra_Ratioplot",
                      "Jitter Transparency",
                      0,
                      1,
                      value = 0.5,
                      step = 0.01
                    )
                  )
                ),
                selectizeTheme$Ratioplot(),
                selectizeYLab$Ratioplot(),
                selectizeFont$Ratioplot(),
                checkboxSetDimensions$Ratioplot()
              )
            )
          )
        )
      ),
      fluidRow(
        column(6,
               plotPreviewBox$Ratioplot())
      ),
      plotModals$Ratioplot()

    ),
    #####SingleTraces
    #####
    tabItem("SingleTraces",
            fluidRow(
              column(
                4,
                saveBox$SingleTraces(id = "myBox")
              ),
              column(
                6,
                colorBox$SingleTraces()
              ),
              column(
                2,
                updatePlotBox$SingleTraces()
              )
            ),

            fluidRow(
              column(
                4,
                solidHeaderBoxes$LightBlue(
                  title = "Data",
                  width = 12,
                  fluidPage(
                    fluidRow(
                      selectizeInput(
                        "SingleTraces_Series",
                        "Series",
                        choices = list(),
                        multiple = FALSE
                      )
                    ),
                    fluidRow(
                      selectizeInput(
                        "SingleTraces_Meas",
                        "Measurement",
                        choices = list(),
                        multiple = TRUE
                      )
                    ),
                    fluidRow(
                      selectizeSelectColumn$SingleTraces()
                                        )
                  )
                )
              ),
              column(
                8,
                solidHeaderBoxes$LightBlue(
                  title = "Settings",
                  width = 12,
                  fluidRow(
                    column(
                      3,
                      div(style = "margin-top: -10px; margin-bottom: 15px; font-size:15px; text-align:center", HTML("<strong> Data </strong>")),
                      traceLabels$SingleTraces(),
                      prettySwitchSplitPlot$SingleTraces()

                    ),
                    column(
                      style = "border-right: 1px solid black; border-left: 1px solid black;",
                      3,
                      div(style = "margin-top: -10px; margin-bottom: 15px; font-size:15px; text-align:center", HTML("<strong> Sizes </strong>")),
                      sliderFontSize$SingleTraces(),
                      sliderLineSize$SingleTraces()
                    ),

                    column(
                      style = "border-right: 1px solid black",
                      4,
                      div(style = "margin-top: -10px; margin-bottom: 15px; font-size:15px; text-align:center", HTML("<strong> Other </strong>")),
                      numericInputYlims$SingleTraces(),

                      selectizeTheme$SingleTraces(),
                      selectizeUsedAxis$SingleTraces()
                    ),
                    column(
                      2,
                      div(style = "margin-top: -10px; margin-bottom: 15px; font-size:15px; text-align:center", HTML("<strong> Other </strong>")),
                      selectizeYLab$SingleTraces(),
                      selectizeFont$SingleTraces(),
                      checkboxSeperateLegend$SingleTraces(),
                      checkboxSetDimensions$SingleTraces()
                    )
                  )
                )
              )
            ),
            fluidRow(
              column(6,
                     plotPreviewBox$SingleTraces())
            ),
            plotModals$SingleTraces()
    ),

  ####
  #####MatTraces
    tabItem("MatTraces",
            fluidRow(
              column(
                4,
                saveBox$MatTraces(id = "myBox")
              ),
              column(
                6,
                colorBox$MatTraces()
              ),
              column(
                2,
                updatePlotBox$MatTraces()
              )
            ),

            fluidRow(
              column(
                4,
                seriesBox$MatTraces(
                  fluidRow(selectizeSelectColumn$MatTraces())
                )
              ),
              column(
                8,
                solidHeaderBoxes$LightBlue(
                  title = "Settings",
                  width = 12,
                  fluidRow(
                    column(
                      3,
                      div(style = "margin-top: -10px; margin-bottom: 15px; font-size:15px; text-align:center", HTML("<strong> Data </strong>")),
                      traceLabels$MatTraces(),
                      prettySwitchSplitPlot$MatTraces()
                    ),
                    column(
                      style = "border-right: 1px solid black; border-left: 1px solid black;",
                      3,
                      div(style = "margin-top: -10px; margin-bottom: 15px; font-size:15px; text-align:center", HTML("<strong> Sizes </strong>")),
                      sliderFontSize$MatTraces(),
                      sliderLineSize$MatTraces(),
                    ),

                    column(
                      style = "border-right: 1px solid black",
                      4,
                      div(style = "margin-top: -10px; margin-bottom: 15px; font-size:15px; text-align:center", HTML("<strong> Other </strong>")),
                      numericInputYlims$MatTraces(),
                      selectizeTheme$MatTraces(),
                      selectizeUsedAxis$MatTraces()
                    ),
                    column(
                      2,
                      div(style = "margin-top: -10px; margin-bottom: 15px; font-size:15px; text-align:center", HTML("<strong> Other </strong>")),
                      selectizeYLab$MatTraces(),
                      selectizeFont$MatTraces(),
                      checkboxSeperateLegend$MatTraces(),
                      checkboxSetDimensions$MatTraces()
                    )
                  )
                )
              )
            ),
            fluidRow(
              column(6,
                     plotPreviewBox$MatTraces())
            ),
            plotModals$MatTraces()
    ),
  ####
  #####MedianTraces
    tabItem("MedianTraces",
            fluidRow(
              column(
                4,
                saveBox$MedianTraces(id = "myBox")
              ),
              column(
                6,
                colorBox$MedianTraces()
              ),
              column(
                2,
                updatePlotBox$MedianTraces()
              )
            ),

            fluidRow(
              column(
                4,
                seriesBox$MedianTraces(
                  fluidRow( selectizeSelectColumn$MedianTraces())
                )
              ),
              column(
                8,
                solidHeaderBoxes$LightBlue(
                  title = "Settings",
                  width = 12,
                  fluidRow(
                    column(
                      3,
                      div(style = "margin-top: -10px; margin-bottom: 15px; font-size:15px; text-align:center", HTML("<strong> Data </strong>")),
                      traceLabels$MedianTraces(),
                      prettySwitchSplitPlot$MedianTraces()
                    ),
                    column(
                      style = "border-right: 1px solid black; border-left: 1px solid black;",
                      3,
                      div(style = "margin-top: -10px; margin-bottom: 15px; font-size:15px; text-align:center", HTML("<strong> Sizes </strong>")),
                      sliderFontSize$MedianTraces(),
                      sliderLineSize$MedianTraces(),
                    ),

                    column(
                      style = "border-right: 1px solid black",
                      3,
                      div(style = "margin-top: -10px; margin-bottom: 15px; font-size:15px; text-align:center", HTML("<strong> Other </strong>")),
                      selectizeTheme$MedianTraces(),
                      selectizeUsedAxis$MedianTraces(),
                      selectizeYLab$MedianTraces(),
                      selectizeFont$MedianTraces()

                      ),
                    column(
                      3,
                      div(style = "margin-top: -10px; margin-bottom: 15px; font-size:15px; text-align:center", HTML("<strong> Other </strong>")),
                      numericInputYlims$MedianTraces(),
                      radioTextSDMADShadow$MedianTraces(),
                      checkboxSeperateLegend$MedianTraces(),
                      checkboxSetDimensions$MedianTraces()
                    )
                  )
                )
              )
            ),
            fluidRow(
              column(6,
                     plotPreviewBox$MedianTraces())
            ),
            plotModals$MedianTraces()
    ),
  ####
  #####P-Plot
    tabItem("P-Plot",
            fluidRow(
              column(
                4,
                saveBox$PPlot(id = "myBox")
              ),
              column(
                6,
                colorBox$PPlot()
              ),
              column(
                2,
                updatePlotBox$PPlot()
              )
            ),

            fluidRow(
              column(
                4,
                seriesBox$PPlot(
                  fluidRow(                      selectizeSelectColumn$PPlot())
                )
              ),
              column(
                8,
                solidHeaderBoxes$LightBlue(
                  title = "Settings",
                  width = 12,
                  fluidRow(
                    column(
                      3,
                      div(style = "margin-top: -10px; margin-bottom: 15px; font-size:15px; text-align:center", HTML("<strong> Data </strong>")),
                      traceLabels$PPlot(),
                      prettySwitchSplitPlot$PPlot(),
                      numericInputSpacerPlots$PPlot(),
                      checkboxTests$PPlot(),
                    ),
                    column(
                      style = "border-right: 1px solid black; border-left: 1px solid black;",
                      3,
                      div(style = "margin-top: -10px; margin-bottom: 15px; font-size:15px; text-align:center", HTML("<strong> Sizes </strong>")),
                      sliderFontSize$PPlot(),
                      sliderLineSize$PPlot(),
                      sliderRatio$PPlot()
                    ),

                    column(
                      style = "border-right: 1px solid black",
                      3,
                      div(style = "margin-top: -10px; margin-bottom: 15px; font-size:15px; text-align:center", HTML("<strong> Other </strong>")),
                      selectizeTheme$PPlot(),
                      selectizeUsedAxis$PPlot(),
                      selectizeYLab$PPlot(),
                      selectizeFont$PPlot()

                    ),
                    column(
                      3,
                      div(style = "margin-top: -10px; margin-bottom: 15px; font-size:15px; text-align:center", HTML("<strong> Other </strong>")),
                      radioTextSDMADShadow$PPlot(),
                      checkboxSeperateLegend$PPlot(),
                      checkboxSetDimensions$PPlot()
                    )
                  )
                )
              )
            ),
            fluidRow(
              column(6,
                     plotPreviewBox$PPlot())
            ),
            plotModals$PPlot()
    ),
  ####
  #####StitchedPlot
  tabItem("StitchedPlot",
          fluidRow(
            column(
              4,
              saveBox$StitchedPlot(id = "myBox")
            ),
            column(
              6,
              colorBox$StitchedPlot()
            ),
            column(
              2,
              updatePlotBox$StitchedPlot()
            )
          ),

          fluidRow(
            column(
              4,
              seriesBox$StitchedPlot(
                fluidRow(fluidRow(column(4,
                                selectizeSelectColumn$StitchedPlot1()),
                         column(4,
                                selectizeSelectColumn$StitchedPlot2()),
                         column(4,
                                selectizeSelectColumn$StitchedPlot3())))

              )
            ),
            column(
              8,
              solidHeaderBoxes$LightBlue(
                title = "Settings",
                width = 12,
                fluidRow(
                  column(
                    3,
                    div(style = "margin-top: -10px; margin-bottom: 15px; font-size:15px; text-align:center", HTML("<strong> Data </strong>")),
                    traceLabels$StitchedPlot(),
                    prettySwitchSplitPlot$StitchedPlot(),
                    numericInputSpacerPlots$StitchedPlot(),
                    checkboxTests$StitchedPlot(),
                  ),
                  column(
                    style = "border-right: 1px solid black; border-left: 1px solid black;",
                    3,
                    div(style = "margin-top: -10px; margin-bottom: 15px; font-size:15px; text-align:center", HTML("<strong> Sizes </strong>")),
                    sliderFontSize$StitchedPlot(),
                    sliderLineSize$StitchedPlot(),
                    sliderRatio$StitchedPlot()
                  ),

                  column(
                    style = "border-right: 1px solid black",
                    3,
                    div(style = "margin-top: -10px; margin-bottom: 15px; font-size:15px; text-align:center", HTML("<strong> Other </strong>")),
                    selectizeTheme$StitchedPlot(),
                    selectizeUsedAxis$StitchedPlot(),
                    selectizeYLab$StitchedPlot(),
                    selectizeFont$StitchedPlot()

                  ),
                  column(
                    3,
                    div(style = "margin-top: -10px; margin-bottom: 15px; font-size:15px; text-align:center", HTML("<strong> Other </strong>")),
                    actionButton("showSidebar_StitchedPlot", "Custom Y-Axis limits"),
                    radioTextSDMADShadow$StitchedPlot(),
                    checkboxSeperateLegend$StitchedPlot(),
                    checkboxSetDimensions$StitchedPlot()
                  )
                ),
                sidebar = boxSidebar(
                  id = "ylimSidebar_StitchedPlot",
                  width = 55,
                  fluidPage(
                    fluidRow(column(
                      3),
                      column(
                        4,
                        HTML("<b> Ylim Min </b>")),
                      column(
                        4,
                        HTML("<b> Ylim Max </b>"))),
                  fluidRow(
                    column(
                      3,
                      HTML("<b> 1. Complete </b>")
                    ),
                    column(
                    4,
                    numericInput("StitchedPlot_YlimMin1", NULL, "", -100000, 100000, 0.1)
                  ), column(
                    4,
                    numericInput("StitchedPlot_YlimMax1", NULL, "", -100000, 100000, 0.1)
                  )),
                  fluidRow(
                    column(
                      3,
                      HTML("<b> 2. Inward </b>")
                    ),
                    column(
                      4,
                      numericInput("StitchedPlot_YlimMin2", NULL, "", -100000, 100000, 0.1)
                    ), column(
                      4,
                      numericInput("StitchedPlot_YlimMax2", NULL, "", -100000, 100000, 0.1)
                    )),
                  fluidRow(
                    column(
                      3,
                      HTML("<b> 2. Outward </b>")
                    ),
                    column(
                      4,
                      numericInput("StitchedPlot_YlimMin3", NULL, "", -100000, 100000, 0.1)
                    ), column(
                      4,
                      numericInput("StitchedPlot_YlimMax3", NULL, "", -100000, 100000, 0.1)
                    )),
                  fluidRow(
                    column(
                      3,
                      HTML("<b> 3. Inward </b>")
                    ),
                    column(
                      4,
                      numericInput("StitchedPlot_YlimMin4", NULL, "", -100000, 100000, 0.1)
                    ), column(
                      4,
                      numericInput("StitchedPlot_YlimMax4", NULL, "", -100000, 100000, 0.1)
                    )),
                  fluidRow(
                    column(
                      3,
                      HTML("<b> 3. Outward </b>")
                    ),
                    column(
                      4,
                      numericInput("StitchedPlot_YlimMin5", NULL, "", -100000, 100000, 0.1)
                    ), column(
                      4,
                      numericInput("StitchedPlot_YlimMax5", NULL, "", -100000, 100000, 0.1)
                    )),
                  actionButton("hideSidebar_StitchedPlot", "Hide")
)
                )
              )
            )
          ),
          fluidRow(
            column(6,
                   plotPreviewBox$StitchedPlot())
          ),
          plotModals$StitchedPlot()
  ),
    #####
    ##### View Data in Tables#####
    tabItem(
      "View",
      fluidRow(
      solidHeaderBoxes$LightBlue(
        title = "Selection",
        width = 9,
        fluidRow(
          column(
            width = 4,
            selectizeInput(
              "choose_series",
              label = "Choose Series",
              choices = list("No data loaded" = 1, "No data loaded" = 2),
              selected = 1,
              width = "150%"
            )
          ),
          column(
            width = 7,
            offset = 1,
            selectizeInput(
              "choose_IV",
              label = "Choose IV",
              choices = list("No data loaded" = 1, "No data loaded" = 20),
              selected = 1,
              width = "200%"
            )
          )
        )
      )
      ),
      fluidRow(
        solidHeaderBoxes$LightBlue(
          title = "Summary",
          width = 12,
          DT::dataTableOutput("Summary_table")
        )
      ),
      fluidRow(
        solidHeaderBoxes$LightBlue(
          title = "IV",
          width = 12,
          DT::dataTableOutput("IV_table")
        )
      )
    ),
    #####
    ##### Settings Import#####
    tabItem(
      "SettingsDataImport",
      fluidRow(
        solidHeaderBoxes$LightBlue(
          title = "Current Export",
          width = 10,
          fluidRow(
            column(
              6,
              style = "border-right: 1px solid black",
              fluidRow(
                column(
                  6,
                  HTML("<b>Current Density Marker:</b>"
                  )
                ),
                column(
                  6,
                  selectInput(
                    "markerSelection",
                    label = NULL,
                    choices = c("upramp100", "ultrafast"),
                    selected = "upramp100"
                  )
                )
              ),
              fluidRow(column(
                12,
                h5(
                  HTML("<b>Inward (Index #):</b>")
                )
              ), ),
              fluidRow(
                column(
                  6,
                  numericInput("CurrentMarker_minmin", label = NULL, value = 50)
                ),
                column(
                  6,
                  numericInput("CurrentMarker_minmax", label = NULL, value = 249)
                )
              ),
              fluidRow(column(
                12,
                h5(
                  HTML("<b>Outward (Index #):</b>")
                )
              )),
              fluidRow(
                column(
                  6,
                  numericInput("CurrentMarker_maxmin", label = NULL, value = 2251)
                ),
                column(
                  6,
                  numericInput("CurrentMarker_maxmax", label = NULL, value = 2450)
                )
              ),
              switchInput(
                "inputMarkers",
                value = TRUE,
                onLabel = "ON",
                offLabel = "OFF"
              )
            ),
            column(
              6,
              imageOutput("markerplot", height = "60%")
            )
          )
        )
      ),
      fluidRow(
        solidHeaderBoxes$LightBlue(
          title = "Split at reverse Potential",
          width = 3,
          verticalLayout(
            numericInput("revPotential_split", "Reverse potential (mV)", value = 0),
            switchInput(
              "inputSplit",
              value = FALSE,
              onLabel = "ON",
              offLabel = "OFF"
            )
          )
        ),
        solidHeaderBoxes$LightBlue(
          title = "Fit through Reverse Potential",
          width = 3,
          verticalLayout(
            numericInput("revPotential", "Reverse potential (mV)", value = 0),
            switchInput(
              "inputForceReversal",
              value = FALSE,
              onLabel = "ON",
              offLabel = "OFF"
            )
          )
        ),

        solidHeaderBoxes$LightBlue(
          title = "Fit parameters",
          width = 3,
          leftLabeldWidgets$numericInput("spar", "Spar: ", value = 0.8)
        )
      ),
      fluidRow(
        solidHeaderBoxes$Green(
          title = "Change voltage ramp after import",
          width = 3,
          switchInput(
            "inputAutoVoltageRamp",
            value = TRUE,
            onLabel = "Yes",
            offLabel = "No"
          )
        )
      )
    ),
    #####
    ##### Settings Packages#####
    tabItem(
      "SettingsPackages",
      fluidRow(
        solidHeaderBoxes$LightBlue(
          title = "Ephys.WSI",
          width = 6,
          textOutput("Ephys.WSI_Version")
        ),
        solidHeaderBoxes$LightBlue(
          title = "Support Packages",
          width = 6,
          tableOutput("SupportPackages_Version")
        )
      ),
      fluidRow(
        solidHeaderBoxes$LightBlue(
          title = "Install Packages",
          width = 6,
          actionButton("InstallPackages", "Install!", icon = icon("import", lib="glyphicon"))
        )
      )
    ),
    ##### Settings Other#####
    tabItem(
      "OtherSettings",
      fluidRow(
        solidHeaderBoxes$LightBlue(
          title = "Original Voltage Ramp",
          width = 6,
          tags$table(
            width = "100%",
            tags$tr(
              tags$td(width = "15%", HTML("<b>Ramp Start:</b>")),
              tags$td(
                width = "30%",
                leftLabeldWidgets$numericInput("rampMin_Original", label = NULL, value = settings_envir$ramp_data[1])
              ),
              tags$td(width = "13%", HTML("<b>Ramp End:</b>")),
              tags$td(
                width = "20%",
                leftLabeldWidgets$numericInput("rampMax_Original", label = NULL, value = settings_envir$ramp_data[2])
              )
            ),
            tags$tr(
              tags$td(width = "15%", HTML("<b>Ramp Length:</b>")),
              tags$td(
                width = "30%",
                leftLabeldWidgets$numericInput(
                  "rampLength_Original",
                  label = NULL,
                  value =  settings_envir$length_Vramp
                )
              ),
              tags$td(width = "13%", HTML("<b>Ramp Steps:</b>")),
              tags$td(
                width = "20%",
                leftLabeldWidgets$numericInput("rampStep_Original", label = NULL, value = settings_envir$step)
              )
            ),
            tags$tr(
              tags$td(width = "15%", HTML("<b>Ramp Unit:</b>")),
              tags$td(
                width = "30%",
                leftLabeldWidgets$textInput("rampUnit_Original", label = NULL, value = settings_envir$voltage_unit)
              ),
              tags$td(width = "13%", HTML("<b></b>")),
              tags$td(
                width = "20%",
                actionButton("ChangeVoltageRamp_Original", "Change", icon = icon("duplicate", lib="glyphicon"))
              )

            )
          )
        ),
        solidHeaderBoxes$LightBlue(
          title = "New Voltage Ramp",
          width = 6,
          tags$table(
            width = "100%",
            tags$tr(
              tags$td(width = "15%", HTML("<b>Ramp Start:</b>")),
              tags$td(
                width = "30%",
                leftLabeldWidgets$numericInput("rampMin_New", label = NULL, value = -100)
              ),
              tags$td(width = "13%", HTML("<b>Ramp End:</b>")),
              tags$td(
                width = "20%",
                leftLabeldWidgets$numericInput("rampMax_New", label = NULL, value = 100)
              )
            ),
            tags$tr(
              tags$td(width = "15%", HTML("<b>Ramp Length:</b>")),
              tags$td(
                width = "30%",
                leftLabeldWidgets$numericInput(
                  "rampLength_New",
                  label = NULL,
                  value =  settings_envir$length_Vramp
                )
              ),
              tags$td(width = "13%", HTML("<b>Ramp Steps:</b>")),
              tags$td(
                width = "20%",
                leftLabeldWidgets$numericInput("rampStep_New", label = NULL, value = 0.1)
              )
            ),
            tags$tr(
              tags$td(width = "15%", HTML("<b>Ramp Unit:</b>")),
              tags$td(
                width = "30%",
                leftLabeldWidgets$textInput("rampUnit_New", label = NULL, value = "mV")
              ),
              tags$td(width = "13%", HTML("<b></b>")),
              tags$td(
                width = "20%",
                # actionButton("ChangeVoltrageRamp_New", "Change")
              )

            )
          )
        ),
        solidHeaderBoxes$LightBlue(
          title = "Manually Change Voltage Ramp",
          width = 6,
          tags$table(
            width = "100%",
            tags$tr(
              tags$td(width = "15%", HTML("<b>Ramp Start:</b>")),
              tags$td(
                width = "30%",
                leftLabeldWidgets$numericInput("rampMin_Manual", label = NULL, value = -100)
              ),
              tags$td(width = "13%", HTML("<b>Ramp End:</b>")),
              tags$td(
                width = "20%",
                leftLabeldWidgets$numericInput("rampMax_Manual", label = NULL, value = 100)
              )
            ),
            tags$tr(
              tags$td(width = "15%", HTML("<b>Ramp Length:</b>")),
              tags$td(
                width = "30%",
                leftLabeldWidgets$numericInput(
                  "rampLength_Manual",
                  label = NULL,
                  value =  2455
                )
              ),
              tags$td(width = "13%", HTML("<b>Ramp Steps:</b>")),
              tags$td(
                width = "20%",
                leftLabeldWidgets$numericInput("rampStep_Manual", label = NULL, value = 0.1)
              )
            ),
            tags$tr(
              tags$td(width = "15%", HTML("<b>Ramp Unit:</b>")),
              tags$td(
                width = "30%",
                leftLabeldWidgets$textInput("rampUnit_Manual", label = NULL, value = "mV")
              ),
              tags$td(width = "13%", HTML("<b></b>")),
              tags$td(
                width = "20%",
                actionButton("Switch_Voltage_Ramp_Manual", "Switch Voltage Ramp", icon = icon("pencil", lib="glyphicon"))
              )

            )
          )
        )
      ),
      fluidRow(
        solidHeaderBoxes$Green(
          title = "Plot Dimensions",
          width = 6,
          fluidRow(
            column(6,
                  selectInput("SelectDimensions", NULL,
                              choices = c("Boxplot" = "Boxplot",
                              "Ratioplot" = "Ratioplot",
                              "Single Traces" = "SingleTraces",
                              "Matrix Traces" = "MatTraces",
                              "Median Traces" = "MedianTraces",
                              "P-Plot" = "PPlot",
                              "Stitched Plot" = "StitchedPlot"))
          ),
          column(3,
                 actionButton("UploadDimensions", "Upload Dimensions", icon = icon("export", lib="glyphicon"))),
          column(3,
                 actionButton("SaveDimensions", "Save Dimensions", icon = icon("import", lib="glyphicon")))
          )
        )
      )
    )
  )
  #####
)

##### Header#####
header <- dashboardHeader(title = span(
  "DetectIVe",
  tags$img(src="detectIVe.png", width = '20%')),
                          titleWidth  = 200)
#####


ui <- function(request) {
  dashboardPage(title = "DetectIVe", header, sidebar, body, skin = "black-light")
}

