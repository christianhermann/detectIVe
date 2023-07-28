
shinyServer(function(input, output, session) {
  #####
  ##### Reactives#####


  #####
  ##### Observe Tabs######
  observeEvent(input$tabs, {
    ##### Import Export Data######
    if (input$tabs == "ImportExport") {
      output$Workspace <- renderText({
        getwd()
      })
      output$StatusDataImport <- renderText({
        if (isDataImported == FALSE) {
          paste0("No data has been imported or loaded yet!")
        } else {
          paste0("Data has been successfully loaded!")
        }
      })
    }
    ##### Settings Packages######
    if (input$tabs == "SettingsPackages") {
      output$Ephys.WSI_Version <- renderText({
        paste0("Version: ", paste(packageVersion("ephys.WSI")))
      })

      output$SupportPackages_Version <- renderTable({
        data.frame(
          "Package" = supportPackages,
          "Version" = sapply(supportPackages, checkPackageVersion)
        )
      })
    }
    #####
  })
  ##### Change Workspace######
  observeEvent(input$ChangeWorkspace, {
    wddir <- choose.dir()
    if(!is.na(wddir)) setwd(wddir)

    output$Workspace <- renderText({
      getwd()
    })
  })
  #####

  ##### Import Data#####
  observeEvent(input$Check_Data, {
    sapply(supportPackages, activatePackages)

    setwd(choose.dir())
    try({
      data_list <- import_excel_Data("summary_Data")


      IV_list <- list()
      IV_dir <- list()
      IV_names <- list()
      IV_peaks <- list()
      sheet_names <- unlist(data_list["sheet_names"])
      IV_dir <- as.list(paste0(getwd(), "/ASC files/", sheet_names))
      names(IV_dir) <- sheet_names
      IV_dir <-
        map(IV_dir, function(x) {
          list.files(
            x,
            pattern = "*.asc",
            recursive  = T,
            full.names = T,
            include.dirs = T
          )
        })
      for (i in unlist(data_list["sheet_names"])) {
        print(i)

        IV_names[[i]] <-
          str_remove(word(IV_dir[[i]], -1, sep = fixed("/")), ".asc")
        IV_peaks[[i]] <- unique(flatten_chr(map(
          IV_names[[i]],
          word,
          start = -1,
          sep = "_"
        )))
      }
      ivNamesClean <- map(IV_names, word, 1, -2, "_")
      summaryNames <-
        map(data_list$summary_Data, function(x) {
          x[, 1]
        })


      ASCIperMeas <-
        map2(IV_names, summaryNames, function(x, y) {
          length(x) / length(IV_peaks[[1]]) == length(unlist(y))
        })

      NamesBool <-
        map2(ivNamesClean, summaryNames, function(y, x) {
          map_chr(y, function(y, x) {
            y %in% unlist(x)
          }, x)
        })

      Names <-
        map2(ivNamesClean, summaryNames, function(y, x) {
          map_chr(y, function(y, x) {
            paste0(y, ": ", y %in% unlist(x))
          }, x)
        })

      output$checkReport1 <- renderPrint({
        ASCIperMeas
      })

      output$checkReport2 <- renderPrint({
        Names
      })


      if (FALSE %in% unlist(unique(ASCIperMeas)) ||
        FALSE %in% unlist(unique(NamesBool))) {
        easyShinyAlarm(
          title = "",
          text = HTML(paste0("Data needs correction!")),
          type = "error"
        )
      } else {
        easyShinyAlarm(
          title = "",
          text = HTML(paste0("Data is ready to process!")),
          type = "info"
        )
      }
    })
  })



  observeEvent(input$Quick_Import_Data, {
    isolate({
      output$StatusDataImport <- renderText({
        sapply(supportPackages, activatePackages)
        quickTryCatchLogWithVar(importData, TRUE, "Data could not be importet: <br/> ")
        if (isDataImported == TRUE) {
          paste0("Data import succesfull!")
        } else {
          paste0("Data import not succesfull!")
        }
      })
    })
  })

  observeEvent(input$Import_Data, {
    output$StatusDataImport <- renderText({
      sapply(supportPackages, activatePackages)
      quickTryCatchLogWithVar(importData, FALSE, "Data could not be importet: <br/> ")
    })
  })

  observeEvent(input$Load_Rdata, {
    sapply(supportPackages, activatePackages)
    #   output$StatusDataImport <- renderText({
    tryCatch(
      {
        super_env <- new.env()
        load(gfile(
          type = "open",
          filter = list("R files" = list(
            patterns = c("*.Rdata")
          ))
        ), envir = super_env)
        data_storage_envir <<- super_env$data_storage_envir
        data_envir <<- super_env$data_envir
        settings_envir <<- super_env$settings_envir
        isDataImported <<- TRUE
        updateSelectizeInput(
          session,
          "choose_series",
          choices = data_storage_envir$sheet_names,
          selected = data_storage_envir$sheet_names[1]
        )

        #     })
        if (exists("markers", envir = settings_envir)) {
          createInputMarkerPlot()
        }

        updateSeriesSelectizeInputs("Boxplot")
        updateSeriesSelectizeInputs("Ratioplot")
        updateSeriesSelectizeInputs("SingleTraces")
        updateSeriesSelectizeInputs("MatTraces")
        updateSeriesSelectizeInputs("MedianTraces")
        updateSeriesSelectizeInputs("PPlot")
        updateSeriesSelectizeInputs("StitchedPlot")
        isDataImported <<- TRUE
      },
      error = function(e) {
        easyShinyAlarm(
          title = "",
          text = HTML(paste0("Data could not be loaded: <br/>"), e),
          type = "warning"
        )
      }
    )


    output$StatusDataImport <- renderText({
      if (isDataImported == TRUE) {
        paste0("Data import succesfull!")
      } else {
        paste0("Data import not succesfull!")
      }
    })
  })



  importData <- function(quickImport) {
    isDataImported <- FALSE
    process_new_IVs(
      spar = input$spar,
      quickImport = quickImport,
      forceThroughZero = input$inputForceReversal,
      revPotential = input$revPotential,
      splitFit = input$inputSplit,
      splitPotential = input$revPotential_split,
      exportCurrent = input$inputMarkers,
      markers = c(
        input$CurrentMarker_minmin,
        input$CurrentMarker_minmax,
        input$CurrentMarker_maxmin,
        input$CurrentMarker_maxmax
      )
    )
    process_existing_summary()
    if (input$inputMarkers == TRUE) {
      createInputMarkerPlot()
    }

    updateSelectizeInput(
      session,
      "choose_series",
      choices = data_storage_envir$sheet_names,
      selected = data_storage_envir$sheet_names[1]
    )
    if (input$inputAutoVoltageRamp) {
      changeIVVoltageRamp(
        settings_envir$voltage_unit,
        input$rampUnit_New,
        settings_envir$ramp_data,
        c(input$rampMin_New, input$rampMax_New),
        settings_envir$step,
        input$rampStep_New,
        settings_envir$length_Vramp,
        input$rampLength_New
      )
    }

    updateSeriesSelectizeInputs("Boxplot")
    updateSeriesSelectizeInputs("Ratioplot")
    updateSeriesSelectizeInputs("SingleTraces")
    updateSeriesSelectizeInputs("MatTraces")
    updateSeriesSelectizeInputs("MedianTraces")
    updateSeriesSelectizeInputs("PPlot")
    updateSeriesSelectizeInputs("StitchedPlot")


    isDataImported <<- TRUE
  }

  observeEvent(input$choose_series, {
    updateSelectizeInput(session,
      "choose_IV",
      choices = names(data_storage_envir$IV_list[[input$choose_series]])
    )
  })


  output$IV_table <-
    DT::renderDataTable(data_storage_envir$IV_list[[input$choose_series]][[input$choose_IV]],
      options = list(
        escape = F,
        scrollX = T,
        scrollY = 300
      )
    )

  output$Summary_table <-
    DT::renderDataTable(data_storage_envir$summary_list[[input$choose_series]],
      options = list(
        escape = F,
        scrollX = T,
        scrollY = 300
      )
    )
  #####

  ##### Export Data#####
  observeEvent(input$Save_rData, {
    saveData("rData")
  })

  observeEvent(input$Save_asc, {
    saveData("asc")
  })

  observeEvent(input$Save_xlsx, {
    saveData("xlsx")
  })
  #####
  ##### Color List#####
  #### Boxplots
  observeEvent(input$Boxplot_colors, {
    colorSelected$Boxplot <<- input$Boxplot_colors
  })



  observe({
    col <- input$Boxplot_colorPicker
    colorChoices$Boxplot <<- c(colorChoices$Boxplot, col)
    updateSelectizeInput(
      inputId = "Boxplot_colors",
      choices = colorChoices$Boxplot,
      selected = colorSelected$Boxplot
    )
    isolate(updateTextInput(session, inputId = "Boxplot_colorsText",
                            label = NULL,
                            value  = c(input$Boxplot_colorsText,col)))
  })



  ####
  #### Ratioplots
  observeEvent(input$Ratioplot_colors, {
    colorSelected$Ratioplot <<- input$Ratioplot_colors
  })
  observe({
    col <- input$Ratioplot_colorPicker
    colorChoices$Ratioplot <<- c(colorChoices$Ratioplot, col)
    updateSelectizeInput(
      inputId = "Ratioplot_colors",
      choices = colorChoices$Ratioplot,
      selected = colorSelected$Ratioplot
    )
    isolate(updateTextInput(session, inputId = "Ratioplot_colorsText",
                            label = NULL,
                            value  = c(input$Ratioplot_colorsText,col)))
  })
  ####
  #### SingleTraces
  observeEvent(input$SingleTraces_colors, {
    colorSelected$SingleTraces <<- input$SingleTraces_colors
  })
  observe({
    col <- input$SingleTraces_colorPicker
    colorChoices$SingleTraces <<- c(colorChoices$SingleTraces, col)
    updateSelectizeInput(
      inputId = "SingleTraces_colors",
      choices = colorChoices$SingleTraces,
      selected = colorSelected$SingleTraces
    )
    isolate(updateTextInput(session, inputId = "SingleTraces_colorsText",
                            label = NULL,
                            value  = c(input$SingleTraces_colorsText,col)))
  })
  ####
  #### MatTraces
  observeEvent(input$MatTraces_colors, {
    colorSelected$MatTraces <<- input$MatTraces_colors
  })
  observe({
    col <- input$MatTraces_colorPicker
    colorChoices$MatTraces <<- c(colorChoices$MatTraces, col)
    updateSelectizeInput(
      inputId = "MatTraces_colors",
      choices = colorChoices$MatTraces,
      selected = colorSelected$MatTraces
    )
    isolate(updateTextInput(session, inputId = "MatTraces_colorsText",
                            label = NULL,
                            value  = c(input$MatTraces_colorsText,col)))
  })
  ####
  #### MedianTraces
  observeEvent(input$MedianTraces_colors, {
    colorSelected$MedianTraces <<- input$MedianTraces_colors
  })
  observe({
    col <- input$MedianTraces_colorPicker
    colorChoices$MedianTraces <<- c(colorChoices$MedianTraces, col)
    updateSelectizeInput(
      inputId = "MedianTraces_colors",
      choices = colorChoices$MedianTraces,
      selected = colorSelected$MedianTraces
    )
    isolate(updateTextInput(session, inputId = "MedianTraces_colorsText",
                            label = NULL,
                            value  = c(input$MedianTraces_colorsText,col)))
  })
  ####
  #### PPlot
  observeEvent(input$PPlot_colors, {
    colorSelected$PPlot <<- input$PPlot_colors
  })
  observe({
    col <- input$PPlot_colorPicker
    colorChoices$PPlot <<- c(colorChoices$PPlot, col)
    updateSelectizeInput(
      inputId = "PPlot_colors",
      choices = colorChoices$PPlot,
      selected = colorSelected$PPlot
    )
    isolate(updateTextInput(session, inputId = "PPlot_colorsText",
                            label = NULL,
                            value  = c(input$PPlot_colorsText,col)))
  })
  ####
  #### StitchedPlot
  observeEvent(input$StitchedPlot_colors, {
    colorSelected$StitchedPlot <<- input$StitchedPlot_colors
  })
  observe({
    col <- input$StitchedPlot_colorPicker
    colorChoices$StitchedPlot <<- c(colorChoices$StitchedPlot, col)
    updateSelectizeInput(
      inputId = "StitchedPlot_colors",
      choices = colorChoices$StitchedPlot,
      selected = colorSelected$StitchedPlot
    )
    isolate(updateTextInput(session, inputId = "StitchedPlot_colorsText",
                            label = NULL,
                            value  = c(input$StitchedPlot_colorsText,col)))
  })

  #####
  ##### Load Packages
  observeEvent(input$tabs, {
    if (input$tabs %in% c(
      "BoxPlots",
      "Single Traces",
      "MatTraces",
      "Median Traces",
      "P-Plot",
      "Stitched Plot"
    )) {
      show_modal_progress_circle(text = "Loading packages")
      sapply(supportPackages, activatePackages)
      remove_modal_progress()
    }
  })

  ##### Settings Packages Install Packages######
  observeEvent(input$InstallPackages, {
    withProgress(message = "Installing packages", value = 0, {
      n <- length(supportPackages)

      for (i in 1:n) {
        output$SupportPackages_Version <- renderTable({
          data.frame(
            "Package" = supportPackages,
            "Version" = sapply(supportPackages, checkPackageVersion)
          )
        })
        if (!require(supportPackages[i], character.only = T)) {
          install.packages(supportPackages[i])
        }

        incProgress(1 / n, detail = paste("Installing", supportPackages[i]))
      }
    })
  })
  #####

  ##### Show Plots#####
  ##### BoxPlots#####
  observeEvent(input$Boxplot_UpdatePlot, {
    if (input$Boxplot_PreviewPlot == FALSE) {
      buildPlot("Boxplot")
      changeYaxis("Boxplot")
      changeXaxisAngle("Boxplot")
    }
    displayPlot(plot_list$Boxplot, "Boxplot_", preview = FALSE)
  })

  observe({
    if (input$Boxplot_PreviewPlot == TRUE) {
      buildPlot("Boxplot")
      changeYaxis("Boxplot")
      changeXaxisAngle("Boxplot")
      displayPlot(plot_list$Boxplot, "Boxplot_", preview = TRUE)
    }
  })


  observeEvent(input$Boxplot_Plotly, {
    displayPlotly(plot_list$Boxplot, "Boxplot_")
  })

  ##### Ratioplots#####
  observeEvent(input$Ratioplot_UpdatePlot, {
    if (input$Ratioplot_PreviewPlot == FALSE) {
      buildPlot("Ratioplot")
      changeYaxis("Ratioplot")
      changeXaxisAngle("Ratioplot")
    }
    displayPlot(plot_list$Ratioplot, "Ratioplot_", preview = FALSE)
  })

  observe({
    if (input$Ratioplot_PreviewPlot == TRUE) {
      buildPlot("Ratioplot")
      changeYaxis("Ratioplot")
      changeXaxisAngle("Ratioplot")
      displayPlot(plot_list$Ratioplot, "Ratioplot_", preview = TRUE)
    }
  })

  observeEvent(input$Ratioplot_Plotly, {
    displayPlotly(plot_list$Ratioplot, "Ratioplot_")
  })

  ##### SingleTraces#####
  observeEvent(input$SingleTraces_UpdatePlot, {
    if (input$SingleTraces_PreviewPlot == FALSE) {
      buildPlot("SingleTraces")
    }
    if (input$legend_seperate_SingleTraces == T) {
      displayPlot(plot_list_woLegend$SingleTraces, "SingleTraces_", preview = FALSE)
    } else {
      displayPlot(plot_list$SingleTraces, "SingleTraces_", preview = FALSE)
    }
  })

  observe({
    if (input$SingleTraces_PreviewPlot == TRUE) {
      buildPlot("SingleTraces")
      if (input$legend_seperate_SingleTraces == T) {
        displayPlot(plot_list_woLegend$SingleTraces, "SingleTraces_", preview = TRUE)
      } else {
        displayPlot(plot_list$SingleTraces, "SingleTraces_", preview = TRUE)
      }
    }
  })

  observeEvent(input$SingleTraces_Plotly, {
    if (input$legend_seperate_SingleTraces == T) {
      displayPlotly(plot_list_woLegend$SingleTraces, "SingleTraces_")
    } else {
      displayPlotly(plot_list$SingleTraces, "SingleTraces_")
    }
  })

  ##### MatTraces#####
  observeEvent(input$MatTraces_UpdatePlot, {
    if (input$MatTraces_PreviewPlot == FALSE) {
      buildPlot("MatTraces")
    }
    if (input$legend_seperate_MatTraces == T) {
      displayPlot(plot_list_woLegend$MatTraces, "MatTraces_", preview = FALSE)
    } else {
      displayPlot(plot_list$MatTraces, "MatTraces_", preview = FALSE)
    }
  })

  observe({
    if (input$MatTraces_PreviewPlot == TRUE) {
      buildPlot("MatTraces")
      if (input$legend_seperate_MatTraces == T) {
        displayPlot(plot_list_woLegend$MatTraces, "MatTraces_", preview = TRUE)
      } else {
        displayPlot(plot_list$MatTraces, "MatTraces_", preview = TRUE)
      }
    }
  })

  observeEvent(input$MatTraces_Plotly, {
    if (input$legend_seperate_MatTraces == T) {
      displayPlotly(plot_list_woLegend$MatTraces, "MatTraces_")
    } else {
      displayPlotly(plot_list$MatTraces, "MatTraces_")
    }
  })
  ##### MedianTraces#####
  observeEvent(input$MedianTraces_UpdatePlot, {
    if (input$MedianTraces_PreviewPlot == FALSE) {
      buildPlot("MedianTraces")
    }
    if (input$legend_seperate_MedianTraces == T) {
      displayPlot(plot_list_woLegend$MedianTraces, "MedianTraces_", preview = FALSE)
    } else {
      displayPlot(plot_list$MedianTraces, "MedianTraces_", preview = FALSE)
    }
  })

  observe({
    if (input$MedianTraces_PreviewPlot == TRUE) {
      buildPlot("MedianTraces")
      if (input$legend_seperate_MedianTraces == T) {
        displayPlot(plot_list_woLegend$MedianTraces, "MedianTraces_", preview = TRUE)
      } else {
        displayPlot(plot_list$MedianTraces, "MedianTraces_", preview = TRUE)
      }
    }
  })

  observeEvent(input$MedianTraces_Plotly, {
    if (input$legend_seperate_MedianTraces == T) {
      displayPlotly(plot_list_woLegend$MedianTraces, "MedianTraces_")
    } else {
      displayPlotly(plot_list$MedianTraces, "MedianTraces_")
    }
  })

  ##### PPlot#####
  observeEvent(input$PPlot_UpdatePlot, {
    if (input$PPlot_PreviewPlot == FALSE) {
      buildPlot("PPlot")
    }
    if (input$legend_seperate_PPlot == T) {
      displayPlot(plot_list_woLegend$PPlot, "PPlot_", preview = FALSE)
    } else {
      displayPlot(plot_list$PPlot, "PPlot_", preview = FALSE)
    }
  })

  observe({
    if (input$PPlot_PreviewPlot == TRUE) {
      buildPlot("PPlot")
      if (input$legend_seperate_PPlot == T) {
        displayPlot(plot_list_woLegend$PPlot, "PPlot_", preview = TRUE)
      } else {
        displayPlot(plot_list$PPlot, "PPlot_", preview = TRUE)
      }
    }
  })

  observeEvent(input$PPlot_Plotly, {
    if (input$legend_seperate_PPlot == T) {
      displayPlotly(plot_list_woLegend$PPlot, "PPlot_")
    } else {
      displayPlotly(plot_list$PPlot, "PPlot_")
    }
  })
  ##### StitchedPlot#####
  observeEvent(input$StitchedPlot_UpdatePlot, {
    if (input$StitchedPlot_PreviewPlot == FALSE) {
      buildPlot("StitchedPlot")
    }
    if (input$legend_seperate_StitchedPlot == T) {
      displayPlot(plot_list_woLegend$StitchedPlot, "StitchedPlot_", preview = FALSE)
    } else {
      displayPlot(plot_list$StitchedPlot, "StitchedPlot_", preview = FALSE)
    }
  })

  observe({
    if (input$StitchedPlot_PreviewPlot == TRUE) {
      buildPlot("StitchedPlot")
      if (input$legend_seperate_StitchedPlot == T) {
        displayPlot(plot_list_woLegend$StitchedPlot, "StitchedPlot_", preview = TRUE)
      } else {
        displayPlot(plot_list$StitchedPlot, "StitchedPlot_", preview = TRUE)
      }
    }
  })

  observeEvent(input$StitchedPlot_Plotly, {
    if (input$legend_seperate_StitchedPlot == T) {
      displayPlotly(plot_list_woLegend$StitchedPlot, "StitchedPlot_")
    } else {
      displayPlotly(plot_list$StitchedPlot, "StitchedPlot_")
    }
  })
  #####
  ##### Save Plots#####

  observeEvent(input$Boxplot_Save_plot, {
    savePlots(plot_list$Boxplot, "Boxplot_")
  })

  observeEvent(input$Boxplot_Modal_save_Plot, {
    quickSavePlot(plot_list$Boxplot, "Boxplot")
  })

  observeEvent(input$Boxplot_Modal_save_Plot_R, {
    savePlotasR(plot_list$Boxplot, "Boxplot")
  })

  observeEvent(input$Ratioplot_Save_plot, {
    savePlots(plot_list$Ratioplot, "Ratioplot_")
  })

  observeEvent(input$Ratioplot_Modal_save_Plot, {
    quickSavePlot(plot_list$Ratioplot, "Ratioplot_")
  })

  observeEvent(input$Ratioplot_Modal_save_Plot_R, {
    savePlotasR(plot_list$Ratioplot, "Ratioplot_")
  })

  observeEvent(input$SingleTraces_Save_plot, {
    if (input$legend_seperate_SingleTraces == TRUE) {
      savePlots(plot_list_woLegend$SingleTraces, "SingleTraces_")
      savePlots(legend_list$SingleTraces, "SingleTraces_","_Legend")
    } else {
      savePlots(plot_list$SingleTraces, "SingleTraces_")
    }
  })

  observeEvent(input$SingleTraces_Modal_save_Plot, {
    if (input$legend_seperate_SingleTraces == TRUE) {
      quickSavePlot(plot_list_woLegend$SingleTraces, "SingleTraces_")
      quickSavePlot(legend_list$SingleTraces, "SingleTraces_","_Legend")
    } else {
      quickSavePlot(plot_list$SingleTraces, "SingleTraces_")
    }
  })

  observeEvent(input$SingleTraces_Modal_save_Plot_R, {
    savePlotasR(list(plot_list$SingleTraces, legend_list$SingleTraces, plot_list_woLegend$SingleTraces), "SingleTraces_")
  })

  observeEvent(input$MatTraces_Save_plot, {
    if (input$legend_seperate_MatTraces == TRUE) {
      savePlots(plot_list_woLegend$MatTraces, "MatTraces_")
      savePlots(legend_list$MatTraces, "MatTraces_","_Legend")
    } else {
      savePlots(plot_list$MatTraces, "MatTraces_")
    }
  })

  observeEvent(input$MatTraces_Modal_save_Plot, {
    if (input$legend_seperate_MatTraces == TRUE) {
      quickSavePlot(plot_list_woLegend$MatTraces, "MatTraces_")
      quickSavePlot(legend_list$MatTraces, "MatTraces_", "_Legend")
    } else {
      quickSavePlot(plot_list$MatTraces, "MatTraces_")
    }
  })

  observeEvent(input$MatTraces_Modal_save_Plot_R, {
    savePlotasR(list(plot_list$MatTraces, legend_list$MatTraces, plot_list_woLegend$MatTraces), "MatTraces_")
  })

  observeEvent(input$MedianTraces_Save_plot, {
    if (input$legend_seperate_MedianTraces == TRUE) {
      savePlots(plot_list_woLegend$MedianTraces, "MedianTraces_")
      savePlots(legend_list$MedianTraces, "MedianTraces_", "_Legend")
    } else {
      savePlots(plot_list$MedianTraces, "MedianTraces_")
    }
  })

  observeEvent(input$MedianTraces_Modal_save_Plot, {
    if (input$legend_seperate_MedianTraces == TRUE) {
      quickSavePlot(plot_list_woLegend$MedianTraces, "MedianTraces_")
      quickSavePlot(legend_list$MedianTraces, "MedianTraces_", "_Legend")
    } else {
      quickSavePlot(plot_list$MedianTraces, "MedianTraces_")
    }
  })

  observeEvent(input$MedianTraces_Modal_save_Plot_R, {
    savePlotasR(list(plot_list$MedianTraces, legend_list$MedianTraces, plot_list_woLegend$MedianTraces), "MedianTraces_")
  })

  observeEvent(input$PPlot_Save_plot, {
    if (input$legend_seperate_PPlot == TRUE) {
      savePlots(plot_list_woLegend$PPlot, "PPlot_")
      savePlots(legend_list$PPlot, "PPlot_","_Legend")
    } else {
      savePlots(plot_list$PPlot, "PPlot_")
    }
  })

  observeEvent(input$PPlot_Modal_save_Plot, {
    if (input$legend_seperate_PPlot == TRUE) {
      quickSavePlot(plot_list_woLegend$PPlot, "PPlot_")
      quickSavePlot(legend_list$PPlot, "PPlot_","_Legend")
    } else {
      quickSavePlot(plot_list$PPlot, "PPlot_")
    }
  })

  observeEvent(input$PPlot_Modal_save_Plot_R, {
    savePlotasR(list(plot_list$PPlot, legend_list$PPlot, plot_list_woLegend$PPlot), "PPlot_")
  })

  observeEvent(input$StitchedPlot_Save_plot, {
    if (input$legend_seperate_StitchedPlot == TRUE) {
      savePlots(plot_list_woLegend$StitchedPlot, "StitchedPlot_")
      savePlots(legend_list$StitchedPlot, "StitchedPlot_", "_Legend")
    } else {
      savePlots(plot_list$StitchedPlot, "StitchedPlot_")
    }
  })

  observeEvent(input$StitchedPlot_Modal_save_Plot, {
    if (input$legend_seperate_StitchedPlot == TRUE) {
      quickSavePlot(plot_list_woLegend$StitchedPlot, "StitchedPlot_")
      quickSavePlot(legend_list$StitchedPlot, "StitchedPlot_", "_Legend")
    } else {
      quickSavePlot(plot_list$StitchedPlot, "StitchedPlot_")
    }
  })

  observeEvent(input$StitchedPlot_Modal_save_Plot_R, {
    savePlotasR(list(plot_list$StitchedPlot, legend_list$StitchedPlot, plot_list_woLegend$StitchedPlot), "StitchedPlot_")
  })
  #####

  ##### Marker Selection#####
  observeEvent(input$markerSelection, {
    if (input$markerSelection == "upramp100") {
      updateNumericInput(session, "CurrentMarker_minmin", value = 50)
      updateNumericInput(session, "CurrentMarker_minmax", value = 249)
      updateNumericInput(session, "CurrentMarker_maxmin", value = 2251)
      updateNumericInput(session, "CurrentMarker_maxmax", value = 2450)

      updateNumericInput(session, "rampMin_Original", value = -0.1)
      updateNumericInput(session, "rampMax_Original", value = 0.1)
      updateNumericInput(session, "rampLength_Original", value = 2001)
      updateNumericInput(session, "rampStep_Original", value = 0.0001)
      updateNumericInput(session, "rampUnit_Original", value = "V")

      updateNumericInput(session, "rampMin_New", value = -100)
      updateNumericInput(session, "rampMax_New", value = 100)
      updateNumericInput(session, "rampLength_New", value = 2001)
      updateNumericInput(session, "rampStep_Newl", value = 0.1)
      updateNumericInput(session, "rampUnit_New", value = "mV")

      changeVoltrageRampData(
        input$rampUnit_Original,
        c(input$rampMin_Original, input$rampMax_Original),
        input$rampStep_Original,
        input$rampLength_Original
      )
    }
    if (input$markerSelection == "ultrafast") {
      updateNumericInput(session, "CurrentMarker_minmin", value = 30)
      updateNumericInput(session, "CurrentMarker_minmax", value = 34)
      updateNumericInput(session, "CurrentMarker_maxmin", value = 95)
      updateNumericInput(session, "CurrentMarker_maxmax", value = 98)

      updateNumericInput(session, "rampMin_Original", value = -0.1)
      updateNumericInput(session, "rampMax_Original", value = 0.1)
      updateNumericInput(session, "rampLength_Original", value = 51)
      updateNumericInput(session, "rampStep_Original", value = 0.004)
      updateNumericInput(session, "rampUnit_Original", value = "V")

      updateNumericInput(session, "rampMin_New", value = -100)
      updateNumericInput(session, "rampMax_New", value = 100)
      updateNumericInput(session, "rampLength_New", value = 51)
      updateNumericInput(session, "rampStep_New", value = 4)
      updateNumericInput(session, "rampUnit_New", value = "mV")

      changeVoltrageRampData(
        input$rampUnit_Original,
        c(input$rampMin_Original, input$rampMax_Original),
        input$rampStep_Original,
        input$rampLength_Original
      )
    }
  })

  createInputMarkerPlot <- function() {
    origExample <- data_storage_envir$origExample
    origExample$`CurrentIn[A]` <-
      origExample$`CurrentIn[A]` * 10^12
    origExample$`Time[s]` <- origExample$`Time[s]` * 1000
    colnames(origExample)[2:3] <- c("Time[ms]", "CurrentIn[pA]")

    markerPlot <-
      ggplot(origExample) +
      geom_line(aes(x = `Time[ms]`, y = `CurrentIn[pA]`)) +
      theme_chris_IV_analysis(10, 8) +
      theme(
        axis.line.x = element_line(colour = "black", linetype = "solid"),
        axis.line.y = element_line(colour = "black", linetype = "solid")
      ) +
      scale_x_continuous(
        breaks = scales::pretty_breaks(n = 10),
        name = "Time (ms)",
        sec.axis = dup_axis(
          breaks = c(
            min(origExample$`Time[ms]`[origExample$`Potential[V]` == -0.1]),
            max(origExample$`Time[ms]`[origExample$`Potential[V]` == -0.1]),
            max(origExample$`Time[ms]`[origExample$`Potential[V]` == 0]),
            min(origExample$`Time[ms]`[origExample$`Potential[V]` == 0.1]),
            max(origExample$`Time[ms]`[origExample$`Potential[V]` == 0.1])
          ),
          labels = c(-100, -100, 0, 100, 100),
          name = "Potential (mV)"
        )
      ) +
      ylab("CurrentIn (pA)")

    for (i in 1:4) {
      markerPlot <-
        add_vertical_line_to_plot(markerPlot, origExample$`Time[ms]`[which(origExample$Index == settings_envir$markers[i])], col = "red")
    }

    output$markerplot <- renderImage(
      {
        img <- htmltools::capturePlot(
          {
            markerPlot
          },
          height = 500,
          width = 1200,
          res = 300
        )
        list(
          src = img,
          width = 600,
          height = 250
        )
      },
      deleteFile = TRUE
    )
  }
  #####

  ##### Packages#####
  checkPackageVersion <- function(package) {
    tryCatch(
      paste(packageVersion(package)),
      error = function(e) {
        return("Not installed")
      }
    )
  }

  activatePackages <- function(package) {
    tryCatch(
      library(package, character.only = T),
      error = function(e) {
        easyShinyAlarm(
          title = "",
          text = HTML(paste0(
            package, " could not be loaded: <br/>", e
          )),
          type = "error"
        )
      }
    )
  }
  #####



  saveData <- function(type) {
    tryCatch(
      {
        switch(type,
          "asc" = {
            save_IV_list(
              data_storage_envir$IV_list,
              data_storage_envir$IV_names,
              data_storage_envir$sheet_names
            )
          },
          "xlsx" = {
            save_tbl_list_as_excel(
              data_storage_envir$summary_list,
              data_storage_envir$sheet_names
            )
            # fileName <-
            #   gfile("Save Data",
            #         type = "save",
            #         filter = c('Excel' = 'xlsx'))
          },
          "rData" = {
            fp <- gfile(
              type = "save",
              filter = c("rData" = ".rData")
            )

            if (length(fp) == 0) {
              easyShinyAlarm(
                title = "", text = HTML(paste0("Data has not been saved!")),
                type = "warning"
              )
              return()
            }
            fp <- paste0(fp, ".rdata")

            save(data_storage_envir,
              data_envir,
              settings_envir,
              file = fp
            )
          },
          {
            print("Something went wrong")
          }
        )
        easyShinyAlarm(
          title = "",
          text = HTML(
            paste0("Succesfully saved .", type, " data! <br/> In: ", getwd())
          ),
          type = "success"
        )
      },
      error = function(e) {
        easyShinyAlarm(
          title = "",
          text = HTML(
            paste0("Could not save data! <br/> ", e)
          ),
          type = "error"
        )
      }
    )
  }

  quickSavePlot <- function(curPlot, plotType, name_suffix = "") {
    rand <- sample(1:21, 2)
    name <-
      paste0(nameGenListAdj[rand[1]], nameGenListNouns[rand[2]], "_", plotType, name_suffix,".png")

    ggsave(
      paste0(name),
      curPlot,
      device = "png",
      width = input[[paste0(plotType, "Save_width")]],
      height = input[[paste0(plotType, "Save_height")]],
      dpi = 300,
      units = "mm",
      limitsize = F
    )

    easyShinyAlarm(
      title = "",
      text = HTML(paste0(
        "Succesfully saved :", name, " <br/> In: ", getwd()
      )),
      type = "success"
    )
  }

  savePlotasR <- function(curPlot, plotType) {
    name <- paste0(
      gfile(
        type = "save",
        filter = c("rData" = ".rData")
      ),
      ".rData"
    )

    save(curPlot, file = name)

    easyShinyAlarm(
      title = "",
      text = HTML(paste0(
        "Succesfully saved .", name, " <br/> In: ", getwd()
      )),
      type = "success"
    )
  }


  easyShinyAlarm <- function(...) {
    shinyalert(
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      html = TRUE,
      showConfirmButton = FALSE,
      showCancelButton = FALSE,
      imageUrl = "",
      animation = TRUE,
      ...
    )
  }


  quickTryCatchLogWithVar <- function(...) {
    ## First Argument is the function, second argument the variable for the function, third is the error text!
    argList <- list(...)
    tryCatchLog(
      {
        argList[[1]](quickImport = argList[[2]])
      },
      error = function(e) {
        easyShinyAlarm(
          title = "",
          text = paste0(argList[[3]], e),
          type = "error"
        )
      },
#      write.error.dump.file = TRUE,
      include.compact.call.stack = TRUE,
#      write.error.dump.folder = file.path(path, "Error Dumps"),
      include.full.call.stack = FALSE
    )
  }

  ##### Plots#####
  buildPlot <- function(plotType) {
    tryCatch(
      {
        plotDataList <- markOutlierForPlots(plotType)
        IV_list <- plotDataList[[2]]
        summary_list <- plotDataList[[1]]

        if (!is.null(input[[paste0("Significance_", plotType)]])) {
          statTests <- selectTestForPlot(input[[paste0("Significance_", plotType)]])
        }

        if (input[[paste0(plotType,"_switchColors")]] == TRUE) {
          if(any(str_detect(str_split(input[[paste0(plotType, "_colorsText")]], ",", simplify= T), "(#\\w{6})")) == FALSE) {
            showModal(
              modalDialog(
                title = "Error!",
                "Colors are not submitted correctly!",
                easyClose = TRUE
              )
            )
            return()
          }
          used_colors <- str_split(input[[paste0(plotType, "_colorsText")]], ",", simplify= T)
        } else
        {
          used_colors = input[[paste0(plotType, "_colors")]]

        }


        if (plotType == "Boxplot") {
          compareVector <-
            c(
              "Errorbars",
              "Jitter",
              "Violins",
              "Observations",
              "P-Values",
              "Significances",
              "Median"
            )
          extrasVec <- compareVector %in% input$Boxplot_Extras_CD

          Boxplot_Series <- input$Boxplot_Series

          if (!invalid(input$Boxplot_AddUp) &&
            (input$Boxplot_AddUp > length(Boxplot_Series))) {
            Boxplot_Series <-
              c(
                Boxplot_Series,
                rep(
                  Boxplot_Series[1],
                  input$Boxplot_AddUp - length(Boxplot_Series)
                )
              )

            if (length(input$Names_Boxplot) != input$Boxplot_AddUp) {
              showModal(
                modalDialog(
                  title = "Error!",
                  "When using Spacer Boxes names must be submitted",
                  easyClose = TRUE
                )
              )
              return()
            }
          }

          summary_list <-
            flatten(map(Boxplot_Series, function(x, y) {
              y[x]
            }, summary_list))

          column_list <- map_depth(flatten(
            map(Boxplot_Series, function(x, y) {
              y[x]
            }, data_storage_envir$column_list)
          ), 2, function(x) {
            paste0("CD_Corrected_", x)
          })

          if (input$Include_Bef_Boxplot == TRUE) {
            column_list <-
              column_list
          } else {
            column_list <- map(column_list, function(x) {
              x[-1]
            })
          }

          if (length(input$Names_Boxplot) == length(summary_list)) {
            names(summary_list) <- input$Names_Boxplot
            names(column_list) <- input$Names_Boxplot
          }



          currentPlot <- createPlot(
            "Boxplot",
            plot_type = "Boxplot",
            summary_list = summary_list,
            column_list = column_list,
            jittersize = input$Jitter_Size_CD,
            size = (input$Fontsize_Boxplot / (14 / 5)) * input$`P-Value_Size_Boxplot`,
            errorbar = extrasVec[1],
            jitter = extrasVec[2],
            violin = extrasVec[3],
            observation = extrasVec[4],
            p_value = extrasVec[5],
            significance = extrasVec[6],
            showMedian = extrasVec[7],
            paired = statTests[2],
            alphaV = input$Alpha_Extra_CD,
            placement_type = input$placement_type_CD,
            scale_relative = input$relative_placement_scale_obs_CD,
            scale_relative_sig = input$relative_placement_scale_sig_CD,
            scale_relative_sig_min = input$relative_placement_scale_sig_CD_Min,
            distance_p_value_stars = input$distance_p_value_stars_CD,
            kruskal = statTests[4],
            compIntern = statTests[3],
            used_colors = used_colors
          )
          plot_list[[plotType]] <<- currentPlot[[1]]
        }

        if (plotType == "Ratioplot") {
          compareVector <-
            c(
              "Errorbars",
              "Jitter",
              "Violins",
              "Observations",
              "P-Values",
              "Significances",
              "Median"
            )
          extrasVec <- compareVector %in% input$Ratioplot_Extras_CD

          Ratioplot_Series <- input$Ratioplot_Series

          if (!invalid(input$Ratioplot_AddUp) &&
            (input$Ratioplot_AddUp > length(Ratioplot_Series))) {
            Ratioplot_Series <-
              c(
                Ratioplot_Series,
                rep(
                  Ratioplot_Series[1],
                  input$Ratioplot_AddUp - length(Ratioplot_Series)
                )
              )

            if (length(input$Names_Ratioplot) != input$Ratioplot_AddUp) {
              showModal(
                modalDialog(
                  title = "Error!",
                  "When using Spacer Boxes names must be submitted",
                  easyClose = TRUE
                )
              )
              return()
            }
          }

          summary_list <-
            flatten(map(Ratioplot_Series, function(x, y) {
              y[x]
            }, summary_list))

          peak_list <- map_depth(flatten(
            map(Ratioplot_Series, function(x, y) {
              y[x]
            }, data_storage_envir$peak_list)
          ), 2, function(x) {
            return(x)
          })


          if (input$Include_Bef_Ratioplot == TRUE) {
            peak_list <-
              peak_list
          } else {
            peak_list <- map_depth(peak_list, 2, function(x) {
              x[-1]
            })
          }

          if (length(input$Names_Ratioplot) == length(summary_list)) {
            names(summary_list) <- input$Names_Ratioplot
            names(peak_list) <- input$Names_Ratioplot
          }

          currentPlot <- createPlot(
            "Ratioplot",
            plot_type = "RatioPlot",
            summary_list = summary_list,
            peak_list = peak_list,
            jittersize = input$Jitter_Size_Ratioplot,
            size = (input$Fontsize_Ratioplot / (14 / 5)) * input$`P-Value_Size_Ratioplot`,
            errorbar = extrasVec[1],
            jitter = extrasVec[2],
            violin = extrasVec[3],
            observation = extrasVec[4],
            p_value = extrasVec[5],
            significance = extrasVec[6],
            showMedian = extrasVec[7],
            paired = statTests[2],
            alphaV = input$Alpha_Extra_Ratioplot,
            placement_type = input$placement_type_Ratioplot,
            scale_relative = input$relative_placement_scale_obs_Ratioplot,
            scale_relative_sig = input$relative_placement_scale_sig_Ratioplot,
            scale_relative_sig_min = input$relative_placement_scale_sig_Ratioplot_Min,
            distance_p_value_stars = input$distance_p_value_stars_Ratioplot,
            kruskal = statTests[4],
            compIntern = statTests[3],
            used_colors = used_colors
          )
          plot_list[[plotType]] <<- currentPlot[[1]]
        }

        if (plotType == "SingleTraces") {
          if (!(length(input$SingleTraces_Series) > 0 &&
            input$SingleTraces_Meas != "None")) {
            return()
          }

          IV_list <-
            map(input$SingleTraces_Meas, function(x) {
              return(data_storage_envir$IV_list[[input$SingleTraces_Series]][[x]])
            })

          names(IV_list) <- input$SingleTraces_Meas

          if (sum(unlist(map(IV_list, is.null))) > 0) {
            return()
          }

          currentPlot <- createPlot(
            "SingleTraces",
            plot_type = "SingleIV",
            IV_list = IV_list,
            column_list = input$Selected_Column_SingleTraces,
            measurement_name = input$Names_SingleTraces,
            splitted = input$split_plot_SingleTraces,
            overrideYlim = c(
              input$SingleTraces_YlimMin,
              input$SingleTraces_YlimMax
            ),
            display_N_legend = FALSE,
            used_colors = used_colors
          )
          plot_list[[plotType]] <<- currentPlot[[1]]
          plot_list_woLegend[[plotType]] <<- currentPlot[[2]]
          legend_list[[plotType]] <<- currentPlot[[3]]
        }


        if (plotType == "MatTraces") {
          if (length(input$MatTraces_Series) == 0) {
            return()
          }
          if (length(input$Selected_Column_MatTraces) == 0 ||
            input$Selected_Column_MatTraces == "None") {
            return()
          }


          if (sum(unlist(map(IV_list, is.null))) > 0) {
            return()
          }

          currentPlot <- createPlot(
            "MatTraces",
            plot_type = "Matplot",
            IV_list = IV_list,
            column_list = input$Selected_Column_MatTraces,
            peak_list = trimws(map(
              input$MatTraces_Series,
              word,
              start = -1,
              sep = "-"
            )),
            series_vector = flatten(
              map(
                input$MatTraces_Series,
                word,
                start = 1,
                end = -2,
                sep = "-"
              )
            ),
            measurement_name = input$Names_MatTraces,
            splitted = input$split_plot_MatTraces,
            display_N_legend = TRUE,
            overrideYlim = c(input$MatTraces_YlimMin, input$MatTraces_YlimMax),
            used_colors = used_colors
          )
          plot_list[[plotType]] <<- currentPlot[[1]]
          plot_list_woLegend[[plotType]] <<- currentPlot[[2]]
          legend_list[[plotType]] <<- currentPlot[[3]]
        }

        if (plotType == "MedianTraces") {
          if (length(input$MedianTraces_Series) == 0) {
            return()
          }
          if (length(input$Selected_Column_MedianTraces) == 0 ||
            input$Selected_Column_MedianTraces == "None") {
            return()
          }


          if (sum(unlist(map(IV_list, is.null))) > 0) {
            return()
          }

          currentPlot <- createPlot(
            "MedianTraces",
            plot_type = "MedianTraces",
            IV_list = IV_list,
            column_list = input$Selected_Column_MedianTraces,
            peak_list = trimws(map(
              input$MedianTraces_Series,
              word,
              start = -1,
              sep = "-"
            )),
            series_vector = flatten(
              map(
                input$MedianTraces_Series,
                word,
                start = 1,
                end = -2,
                sep = "-"
              )
            ),
            measurement_name = input$Names_MedianTraces,
            splitted = input$split_plot_MedianTraces,
            overrideYlim = c(input$MedianTraces_YlimMin, input$MedianTraces_YlimMax),
            used_colors = used_colors,
            display_N_legend = TRUE,
            SD_MAD = input$Deviations_MedianTraces,
            SD_MAD_shadow_direction = unlist(strsplit(input$SD_MAD_Shadow_MedianTraces, ","))
          )
          plot_list[[plotType]] <<- currentPlot[[1]]
          plot_list_woLegend[[plotType]] <<- currentPlot[[2]]
          legend_list[[plotType]] <<- currentPlot[[3]]
        }

        if (plotType == "PPlot") {
          if (length(input$PPlot_Series) == 0) {
            return()
          }
          if (length(input$Selected_Column_PPlot) == 0 ||
            input$Selected_Column_PPlot == "None") {
            return()
          }


          if (sum(unlist(map(IV_list, is.null))) > 0) {
            return()
          }


          currentPlot <- createPlot(
            "PPlot",
            plot_type = "P-plot",
            IV_list = IV_list,
            column_list = input$Selected_Column_PPlot,
            peak_list = trimws(map(
              input$PPlot_Series,
              word,
              start = -1,
              sep = "-"
            )),
            series_vector = flatten(
              map(
                input$PPlot_Series,
                word,
                start = 1,
                end = -2,
                sep = "-"
              )
            ),
            measurement_name = input$Names_PPlot,
            splitted = input$split_plot_PPlot,
            overrideYlim = c(input$PPlot_YlimMin, input$PPlot_YlimMax),
            used_colors = used_colors,
            SD_MAD = input$Deviations_PPlot,
            SD_MAD_shadow_direction = unlist(strsplit(input$SD_MAD_Shadow_PPlot, ",")),
            ratio_Median_P_plot = input$PlotRatio_PPlot,
            n_spacer = input$Spacer_Plots_PPlot,
            display_N_legend = TRUE,
            kruskal = statTests[4],
            paired = statTests[2]
          )
          plot_list[[plotType]] <<- currentPlot[[1]]
          plot_list_woLegend[[plotType]] <<- currentPlot[[2]]
          legend_list[[plotType]] <<- currentPlot[[3]]
        }

        if (plotType == "StitchedPlot") {
          if (length(input$StitchedPlot_Series) == 0) {
            return()
          }
          # if (length(input$Selected_Column_StitchedPlot) == 0 ||
          #     input$Selected_Column_StitchedPlot == "None")
          #   return()


          if (sum(unlist(map(IV_list, is.null))) > 0) {
            return()
          }

          currentPlot <- createPlot(
            "StitchedPlot",
            plot_type = "Complete",
            IV_list = IV_list,
            column_list = c(
              input$Selected_Column_StitchedPlot1,
              input$Selected_Column_StitchedPlot2,
              input$Selected_Column_StitchedPlot3
            ),
            peak_list = trimws(map(
              input$StitchedPlot_Series,
              word,
              start = -1,
              sep = "-"
            )),
            series_vector = flatten(
              map(
                input$StitchedPlot_Series,
                word,
                start = 1,
                end = -2,
                sep = "-"
              )
            ),
            measurement_name = input$Names_StitchedPlot,
            splitted = input$split_plot_StitchedPlot,
            overrideYlim = list(
              c(input$StitchedPlot_YlimMin1, input$StitchedPlot_YlimMax1),
              c(input$StitchedPlot_YlimMin2, input$StitchedPlot_YlimMax2),
              c(input$StitchedPlot_YlimMin3, input$StitchedPlot_YlimMax3),
              c(input$StitchedPlot_YlimMin4, input$StitchedPlot_YlimMax4),
              c(input$StitchedPlot_YlimMin5, input$StitchedPlot_YlimMax5)
            ),
            used_colors = used_colors,
            SD_MAD = input$Deviations_StitchedPlot,
            SD_MAD_shadow_direction = unlist(strsplit(input$SD_MAD_Shadow_StitchedPlot, ",")),
            ratio_Median_P_plot = input$PlotRatio_StitchedPlot,
            n_spacer = input$Spacer_Plots_StitchedPlot,
            display_N_legend = TRUE,
            kruskal = statTests[4],
            paired = statTests[2]
          )

          plot_list[[plotType]] <<- currentPlot[[1]]
          plot_list_woLegend[[plotType]] <<- currentPlot[[2]]
          legend_list[[plotType]] <<- currentPlot[[3]]
        }
      },
      error = function(e) {
        easyShinyAlarm(
          title = "",
          text = HTML(paste0("Plot could not be created: <br/>", e)),
          type = "error"
        )
      }
    )
  }

  changeYaxis <- function(plotType) {
    if (!is.na(input[[paste0(plotType, "_YlimMin")]]) &&
      (!is.na(input[[paste0(plotType, "_YlimMax")]]))) {
      plot_list[[plotType]] <<- plot_list[[plotType]] +
        ylim(input[[paste0(plotType, "_YlimMin")]], input[[paste0(plotType, "_YlimMax")]])
    }
  }

  changeXaxisAngle <- function(plotType) {
    angle <- input[[paste0(plotType, "_X_Axis_Angle")]]
    if (angle == 0) {
      plot_list[[plotType]] <<- plot_list[[plotType]] +
        theme(axis.text.x = element_text(
          angle = angle,
          hjust = 0.5,
          vjust = 0.5
        ))
      return()
    }
    plot_list[[plotType]] <<- plot_list[[plotType]] +
      theme(
        axis.text.x = element_text(
          angle = angle,
          hjust = 1,
          vjust = 0.5
        ),
        plot.margin = margin(10, 0, 15, 0)
      )
  }

  markOutlierForPlots <- function(plotType) {
    if (!isTruthy(input[[paste0(plotType, "_SummaryOutlier")]] != "None")) {
      return(
        list(
          summary_list = data_storage_envir$summary_list,
          IV_list = data_storage_envir$IV_list
        )
      )
    }


    summary_list <- data_storage_envir$summary_list
    if (input[[paste0(plotType, "_SummaryOutlier")]] != "None") {
      summary_list <-
        map(
          data_storage_envir$summary_list,
          deselect_outlier,
          input[[paste0(plotType, "_SummaryOutlier")]]
        )
    }

    IV_list <- data_storage_envir$IV_list

    if (input[[paste0(plotType, "_IVOutlier")]] != "None") {
      show_modal_spinner() # show the modal window
      OutlierIV_list <-
        detect_IV_outlier(
          IV_list,
          data_storage_envir$IV_names,
          input[[paste0(plotType, "_IVOutlier")]],
          100
        )
      OutlierCombined_list <-
        get_complete_outlier(
          OutlierIV_list$outlier_tibble,
          data_storage_envir$IV_list,
          data_storage_envir$IV_names,
          data_storage_envir$summary_list,
          "Outlier"
        )
      IV_list <- OutlierCombined_list$IV_list

      IV_list <-
        summary_list <-
        map(
          OutlierCombined_list$summary_list,
          deselect_outlier,
          "Outlier"
        )
      # remove it when done
      remove_modal_spinner()
    }

    return(list(summary_list, IV_list))
  }

  selectTestForPlot <- function(testInput) {
    testList <- c(
      "Mann-Whitney U",
      "Paired_SigTest",
      "Paired_SigTest_Intern",
      "Kruskal_Wallis"
    )

    testOutput <- c(
      "Mann-Whitney U" = FALSE,
      "Paired_SigTest" = FALSE,
      "Paired_SigTest_Intern" = FALSE,
      "Kruskal_Wallis" = FALSE
    )
    testOutput[which(testList %in% testInput)] <- TRUE

    return(testOutput)
  }
  createPlot <- function(plotType, ...) {
    newPlotList <-
      create_plot(
        linesize = input[[paste0("Line_Size_", plotType)]],
        used_fontsize = input[[paste0("Fontsize_", plotType)]],
        used_font = input[[paste0("Font_", plotType)]],
        ylab = TeX(yAxisLabelChoices(input[[paste0(plotType, "_Used_yLabel")]])),
        used_axis_function = axisChoices(input[[paste0(plotType, "_Used_Axis")]]),
        used_theme = themeChoices(input[[paste0(plotType, "_Used_theme")]]),
        display_N_legend_divisor = length(data_storage_envir$IV_peaks[[1]]),
        ...
      )

    if (input[[paste0("SetDimensions_", plotType)]] == TRUE) {
      tryCatch(
        {
          newPlotList <- map(newPlotList, set_dim, dimensions_list[[plotType]])
        },
        error = function(e) {
          easyShinyAlarm(HTML(paste0(
            "Can't set the plot dimensions: <br/>", e
          )),
          type = "error"
          )
        }
      )
    }

    return(newPlotList)
  }

  displayPlot <- function(curPlot, curPlotType, preview) {
    if (preview == TRUE) {
      output[[paste0(curPlotType, "PreviewPlot")]] <- renderImage(
        {
          # A temp file to save the output. It will be deleted after renderImage
          # sends it, because deleteFile=TRUE.
          outfile <- tempfile(fileext = ".png")
          # Generate a png
          ggsave(
            outfile,
            curPlot,
            device = "png",
            width = input[[paste0(curPlotType, "Save_width")]],
            height = input[[paste0(curPlotType, "Save_height")]],
            dpi = 300,
            units = "mm",
            limitsize = F
          )
          # Return a list
          list(
            src = outfile,
            alt = "This is alternate text",
            width = input[[paste0(curPlotType, "Save_width")]] * 0.5 * 11.811023622,
            height = input[[paste0(curPlotType, "Save_height")]] * 0.5 * 11.811023622
          )
        },
        deleteFile = TRUE
      )
    }

    output[[paste0(curPlotType, "Plot")]] <- renderImage(
      {
        # A temp file to save the output. It will be deleted after renderImage
        # sends it, because deleteFile=TRUE.
        outfile <- tempfile(fileext = ".png")
        # Generate a png
        ggsave(
          outfile,
          curPlot,
          device = "png",
          width = input[[paste0(curPlotType, "Save_width")]],
          height = input[[paste0(curPlotType, "Save_height")]],
          dpi = 300,
          units = "mm",
          limitsize = F
        )
        # Return a list
        list(
          src = outfile,
          alt = "This is alternate text"
        )
      },
      deleteFile = TRUE
    )
  }

  displayPlotly <- function(curPlot, curPlotType) {
    output[[paste0(curPlotType, "Plotly_Plot")]] <- renderPlotly(
      ggplotly(
        curPlot + ylab(""),
        width = as.numeric(input[[paste0("Boxplot_", "Save_width")]]) * 11.811023622,
        height = as.numeric(input[[paste0("Boxplot_", "Save_height")]]) * 11.811023622
      )
    )
  }

  savePlots <- function(curPlot, curPlotType, name_suffix = "") {
    ggsave(
      paste0(input[[paste0(curPlotType, "saveName")]], name_suffix ,".", input[[paste0(curPlotType, "Filetype")]]),
      curPlot,
      device = ggplotSavedevice(input[[paste0(curPlotType, "Filetype")]]),
      width = input[[paste0(curPlotType, "Save_width")]],
      height = input[[paste0(curPlotType, "Save_height")]],
      dpi = 300,
      units = "mm",
      limitsize = F
    )
  }

  ##### Observer for Plots######
  updateSeriesSelectizeInputs <- function(plotType) {
    if (plotType == "MatTraces") {
      updateSelectizeInput(session,
        paste0(plotType, "_Series"),
        choices = flatten(map2(
          names(data_storage_envir$IV_list),
          flatten(data_storage_envir$peak_list),
          paste,
          sep = "-"
        ))
      )
      return()
    }

    if (plotType == "MedianTraces") {
      updateSelectizeInput(session,
        paste0(plotType, "_Series"),
        choices = flatten(map2(
          names(data_storage_envir$IV_list),
          flatten(data_storage_envir$peak_list),
          paste,
          sep = "-"
        ))
      )
      return()
    }

    if (plotType == "PPlot") {
      updateSelectizeInput(session,
        paste0(plotType, "_Series"),
        choices = flatten(map2(
          names(data_storage_envir$IV_list),
          flatten(data_storage_envir$peak_list),
          paste,
          sep = "-"
        ))
      )
      return()
    }


    if (plotType == "StitchedPlot") {
      updateSelectizeInput(session,
        paste0(plotType, "_Series"),
        choices = flatten(map2(
          names(data_storage_envir$IV_list),
          flatten(data_storage_envir$peak_list),
          paste,
          sep = "-"
        ))
      )
      return()
    }
    updateSelectizeInput(session,
      paste0(plotType, "_Series"),
      choices =  data_storage_envir$sheet_names
    )
  }


  ggplotSavedevice <- function(device) {
    return(switch(device,
      "png" = {
        "png"
      },
      "tiff" = {
        "tiff"
      },
      "svg" = {
        svglite
      },
      "pdf" = {
        CairoPDF
      },
      "emf" = {
        "emf"
      },
      {
        input$yLab
      }
    ))
  }

  changeIVVoltageRamp <-
    function(oldUnit,
             newUnit,
             oldRampData,
             newRampData,
             oldSteps,
             newSteps,
             oldLength,
             newLength) {
      if (length(seq(oldRampData[1], oldRampData[2], oldSteps)) != length(seq(newRampData[1], newRampData[2], newSteps))) {
        easyShinyAlarm(
          text = "Cant convert the voltage ramp. Ramp length is not the same!",
          type = "error"
        )
        return(0)
      }

      conversionFactor <- newSteps / oldSteps

      data_storage_envir$IV_list <-
        change_potential_unit(
          data_storage_envir$IV_list,
          oldUnit,
          newUnit,
          conversionFactor
        )
      return(1)
    }

  changeVoltrageRampData <- function(unit, rampData, steps, length) {
    settings_envir$length_Vramp <- length
    settings_envir$ramp_data <- rampData
    settings_envir$step <- steps
    settings_envir$voltage_unit <- unit
  }

  #####
  ##### Observer for settings#####
  observeEvent(input$ChangeVoltageRamp_Original, {
    changeVoltrageRampData(
      input$rampUnit_Original,
      c(input$rampMin_Original, input$rampMax_Original),
      input$rampStep_Original,
      input$rampLength_Original
    )
  })

  observeEvent(input$Switch_Voltage_Ramp_Manual, {
    mark <-
      changeIVVoltageRamp(
        settings_envir$voltage_unit,
        input$rampUnit_Manual,
        settings_envir$ramp_data,
        c(input$rampMin_Manual, input$rampMax_Manual),
        settings_envir$step,
        input$rampStep_Manual,
        settings_envir$length_Vramp,
        input$rampLength_Manual
      )

    if (mark == 1) {
      easyShinyAlarm(
        title = "",
        text = "Succesfully switched Voltage Ramp Data",
        type = "success"
      )
    }
  })

  yAxisLabelChoices <- function(name) {
    return(switch(name,
      "Boxplot" = {
        add_phantom_supersubscript("Curr. dens. (pA pF$^{-1}$)")
      },
      "Ratio" = {
        add_phantom_supersubscript("Curr. dens.$_{max}$ / curr. dens.$_{min}$")
      },
      "IV" = {
        add_phantom_supersubscript("Curr. dens. (pA pF$^{-1}$)")
      },
      "Normalized IV" = {
        add_phantom_supersubscript("Curr. dens.$_{norm}$ (%)")
      },
      "Slope" = {
        add_phantom_supersubscript("Norm. G$_{slope}")
      },
      "Complete-Plot" = {
        cp1 <- switch(input$Selected_Column_StitchedPlot1,
          "CurrentDensity[pA/pF]" = {
            add_phantom_supersubscript("Curr. dens. (pA pF$^{-1}$)")
          },
          "normalized_CurrentDensity" = {
            add_phantom_supersubscript("Curr. dens.$_{norm}$ (%)")
          },
          "fitted_normalized_CurrentDensity" = {
            add_phantom_supersubscript("Curr. dens.$_{norm}$ (%)")
          },
          "normalized_slopeConductance" = {
            add_phantom_supersubscript("Norm. G$_{slope}")
          },
          {
            ""
          }
        )
        cp2 <- switch(input$Selected_Column_StitchedPlot2,
          "CurrentDensity[pA/pF]" = {
            add_phantom_supersubscript("Curr. dens. (pA pF$^{-1}$)")
          },
          "normalized_CurrentDensity" = {
            add_phantom_supersubscript("Curr. dens.$_{norm}$ (%)")
          },
          "fitted_normalized_CurrentDensity" = {
            add_phantom_supersubscript("Curr. dens.$_{norm}$ (%)")
          },
          "normalized_slopeConductance" = {
            add_phantom_supersubscript("Norm. G$_{slope}")
          },
          {
            ""
          }
        )
        cp3 <- NULL
        if (!is.null(input$Selected_Column_StitchedPlot3)) {
          cp3 <- switch(input$Selected_Column_StitchedPlot3,
            "CurrentDensity[pA/pF]" = {
              add_phantom_supersubscript("Curr. dens. (pA pF$^{-1}$)")
            },
            "normalized_CurrentDensity" = {
              add_phantom_supersubscript("Curr. dens.$_{norm}$ (%)")
            },
            "fitted_normalized_CurrentDensity" = {
              add_phantom_supersubscript("Curr. dens.$_{norm}$ (%)")
            },
            "normalized_slopeConductance" = {
              add_phantom_supersubscript("Norm. G$_{slope}")
            },
            {
              ""
            }
          )
        }
        c(cp1, cp2, cp3)
      },
      {
        name
      }
    ))
  }

  ##### Plot Dimensions
  savePlotDimensions <- function(plotType) {
    if (is.null(plot_list[[plotType]])) {
      easyShinyAlarm(
        title = "",
        text = "No plot to save dimensions from found!",
        type = "Error"
      )

      return()
    }

    saveRDS(get_dim(plot_list[[plotType]]),
      file = paste0(gfile(
        type = "save", initial.filename = "Dimensions"
      ), ".RDS")
    )
  }

  loadPlotDimensions <- function(plotType) {
    file <-
      gfile(type = "open", filter = list("RDS files" = list(
        patterns = c("*.RDS")
      )))
    try(
      dimensions_list[[plotType]] <<- readRDS(file)
    )
  }

  observeEvent(input$UploadDimensions, {
    loadPlotDimensions(input$SelectDimensions)
  })

  observeEvent(input$SaveDimensions, {
    savePlotDimensions(input$SelectDimensions)
  })

  #####
  ##### Observers#####
  observe(
    updateSelectizeInput(
      session,
      "SingleTraces_Meas",
      choices = names(data_storage_envir$IV_list[[input$SingleTraces_Series]]),
      selected = names(data_storage_envir$IV_list[[input$SingleTraces_Series]][1])
    )
  )

  observeEvent(input$split_plot_StitchedPlot, {
    if (input$split_plot_StitchedPlot == FALSE) {
      disable("Selected_Column_StitchedPlot3")
      disable("StitchedPlot_YlimMin3")
      disable("StitchedPlot_YlimMax3")
      disable("StitchedPlot_YlimMin4")
      disable("StitchedPlot_YlimMax4")
      disable("StitchedPlot_YlimMin5")
      disable("StitchedPlot_YlimMax5")
    }

    if (input$split_plot_StitchedPlot == TRUE) {
      enable("Selected_Column_StitchedPlot3")
      enable("StitchedPlot_YlimMin3")
      enable("StitchedPlot_YlimMax3")
      enable("StitchedPlot_YlimMin4")
      enable("StitchedPlot_YlimMax4")
      enable("StitchedPlot_YlimMin5")
      enable("StitchedPlot_YlimMax5")
    }
  })
  ##### SD MAD Shadows#####

  observeEvent(input$SD_MAD_Shadow_MedianTraces, {
    if (invalid(input$SD_MAD_Shadow_MedianTraces)) {
      return()
    }
    if (any(grepl("[^-+]", unlist(
      strsplit(input$SD_MAD_Shadow_MedianTraces, ",")
    ))) == TRUE) {
      text <- unlist(strsplit(input$SD_MAD_Shadow_MedianTraces, ","))[-grep("[^-+]", unlist(strsplit(input$SD_MAD_Shadow_MedianTraces, ",")))]
      text <- paste0(text, collapse = ",")
      updateTextInput(session, "SD_MAD_Shadow_MedianTraces", value = text)
      showModal(modalDialog(
        title = "Error!",
        "Only + - and , are accepted signs",
        easyClose = TRUE
      ))
    }
  })

  observeEvent(input$SD_MAD_Shadow_PPlot, {
    if (invalid(input$SD_MAD_Shadow_PPlot)) {
      return()
    }
    if (any(grepl("[^-+]", unlist(
      strsplit(input$SD_MAD_Shadow_PPlot, ",")
    ))) == TRUE) {
      text <- unlist(strsplit(input$SD_MAD_Shadow_PPlot, ","))[-grep("[^-+]", unlist(strsplit(input$SD_MAD_Shadow_PPlot, ",")))]
      text <- paste0(text, collapse = ",")
      updateTextInput(session, "SD_MAD_Shadow_PPlot", value = text)
      showModal(modalDialog(
        title = "Error!",
        "Only + - and , are accepted signs",
        easyClose = TRUE
      ))
    }
  })

  observeEvent(input$SD_MAD_Shadow_StitchedPlot, {
    if (invalid(input$SD_MAD_Shadow_StitchedPlot)) {
      return()
    }
    if (any(grepl("[^-+]", unlist(
      strsplit(input$SD_MAD_Shadow_StitchedPlot, ",")
    ))) == TRUE) {
      text <- unlist(strsplit(input$SD_MAD_Shadow_StitchedPlot, ","))[-grep("[^-+]", unlist(strsplit(input$SD_MAD_Shadow_StitchedPlot, ",")))]
      text <- paste0(text, collapse = ",")
      updateTextInput(session, "SD_MAD_Shadow_StitchedPlot", value = text)
      showModal(modalDialog(
        title = "Error!",
        "Only + - and , are accepted signs",
        easyClose = TRUE
      ))
    }
  })
  ##### BoxSidebar####
  observeEvent(input$showSidebar_StitchedPlot, {
    updateBoxSidebar("ylimSidebar_StitchedPlot")
  })
  observeEvent(input$hideSidebar_StitchedPlot, {
    updateBoxSidebar("ylimSidebar_StitchedPlot")
  })
  #####

  if (!interactive()) {
    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
  }
})
