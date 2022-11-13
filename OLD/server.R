options("guiToolkit" = "tcltk")


# prepare_packages()

enableBookmarking("server")
server <- function(input, output, session) {
  showModal(modalDialog(
    title = "Obacht!",
    HTML(
      "Neue Funktion für automatisches ausfüllen der Summary-Tabelle! <br>
      Um alte Tabellen zu importieren unter Settings ausschalten!"
    ),
    easyClose = TRUE,
    fade = FALSE,
  ))

  output$splitPlotExtraOutput <-
    renderUI(
      selectInput(
        inputId = 'Selected_CP_plot3',
        "Selected Column",
        choices = "None",
        "None"
      )
    )
  output$splitPlotExtraOutput <-
    renderUI(NULL)


  output$IV_data <-
    DT::renderDataTable(data_storage_envir$IV_list[[input$choose_series]][[input$choose_IV]],
                        options = list(
                          escape = F,
                          scrollX = T,
                          scrollY = 450
                        ))

  output$Summary_data <-
    DT::renderDataTable(data_storage_envir$summary_list[[input$choose_series]],
                        options = list(
                          escape = F,
                          scrollX = T,
                          scrollY = 500
                        ))


  themeChoices <- reactive({
    return(switch(
      input$yLab,
      "Boxplot" = {
        add_phantom_supersubscript("Curr. dens$.$ (pA pF$^-$$^1$)")
      },
      "Ratio" = {
        add_phantom_supersubscript("Curr. dens$.$_m_a_x $/$ curr. dens$.$_m_i_n")
      },
      "IV" = {
        add_phantom_supersubscript("Curr. dens$.$ (pA pF$^-^1$)")
      },
      "Slope" = {
        add_phantom_supersubscript("G_s_l_o_p_e (curr.dens$.$_n_o_r_m mV$^-^1$)")
      },
      "Complete-Plot" = {
        cp1 <- switch(
          input$Selected_CP_plot,
          "CurrentDensity[pA/pF]" = {
            add_phantom_supersubscript("Curr. dens$.$ (pA pF$^-$$^1$)")
          },
          "normalized_CurrentDensity" = {
            add_phantom_supersubscript("Curr. dens$.$_n_o_r_m (%)")
          },
          "fitted_normalized_CurrentDensity" = {
            add_phantom_supersubscript("Curr. dens$.$_n_o_r_m (%)")
          },
          "normalized_slopeConductance" = {
            add_phantom_supersubscript("Norm. G_s_l_o_p_e")
          },
          {
            ""
          }
        )
        cp2 <-  switch(
          input$Selected_CP_plot2,
          "CurrentDensity[pA/pF]" = {
            add_phantom_supersubscript("Curr. dens. (pA pF$^-^1$)")
          },
          "normalized_CurrentDensity" = {
            add_phantom_supersubscript("Curr. dens$.$_n_o_r_m (%)")
          },
          "fitted_normalized_CurrentDensity" = {
            add_phantom_supersubscript("Curr. dens$.$_n_o_r_m (%)")
          },
          "normalized_slopeConductance" = {
            add_phantom_supersubscript("Norm. G_s_l_o_p_e")
          },
          {
            ""
          }
        )

        cp3 <- NULL
        if (!is.null(input$Selected_CP_plot3)) {
          cp3 <-  switch(
            input$Selected_CP_plot3,
            "CurrentDensity[pA/pF]" = {
              add_phantom_supersubscript("Curr. dens. (pA pF$^-^1$)")
            },
            "normalized_CurrentDensity" = {
              add_phantom_supersubscript("Curr. dens$.$_n_o_r_m (%)")
            },
            "fitted_normalized_CurrentDensity" = {
              add_phantom_supersubscript("Curr. dens$.$_n_o_r_m (%)")
            },
            "normalized_slopeConductance" = {
              add_phantom_supersubscript("Norm. G_s_l_o_p_e")
            },
            {
              ""
            }
          )
        }
        c(cp1, cp2, cp3)

      },
      {
        input$yLab
      }

    ))
  })


  ggplotSavedevice <- reactive({
    return(switch(
      input$Filetype,
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
        "pdf"
      },
      {
        input$yLab
      }

    ))
  })


  utheme <- reactive({
    return(switch(
      input$Used_theme,
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
  })


  axisChoices <- reactive({
    return(switch(
      input$Used_Axis,
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
  })


  observeEvent(input$V2mV, {
    settings_envir$step <- input$Steps
    data_storage_envir$IV_list <-
      change_potential_unit(data_storage_envir$IV_list, "V" , "mV", 1000)
  })



  observeEvent(input$loadDimensions, {
    file <-
      gfile(type = "open", list("R files" = list(patterns = c("*.Rdata"))))
    data_storage_envir$dimensions <- readRDS(file)

    dimName <-
      last(stri_split_fixed(last(
        stri_split_fixed(file, "\\", simplify = T)
      ), ".", simplify = T))

    updateSelectizeInput(session,
                         "Used_Dimension",
                         choices =  c("None", dimName))


  })

  observeEvent(input$getDimensions, {
    if (!exists("active_plot", data_storage_envir))
    {
      showModal(
        modalDialog(
          title = "Error!",
          "No Plot to get Dimensions from found",
          easyClose = TRUE
        )
      )
      return()
    }
    saveRDS(get_dim(data_storage_envir$active_plot),
            file = paste0(gfile(
              type = "save", initial.filename = ".RDS"
            ), ".RDS"))


  })

  # observeEvent(input$saveInputs, {
  #   data_storage_envir$inputList <- lapply(reactiveValuesToList(input), unclass)
  #   saveRDS(data_storage_envir$inputList, file = paste0(gfile(
  #     type = "save", initial.filename = ".RDS"
  #   ), ".RDS"))
  # })
  #
  # observeEvent(input$loadInputs, {
  #
  #
  #   file <- gfile(type = "open", list("R files" = list(
  #     patterns = c("*.Rdata"))))
  #   data_storage_envir$inputList <- readRDS(file)
  #
  #
  #   if (exists("inputList", data_storage_envir)) {
  #     lapply(names(data_storage_envir$inputList),
  #            function(x) session$sendInputMessage(x, list(value = data_storage_envir$inputList[[x]]))
  #     )
  #   }
  #
  # })



  observeEvent(input$Import_Data, {
    tryCatch({
      process_new_IVs(
        spar = input$spar,
        quickImport = F,
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
      ) #spar = Freiheitsgrad der splines
      updateSelectizeInput(session, "choose_series", choices = data_storage_envir$sheet_names)
      process_existing_summary()
      updateSelectizeInput(session, "choose_series", choices = data_storage_envir$sheet_names)


      updateSelectizeInput(session,
                           "Boxplot_Series",
                           choices =  data_storage_envir$sheet_names)
      updateSelectizeInput(session,
                           "Ratio_Series",
                           choices =  data_storage_envir$sheet_names)
      updateSelectizeInput(
        session,
        "SIV_Series",
        choices = data_storage_envir$sheet_names,
        selected = data_storage_envir$sheet_names[1]
      )


      updateSelectizeInput(session,
                           "MIV_Series",
                           choices = flatten(
                             map2(
                               names(data_storage_envir$IV_list),
                               data_storage_envir$IV_peaks,
                               paste,
                               sep = "-"
                             )
                           ))
      updateSelectizeInput(session,
                           "P_plot_Series",
                           choices = flatten(
                             map2(
                               names(data_storage_envir$IV_list),
                               data_storage_envir$IV_peaks,
                               paste,
                               sep = "-"
                             )
                           ))
      updateSelectizeInput(session,
                           "CP_plot_Series",
                           choices = flatten(
                             map2(
                               names(data_storage_envir$IV_list),
                               data_storage_envir$IV_peaks,
                               paste,
                               sep = "-"
                             )
                           ))

      if(input$inputMarkers == T) {
      origExample <- data_storage_envir$origExample
      origExample$`CurrentIn[A]` <- origExample$`CurrentIn[A]` * 10 ^ 12
      origExample$`Time[s]` <- origExample$`Time[s]` * 1000
      colnames(origExample)[2:3] <- c("Time[ms]", "CurrentIn[pA]")

      markerPlot <-
        ggplot(origExample) + geom_line(aes(x = `Time[ms]`, y = `CurrentIn[pA]`)) +
        theme_chris_IV_analysis(22,20) +
        theme(
          axis.line.x = element_line(colour = "black", linetype = "solid"),
          axis.line.y = element_line(colour = "black", linetype = "solid")
        ) +
        scale_x_continuous(breaks = scales::pretty_breaks(n = 10), name = "Time (ms)",
                           sec.axis = dup_axis(
          breaks = c(min(origExample$`Time[ms]`[origExample$`Potential[V]` == -0.1]),max(origExample$`Time[ms]`[origExample$`Potential[V]` == -0.1]), max(origExample$`Time[ms]`[origExample$`Potential[V]` == 0]),min(origExample$`Time[ms]`[origExample$`Potential[V]` == 0.1]), max(origExample$`Time[ms]`[origExample$`Potential[V]` == 0.1])),
          labels = c(-100, -100, 0, 100, 100),
          name = "Potential (mV)"
        ))+
        ylab("CurrentIn (pA)")

      for (i in 1:4)
        markerPlot <-
        add_vertical_line_to_plot(markerPlot, origExample$`Time[ms]`[which(origExample$Index == settings_envir$markers[i])], col = "red")

      output$markerplot <-  renderImage({
        img <- htmltools::capturePlot({
          markerPlot
        }, height = 400, width = 1200)
        list(src = img, width = 600, height = 200)
      }, deleteFile = TRUE)
      }
    },
    error = function(e) {
      showModal(
        modalDialog(
          title = "Error!",
          "Reading in data did not work. Check your .asc files,  the summary table and if their naming is correct!",
          easyClose = TRUE
        )
      )
    })

  })

  observeEvent(input$Quick_Import_Data, {
    tryCatch({
      process_new_IVs(
        spar = input$spar,
        quickImport = T,
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
      ) #spar = Freiheitsgrad der splines
      updateSelectizeInput(session, "choose_series", choices = data_storage_envir$sheet_names)
      process_existing_summary()
      updateSelectizeInput(session, "choose_series", choices = data_storage_envir$sheet_names)
      updateSelectizeInput(session,
                           "Boxplot_Series",
                           choices =  data_storage_envir$sheet_names)
      updateSelectizeInput(session,
                           "Ratio_Series",
                           choices =  data_storage_envir$sheet_names)


      updateSelectizeInput(
        session,
        "SIV_Series",
        choices = data_storage_envir$sheet_names,
        selected = data_storage_envir$sheet_names[1]
      )
      updateSelectizeInput(session,
                           "MIV_Series",
                           choices = flatten(
                             map2(
                               names(data_storage_envir$IV_list),
                               data_storage_envir$IV_peaks,
                               paste,
                               sep = "-"
                             )
                           ))
      updateSelectizeInput(session,
                           "P_plot_Series",
                           choices = flatten(
                             map2(
                               names(data_storage_envir$IV_list),
                               data_storage_envir$IV_peaks,
                               paste,
                               sep = "-"
                             )
                           ))
      updateSelectizeInput(session,
                           "CP_plot_Series",
                           choices = flatten(
                             map2(
                               names(data_storage_envir$IV_list),
                               data_storage_envir$IV_peaks,
                               paste,
                               sep = "-"
                             )
                           ))

      if(input$inputMarkers == T) {

      origExample <- data_storage_envir$origExample
      origExample$`CurrentIn[A]` <- origExample$`CurrentIn[A]` * 10 ^ 12
      origExample$`Time[s]` <- origExample$`Time[s]` * 1000
      colnames(origExample)[2:3] <- c("Time[ms]", "CurrentIn[pA]")

      markerPlot <-
        ggplot(origExample) + geom_line(aes(x = `Time[ms]`, y = `CurrentIn[pA]`)) +
        theme_chris_IV_analysis(22,20)  +
        theme(
          axis.line.x = element_line(colour = "black", linetype = "solid"),
          axis.line.y = element_line(colour = "black", linetype = "solid")
        ) +
        scale_x_continuous(breaks = scales::pretty_breaks(n = 10),  name = "Time (ms)",
                           sec.axis = dup_axis(
          breaks = c(min(origExample$`Time[ms]`[origExample$`Potential[V]` == -0.1]),max(origExample$`Time[ms]`[origExample$`Potential[V]` == -0.1]), max(origExample$`Time[ms]`[origExample$`Potential[V]` == 0]),min(origExample$`Time[ms]`[origExample$`Potential[V]` == 0.1]), max(origExample$`Time[ms]`[origExample$`Potential[V]` == 0.1])),
          labels = c(-100, -100, 0, 100, 100),
          name = "Potential (mV)"
        ))+
        ylab("CurrentIn (pA)")
      for (i in 1:4)
        markerPlot <-
        add_vertical_line_to_plot(markerPlot, origExample$`Time[ms]`[which(origExample$Index == settings_envir$markers[i])], col = "red")

      output$markerplot <-  renderImage({
        img <- htmltools::capturePlot({
          markerPlot
        }, height = 400, width = 1200)
        list(src = img, width = 600, height = 200)
      }, deleteFile = TRUE)
}
    }, error = function(e) {
      showModal(
        modalDialog(
          title = "Error!",
          paste0("Reading in data did not work. Check your .asc files,  the summary table and if their naming is correct!: ",e) ,
          easyClose = TRUE
        )
      )
    })

  })

  observeEvent(input$markerSelection,{
    if(input$markerSelection == "upramp100") {
    updateNumericInput(session, "CurrentMarker_minmin", value = 50)
    updateNumericInput(session, "CurrentMarker_minmax", value = 249)
    updateNumericInput(session, "CurrentMarker_maxmin", value = 2251)
    updateNumericInput(session, "CurrentMarker_maxmax", value = 2450)
    }
    if(input$markerSelection == "ultrafast") {
      updateNumericInput(session, "CurrentMarker_minmin", value = 30)
      updateNumericInput(session, "CurrentMarker_minmax", value = 34)
      updateNumericInput(session, "CurrentMarker_maxmin", value = 95)
      updateNumericInput(session, "CurrentMarker_maxmax", value = 98)
    }

  })


  observeEvent(input$inputSplit, {
    if (input$inputSplit == TRUE)
      updateSwitchInput(session, "inputForceReversal", value = FALSE)
  })

  observeEvent(input$inputForceReversal, {
    if (input$inputForceReversal == TRUE)
      updateSwitchInput(session, "inputSplit", value = FALSE)
  })

  observeEvent(input$Set_WD, {
    wd_dir <- choose.dir()
    if (!is.na(wd_dir))
      setwd(wd_dir)
  })

  observeEvent(input$Set_WD2, {
    wd_dir <- choose.dir()
    if (!is.na(wd_dir))
      setwd(wd_dir)
  })


  observeEvent(input$Save_ASC, {
    save_IV_list(
      data_storage_envir$IV_list,
      data_storage_envir$IV_names,
      data_storage_envir$sheet_names
    )
  })

  observeEvent(input$Save_xls, {
    save_tbl_list_as_excel(data_storage_envir$summary_list,
                           data_storage_envir$sheet_names)
  })

  observeEvent(input$Save_rData, {
    save(data_storage_envir,
         data_envir,
         settings_envir,
         file = paste0(gfile(
           type = "save", initial.filename = ".RData"
         ), ".rData"))
  })

  observeEvent(input$Save_plot, {
    ggsave(
      paste0(input$plotName, ".", input$Filetype),
      data_storage_envir$active_plot ,
      device = ggplotSavedevice(),
      width = input$Save_width,
      height = input$Save_heigth,
      dpi = 300,
      units = "mm",
      limitsize = F
    )
    if (exists("active_legend", envir = data_storage_envir)) {
      ggsave(
        paste0(input$plotName, "_Legend.", input$Filetype),
        data_storage_envir$active_legend ,
        device = ggplotSavedevice(),
        width = input$Save_width,
        height = input$Save_heigth,
        dpi = 300,
        units = "mm",
        limitsize = F
      )
      rm("active_legend", envir = data_storage_envir)

    }
  })

  observeEvent(input$Plottype, {
    switch(
      input$Plottype,
      "CD Boxplot" = {
        updateSelectizeInput(session, "yLab", selected =  "Boxplot")
        updateSelectizeInput(session, "Used_theme", selected = "Boxplot")
        updateSelectizeInput(session, "Used_Axis", selected = "None")
      },
      "Ratio Boxplot" = {
        updateSelectizeInput(session, "yLab", selected =  "Ratio")
        updateSelectizeInput(session, "Used_theme", selected = "Boxplot")
        updateSelectizeInput(session, "Used_Axis", selected = "None")
      },
      "Single IV Plot" = {
        updateSelectizeInput(session, "yLab", selected =  "IV")
        updateSelectizeInput(session, "Used_theme", selected = "IV")
        updateSelectizeInput(session, "Used_Axis", selected = "IV")
      },
      "Median IV Plot" = {
        updateSelectizeInput(session, "yLab", selected =  "IV")
        updateSelectizeInput(session, "Used_theme", selected = "IV")
        updateSelectizeInput(session, "Used_Axis", selected = "IV")
      },
      "P-Plot" = {
        updateSelectizeInput(session, "yLab", selected =  "Slope")
        updateSelectizeInput(session, "Used_theme", selected = "P-Plot")
        updateSelectizeInput(session, "Used_Axis", selected = "P-Plot")
      },
      "Complete-Plot" = {
        updateSelectizeInput(session, "yLab", selected =  "Complete-Plot")
        updateSelectizeInput(session, "Used_theme", selected = "Complete-Plot")
        updateSelectizeInput(session, "Used_Axis", selected = "Complete-Plot")
        updateNumericInput(session, "Save_width", value = "80")
        updateNumericInput(session, "Save_heigth", value = "160")
      },
    )
  }, priority = 10)

  observeEvent(input$split_plot_Complete, {
    if (input$split_plot_Complete == TRUE)  {
      isolate({
        updateNumericInput(session, "Save_width", value = "120")
      })

      output$splitPlotExtraOutput <-
        renderUI(
          selectInput(
            inputId = 'Selected_CP_plot3',
            "Selected Column",
            choices = c(
              "None",
              "CurrentDensity[pA/pF]",
              "smoothed_CurrentDensity",
              "normalized_CurrentDensity",
              "fitted_normalized_CurrentDensity",
              "normalized_slopeConductance"
            ),
            "None"
          )
        )
    }

    if (input$split_plot_Complete == FALSE)  {
      isolate({
        updateNumericInput(session, "Save_width", value = "80")
      })
      output$splitPlotExtraOutput <-
        renderUI(
          selectInput(
            inputId = 'Selected_CP_plot3',
            "Selected Column",
            choices = "None"
            ,
            "None"
          )
        )
      output$splitPlotExtraOutput <-
        renderUI(NULL)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$Load_rData, {
    try({
      super_env <- new.env()
      load(gfile(type = "open", list("R files" = list(
        patterns = c("*.Rdata")
      ))), envir = super_env)
      print(ls(super_env))
      data_storage_envir <<- super_env$data_storage_envir
      data_envir <<- super_env$data_envir
      settings_envir <<- super_env$settings_envir
      updateSelectizeInput(session, "choose_series", choices = data_storage_envir$sheet_names)
      updateSelectizeInput(session,
                           "Boxplot_Series",
                           choices =  data_storage_envir$sheet_names)
      updateSelectizeInput(session,
                           "Ratio_Series",
                           choices =  data_storage_envir$sheet_names)
      updateSelectizeInput(
        session,
        "SIV_Series",
        choices = data_storage_envir$sheet_names,
        selected = data_storage_envir$sheet_names[1]
      )

      updateSelectizeInput(session,
                           "MIV_Series",
                           choices = flatten(
                             map2(
                               names(data_storage_envir$IV_list),
                               data_storage_envir$IV_peaks,
                               paste,
                               sep = "-"
                             )
                           ))
      updateSelectizeInput(session,
                           "P_plot_Series",
                           choices = flatten(
                             map2(
                               names(data_storage_envir$IV_list),
                               data_storage_envir$IV_peaks,
                               paste,
                               sep = "-"
                             )
                           ))
      updateSelectizeInput(session,
                           "CP_plot_Series",
                           choices = flatten(
                             map2(
                               names(data_storage_envir$IV_list),
                               data_storage_envir$IV_peaks,
                               paste,
                               sep = "-"
                             )
                           ))
    })
  })

  observe({
    if (!grepl("(^[#]\\w{6}){1}(,[#]\\w{6})+$", input$colors))
      return()

    if (input$Plottype == "CD Boxplot") {
      if (length(utheme()) > 1 || length(themeChoices()) > 1)
        return()

      if (length(input$Boxplot_Series) > 0)
      {
        compareVector <-
          c(
            "Errorbars",
            "Jitter",
            "Violins",
            "Observations",
            "P-Values",
            "Significances"
          )
        extrasVec <- compareVector %in% input$Boxplot_Extras_CD
        summary_list <- data_storage_envir$summary_list
        sumOut <- NULL
        if (input$Outlier_Summary_CD != "None") {
          summary_list <-
            map(
              data_storage_envir$summary_list,
              deselect_outlier,
              input$Outlier_Summary_CD
            )
          sumOut <- input$Outlier_Summary
        }

        if (input$Outlier_IV_CD != "None") {
          show_modal_spinner() # show the modal window
          OutlierIV_list <-
            detect_IV_outlier(
              data_storage_envir$IV_list,
              data_storage_envir$IV_names,
              input$Outlier_IV_CD,
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
          summary_list <-
            map(OutlierCombined_list$summary_list,
                deselect_outlier,
                "Outlier")
          # remove it when done
          remove_modal_spinner()
        }


        Boxplot_Series <- input$Boxplot_Series

        if (!is.na(input$Boxplot_AddUp) &&
            (input$Boxplot_AddUp > length(Boxplot_Series))) {
          Boxplot_Series <-
            c(Boxplot_Series,
              rep(
                Boxplot_Series[1],
                input$Boxplot_AddUp - length(Boxplot_Series)
              ))

          if (length(input$Names_BoxPlot) != input$Boxplot_AddUp) {
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

        summary_list = flatten(map(Boxplot_Series, function(x, y)
          y[x], summary_list))

        column_list =  map_depth(flatten(
          map(Boxplot_Series, function(x, y)
            y[x], data_storage_envir$column_list)
        ), 2, function(x)
          paste0("CD_Corrected_", x))

        if (input$Include_Bef == TRUE)
          column_list <-
          column_list
        else
          column_list <- map(column_list, function(x)
            x[-1])



        if (length(input$Names_BoxPlot) == length(summary_list)) {
          names(summary_list) <- input$Names_BoxPlot
          names(column_list) <- input$Names_BoxPlot
        }

        plot_type <- "Boxplot"
        CD_plot <-
          create_plot(
            plot_type = plot_type,
            summary_list = summary_list,
            column_list =  column_list,
            linesize = input$Linesize,
            #size = input$`P-Value_Size_CD`,
            size = (input$Fontsize / (14 / 5)) * input$`P-Value_Size_CD`,
            jittersize = input$Jitter_Size_CD,
            used_fontsize = input$Fontsize,
            used_font = input$Used_Font,
            ylab = TeX(themeChoices()),
            used_colors = str_split(input$colors, ",")[[1]],
            used_theme = utheme(),
            errorbar = extrasVec[1],
            jitter = extrasVec[2],
            violin = extrasVec[3],
            observation = extrasVec[4],
            p_value = extrasVec[5],
            significance = extrasVec[6],
            paired = input$Paired_SigTest_CD,
            alphaV = input$Alpha_Extra_CD,
            placement_type = input$placement_type_CD,
            scale_relative = input$relative_placement_scale_obs_CD,
            scale_relative_sig = input$relative_placement_scale_sig_CD,
            scale_relative_sig_min = input$relative_placement_scale_sig_CD_Min,
            distance_p_value_stars = input$distance_p_value_stars_CD,
            kruskal = input$Kruskal_Wallis_CD

          ) + theme(
            axis.text.x = element_text(
              angle = input$X_Axis_Angle_CD,
              hjust = 1,
              vjust = 1
            ),
            plot.margin = margin(10, 0, 15, 0)
          )


        if (!is.na(input$YlimMinCD) &&
            (!is.na(input$YlimMaxCD))) {
          CD_plot <- CD_plot + ylim(input$YlimMinCD, input$YlimMaxCD)
        }
        data_storage_envir$active_plot <- CD_plot
        if (input$Used_Dimension != "None")
          data_storage_envir$active_plot <-
          set_dim(data_storage_envir$active_plot,
                  data_storage_envir$dimensions)

        # output$Boxplot_CD <- renderPlot({CD_plot}, height = input$Save_heigth*13.12205433 , width = input$Save_width*13.12205433 , res = 300)
        output$Boxplot_CD <- renderImage({
          # A temp file to save the output. It will be deleted after renderImage
          # sends it, because deleteFile=TRUE.
          outfile <- tempfile(fileext = '.png')

          # Generate a png
          ggsave(
            outfile,
            data_storage_envir$active_plot ,
            device = "png",
            width = input$Save_width,
            height = input$Save_heigth,
            dpi = 300,
            units = "mm",
            limitsize = F
          )

          # Return a list
          list(src = outfile,
               alt = "This is alternate text")
        }, deleteFile = TRUE)

      }
    }


    if (input$Plottype == "Ratio Boxplot") {
      if (length(utheme()) > 1 || length(themeChoices()) > 1)
        return()

      if (length(input$Ratio_Series) > 0)
      {
        compareVector <-
          c(
            "Errorbars",
            "Jitter",
            "Violins",
            "Observations",
            "P-Values",
            "Significances"
          )
        extrasVec <- compareVector %in% input$Ratioplot_Extras
        summary_list <- data_storage_envir$summary_list
        sumOut <- NULL
        if (input$Outlier_Summary_Ratio != "None") {
          summary_list <-
            map(
              data_storage_envir$summary_list,
              deselect_outlier,
              input$Outlier_Summary_Ratio
            )
          sumOut <- input$Outlier_Summary
        }

        if (input$Outlier_IV_Ratio != "None") {
          show_modal_spinner() # show the modal window
          OutlierIV_list <-
            detect_IV_outlier(
              data_storage_envir$IV_list,
              data_storage_envir$IV_names,
              input$Outlier_IV_Ratio,
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
          summary_list <-
            map(OutlierCombined_list$summary_list,
                deselect_outlier,
                "Outlier")
          # remove it when done
          remove_modal_spinner()

        }

        Ratio_Series <- input$Ratio_Series

        if (!is.na(input$Ratio_Addup) &&
            (input$Ratio_Addup > length(Ratio_Series))) {
          Ratio_Series <-
            c(Ratio_Series,
              rep(
                Ratio_Series[1],
                input$Ratio_Addup - length(Ratio_Series)
              ))

          if (length(input$Names_Ratio) != input$Ratio_Addup) {
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

        summary_list = flatten(map(Ratio_Series, function(x, y)
          y[x], summary_list))

        peak_list =  map_depth(flatten(
          map(Ratio_Series, function(x, y)
            y[x], data_storage_envir$peak_list)
        ), 2, function(x)
          return(x[-1]))


        if (length(input$Names_Ratio) == length(summary_list)) {
          names(summary_list) <- input$Names_Ratio
          names(peak_list) <- input$Names_Ratio
        }


        plot_type <- "Ratio"
        CD_plot <-
          create_plot(
            plot_type = plot_type,
            summary_list = summary_list,
            peak_list = peak_list,
            linesize = input$Linesize,
            size = (input$Fontsize / (14 / 5)) * input$`P-Value_Size_Ratio`,
            jittersize = input$Jitter_Size_Ratio,
            used_fontsize = input$Fontsize,
            used_font = input$Used_Font,
            ylab = TeX(themeChoices()),
            used_colors = str_split(input$colors, ",")[[1]],
            used_theme = utheme(),
            errorbar = extrasVec[1],
            jitter = extrasVec[2],
            violin = extrasVec[3],
            observation = extrasVec[4],
            p_value = extrasVec[5],
            significance = extrasVec[6],
            paired = input$Paired_SigTest_Ratio,
            alphaV = input$Alpha_Extra_Ratio,
            placement_type = input$placement_type_Ratio,
            scale_relative = input$relative_placement_scale_obs_Ratio,
            scale_relative_sig = input$relative_placement_scale_sig_Ratio,
            scale_relative_sig_min = input$relative_placement_scale_sig_Ratio_Min,
            distance_p_value_stars = input$distance_p_value_stars_Ratio



          ) + theme(axis.text.x = element_text(
            angle = input$X_Axis_Angle_Ratio,
            hjust = 1
          ))

        data_storage_envir$active_plot <- CD_plot
        if (input$Used_Dimension != "None")
          data_storage_envir$active_plot <-
          set_dim(data_storage_envir$active_plot,
                  data_storage_envir$dimensions)

        # output$Boxplot_CD <- renderPlot({CD_plot}, height = input$Save_heigth*13.12205433 , width = input$Save_width*13.12205433 , res = 300)
        output$Ratio_CD <- renderImage({
          # A temp file to save the output. It will be deleted after renderImage
          # sends it, because deleteFile=TRUE.
          outfile <- tempfile(fileext = '.png')

          # Generate a png
          ggsave(
            outfile,
            data_storage_envir$active_plot ,
            device = "png",
            width = input$Save_width,
            height = input$Save_heigth,
            dpi = 300,
            units = "mm",
            limitsize = F
          )

          # Return a list
          list(src = outfile,
               alt = "This is alternate text")
        }, deleteFile = TRUE)

      }
    }



    if (input$Plottype == "Single IV Plot") {
      if (length(utheme()) > 1 || length(themeChoices()) > 1)
        return()

      if ((length(input$SIV_Series) > 0 &&
           input$Selected_SIV != "None") &&
          (length(input$Names_SIV) == length(input$SIV_Measurement)))
      {
        IV_list <-
          map(input$SIV_Measurement, function(x)
            return(data_storage_envir$IV_list[[input$SIV_Series]][[x]]))

        if (sum(unlist(map(IV_list, is.null))) > 0)
          return()


        plot_type <- "SingleIV"
        CD_plot <-
          create_plot(
            plot_type = plot_type,
            IV_list = IV_list,
            column_list = input$Selected_SIV,
            measurement_name =  input$Names_SIV,
            linesize = input$Linesize,
            used_fontsize = input$Fontsize,
            used_font = input$Used_Font,
            used_axis_function = axisChoices(),
            ylab = TeX(themeChoices()),
            used_colors = str_split(input$colors, ",")[[1]],
            used_theme = utheme(),
            overrideYlim = c(input$YlimMin, input$YlimMax),
            splitted = input$split_plot_SIV
          )

        if (input$legend_seperate_SIV == TRUE) {
          legend <-
            get_legend(CD_plot + theme(
              legend.box.margin = margin(0, 0, 0, 0, "cm"),
              legend.title = element_blank()
            ))
          CD_plot <- CD_plot  + theme(legend.position = "none")
          data_storage_envir$active_legend <- legend
        }

        data_storage_envir$active_plot <- CD_plot
        if (input$Used_Dimension != "None")
          data_storage_envir$active_plot <-
          set_dim(data_storage_envir$active_plot,
                  data_storage_envir$dimensions)

        # output$Boxplot_CD <- renderPlot({CD_plot}, height = input$Save_heigth*13.12205433 , width = input$Save_width*13.12205433 , res = 300)
        output$SIV_Plots <- renderImage({
          # A temp file to save the output. It will be deleted after renderImage
          # sends it, because deleteFile=TRUE.
          outfile <- tempfile(fileext = '.png')

          # Generate a png
          ggsave(
            outfile,
            data_storage_envir$active_plot ,
            device = "png",
            width = input$Save_width,
            height = input$Save_heigth,
            dpi = 300,
            units = "mm",
            limitsize = F
          )

          # Return a list
          list(src = outfile,
               alt = "This is alternate text")
        }, deleteFile = TRUE)

      }
    }

    if (input$Plottype == "Median IV Plot") {
      if (length(utheme()) > 1 || length(themeChoices()) > 1)
        return()


      if (length(unlist(strsplit(input$SD_MAD_Shadow_MIV, ","))) != length(input$MIV_Series))
        return()
      if (any(grepl('[^-+]', unlist(
        strsplit(input$SD_MAD_Shadow_MIV, ",")
      ))) == TRUE)
        return()
      if (length(input$MIV_Series) > 0 &&
          (input$Selected_MIV != "None"))
      {
        IV_list <- data_storage_envir$IV_list
        summary_list <- data_storage_envir$summary_list
        sumOut <- NULL
        if (input$Outlier_Summary_MIV != "None") {
          summary_list <-
            map(
              data_storage_envir$summary_list,
              deselect_outlier,
              input$Outlier_Summary_MIV
            )
          sumOut <- input$Outlier_Summary


          if (input$Outlier_IV_MIV == "None") {
            OutlierCombined_list <-
              map_depth(data_storage_envir$IV_list, 2, function(x)
                return(FALSE))
            OutlierCombined_list <-
              map2(data_storage_envir$IV_peaks, OutlierCombined_list, function(x, y)
                return(y[1:(length(y) / length(x))]))
            OutlierCombined_list <-
              get_complete_outlier(
                OutlierCombined_list,
                data_storage_envir$IV_list,
                data_storage_envir$IV_names,
                data_storage_envir$summary_list,
                "Outlier"
              )
            IV_list <- OutlierCombined_list$IV_list
          }
        }

        if (input$Outlier_IV_MIV != "None") {
          show_modal_spinner() # show the modal window
          OutlierIV_list <-
            detect_IV_outlier(
              data_storage_envir$IV_list,
              data_storage_envir$IV_names,
              input$Outlier_IV_MIV,
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
          summary_list <-
            map(OutlierCombined_list$summary_list,
                deselect_outlier,
                "Outlier")
          IV_list <- OutlierCombined_list$IV_list
          remove_modal_spinner()

          # remove it when done
        }



        plot_type <- "Median"
        CD_plot <-
          create_plot(
            plot_type = plot_type,
            IV_list = IV_list,
            column_list = input$Selected_MIV,
            peak_list = map(
              input$MIV_Series,
              word,
              start = -1,
              sep = "-"
            ),
            series_vector = flatten(map(
              input$MIV_Series,
              word,
              start = 1,
              end = -2,
              sep = "-"
            )),
            measurement_name = input$Names_MIV,
            display_N_legend = TRUE,
            display_N_legend_divisor = length(data_storage_envir$IV_peaks[[1]]),
            SD_MAD = input$Deviation_MIV,
            SD_MAD_shadow_direction = unlist(strsplit(input$SD_MAD_Shadow_MIV, ",")),
            linesize = input$Linesize,
            used_fontsize = input$Fontsize,
            used_font = input$Used_Font,
            used_axis_function = axisChoices(),
            ylab = TeX(themeChoices()),
            used_colors = str_split(input$colors, ",")[[1]],
            used_theme = utheme(),
            alphaV = input$Alpha_Extra_MIV,
            overrideYlim = c(input$YlimMin_MIV, input$YlimMax_MIV),
            splitted = input$split_plot_MIV
          )

        if (input$legend_seperate_MIV == TRUE) {
          legend <-
            get_legend(CD_plot + theme(
              legend.box.margin = margin(0, 0, 0, 0, "cm"),
              legend.title = element_blank()
            ))
          CD_plot <- CD_plot  + theme(legend.position = "none")
          data_storage_envir$active_legend <- legend
        }




        data_storage_envir$active_plot <- CD_plot
        if (input$Used_Dimension != "None")
          data_storage_envir$active_plot <-
          set_dim(data_storage_envir$active_plot,
                  data_storage_envir$dimensions)

        # output$Boxplot_CD <- renderPlot({CD_plot}, height = input$Save_heigth*13.12205433 , width = input$Save_width*13.12205433 , res = 300)
        output$MIV_Plots <- renderImage({
          # A temp file to save the output. It will be deleted after renderImage
          # sends it, because deleteFile=TRUE.
          outfile <- tempfile(fileext = '.png')

          # Generate a png
          ggsave(
            outfile,
            data_storage_envir$active_plot ,
            device = "png",
            width = input$Save_width,
            height = input$Save_heigth,
            dpi = 300,
            units = "mm",
            limitsize = F
          )

          # Return a list
          list(src = outfile,
               alt = "This is alternate text")
        }, deleteFile = TRUE)

      }
    }

    if (input$Plottype == "P-Plot") {
      if (length(unlist(strsplit(input$SD_MAD_Shadow_P_plot, ","))) != length(input$P_plot_Series))
        return()
      if (any(grepl('[^-+]', unlist(
        strsplit(input$SD_MAD_Shadow_P_plot, ",")
      ))) == TRUE)
        return()
      if (length(input$P_plot_Series) > 1 &&
          (input$Selected_P_plot != "None"))
      {
        IV_list <- data_storage_envir$IV_list
        summary_list <- data_storage_envir$summary_list
        sumOut <- NULL
        if (input$Outlier_Summary_P_plot != "None") {
          summary_list <-
            map(
              data_storage_envir$summary_list,
              deselect_outlier,
              input$Outlier_Summary_P_plot
            )
          sumOut <- input$Outlier_Summary_P_plot


          if (input$Outlier_IV_P_plot == "None") {
            OutlierCombined_list <-
              map_depth(data_storage_envir$IV_list, 2, function(x)
                return(FALSE))
            OutlierCombined_list <-
              map2(data_storage_envir$IV_peaks, OutlierCombined_list, function(x, y)
                return(y[1:(length(y) / length(x))]))
            OutlierCombined_list <-
              get_complete_outlier(
                OutlierCombined_list,
                data_storage_envir$IV_list,
                data_storage_envir$IV_names,
                data_storage_envir$summary_list,
                "Outlier"
              )
            IV_list <- OutlierCombined_list$IV_list
          }
        }

        if (input$Outlier_IV_P_plot != "None") {
          show_modal_spinner() # show the modal window
          OutlierIV_list <-
            detect_IV_outlier(
              data_storage_envir$IV_list,
              data_storage_envir$IV_names,
              input$Outlier_IV_P_plot,
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
          summary_list <-
            map(OutlierCombined_list$summary_list,
                deselect_outlier,
                "Outlier")
          IV_list <- OutlierCombined_list$IV_list
          remove_modal_spinner()

          # remove it when done
        }



        plot_type <- "P-plot"
        CD_plot <-
          create_plot(
            plot_type = plot_type,
            IV_list = IV_list,
            column_list = input$Selected_P_plot,
            peak_list = map(
              input$P_plot_Series,
              word,
              start = -1,
              sep = "-"
            ),
            series_vector = unlist(flatten(
              map(
                input$P_plot_Series,
                word,
                start = 1,
                end = -2,
                sep = "-"
              )
            )),
            measurement_name = input$Names_P_plot,
            display_N_legend = TRUE,
            display_N_legend_divisor = length(data_storage_envir$IV_peaks[[1]]),
            SD_MAD = input$Deviation_P_plot,
            SD_MAD_shadow_direction = unlist(strsplit(input$SD_MAD_Shadow_P_plot, ",")),
            linesize = input$Linesize,
            used_fontsize = input$Fontsize,
            used_font = input$Used_Font,
            used_axis_function = axisChoices(),
            ylab = TeX(themeChoices()),
            used_colors = str_split(input$colors, ",")[[1]],
            used_theme = utheme(),
            spacer_plot = input$Spacer_Plots_P != 0,
            n_spacer = input$Spacer_Plots_P,
            ratio_Median_P_plot = input$median_P_plot_ratio,
            paired = input$Paired_SigTest_P_Plot,
            splitted = input$split_plot_P_Plot
          )

        if (input$legend_seperate_P_plot == TRUE) {
          legend <-
            get_legend(
              CD_plot[[1]] + theme(
                legend.box.margin = margin(0, 0, 0, 0, "cm"),
                legend.title = element_blank()
              )
            )
          CD_plot[[1]] <-
            CD_plot[[1]]  + theme(legend.position = "none")
          data_storage_envir$active_legend <- legend
        }




        data_storage_envir$active_plot <- CD_plot
        if (input$Used_Dimension != "None")
          data_storage_envir$active_plot <-
          set_dim(data_storage_envir$active_plot,
                  data_storage_envir$dimensions)

        # output$Boxplot_CD <- renderPlot({CD_plot}, height = input$Save_heigth*13.12205433 , width = input$Save_width*13.12205433 , res = 300)
        output$P_plot_Plots <- renderImage({
          # A temp file to save the output. It will be deleted after renderImage
          # sends it, because deleteFile=TRUE.
          outfile <- tempfile(fileext = '.png')

          # Generate a png
          ggsave(
            outfile,
            data_storage_envir$active_plot ,
            device = "png",
            width = input$Save_width,
            height = input$Save_heigth,
            dpi = 300,
            units = "mm",
            limitsize = F
          )

          # Return a list
          list(src = outfile,
               alt = "This is alternate text")
        }, deleteFile = TRUE)

      }
    }

    if (input$Plottype == "Complete-Plot") {
      if (length(unlist(strsplit(input$SD_MAD_Shadow_CP_plot, ","))) != length(input$CP_plot_Series))
        return()
      if (any(grepl('[^-+]', unlist(
        strsplit(input$SD_MAD_Shadow_CP_plot, ",")
      ))) == TRUE)
        return()

      if ((input$split_plot_Complete == TRUE) &&
          !is.null(input$Selected_CP_plot3))  {
        if ((input$Selected_CP_plot == "None") ||
            (input$Selected_CP_plot2 == "None") ||
            (input$Selected_CP_plot3 == "None"))
        {
          return()
        }
      }

      if (length(input$CP_plot_Series) > 1 &&
          (input$Selected_CP_plot != "None") &&
          (input$Selected_CP_plot2 != "None"))
      {
        IV_list <- data_storage_envir$IV_list
        summary_list <- data_storage_envir$summary_list
        sumOut <- NULL
        if (input$Outlier_Summary_CP_plot != "None") {
          summary_list <-
            map(
              data_storage_envir$summary_list,
              deselect_outlier,
              input$Outlier_Summary_CP_plot
            )
          sumOut <- input$Outlier_Summary_Summary_CP_plot


          if (input$Outlier_IV_CP_plot == "None") {
            OutlierCombined_list <-
              map_depth(data_storage_envir$IV_list, 2, function(x)
                return(FALSE))
            OutlierCombined_list <-
              map2(data_storage_envir$IV_peaks, OutlierCombined_list, function(x, y)
                return(y[1:(length(y) / length(x))]))
            OutlierCombined_list <-
              get_complete_outlier(
                OutlierCombined_list,
                data_storage_envir$IV_list,
                data_storage_envir$IV_names,
                data_storage_envir$summary_list,
                "Outlier"
              )
            IV_list <- OutlierCombined_list$IV_list
          }
        }

        if (input$Outlier_IV_CP_plot != "None") {
          show_modal_spinner() # show the modal window
          OutlierIV_list <-
            detect_IV_outlier(
              data_storage_envir$IV_list,
              data_storage_envir$IV_names,
              input$Outlier_IV_CP_plot,
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
          summary_list <-
            map(OutlierCombined_list$summary_list,
                deselect_outlier,
                "Outlier")
          IV_list <- OutlierCombined_list$IV_list
          remove_modal_spinner()

          # remove it when done
        }



        plot_type <- "Complete"
        CD_plot <-
          create_plot(
            plot_type = plot_type,
            IV_list = IV_list,
            column_list = c(
              input$Selected_CP_plot,
              input$Selected_CP_plot2,
              input$Selected_CP_plot3
            ),
            peak_list = map(
              input$CP_plot_Series,
              word,
              start = -1,
              sep = "-"
            ),
            series_vector = unlist(flatten(
              map(
                input$CP_plot_Series,
                word,
                start = 1,
                end = -2,
                sep = "-"
              )
            )),
            measurement_name = input$Names_CP_plot,
            display_N_legend = TRUE,
            display_N_legend_divisor = length(data_storage_envir$IV_peaks[[1]]),
            SD_MAD = input$Deviation_CP_plot,
            SD_MAD_shadow_direction = unlist(strsplit(
              input$SD_MAD_Shadow_CP_plot, ","
            )),
            linesize = input$Linesize,
            used_fontsize = input$Fontsize,
            used_font = input$Used_Font,
            used_axis_function = axisChoices(),
            ylab = map(themeChoices(), TeX),
            overrideYlim = c(input$YlimMinCPInw, input$YlimMaxCPInw, input$YlimMinCPOut, input$YlimMaxCPOut),
            used_colors = str_split(input$colors, ",")[[1]],
            used_theme = utheme(),
            spacer_plot = input$Spacer_Plots_CP != 0,
            n_spacer = input$Spacer_Plots_CP,
            ratio_Median_P_plot = input$median_CP_plot_ratio,
            paired = input$Paired_SigTest_CP_Plot,
            kruskal = input$Kruskal_Wallis,
            splitted = input$split_plot_Complete
          )

        if (input$split_plot_Complete == TRUE) {
        CD_plot[[2]] <- CD_plot[[2]] + scale_y_continuous(breaks = c(0,-25,-50,-75,-100))
        CD_plot[[5]] <- CD_plot[[5]] + scale_y_continuous(breaks = c(0,25,50,75,100))
        }
        if (input$split_plot_Complete == FALSE) {
          CD_plot[[2]] <- CD_plot[[2]] + scale_y_continuous(breaks = c(-100,-50,0,50,100))
        }




        if (input$legend_seperate_CP_plot == TRUE) {
          if (input$split_plot_Complete == TRUE) {
            legend <- CD_plot[[8]]

            CD_plot[[8]] <- NULL


            data_storage_envir$active_legend <- legend

          }

          if (input$split_plot_Complete == FALSE) {
            legend <-
              get_legend(
                CD_plot[[2]] + theme(
                  legend.box.margin = margin(0, 0, 0, 0, "cm"),
                  legend.title = element_blank()
                )
              )

            CD_plot[[1]] <-
              CD_plot[[1]]  + theme(legend.position = "none")
            CD_plot[[2]] <-
              CD_plot[[2]]  + theme(legend.position = "none")
          }
          data_storage_envir$active_legend <- legend
        }



        data_storage_envir$active_plot <- CD_plot
        if (input$Used_Dimension != "None")
          data_storage_envir$active_plot <-
          set_dim(data_storage_envir$active_plot,
                  data_storage_envir$dimensions)

        # output$Boxplot_CD <- renderPlot({CD_plot}, height = input$Save_heigth*13.12205433 , width = input$Save_width*13.12205433 , res = 300)
        output$CP_plot_Plots <- renderImage({
          # A temp file to save the output. It will be deleted after renderImage
          # sends it, because deleteFile=TRUE.
          outfile <- tempfile(fileext = '.png')

          # Generate a png
          ggsave(
            outfile,
            data_storage_envir$active_plot ,
            device = "png",
            width = input$Save_width,
            height = input$Save_heigth,
            dpi = 300,
            units = "mm",
            limitsize = F
          )

          # Return a list
          list(src = outfile,
               alt = "This is alternate text")
        }, deleteFile = TRUE)

      }
    }


  }, priority = 5)


  observe(
    updateSelectizeInput(session, "choose_series", choices = data_storage_envir$sheet_names)
  )

  observe(updateSelectizeInput(
    session,
    "choose_IV",
    choices = names(data_storage_envir$IV_list[[input$choose_series]])
  ))

  # observe(updateTextInput(session, "colors", value = paste(c(
  #   "#000000", palette_pander(8)[c(2, 4, 8, 6, 5, 1, 3, 7)]
  # ))))

  observe(
    updateSelectizeInput(session,
                         "Boxplot_Series",
                         choices =  data_storage_envir$sheet_names)
  )
  observe(
    updateSelectizeInput(session,
                         "Ratio_Series",
                         choices =  data_storage_envir$sheet_names)
  )
  observe(
    updateSelectizeInput(
      session,
      "SIV_Series",
      choices = data_storage_envir$sheet_names,
      selected = data_storage_envir$sheet_names[1]
    )
  )



  observe(
    updateSelectizeInput(
      session,
      "SIV_Measurement",
      choices = names(data_storage_envir$IV_list[[input$SIV_Series]]),
      selected = names(data_storage_envir$IV_list[[input$SIV_Series]][1])
    )
  )



  observe(if (exists("data_storage_envir", envir = globalenv())) {
    if (exists("IV_list", envir = data_storage_envir)) {
      updateSelectizeInput(session,
                           "MIV_Series",
                           choices = flatten(map2(
                             names(data_storage_envir$IV_list),
                             flatten(data_storage_envir$peak_list),
                             paste,
                             sep = "-"
                           )))
    }
  })

  observe(if (exists("data_storage_envir", envir = globalenv())) {
    if (exists("IV_list", envir = data_storage_envir)) {
      updateSelectizeInput(session,
                           "P_plot_Series",
                           choices = flatten(map2(
                             names(data_storage_envir$IV_list),
                             flatten(data_storage_envir$peak_list),
                             paste,
                             sep = "-"
                           )))
    }
  })

  observe(if (exists("data_storage_envir", envir = globalenv())) {
    if (exists("IV_list", envir = data_storage_envir)) {
      updateSelectizeInput(session,
                           "CP_plot_Series",
                           choices = flatten(map2(
                             names(data_storage_envir$IV_list),
                             flatten(data_storage_envir$peak_list),
                             paste,
                             sep = "-"
                           )))
    }
  })




  observeEvent(input$SD_MAD_Shadow_MIV, {
    if (grepl('[^-,+]', input$SD_MAD_Shadow_MIV))
    {
      showModal(
        modalDialog(
          title = "Error!",
          "Shoud be a plus (+) or minus (-) sign",
          easyClose = TRUE
        )
      )
    }
  })

  observeEvent(input$SD_MAD_Shadow_P_plot, {
    if (grepl('[^-,+]', input$SD_MAD_Shadow_P_plot))
    {
      showModal(
        modalDialog(
          title = "Error!",
          "Shoud be a plus (+) or minus (-) sign",
          easyClose = TRUE
        )
      )
    }
  })
  #
  observeEvent(input$colors, {
    if (!grepl("(^[#]\\w{6}){1}(,[#]\\w{6})+$", input$colors))
    {
      showModal(
        modalDialog(
          title = "Error!",
          "Colors should be given in hexcode seperated by a comma with no whitespace",
          easyClose = TRUE
        )
      )
    }
  }, ignoreInit = TRUE)


  observeEvent(input$SD_MAD_Shadow_CP_plot, {
    if (grepl('[^-,+]', input$SD_MAD_Shadow_CP_plot))
    {
      showModal(
        modalDialog(
          title = "Error!",
          "Shoud be a plus (+) or minus (-) sign",
          easyClose = TRUE
        )
      )
    }
  })
  #
  #   observeEvent(input$SD_MAD_Shadow_CP_plot, {
  #     if (grepl('[^-,+]', input$SD_MAD_Shadow_CP_plot))
  #     {
  #       showModal(
  #         modalDialog(
  #           title = "Error!",
  #           "Shoud be a plus (+) or minus (-) sign",
  #           easyClose = TRUE
  #         )
  #       )
  #     }
  #   })



  if (!interactive()) {
    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
  }

  onRestored(function(state) {
    if (exists("sheet_names", data_storage_envir)) {
      updateSelectizeInput(
        session,
        "Boxplot_Series",
        selected = state$input$Boxplot_Series,
        choices = data_storage_envir$sheet_names,
        server = TRUE
      )
      updateSelectizeInput(
        session,
        "Names_BoxPlot",
        selected = state$input$Names_BoxPlot,
        choices = state$input$Names_BoxPlot,
        server = TRUE
      )
      updateSelectizeInput(
        session,
        "Ratio_Series",
        selected = state$input$Ratio_Series,
        choices = data_storage_envir$sheet_names,
        server = TRUE
      )
      updateSelectizeInput(
        session,
        "Names_Ratio",
        selected = state$input$Names_Ratio,
        choices = state$input$Names_Ratio,
        server = TRUE
      )
      updateSelectizeInput(
        session,
        "P_plot_Series",
        selected = state$input$P_plot_Series,
        choices = flatten(map2(
          names(data_storage_envir$IV_list),
          flatten(data_storage_envir$peak_list),
          paste,
          sep = "-"
        )),
        server = TRUE
      )
      updateSelectizeInput(
        session,
        "MIV_Series",
        selected = state$input$MIV_Series,
        choices = flatten(map2(
          names(data_storage_envir$IV_list),
          flatten(data_storage_envir$peak_list),
          paste,
          sep = "-"
        )),
        server = TRUE
      )

      updateSelectizeInput(
        session,
        "SIV_Measurement",
        selected = state$input$SIV_Measurement,
        choices = names(data_storage_envir$IV_list[[input$SIV_Series]]),
        server = TRUE
      )

      updateSelectizeInput(
        session,
        "CP_plot_Series",
        selected = state$input$CP_plot_Series,
        choices = flatten(map2(
          names(data_storage_envir$IV_list),
          flatten(data_storage_envir$peak_list),
          paste,
          sep = "-"
        )),
        server = TRUE
      )

    } else {
      updateSelectizeInput(
        session,
        "Boxplot_Series",
        selected = state$input$Boxplot_Series,
        choices = state$input$Boxplot_Series,
        server = TRUE
      )
      updateSelectizeInput(
        session,
        "Names_BoxPlot",
        selected = state$input$Names_BoxPlot,
        choices = state$input$Names_BoxPlot,
        server = TRUE
      )
      updateSelectizeInput(
        session,
        "Ratio_Series",
        selected = state$input$Ratio_Series,
        choices = state$input$Ratio_Series,
        server = TRUE
      )
      updateSelectizeInput(
        session,
        "Names_Ratio",
        selected = state$input$Names_Ratio,
        choices = state$input$Names_Ratio,
        server = TRUE
      )
      updateSelectizeInput(
        session,
        "P_plot_Series",
        selected = state$input$P_plot_Series,
        choices = state$input$P_plot_Series,
        server = TRUE
      )
      updateSelectizeInput(
        session,
        "MIV_Series",
        selected = state$input$MIV_Series,
        choices = state$input$MIV_Series,
        server = TRUE
      )

      updateSelectizeInput(
        session,
        "SIV_Measurement",
        selected = state$input$SIV_Measurement,
        choices = state$input$SIV_Measurement,
        server = TRUE
      )

      updateSelectizeInput(
        session,
        "CP_plot_Series",
        selected = state$input$CP_plot_Series,
        choices = state$input$CP_plot_Series,
        server = TRUE
      )
    }







  })

}
