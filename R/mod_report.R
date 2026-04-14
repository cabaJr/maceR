#' report UI Function
#'
#' @description A shiny Module to generate downloadable reports from the analysis
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_report_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinydashboardPlus::box(
      title = "Generate Analysis Report",
      status = "info",
      solidHeader = TRUE,
      width = 12,
      fluidRow(
        column(
          width = 6,
          h4("Report Options"),
          radioButtons(
            ns("report_format"),
            "Output Format:",
            choices = c("HTML" = "html_document", "PDF" = "pdf_document"),
            selected = "html_document"
          ),
          checkboxInput(
            ns("include_metadata"),
            "Include animal metadata table",
            value = TRUE
          ),
          checkboxInput(
            ns("include_summary"),
            "Include analysis summary",
            value = TRUE
          )
        ),
        column(
          width = 6,
          h4("Available Content"),
          uiOutput(ns("content_summary")),
          br(),
          actionButton(
            ns("generate_report"),
            "Generate Report",
            icon = icon("file-alt"),
            class = "btn-primary btn-lg"
          ),
          br(),
          br(),
          uiOutput(ns("download_ui"))
        )
      )
    )
  )
}

#' report Server Functions
#'
#' @noRd
mod_report_server <- function(id, env){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Reactive value to store report data
    report_data <- reactiveVal(NULL)
    if (!is.null(env$env4$Annotate$Report) && length(env$env4$Annotate$Report) > 0) {
      report_data(env$env4$Annotate$Report)
    }

    # Content summary
    output$content_summary <- renderUI({
      req(env$env4$Annotate)

      plot_counts <- list(
        actograms = length(env$env4$Annotate$Actograms[sapply(env$env4$Annotate$Actograms, function(x) length(x) > 0)]),
        dp_actograms = length(env$env4$Annotate$DPActograms[sapply(env$env4$Annotate$DPActograms, function(x) length(x) > 0)]),
        daily_activity = length(env$env4$Annotate$DAct_plots[sapply(env$env4$Annotate$DAct_plots, function(x) !is.null(x) && length(x) > 0)]),
        periodograms = length(env$env4$Annotate$period_plots[sapply(env$env4$Annotate$period_plots, function(x) !is.null(x) && length(x) > 0)]),
        period_boxplots = length(env$env4$Annotate$period_plots_box[sapply(env$env4$Annotate$period_plots_box, function(x) !is.null(x) && length(x) > 0)]),
        average_day = length(env$env4$Annotate$avg_day_plots[sapply(env$env4$Annotate$avg_day_plots, function(x) !is.null(x) && length(x) > 0)])
      )

      total_plots <- sum(unlist(plot_counts))

      if (total_plots == 0) {
        return(tags$div(
          class = "alert alert-warning",
          icon("exclamation-triangle"),
          "No plots have been generated yet. Please run analyses in the Analysis tab first."
        ))
      }

      tags$div(
        tags$ul(
          if (plot_counts$actograms > 0) tags$li(glue::glue("{plot_counts$actograms} Actogram(s)")),
          if (plot_counts$dp_actograms > 0) tags$li(glue::glue("{plot_counts$dp_actograms} Double-plotted actogram(s)")),
          if (plot_counts$daily_activity > 0) tags$li(glue::glue("{plot_counts$daily_activity} Daily activity plot(s)")),
          if (plot_counts$periodograms > 0) tags$li(glue::glue("{plot_counts$periodograms} Periodogram(s)")),
          if (plot_counts$period_boxplots > 0) tags$li(glue::glue("{plot_counts$period_boxplots} Period distribution plot(s)")),
          if (plot_counts$average_day > 0) tags$li(glue::glue("{plot_counts$average_day} Average daily activity plot(s)"))
        ),
        tags$p(glue::glue("Total: {total_plots} plot(s) ready for report"))
      )
    })

    # Generate report data
    observeEvent(input$generate_report, {
      req(env$env4$Annotate)

      # Collect all plots
      plots_data <- list(
        actograms = lapply(env$env4$Annotate$Actograms, function(x) if(length(x) > 0) x[[1]] else NULL),
        dp_actograms = lapply(env$env4$Annotate$DPActograms, function(x) if(length(x) > 0) x[[1]] else NULL),
        daily_activity = env$env4$Annotate$DAct_plots,
        periodograms = env$env4$Annotate$period_plots,
        period_boxplots = env$env4$Annotate$period_plots_box,
        average_day = env$env4$Annotate$avg_day_plots
      )

      # Remove NULL entries
      plots_data <- lapply(plots_data, function(x) x[!sapply(x, is.null)])

      # Collect metadata if requested
      metadata <- NULL
      if (input$include_metadata && !is.null(env$env4$Annotate$metaTable)) {
        metadata <- env$env4$Annotate$metaTable
      }

      # Create analysis summary
      analysis_summary <- NULL
      if (input$include_summary) {
        analysis_summary <- data.frame(
          Parameter = c("Report Generated", "Total Animals", "Plots Included"),
          Value = c(
            format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
            ifelse(!is.null(metadata), nrow(metadata), "Unknown"),
            length(unlist(plots_data, recursive = FALSE))
          )
        )
      }

      report_list <- list(
        plots_data = plots_data,
        metadata = metadata,
        analysis_summary = analysis_summary,
        format = input$report_format
      )

      report_data(report_list)
      env$env4$Annotate$Report <- report_list

      showNotification("Report data prepared! Click download to generate the file.",
                      type = "message", duration = 5)
    })

    # Download UI
    output$download_ui <- renderUI({
      req(report_data())
      downloadButton(
        ns("download_report"),
        "Download Report",
        icon = icon("download"),
        class = "btn-success btn-lg"
      )
    })

    # Download handler
    output$download_report <- downloadHandler(
      filename = function() {
        ext <- ifelse(report_data()$format == "html_document", "html", "pdf")
        paste0("maceR_report_", format(Sys.Date(), "%Y%m%d"), ".", ext)
      },
      content = function(file) {
        # Create temporary directory for rendering
        temp_dir <- tempdir()
        temp_rmd <- file.path(temp_dir, "report.Rmd")

        # Get the skeleton template
        skeleton_path <- system.file("rmarkdown", "templates", "maceR_report", "skeleton", "skeleton.Rmd",
                                   package = "maceR")

        if (skeleton_path == "") {
          stop("Report template not found")
        } else {
          rmd_content <- readLines(skeleton_path)
        }

        # Write the Rmd file
        writeLines(rmd_content, temp_rmd)

        # Extract format and remove from params before rendering
        output_fmt <- report_data()$format
        params_to_pass <- report_data()
        params_to_pass$format <- NULL

        # Render the report with error handling
        tryCatch({
          rendered_file <- rmarkdown::render(
            temp_rmd,
            output_format = output_fmt,
            params = params_to_pass,
            output_dir = temp_dir,
            quiet = FALSE,
            envir = new.env()
          )
          
          # Copy rendered file to download location
          if (file.exists(rendered_file)) {
            file.copy(rendered_file, file, overwrite = TRUE)
          } else {
            stop("Rendered file not found at: ", rendered_file)
          }
          
        }, error = function(e) {
          stop("Error rendering report: ", conditionMessage(e))
        })
      }
    )

  })
}

## To be copied in the UI
# mod_report_ui("report_ui_1")

## To be copied in the server
# mod_report_server("report_ui_1")