#' Compare package file sizes between development and installed versions
#'
#' @param dev_package_path Path to the package under development.
#' @param installed_package_path Path to the installed package (for example from CRAN).
#' @param output_dir Directory where the reports should be written.
#'
#' @return Invisible list containing both generated report paths.
#' @noRd
compare_package_sizes <- function(dev_package_path,
                                  installed_package_path,
                                  output_dir = path.expand("~")) {
  if (!dir.exists(dev_package_path)) {
    stop("`dev_package_path` must be an existing directory.")
  }

  if (!dir.exists(installed_package_path)) {
    stop("`installed_package_path` must be an existing directory.")
  }

  if (!dir.exists(output_dir)) {
    stop("`output_dir` must be an existing directory.")
  }

  get_file_sizes <- function(path) {
    root <- normalizePath(path, winslash = "/", mustWork = TRUE)
    files <- list.files(
      path = root,
      recursive = TRUE,
      full.names = TRUE,
      all.files = TRUE,
      no.. = TRUE,
      include.dirs = FALSE
    )

    if (!length(files)) {
      return(data.frame(
        root_path = character(),
        relative_path = character(),
        file_path = character(),
        file_size_bytes = numeric(),
        stringsAsFactors = FALSE
      ))
    }

    files <- normalizePath(files, winslash = "/", mustWork = TRUE)
    file_info <- file.info(files)
    relative_paths <- substring(files, nchar(root) + 2L)

    data.frame(
      root_path = root,
      relative_path = relative_paths,
      file_path = files,
      file_size_bytes = as.numeric(file_info$size),
      stringsAsFactors = FALSE
    )
  }

  dev_files <- get_file_sizes(dev_package_path)
  installed_files <- get_file_sizes(installed_package_path)

  file_sizes_report <- rbind(
    transform(dev_files, package_version = "development"),
    transform(installed_files, package_version = "installed")
  )
  file_sizes_report <- file_sizes_report[
    order(file_sizes_report$file_size_bytes, decreasing = TRUE),
    c("package_version", "root_path", "relative_path", "file_path", "file_size_bytes")
  ]

  comparison_report <- merge(
    dev_files[, c("relative_path", "file_path", "file_size_bytes")],
    installed_files[, c("relative_path", "file_path", "file_size_bytes")],
    by = "relative_path",
    all = TRUE,
    suffixes = c("_dev", "_installed")
  )
  comparison_report$dev_exists <- !is.na(comparison_report$file_path_dev)
  comparison_report$installed_exists <- !is.na(comparison_report$file_path_installed)
  comparison_report$size_diff_bytes <- ifelse(
    comparison_report$dev_exists & comparison_report$installed_exists,
    comparison_report$file_size_bytes_dev - comparison_report$file_size_bytes_installed,
    NA_real_
  )
  comparison_report <- comparison_report[
    order(abs(comparison_report$size_diff_bytes), decreasing = TRUE, na.last = TRUE),
    c(
      "relative_path",
      "file_path_dev",
      "file_path_installed",
      "dev_exists",
      "installed_exists",
      "file_size_bytes_dev",
      "file_size_bytes_installed",
      "size_diff_bytes"
    )
  ]

  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  file_sizes_report_path <- file.path(output_dir, paste0("package_file_sizes_", timestamp, ".csv"))
  comparison_report_path <- file.path(output_dir, paste0("package_size_comparison_", timestamp, ".csv"))

  write.csv(file_sizes_report, file_sizes_report_path, row.names = FALSE)
  write.csv(comparison_report, comparison_report_path, row.names = FALSE)

  invisible(list(
    file_sizes_report = file_sizes_report_path,
    comparison_report = comparison_report_path
  ))
}

#' Open an addin to compare package file sizes
#'
#' @export
run_compare_package_sizes <- function() {
  ui <- fluidPage(
    titlePanel("Compare Package File Sizes"),
    mainPanel(
      textInput("dev_package_path", "Development package path:"),
      textInput("installed_package_path", "Installed package path:"),
      textInput("output_dir", "Report output directory:", value = path.expand("~")),
      actionButton("create_reports", "Create reports"),
      textOutput("status")
    )
  )

  server <- function(input, output, session) {
    observeEvent(input$create_reports, {
      result <- tryCatch(
        compare_package_sizes(
          dev_package_path = input$dev_package_path,
          installed_package_path = input$installed_package_path,
          output_dir = input$output_dir
        ),
        error = function(e) e$message
      )

      output$status <- renderText({
        if (is.list(result)) {
          paste0(
            "Reports created:\n",
            result$file_sizes_report,
            "\n",
            result$comparison_report
          )
        } else {
          result
        }
      })
    })
  }

  runGadget(
    shinyApp(ui, server),
    viewer = dialogViewer("Compare Package Sizes")
  )
}
