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
  is_tarball <- function(path) {
    file.exists(path) && grepl("\\.tar\\.gz$", path, ignore.case = TRUE)
  }

  is_package_input <- function(path) {
    dir.exists(path) || is_tarball(path)
  }

  if (!is_package_input(dev_package_path)) {
    rlang::abort(
      "`dev_package_path` must be an existing directory or a .tar.gz file.",
      class = "pkg_size_validation_error"
    )
  }

  if (!is_package_input(installed_package_path)) {
    rlang::abort(
      "`installed_package_path` must be an existing directory or a .tar.gz file.",
      class = "pkg_size_validation_error"
    )
  }

  if (!dir.exists(output_dir)) {
    rlang::abort(
      "`output_dir` must be an existing directory.",
      class = "pkg_size_validation_error"
    )
  }

  temp_state <- new.env(parent = emptyenv())
  temp_state$paths <- character()
  on.exit(unlink(temp_state$paths, recursive = TRUE, force = TRUE), add = TRUE)

  get_file_sizes <- function(path) {
    path <- normalizePath(path, winslash = "/", mustWork = TRUE)
    root <- path
    is_tar <- is_tarball(path)

    if (is_tar) {
      extract_dir <- tempfile("pkg_compare_")
      created <- dir.create(extract_dir, showWarnings = FALSE, recursive = TRUE)
      if (!created && !dir.exists(extract_dir)) {
        rlang::abort(
          paste("Failed to create temporary extraction directory:", extract_dir),
          class = "pkg_size_validation_error"
        )
      }
      temp_state$paths <- c(temp_state$paths, extract_dir)

      untar_status <- tryCatch(
        utils::untar(path, exdir = extract_dir),
        error = function(e) {
          rlang::abort(
            paste("Failed to extract .tar.gz package:", path),
            class = "pkg_size_validation_error",
            parent = e
          )
        }
      )
      if (!identical(untar_status, 0L) && !identical(untar_status, 0)) {
        rlang::abort(
          paste("Failed to extract .tar.gz package:", path),
          class = "pkg_size_validation_error"
        )
      }
      root <- extract_dir
    }

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
    if (is_tar) {
      extracted_relative_paths <- substring(files, nchar(root) + 2L)
      # .tar.gz packages usually extract into a single top-level versioned folder.
      # Remove that leading folder so paths align across package versions.
      relative_paths <- ifelse(
        grepl("/", extracted_relative_paths, fixed = TRUE),
        sub("^[^/]+/", "", extracted_relative_paths),
        extracted_relative_paths
      )
      file_paths <- paste0(path, "::", extracted_relative_paths)
      root_path <- path
    } else {
      relative_paths <- substring(files, nchar(root) + 2L)
      file_paths <- files
      root_path <- root
    }

    data.frame(
      root_path = root_path,
      relative_path = relative_paths,
      file_path = file_paths,
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

  server <- function(input, output) {
    status_message <- shiny::reactiveVal("")

    output$status <- renderText({
      status_message()
    })

    observeEvent(input$create_reports, {
      result <- tryCatch(
        compare_package_sizes(
          dev_package_path = input$dev_package_path,
          installed_package_path = input$installed_package_path,
          output_dir = input$output_dir
        ),
        pkg_size_validation_error = function(e) paste("Validation error:", e$message),
        error = function(e) paste("Unexpected error:", e$message)
      )

      if (is.list(result)) {
        status_message(
          paste0(
            "Reports created:\n",
            result$file_sizes_report,
            "\n",
            result$comparison_report
          )
        )
      } else {
        status_message(result)
      }
    })
  }

  shiny::runGadget(
    shiny::shinyApp(ui, server),
    viewer = shiny::dialogViewer("Compare Package Sizes")
  )
}
