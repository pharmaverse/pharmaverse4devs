#' Compare package file sizes between development and installed versions
#'
#' @param dev_package_path Path to the package under development.
#' @param installed_package_path Path to the installed package (for example from CRAN).
#' @param output_dir Directory where the reports should be written.
#'
#' @return Invisible list containing generated report paths.
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

  resolve_package_input <- function(path, arg_name) {
    normalized_path <- normalizePath(path, winslash = "/", mustWork = TRUE)

    if (is_tarball(normalized_path)) {
      return(normalized_path)
    }

    if (dir.exists(normalized_path)) {
      tarballs <- list.files(
        path = normalized_path,
        pattern = "\\.tar\\.gz$",
        full.names = TRUE,
        recursive = FALSE,
        ignore.case = TRUE
      )
      has_description <- file.exists(file.path(normalized_path, "DESCRIPTION"))

      if (!has_description && length(tarballs) == 1) {
        return(tarballs[[1]])
      }

      if (!has_description && length(tarballs) > 1) {
        rlang::abort(
          paste0(
            "`", arg_name, "` points to a directory with multiple .tar.gz files. ",
            "Please provide the specific .tar.gz file path."
          ),
          class = "pkg_size_validation_error"
        )
      }
    }

    # Fallback to the provided directory path (e.g., unpacked package directory).
    normalized_path
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

  dev_package_path <- resolve_package_input(dev_package_path, "dev_package_path")
  installed_package_path <- resolve_package_input(
    installed_package_path,
    "installed_package_path"
  )

  temp_state <- new.env(parent = emptyenv())
  temp_state$paths <- character()
  on.exit(unlink(temp_state$paths, recursive = TRUE, force = TRUE), add = TRUE)

  get_file_sizes <- function(path) {
    path <- normalizePath(path, winslash = "/", mustWork = TRUE)
    root <- path
    is_tar <- is_tarball(path)

    if (is_tar) {
      extract_dir <- tempfile("pkg_compare_")
      dir.create(extract_dir, showWarnings = FALSE, recursive = TRUE)
      if (!dir.exists(extract_dir)) {
        rlang::abort(
          paste("Failed to create temporary extraction directory:", extract_dir),
          class = "pkg_size_validation_error"
        )
      }
      temp_state$paths <- c(temp_state$paths, extract_dir)

      untar_status <- tryCatch(
        utils::untar(path, exdir = extract_dir),
        error = function(e) NA_integer_
      )
      if (is.na(untar_status) || untar_status != 0) {
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
      path_parts <- strsplit(extracted_relative_paths, "/", fixed = TRUE)
      part_lengths <- lengths(path_parts)
      top_level_dirs <- vapply(
        path_parts,
        function(parts) if (length(parts) >= 1) parts[1] else NA_character_,
        character(1)
      )
      unique_top_dirs <- unique(top_level_dirs[!is.na(top_level_dirs)])

      # If all entries share one common top-level directory, strip it so
      # versioned archive roots do not affect cross-version path comparisons.
      if (all(part_lengths > 1) && length(unique_top_dirs) == 1) {
        relative_paths <- vapply(
          path_parts,
          function(parts) paste(parts[-1], collapse = "/"),
          character(1)
        )
      } else {
        relative_paths <- extracted_relative_paths
      }

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
  dev_input_is_tar <- is_tarball(dev_package_path)
  installed_input_is_tar <- is_tarball(installed_package_path)

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
  comparison_report$filename <- basename(comparison_report$relative_path)
  comparison_report$file_size_kb_dev <- round(comparison_report$file_size_bytes_dev / 1024, 2)
  comparison_report$file_size_kb_installed <- round(comparison_report$file_size_bytes_installed / 1024, 2)
  comparison_report$size_diff_kb <- round(comparison_report$size_diff_bytes / 1024, 2)
  comparison_report <- comparison_report[
    order(abs(comparison_report$size_diff_bytes), decreasing = TRUE, na.last = TRUE),
    c(
      "filename",
      "relative_path",
      "file_path_dev",
      "file_path_installed",
      "dev_exists",
      "installed_exists",
      "file_size_bytes_dev",
      "file_size_kb_dev",
      "file_size_bytes_installed",
      "file_size_kb_installed",
      "size_diff_bytes",
      "size_diff_kb"
    )
  ]

  dev_total_size_bytes <- sum(dev_files$file_size_bytes)
  installed_total_size_bytes <- sum(installed_files$file_size_bytes)
  totals_diff <- dev_total_size_bytes - installed_total_size_bytes
  dev_input_size_bytes <- if (dev_input_is_tar) file.info(dev_package_path)$size else dev_total_size_bytes
  installed_input_size_bytes <- if (installed_input_is_tar) {
    file.info(installed_package_path)$size
  } else {
    installed_total_size_bytes
  }
  input_size_diff <- dev_input_size_bytes - installed_input_size_bytes

  totals_report <- data.frame(
    package_version = c("development", "installed"),
    input_type = c(
      if (dev_input_is_tar) "tar.gz" else "directory",
      if (installed_input_is_tar) "tar.gz" else "directory"
    ),
    input_size_bytes = c(dev_input_size_bytes, installed_input_size_bytes),
    input_size_kb = round(c(dev_input_size_bytes, installed_input_size_bytes) / 1024, 2),
    input_size_mb = round(c(dev_input_size_bytes, installed_input_size_bytes) / (1024^2), 2),
    total_size_bytes = c(dev_total_size_bytes, installed_total_size_bytes),
    total_size_kb = round(c(dev_total_size_bytes, installed_total_size_bytes) / 1024, 2),
    total_size_mb = round(c(dev_total_size_bytes, installed_total_size_bytes) / (1024^2), 2),
    stringsAsFactors = FALSE
  )
  totals_report <- rbind(
    totals_report,
    data.frame(
      package_version = "diff_development_minus_installed",
      input_type = "difference",
      input_size_bytes = input_size_diff,
      input_size_kb = round(input_size_diff / 1024, 2),
      input_size_mb = round(input_size_diff / (1024^2), 2),
      total_size_bytes = totals_diff,
      total_size_kb = round(totals_diff / 1024, 2),
      total_size_mb = round(totals_diff / (1024^2), 2),
      stringsAsFactors = FALSE
    )
  )

  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  file_sizes_report_path <- file.path(output_dir, paste0("package_file_sizes_", timestamp, ".csv"))
  comparison_report_path <- file.path(output_dir, paste0("package_size_comparison_", timestamp, ".csv"))
  totals_report_path <- file.path(output_dir, paste0("package_size_totals_", timestamp, ".csv"))

  write.csv(file_sizes_report, file_sizes_report_path, row.names = FALSE)
  write.csv(comparison_report, comparison_report_path, row.names = FALSE)
  write.csv(totals_report, totals_report_path, row.names = FALSE)

  invisible(list(
    file_sizes_report = file_sizes_report_path,
    comparison_report = comparison_report_path,
    totals_report = totals_report_path
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
      actionButton("browse_dev_package_path", "Browse development path"),
      textInput("installed_package_path", "Installed package path:"),
      actionButton("browse_installed_package_path", "Browse installed path"),
      textInput("output_dir", "Report output directory:", value = path.expand("~")),
      actionButton("browse_output_dir", "Browse output directory"),
      actionButton("create_reports", "Create reports"),
      textOutput("status")
    )
  )

  server <- function(input, output, session) {
    status_message <- shiny::reactiveVal("")

    browse_for_directory <- function(current_value, dialog_caption) {
      if (!rstudioapi::isAvailable()) {
        status_message("RStudio path chooser is unavailable. Enter paths manually.")
        return(NULL)
      }

      default_dir <- if (nzchar(current_value) && file.exists(current_value)) {
        if (dir.exists(current_value)) current_value else dirname(current_value)
      } else {
        path.expand("~")
      }

      selected_dir <- if (rstudioapi::hasFun("selectDirectory")) {
        rstudioapi::selectDirectory(
          caption = dialog_caption,
          path = default_dir
        )
      } else if (rstudioapi::hasFun("selectFile")) {
        selected_file <- rstudioapi::selectFile(
          caption = "Select a file in the desired directory",
          path = default_dir
        )
        if (nzchar(selected_file)) dirname(selected_file) else ""
      } else {
        status_message("RStudio directory chooser is unavailable. Enter paths manually.")
        ""
      }

      if (nzchar(selected_dir)) {
        return(selected_dir)
      }

      NULL
    }

    browse_for_package_input <- function(current_value, dialog_caption) {
      selected_dir <- browse_for_directory(current_value, dialog_caption)
      if (!is.null(selected_dir)) {
        return(selected_dir)
      }

      if (!rstudioapi::isAvailable() || !rstudioapi::hasFun("selectFile")) {
        return(NULL)
      }

      default_dir <- if (nzchar(current_value) && file.exists(current_value)) {
        if (dir.exists(current_value)) current_value else dirname(current_value)
      } else {
        path.expand("~")
      }

      selected_file <- rstudioapi::selectFile(
        caption = dialog_caption,
        path = default_dir
      )

      if (nzchar(selected_file)) {
        return(selected_file)
      }

      NULL
    }

    output$status <- renderText({
      status_message()
    })

    observeEvent(input$browse_dev_package_path, {
      selected_path <- browse_for_package_input(
        input$dev_package_path,
        "Select development package path"
      )
      if (!is.null(selected_path)) {
        shiny::updateTextInput(session, "dev_package_path", value = selected_path)
      }
    })

    observeEvent(input$browse_installed_package_path, {
      selected_path <- browse_for_package_input(
        input$installed_package_path,
        "Select installed package path"
      )
      if (!is.null(selected_path)) {
        shiny::updateTextInput(session, "installed_package_path", value = selected_path)
      }
    })

    observeEvent(input$browse_output_dir, {
      selected_dir <- browse_for_directory(
        input$output_dir,
        "Select report output directory"
      )
      if (!is.null(selected_dir)) {
        shiny::updateTextInput(session, "output_dir", value = selected_dir)
      }
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
