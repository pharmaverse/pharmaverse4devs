#' Addin Application to set up required pharmaverse blog folders/files
#'
#' @export
#'
initiate_blog <- function() {
  ui <- fluidPage(
    titlePanel("Pharmaverse Blog Skeleton"),
    mainPanel(
      textInput("post_name", "Post Name:  (needs to be character vector of length 1)"),
      textInput("author", "Author(s): (one or more authors are permitted separated by comma)"),
      dateInput("post_date", "Post Date:", value = Sys.Date()),
      textAreaInput("description", "Description:"),
      fileInput("cover_image_upload", "Upload Cover Image:", multiple = FALSE, accept = c('.png', '.jpg')),  # Allow multiple images
      checkboxGroupInput("tags", "Tags:",
                         choices = c("submission", "ADaMs", "conferences",
                                     "admiral", "xportr", "metatools", "metacore")),
      actionButton("create_button", "Create Post"),
      textOutput("created_message")
    )
  )

  server <- function(input, output, session) {
    # Replace key with replacement
    # This is a helping function for `create_post()`
    replace <- function(text, key = c("TITLE", "AUTHOR", "DESCR", "DATE", "TAG", "IMG", "SLUG"), replacement) {
      rlang::arg_match(key)

      if (key == "IMG") {
        replacement <- paste(replacement, ".png", sep = "")
      }

      # Switch to what key actually looks like
      key_with <- paste("[", key, "]", sep = "")

      # Decorate replacement
      replacement <- ifelse(
        key == "AUTHOR", paste("  - name: ", replacement, sep = ""),
        ifelse(key == "TAG", paste(replacement, collapse = ", "),
               paste('"', replacement, '"', sep = "")
        )
      )


      if (key == "AUTHOR") {
        where <- str_which(
          string = text,
          pattern = fixed(key_with)
        )
        text <- append(text, values = replacement, after = where)
        text <- text[-where]
      } else {
        text <- stringr::str_replace(
          string = text,
          pattern = fixed(key_with),
          replacement = replacement
        )
      }
      return(text)
    }

    # Creating the actual files
    create_post <- function(post_name,
                            author = Sys.info()["user"],
                            post_date = format(Sys.time(), "%Y-%m-%d"),
                            description = "",
                            cover_images = "pharmaverse",  # Initialize as default cover image
                            tags = c(
                              "metadata", "submission", "qc", "ADaMs",
                              "SDTMs", "community", "conferences", "admiral",
                              "roak", "xportr", "metatools", "metacore",
                              "displays", "falcon", "Shiny", "TLG"
                            )) {
      path_to_img <- "media"
      available_images <- list.files(path_to_img, full.names = TRUE)

      # Assert inputs
      stopifnot(is.character(description),
                is.character(post_name),
                is.character(post_date),
                is.character(author),
                length(tags) > 0)  # Ensure tags is not empty

      # Check if cover_images is not empty
      if (length(cover_images) == 0) {
        stop("Please select at least one cover image.")
      }

      # Check if selected cover images exist in available images
      cover_images <- cover_images[cover_images %in% available_images]

      # Check if cover images are selected correctly
      if (length(cover_images) == 0) {
        stop("None of the selected cover images exist in the 'media' folder.")
      }

      # Check if date is correctly formatted
      if (is.na(as.Date(post_date, format = "%Y-%m-%d"))) {
        stop("`post_date` has to be in the format '%Y-%m-%d', e.g. '2023-06-15'")
      }

      # Prepare values
      snake_name <- gsub(" ", "_", tolower(gsub("(.)([A-Z])", "\\1 \\2", post_name)))
      short_name <- paste(post_date, snake_name, sep = "_")

      if (short_name != short_name %>% stringr::str_trunc(30)) {
        message("For the folder creation:")
        message(paste(short_name, "has been shortened to", short_name %>% stringr::str_trunc(30), sep = " ") %>% str_wrap())
        short_name <- paste(short_name %>% stringr::str_trunc(30), sep = " ")
      }

      # Create dir for blogpost
      new_dir <- paste("posts", short_name, sep = "/")
      if (dir.exists(new_dir)) {
        stop(paste("A directory called:", new_dir, "already exists. Please work within that directory or choose a different `post_name` argument.") %>%
               str_wrap())
      }
      dir.create(new_dir)

      # Read template
      lines_read <- readLines("inst/template/template.txt")

      result <- lines_read %>%
        replace(key = "TITLE", replacement = post_name) %>%
        replace(key = "AUTHOR", replacement = author) %>%
        replace(key = "DESCR", replacement = description) %>%
        replace(key = "DATE", replacement = post_date) %>%
        replace(key = "IMG", replacement = paste(basename(cover_images), collapse = ", ")) %>%
        replace(key = "TAG", replacement = paste(tags, collapse = ", "))%>%  # Concatenate tags with comma
        replace(key = "SLUG", replacement = short_name)

      # Write new .qmd file
      writeLines(result, con = paste(file.path(new_dir, snake_name), ".qmd", sep = ""))
      file.copy("inst/template/appendix.R", paste(file.path(new_dir, "appendix"), ".R", sep = ""))

      # Copy selected cover images to the new directory
      if (!is.null(cover_images)) {
        file.copy(from = cover_images, to = new_dir)
      }

      message("Congratulations! You have created a new Blog Post skeleton. Find it here:")
      message(new_dir)
    }


    observeEvent(input$create_button, {
      # Validate inputs before calling create_post function
      if (nchar(input$post_name) == 0) {
        showNotification("Please provide a Post Name.", type = "warning")
        return(NULL)
      }
      if (length(input$tags) == 0) {
        showNotification("Please select at least one Tag.", type = "warning")
        return(NULL)
      }

      # Save the uploaded cover image in the media folder
      if (!is.null(input$cover_image_upload)) {
        cover_image_name <- tools::file_path_sans_ext(input$cover_image_upload$name)
        cover_image_path <- file.path("media", paste0(cover_image_name, ".png"))
        if (!file.exists(cover_image_path)) {
          file.copy(from = input$cover_image_upload$datapath, to = cover_image_path)
        }
      }

      # Call create_post function
      create_post(
        post_name = input$post_name,
        post_date = format(input$post_date, "%Y-%m-%d"),
        description = input$description,
        author = input$author,
        cover_images = cover_image_path,  # Use the file path of the uploaded image
        tags = input$tags
      )
      output$created_message <- renderText({
        "Congrats, you just created a new Blog Post skeleton."
      })

      observeEvent(input$create_button, {
        timeText <- paste("Happy Blogging!!")
        rstudioapi::insertText(timeText)
        stopApp()
      })
    })
  }

  runGadget(shinyApp(ui, server), viewer= dialogViewer("Blogging Setup Information"))
}

run_blog_skeleton <- function(){
  initiate_blog()
}
