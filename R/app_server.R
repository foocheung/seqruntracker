#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  observeEvent(input$plot_button, {
    # Check if directory input is provided
    if (input$directory != "") {
      # Set working directory to user specified directory
      setwd(input$directory)

      # Get the list of files matching the pattern
      files <- list.files(pattern = "multi_config_\\d+\\.csv")

      # Initialize an empty data frame to store parsed data
      m <- data.frame(Sample = character(),
                      lib = character(),
                      run = numeric(),
                      type = character(),
                      stringsAsFactors = FALSE)

      # Define a function to parse each file
      parse_file <- function(file) {
        # Extract contig number from the file name
        contig <- sub('.*_(\\d+)\\.csv', '\\1', file)

        # Read the file line by line
        lines <- readLines(file)

        # Define a function to parse each line
        parse_line <- function(line, contig) {
          if (grepl("GEX|CSP|BCR|TCR", line)) {
            parts <- strsplit(line, ",")[[1]]
            id <- parts[1]
            path <- parts[2]
            type <- parts[4]
            run <- sub('.*RUN(\\d+)/.*', '\\1', path)

            if (grepl("Flowcell_222TLLLT1", line)) {
              run <- 4
            } else if (grepl("Flowcell_222TT7LT1", line)) {
              run <- 5
            }

            id <- sub('.*_(GEX|CSP|BCR|TCR)\\d+', '\\1', id)

            # Append parsed data to the m data frame
            m <<- rbind(m, data.frame(Sample = contig, lib = id, run = run, type = type))
          }
        }

        # Apply the parsing function to each line
        for (line in lines) {
          parse_line(line, contig)
        }
      }

      # Apply the parsing function to each file
      for (file in files) {
        parse_file(file)
      }

      m$Sample <- factor(m$Sample, levels = sort(unique(as.numeric(m$Sample))))


      # Generate heatmap plot
      data <- table(m$Sample, m$type, m$run)
      df <- as.data.frame.table(data)

      output$heatmap <- renderPlot({
        ggplot2::ggplot(df, ggplot2::aes(x = as.factor(Var1), y = Var2, fill = factor(Freq))) +
          ggplot2::geom_tile(color = "white") +
          ggplot2::scale_fill_manual(values = c("gray", "blue"), labels = c("no sequencing", "sequencing")) + # Specify fill colors and labels
          ggplot2::labs(x = "", y = "", fill = "Type") +
          ggplot2::theme_minimal() +
          ggplot2::facet_grid(.~Var3) +
          ggplot2::theme(text = ggplot2::element_text(size = 25))+
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5))+
          ggplot2::theme(axis.text.x = ggplot2::element_text(size = 11))
      })
    } else {
      # If directory is not provided, show an error message
      shiny::showModal(shiny::modalDialog(
        title = "Error",
        "Please enter a directory path."
      ))
    }
  })
}
