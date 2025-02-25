library(shiny)
library(readr)
library(ggplot2)
library(dplyr)
library(utils)

# Increase file size limit to 30MB
options(shiny.maxRequestSize = 30 * 1024^2)

shinyServer(
    function(input, output, session) {
        # Reactive value to store extracted file paths and names
        file_data <- reactiveVal(NULL)

        observeEvent(input$zipfile, {
            req(input$zipfile)

            # Create a temp directory
            temp_dir <- tempdir()

            # Unzip files
            unzip(input$zipfile$datapath, exdir = temp_dir)

            # Get list of CSV files (full path & name)
            csv_files <- list.files(temp_dir, pattern = "\\.csv$", full.names = TRUE)
            csv_names <- basename(csv_files) # Extract only file names

            # Store file data (mapping names to paths)
            file_data(
                data.frame(Name = csv_names, Path = csv_files, stringsAsFactors = FALSE) %>%
                    mutate(
                        Channel = case_when(
                            grepl("_G_", Name) ~ "Green",
                            grepl("_Y_", Name) ~ "Yellow",
                            grepl("_O_", Name) ~ "Orange",
                            grepl("_R_", Name) ~ "Red",
                            grepl("_REF_", Name) ~ "Reference"
                        )
                    )
            )
        })

        # Create a dropdown for file selection (appears after upload)
        output$file_selector <- renderUI({
            req(file_data())
            selectInput("selected_file", "Select fluorescence detection channel to analyze:", choices = file_data()$Channel)
        })

        output$analyze_button <- renderUI({
            req(file_data())
            actionButton("process", "View scatterplot")
        })

        # Process and display scatter plot for the selected file
        observeEvent(input$process, {
            req(file_data(), input$selected_file)

            # Find the path of the selected file
            selected_path <- file_data()$Path[file_data()$Channel == input$selected_file]

            # Read the selected file
            data <- read_csv(selected_path, skip = 1, show_col_types = FALSE)

            # output$histo <- renderPlot({
            #     req(data)
            #     data %>%
            #         filter(`Is invalid` == 0) %>%
            #         ggplot(aes(x = RFU)) +
            #         geom_histogram(binwidth = 1) +
            #         theme_void()
            # })

            # Create slider UI for threshold values (appears after clicking "Process")
            output$slider_thresholding <- renderUI({
                req(data)
                sliderInput(
                    "thresholding",
                    "Set lower and upper threshold",
                    min = floor(min(data$RFU, na.rm = TRUE)),
                    max = ceiling(max(data$RFU, na.rm = TRUE)),
                    value = c(
                        floor(min(data$RFU, na.rm = TRUE)),
                        ceiling(max(data$RFU, na.rm = TRUE))
                    ),
                    step = 1,
                    ticks = FALSE
                )
            })

            # Generate scatter plot
            output$scatterPlot <- renderPlot({
                req(data, input$thresholding)

                data %>%
                    filter(`Is invalid` == 0) %>%
                    # Subsample data to prevent performance issues
                    sample_n(100000) %>%
                    ggplot(
                        aes(x = Partition, y = RFU)
                    ) +
                    geom_point(alpha = 0.5, color = "black") +
                    geom_hline(
                        yintercept = input$thresholding[1],
                        linetype = "dashed",
                        linewidth = 1.5,
                        color = "red"
                    ) +
                    geom_hline(
                        yintercept = input$thresholding[2],
                        linetype = "dashed",
                        linewidth = 1.5,
                        color = "blue",
                    ) +
                    labs(x = "Partition index", y = "RFU") +
                    theme_minimal() +
                    theme(text = element_text(size = 20))
            })
        })
    }
)
