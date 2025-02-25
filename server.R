library(shiny)
library(readr)
library(ggplot2)
library(dplyr)
library(utils)
library(tidyr)

# Increase file size limit to 30MB
options(shiny.maxRequestSize = 30 * 1024^2)

shinyServer(
    function(input, output, session) {
        observeEvent(input$process, {
            req(input$zipfile)

            # Create a temp directory
            temp_dir <- tempdir()

            # Unzip files
            unzip(input$zipfile$datapath, exdir = temp_dir)

            # Get list of CSV files
            csv_files <- list.files(temp_dir, pattern = "\\.csv$", full.names = TRUE)

            # Read all CSVs into a single dataframe
            df_list <- lapply(csv_files, read_csv, skip = 1)
            data <- bind_rows(df_list, .id = "file_id")

            browser()

            # Example processing: Assuming "Amplitude" column exists
            data <- data %>%
                mutate(
                    is_rain = ifelse(Amplitude < 2000, "Rain", "Droplet"),
                    file = gsub(temp_dir, "", file_id)
                )

            # Summarize rain quantification
            rain_summary <- data %>%
                group_by(file, is_rain) %>%
                summarise(count = n(), .groups = "drop") %>%
                pivot_wider(names_from = is_rain, values_from = count, values_fill = 0)

            output$summary <- renderTable(rain_summary)

            output$rainPlot <- renderPlot({
                ggplot(data, aes(x = Amplitude, fill = is_rain)) +
                    geom_histogram(bins = 50, alpha = 0.7, position = "identity") +
                    labs(title = "Rain vs Droplet Distribution", x = "Amplitude", y = "Count") +
                    theme_minimal()
            })
        })
    }
)
