library(shiny)

shinyUI(fluidPage(
    titlePanel("dPCR Rain Quantification"),
    sidebarLayout(
        sidebarPanel(
            fileInput(
                "zipfile",
                "Upload RFU values as .zip archive:",
                accept = ".zip"
            ),
            uiOutput("file_selector"),
            uiOutput("analyze_button"),
            # plotOutput("histo", height = "100px"),
            uiOutput("slider_thresholding"),
            uiOutput("summary"),
            uiOutput("fraction_pos")
        ),
        mainPanel(
            plotOutput("scatterPlot", height = "600px")
        )
    )
))
