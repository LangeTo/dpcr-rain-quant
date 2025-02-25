library(shiny)

shinyUI(fluidPage(
    titlePanel("dPCR Rain Quantification"),
    sidebarLayout(
        sidebarPanel(
            fileInput("zipfile", "Upload ZIP File", accept = ".zip"),
            actionButton("process", "Process Files")
        ),
        mainPanel(
            tableOutput("summary"),
            plotOutput("rainPlot")
        )
    )
))
