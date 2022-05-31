library(shiny)

# remove this when deploying app
#setwd("~/Documents/areadata-app/")

# Relevant reviewer's comment:
#  "This should be done by creating a dynamic web (i.e. create several buttons) that allow users to select
# country -> state -> county -> data type -> download. This allows users to select and download only
# the part they need rather than the entire dataset and then have to handle/process with lot of difficulties."

name_matching <- read.csv("data/name-matching.csv")

# ---------------- UI ------------------#

ui <- fluidPage(
  
  # App title ----
  titlePanel("Downloading County-level (GID2) AREAdata"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Choose dataset ----
      selectInput("dataset", "Choose a dataset:",
                  choices = c("Temperature", "Specific Humidity", "Relative Humidity",
                              "Precipitation", "UV")),
      selectInput("country", "Choose a Country:",
                  choices = unique(name_matching$NAME_0)),
      selectInput("states", "Choose a State:",
                  choices = "--All--"),
      selectInput("counties", "Choose a County:",
                  choices = "--All--"),
      
      # Button
      downloadButton("downloadData", "Download")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      h1("Download Instructions"),
      p("This app loads county-level AREAdata files (GID2), subsets them according to the your requirements, and provides a .txt file download."),
      p("See",
        a("the github page", 
          href = "https://pearselab.github.io/areadata"), 
          "for full details of how this data is generated and processed."),
      p("1. Select a climate variable you wish to download."),
      p("2. Select a country from which you would like to download county-level data (to download for all counties in all countries, see the main github site)."),
      p("3. Select a state within which you would like to download county data, or select '--All--' if you wish to download data from all counties within the selected country."),
      p("4. Select a county which you would like to download data for, or select '--All--' if you wish to download data from all counties within the selected state."),
      p("5. Click 'download', there will be a short pause while the download file is generated for you.")
    )
    
  )
)

# ---------------- SERVER ------------------#

server <- function(input, output, session) {
  
  # Reactive value for selected dataset ----
  # note - need to check these file names don't change with each figshare update...
  datasetInput <- reactive({
    switch(input$dataset,
           "Temperature" = readRDS(url("https://figshare.com/ndownloader/files/35067691")),
           "Specific Humidity" = readRDS(url("https://figshare.com/ndownloader/files/35068528")),
           "Relative Humidity" = readRDS(url("https://figshare.com/ndownloader/files/35070109")),
           "Precipitation" = readRDS(url("https://figshare.com/ndownloader/files/35074111")),
           "UV" = readRDS(url("https://figshare.com/ndownloader/files/35071861")))
  })
  
  # find which country has been picked, and update second dropdown to match
  observe({

    state_choices <- unique(name_matching[name_matching$NAME_0 == input$country,]$NAME_1)
    
    updateSelectInput(session, "states", choices = c("--All--", state_choices))
  })
  
  observe({
    if(input$states != "--All--"){
      county_choices <- unique(name_matching[name_matching$NAME_1 == input$states,]$NAME_2)
      updateSelectInput(session, "counties", choices = c("--All--", county_choices))
    }
  })
  
  # create the output dataset based on inputs
  final_data <- reactive({
    withProgress(message = "Preparing download...", value = 0, {
    req(datasetInput())
    country_id <- unique(name_matching[name_matching$NAME_0 == input$country,]$GID_0)
    country_data <- datasetInput()[grepl(country_id, rownames(datasetInput())),]
    if(input$states == "--All--"){
      final_data <- country_data
    } else if(input$counties == "--All--"){
      state_id <- unique(name_matching[name_matching$NAME_0 == input$country & name_matching$NAME_1 == input$states,]$GID_1)
      # get all counties with this state ID
      county_ids <- unique(name_matching[name_matching$GID_1 == state_id,]$GID_2)
      final_data <- country_data[rownames(country_data) %in% county_ids,]
    } else{
      # get single county ID
      county_id <- unique(name_matching[name_matching$NAME_0 == input$country & name_matching$NAME_1 == input$states &
                                          name_matching$NAME_2 == input$counties,]$GID_2)
      final_data <- country_data[rownames(country_data) == county_id,]
    }
    })
  })

  # Table of selected dataset ----
  # output$table <- renderTable({
  #   req(country_data())
  #   country_data()[1,]
  # })

  # Downloadable .txt of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      selected <- input$country
      if (input$states != "--All--") {
        selected <-c(selected, input$states)
      }
      if (input$counties != "--All--"){
        selected <- c(selected, input$counties)
      }
      paste(paste(selected, collapse="-"), ".txt", sep = "")
    },
    content = function(con) {
      write.table(final_data(), con, row.names = TRUE)
    }
  )

}


shinyApp(ui = ui, server = server)
