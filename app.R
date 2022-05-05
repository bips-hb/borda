########################################################
# Main App File 
########################################################

# empty environment
rm(list = ls())

# Load libaries --- 
library(shiny)
library(shinyalert)

library(stringr)
library(dplyr)
library(ggrepel)
library(readr)
library(ggplot2)
library(reshape2)

# Load all the functionality (functions for computing Borda count etc.)
source("R/functionality.R")

# Load the individual tabs ---
source("R/tabMain.R")
source("R/tabData.R")
source("R/tabBorda.R")
source("R/tabKendall.R")
source("R/tabICDs.R")

### Global variables --- used to store the dataset 

#' Create a Dataset Object
#' 
#' Returns a list containing all the revalant data for a given raw dataset. 
#' 
#' @param raw_data Raw dataset
#' 
#' @return A list
#' @export
createDatasetObject <- function(raw_data) { 
  list(
    raw_data = raw_data, 
    method_names = names(raw_data)[3:length(raw_data)], # names of the methods involved 
    borda_dataset = computeBordaRanking(raw_data), # used to store the Borda count 
    kendall_tau = computeKendallsTau(raw_data) # the Kendall tau's correlation matrix
  )
}

# the dataset from the paper
rivaroxaban_dataset <- readRDS("data/rivaroxaban_dataset.rds")

# used to store a possibly custom dataset (initially the dataset from the paper)
dataset <- rivaroxaban_dataset

# Define UI ----
ui <- fluidPage( withMathJax(),
  tagList(tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  )),
  
  uiOutput("UIafterLogin")
)

# Define server logic ----
server <- function(input, output, session) {
  
  output$UIafterLogin <- renderUI({
    navbarPage("Pharmacovigilance",
               tabMain,
               tabData,
               tabBorda,
               tabKendall,
               tabICDs)
    })
  
  # create the example table for main page 
  example_data <- read_csv("data/example_data.csv")
  output$example_table <- renderDataTable({return(example_data)})
  
  # Changes all the plots/tables in the app with the given dataset object
  useThisDataset <- function(dataset) { 
    # change the ranking table on the 'Data' tab
    output$ranking <- renderDataTable({return(dataset$raw_data)})
    # change the ranking table on the 'Borda' tab
    output$borda_rank <- renderDataTable({return(dataset$borda_dataset)})
    # update the Kendall's Tau plot
    output$Kendall <- renderPlot({createKendallCorrelationPlot(dataset$kendall_tau)})
  }
  
  # ---- FUNCTIONALITY OF THE DATA TAB ---- 
  
  # when the app starts, firs show the rivaroxaban dataset 
  useThisDataset(rivaroxaban_dataset)
  
  # if a new file get uploaded, the table is updated
  observeEvent(input$uploaded_file, {
      raw_data <- read.csv(input$uploaded_file$datapath, header = TRUE)
      
      # check whether documents is correct
      if (colnames(raw_data)[1] != "ICD" || colnames(raw_data)[2] != "Description") { 
        shinyalert::shinyalert("Wrong Datafile", "The first and second column should be called 'ICD' and 'Description'") 
        return()
      }
      
      if (ncol(raw_data) < 3) { 
         shinyalert::shinyalert("Wrong Datafile", "Only contains 'ICD' and 'Description'; there are no results")
        return()
      }
      
      dataset <<- createDatasetObject(raw_data)
      useThisDataset(dataset)
    }
  )
  
  # if the button "(Re)load data" is pressed, the rivaroxaban dataset is shown again
  observeEvent(input$load_original_data, {
    dataset <<- rivaroxaban_dataset
    useThisDataset(dataset)  
  }) 
  
  # ---- FUNCTIONALITY OF THE BORDA TAB ----

  output$downloadBorda <- downloadHandler(
    filename = function() {
      'borda_ranking.csv'
    },
    content = function(file) {
      write.csv(dataset$borda_dataset, file)
    }
  )
  
  # ---- FUNCTIONALITY OF THE ICD PLOTS ----
  
  observeEvent(input$plot_icd, {
    ### Process the ICD input
    patterns <- strsplit(input$icd_codes, split = ", ")[[1]]
    
    print(patterns)
    
    # Add an OR | sign for detecting the patterns later
    patterns <- paste(patterns, collapse = "|")
  
    print(patterns)
    
    # Find the appropriate indices 
    indices <- stringr::str_which(dataset$borda_dataset$ICD, patterns)

    # get the selected dataset 
    borda <- dataset$borda_dataset %>% dplyr::slice(indices)
  
    # Check whether the ICD codes actually exist
    if (nrow(borda) == 0) { 
      shinyalert("ICD codes not found", 
                 "The ICD codes were not found in the dataset. Please check your input. ") 
      return()
    }
    
    # Create the plot
    output$ICDPlot <- renderPlot(createICDPlot(borda, 
                                               show_icd_codes = input$show_icd_codes)) 
  })
  
  output$downloadICDPlot <- downloadHandler(
    filename = function() {
      # create filename given ICD codes
      icds <- strsplit(input$icd_codes, split = ", ")[[1]]
      icds <- sort(icds) # sort them
      filename <- paste(icds, collapse = "_")
      paste(filename, '.png', sep = "")
    },
    content = function(file) {
      ggsave(file)
    }
  )
}

# Run the app ----
shinyApp(ui = ui, server = server)
