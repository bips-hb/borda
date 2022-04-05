tabMean <-  tabPanel("Create Borda", 
                     withMathJax(),
                     shinyjs::useShinyjs(),
                     sidebarLayout(
                       sidebarPanel(
                         uiOutput("moreControls"),
                         
                         p("Click Process to create the Bordacount with your chosen methods"),
                         actionButton("rnks", "Process"),
                         p(""),
                        
                         p(""),
                         actionButton("init", "Download", icon = icon("download")),
                         downloadButton("downloadData", "Download", style = "visibility: hidden;")
                         
                         
                         


                       ),
                       mainPanel(
                      
                        tableOutput("ranks")
                         
                       ))
)