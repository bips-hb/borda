tabBorda <-  tabPanel("Borda Ranking", 
                     withMathJax(),
                     shinyjs::useShinyjs(),
                     sidebarLayout(
                       sidebarPanel(
                         #uiOutput("selection_methods"),
                         # 
                         # p("Click Process to create the Bordacount with your chosen methods"),
                         # actionButton("rnks", "Process"),
                         # p(""),
                         # 
                         #p(""),
                         #actionButton("init", "Download", icon = icon("download")),
                         downloadButton("downloadBorda", "Download")
                       ),
                       mainPanel(
                         dataTableOutput("borda_rank")
                         #tableOutput("ranks")
                         
                       ))
)