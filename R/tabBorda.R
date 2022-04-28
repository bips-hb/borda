tabBorda <-  tabPanel("Borda Ranking", 
                     withMathJax(),
                     shinyjs::useShinyjs(),
                     sidebarLayout(
                       sidebarPanel(
                         downloadButton("downloadBorda", "Download")
                       ),
                       mainPanel(
                         dataTableOutput("borda_rank")
                       ))
)