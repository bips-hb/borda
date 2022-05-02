tabBorda <-  tabPanel("Borda Ranking", 
                     withMathJax(),
                     shinyjs::useShinyjs(),
                       mainPanel(
                         downloadButton("downloadBorda", "Download"),
                         p(br()),
                         dataTableOutput("borda_rank")
                       )
)