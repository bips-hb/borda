tabPlots <- tabPanel("tabPlots", 
                    withMathJax(),
                    useShinyalert(),
                    sidebarLayout(
                      sidebarPanel(
                        actionButton("ken", "Plot Kendall"),
                      ),
                      mainPanel(
                        
                        plotOutput("Kendall")
                        
                        
                      ))
)
