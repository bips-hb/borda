tabKendall <- tabPanel("Kendall's tau", 
                    withMathJax(),
                    useShinyalert(),
                    mainPanel(
                      plotOutput("Kendall")
                    )
)
