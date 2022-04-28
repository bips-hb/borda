tabICDs <-  tabPanel("Plot ICDs", 
                     withMathJax(),
                     shinyjs::useShinyjs(),
                     sidebarLayout(
                       sidebarPanel(
                         textInput("icd_codes", "Type ICD code(s)"),
                         
                         actionButton("plot_icd", "Plot"),
                         checkboxInput("show_icd_codes", label = "Show ICD codes", value = TRUE),
                         hr(""),
                         downloadButton('downloadICDPlot','Download Figure')
                         
                       ),
                       mainPanel(
                         
                         plotOutput("ICDPlot")
                         
                       ))
)