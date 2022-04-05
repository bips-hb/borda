tabICDs <-  tabPanel("Plot - ICD's", 
                     withMathJax(),
                     shinyjs::useShinyjs(),
                     sidebarLayout(
                       sidebarPanel(
                         textInput("Code", "Type Icd code"),
                         
                         actionButton("goIcd", "Plot"),
                         hr(""),
                         checkboxInput("label_check", label = "Add Labels", value = TRUE),
                         hr(""),
                         hr(""),
                         checkboxInput("Hide", label = "Hide ICD-Codes", value = FALSE),
                         hr(""),
                         hr(""),
                         downloadButton('downloadPlot','Download Plot')
                         
                       ),
                       mainPanel(
                         
                         plotOutput("ICDs")
                         
                       ))
)