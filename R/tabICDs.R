# ICD codes text initially on the ICD codes tab
initial_icd_codes <<- "D508, E8"

tabICDs <-  tabPanel("Plot ICDs", 
                     withMathJax(),
                     shinyjs::useShinyjs(),
                     sidebarLayout(
                       sidebarPanel(
                         p("This tab provides to possiblity to explore the relative rank of multiple ICD codes 
                           simultaneosly. This can useful if one wants to explore the signal strength for a 
                           specific class of ICD codes."),
                         p("You can fill in multiple ICD codes simultaneously. If you want to see a class of 
                           ICD codes, you can just write the first few symbols, e.g., if you want to see 
                           all codes starting with 'E8', you can just type E8."), 
                         textInput("icd_codes", "Type ICD code(s)", value = initial_icd_codes),
                         
                         actionButton("plot_icd", "Plot"),
                         checkboxInput("show_icd_codes", label = "Show ICD codes", value = TRUE),
                         hr(""),
                         downloadButton('downloadICDPlot','Download Figure')
                         
                       ),
                       mainPanel(
                         
                         plotOutput("ICDPlot")
                         
                       ))
)