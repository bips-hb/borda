tabCorrelation <- tabPanel("Correlation of the Methods", withMathJax(),
                       sidebarLayout(
                         sidebarPanel(

                           
                           radioButtons(
                             "icd_setting",
                             label = "ICD",
                             choiceNames = list(
                               "3 Digits",
                               "4 Digits",
                               "5 Digits",
                               "Hmg"
                             ),
                             choiceValues = list(
                               "3",
                               "4", 
                               "5", 
                               "6" 
                             ),
                             selected = "3"
                           ),
                           hr(),
                           
                           radioButtons(
                             "drug_setting2",
                             label = "Drug",
                             choiceNames = list(
                               "Apixaban", 
                               "Dabigatran",
                               "Edoxaban",
                               "Rivaroxaban"
                              
                             ),
                             choiceValues = list(
                               "A",
                               "D", 
                               "E", 
                               "R" 
                             ),
                             selected = "A"
                           )
                           
                         ),
                         mainPanel(
                           p("Here we can see the Correlation of handpicked Methods, from the Simulation Study"),
                                   
                           plotOutput("corr")
                         )
                       ))

