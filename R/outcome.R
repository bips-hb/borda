outcome <- tabPanel("Outcome - Ranking", withMathJax(),
                   sidebarLayout(
                     sidebarPanel(
                       
                       hr(),
                       
                       radioButtons(
                         "drug_setting",
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
                       ),
                       
                       hr(),
                       
                       radioButtons(
                         "icd",
                         label = "ICD",
                         choiceNames = list(
                           "ICD_3", 
                           "ICD_4",
                           "ICD_5",
                           "HMG"
                           
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
                       "method",
                       label = "Method",
                       choiceNames = list(
                         "ICA", 
                         "ICO",
                         "LGPS",
                         "POIS",
                         "NUM_Reports",
                         "PRR",
                         "RF_IMPU",
                         "RF_CORR",
                         "LASSO"
                         
                       ),
                       choiceValues = list(
                         1,
                         2, 
                         3, 
                         4,
                         5,
                         6,
                         7,
                         8,
                         9
                         
                         
                       ),
                       selected = 1
                     )
                     
                   ),
                     mainPanel(
                       p("Put your description here"),
                       dataTableOutput("ranking")
                       
                     
                       )
                   
                   ))




