Cohort <- tabPanel("Info on the Cohort Data", withMathJax(),
                           sidebarLayout(
                             sidebarPanel(

                               hr(),
                               
                               radioButtons(
                                 "code",
                                 label = "Drug",
                                 choiceNames = list(
                                   "3 Digits", 
                                   "4 Digits",
                                   "5 Digits",
                                   "Hmg"
                                   
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
                               verbatimTextOutput("info")
                             )
                           ))


                     
                     
                     
                     
                     
                     
                     
                     
                     
                     
                     
                     
