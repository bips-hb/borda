tabData <- tabPanel("Data", 
                    withMathJax(),
                    useShinyalert(),
                       sidebarLayout(
                         sidebarPanel(
                           
                           fileInput("file1", "Choose CSV File",
                                     accept = c(
                                       "text/csv",
                                       "text/comma-separated-values,text/plain",
                                       ".csv")
                           ),
                           
                           p(""),
                           checkboxInput("desc","Description", TRUE),
                           p("Tick this if you have a description in your 2nd colum, thus the methods will start in the 3rd colum"),
                           p("This will only effect your input data, ours always comes with a description"),
                           
                           p(""),
                           
                           radioButtons(
                             "dat",
                             label = "Which Data?",
                             choiceNames = list(
                               "Ours - Rivaroxaban",
                               "Ours - Dabigatran",
                               "Ours - Apixaban",
                               "Ours - Edoxaban",
                               "Yours (Need Upload First)"
                               
                             ),
                             choiceValues = list ("R","D","A","E","Y"),
                           ),
                           p(""),
                           p(""),
                           p("When you are done with the data please confirm, which set you want to analyze further"),
                           actionButton("do", "Confirm"),
   
                           


                           
                           ),
                           mainPanel(
                             tipify(actionButton("btn2", "On click"),
                                    "Hello again! This is a click-able pop-up", placement="bottom", trigger = "click"),
                             p("Here you see an example of the data we used in our studies"),
                             p("You can use this data to verify our results, but you may also use your own data"),
                             p("If you want to do so, please put your data in a smilar form we use and use a .csv file"),
                             p("It is important that the names of your csv contains 'methodname' and additional info as
                             'methodname.info', exactly like the example data suggests"),
                            
                             dataTableOutput("Data")
                             
                           
                         ))
)