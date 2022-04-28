tabData <- tabPanel("Data", 
                withMathJax(),
                useShinyalert(),
                       
                sidebarLayout(
                  # sidebar --- 
                  sidebarPanel(
                    
                    # load the original data from the paper 
                    p(strong("Results from the paper")),
                    actionButton("load_original_data", "(Re)load data from the paper"),
                    p(br()), 

                    # possibility to upload your own CSV file 
                    fileInput("uploaded_file", strong("Upload your own ranking:"),
                             accept = c("text/csv",
                                       "text/comma-separated-values,text/plain",
                                       ".csv")),
                    #actionButton("load_custom_data", "Upload"),
                    p(br()), 
                    
                    p(
                      strong("How to upload your own ranking"),
                    )
                ),
                           
                mainPanel(
                  # p("Here you see an example of the data we used in our studies"),
                  # p("You can use this data to verify our results, but you may also use your own data"),
                  # p("If you want to do so, please put your data in a smilar form we use and use a .csv file"),
                  # p("It is important that the names of your csv contains 'methodname' and additional info as
                  #            'methodname.info', exactly like the example data suggests"),
                  # 
                  dataTableOutput("ranking")
                ))
)