tabData <- tabPanel("Data", 
                withMathJax(),
                useShinyalert(),
                       
                sidebarLayout(

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
                    p(br()), 
                    p(
                      "See the Main page for how to upload your own dataset",
                    )
                ),
                           
                mainPanel(
                  dataTableOutput("ranking")
                ))
)