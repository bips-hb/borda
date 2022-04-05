tabBorda <-  tabPanel("BordaCount", 
                     withMathJax(),
                     sidebarLayout(
                       sidebarPanel(
                         p(""),
                         actionButton("borda", "Add Borda Count"),
                         p(""),
                         downloadLink("downloadData", "Download"),
                       ),
                       mainPanel(
                         
                         tableOutput("bordacount")
                         
                       ))
)