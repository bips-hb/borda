tabMain <- tabPanel("Main", 
                    withMathJax(),
                    sidebarLayout(
                      sidebarPanel(
                        
                        
                        
                        p(strong("A Discovery and Verification Approach for Pharmacovigilance using Electronic Health Care Data"),
                          br(),
                          em("L.J. Dijkstra, T. Schink, R. Linder, M. Schwaninger, I. Pigeot, M.N. Wright, R. Foraita"),
                          br(),
                          br(),
                          em("Submitted (2022)"),
                          hr(),
                          p("In order to explore the data presented in the paper or to upload your 
                            own dataset, go the tab", 
                            em("Data")),
                          p("In order to see the Borda count ranking go to tab ",
                            em("Borda Ranking")),
                          p("The Kendall's tau correlations between the different methods 
                           can be found on tab",  
                           em("Borda Ranking")), 
                          p("The R code used for this project is publicly available under the
                            GPL-3 license and can be found at",  
                            a("borda", href="https://github.com/bips-hb/borda")), 
                          hr(),
                          
                          h4("Conflict of Interest"), 
                          
                          p("The authors declare that there are no conflicts of interest"),
                          
                          hr(), 
                          
                          h4("Contact"),
                          
                          p("Louis Dijkstra",
                            br(),
                            "Leibniz Institute for Prevention Research and Epidemiology - BIPS",
                            br(),
                            "Department Biometry & Data Management",
                            br(),
                            "E-mail:", 
                            a("dijkstra@leibniz-bips.de"),
                            br(),
                            a("http://www.leibniz-bips.de/en/")
                          )
                          
                        )
                      )
                      ,
                      mainPanel(
                        h1("Pharmacovigilance based on the Borda Count"), 
                        p("A plethora of statistical signal detection methods 
                          have been developed for pharmacoviglance, 
                          both for spontaneous reporting as well as for 
                          longitudinal electronic patient data. No signal 
                          detection method uniformly outperforms all. 
                          Here we use the Borda count ranking to combine the results 
                          of multiple methods simultaneously. 
                          The site shows the results presented in our paper"),
                        p(strong("A Discovery and Verification Approach for Pharmacovigilance using Electronic Health Care Data"),
                          br(),
                          em("L.J. Dijkstra, T. Schink, R. Linder, M. Schwaninger, I. Pigeot, M.N. Wright, R. Foraita"),
                          br(),
                          em("Submitted (2022)")
                        ),
                        p("Not only can you explore our results in more depth, this app also 
                          allows you to use your dataset in order to select signals 
                          of interest by employing multiple methods. "), 
                        p("First we introduce Borda count-based ranking. 
                        We then shortly describe our dataset. We introduce the different tabs in this app and explain what they do. 
                          We end with describing the process of how to use this app for your own dataset."),
                        p("All the details can be found in the paper."),
                        
                        h3("Borda Count Ranking"),
                        p("Each signal detection method generates a ranking of drug and adverse drug 
                        reaction (ADR) pairs. The pair that is placed highest (with rank 1 in our case) is thought be have the strongest association  based on the metric that the method uses. The pair for which it seems that there is 
                          no association at all (or is without sufficient observations) is placed last. "),
                        p("In case multiple methods are applied, each drug-ADR pair 
                        is assigned a rank by each of those methods. The Borda count for a drug-ADR 
                        pair is then the sum of those ranks. We can use the Borda count to create 
                        another ranking: the drug-ADR pair that with the lowest Borda count is 
                          placed first (rank 1) and with the highest Borda count is placed last. "), 
                        
                        h3("Case Study"), 
                        p("We performed a case study to examplify our approach. We used 
                          data from the German Pharmacoepidemiological Research Database (GePaRD) 
                          and the direct oral anticoagulant Rivaroxaban."),
                        p("We employ four methods:"),
                        tags$ul(
                          tags$li("The Bayesian Confidence Propagation Neural Network (BCPNN)"), 
                          tags$li("The Longitudinal Gamma Poisson Shrinker (LGPS)"), 
                          tags$li("Random Forests (RF)"), 
                          tags$li("LASSO")
                        ),
                        p("All details of the study can be found in the paper and in the Supplementary Material."),
                        
                        h3("Tabs"), 
                        
                        p("There are four (five if you include this one) tabs:"), 
                        tags$ul(
                          tags$li("Data"), 
                          tags$li("Borda Ranking"), 
                          tags$li("Kendall's tau"), 
                          tags$li("Plot ICDs")
                        ), 
                        p(" We will go over each tab and explain what it does."), 
                        
                        h4("Tab 'Data'"),
                        p("The dataset can be found here. There are multiple columns. The first, 'ICD' contains 
                          the ICD code. The second 'Description' contains the description of the ICD code. The other columns
                          show the ranks given to the ICD code by each method. The column 'BCPNN' shows the ranking for 
                          the BCPNN; the column 'LGS' shows the ranking for the LGPS etc."),
                        p("You can upload your own file and use the app to analyze the data. See Section 
                          'Uploading your own dataset' for the details."), 
                        
                        h4("Tab 'Borda Ranking'"), 
                        p("This tab shows the dataset as in the Data tab, but with two additional columns: 'Borda'
                          shows the Borda ranking, based on the Borda count. The column 'relative_rank' is the 
                          Borda ranking divided by the total number of ICD codes listed."),
                        p("The Download button provides you with the possibility to download the table as a CSV file."), 
                        
                        h4("Tab 'Kendall's tau'"), 
                        p("This tab shows the Kendall's tau correlations between the rankings of the different methods 
                          as a heatmap. See the paper for more details."), 
                        
                        h4("Tab 'Plot ICDs'"), 
                        p("This tab provides to possiblity to explore the relative rank of multiple ICD codes 
                          simultaneosly. This can useful if one wants to explore the signal strength for a 
                          specific class of ICD codes."), 
                        
                        h2("Uploading Your Own Dataset"), 
                        p("In case you want to upload your own dataset, you can (see Tab 'Data'). 
                        The file show be a CSV (comma-separated values) files. The first two columns
                        should have the header 'ICD' and 'Description'. An error message will appear 
                        if the file does not have these. In case there is no description, you can leave the 
                        column empty. 
                        The other columns need to contain the rankings of the different methods you used. 
                        The name of the method should be the name of the column."),
                        p("For example, the CSV file: "),
                        p("\"ICD\",\"Description\",\"method1\", \"method2\", \"method3\"", br(),
                          "\"ICD01\", \"hello\", 1, 3, 2", br(),
                          "\"ICD02\", \"how\", 2, 2, 1", br(),
                          "\"ICD03\", \"are\", 3, 1, 4", br(),
                          "\"ICD04\", \"you\", 4, 4, 3", br(),
                          style = "font-family: 'Courier'"), 
                        p("leads to the table:"), 
                        dataTableOutput("example_table"), 
                        p("Go to the tab 'Data' and upload the file. You will see it 
                        gets uploaded. The other tabs with the Borda count, 
                        the Kendall's tau correlation matrix and the ability 
                        to plot individual or multiple ICD codes, 
                        get updated immediately.")
                        
                      ) 
                      
                    )
)
