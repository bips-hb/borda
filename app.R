########################################################
# Main App File 
########################################################

rm(list = ls())
library(openxlsx)
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(shinyBS)
library(readr)
library(dplyr)
library(ggplot2)
library(latex2exp)
library(gtable)
library(gridExtra)
library(grid)
library(openxlsx)
library(xtable)
library(shinyalert)
  


# Load the data ---
#source("readXLS.R")
# Load the plot Functions
source("R/plotBoxplot.R")
source("R/plotLinePlot.R")
source("R/cor_plot.R")

# Load the individual tabs ---
source("R/Cohort.R")
source("R/outcome.R")
source("R/tabMain.R")
source("R/tabRanking.R")
source("R/tabSetting.R")
source("R/tabAbout.R")
source("R/tabCorrelation.R")
source("R/tabData.R")
source("R/tabTest.R")
source("R/tabBorda.R")
source("R/tabPlots.R")
source("R/tabICDs.R")

#this is our data, we don't want to read it in twice
Riva <- read.csv("data/Riva.csv")
Apix <- read.csv("data/Apix.csv")
Edox <- read.csv("data/Edox.csv")
Dabi <- read.csv("data/Dabi.csv")
Clicker1 <<- FALSE
BigData <<- NULL
Description <<- TRUE
CreateBorda <<- FALSE

create_lineplot <- function(ICD_Code = "A010", dot_size = 3, height_ticks = .005, 
                            line_thickness = .5, font_size = 3, font_size_label = 4){ 
  ggplot(data %>% filter(ICD == ICD_Code), aes(x = relative_rank, y = 0, color = method)) +
    # create axis
    annotate("segment", x = 0, xend = 1, y = 0, yend = 0, size = line_thickness) +
    annotate("segment", x = 0,xend = 0, y = -height_ticks, yend = height_ticks, size= line_thickness) +
    annotate("segment",x = .25, xend = .25, y = -height_ticks, yend = height_ticks, size= line_thickness) +
    annotate("segment",x = .5, xend = .5, y = -height_ticks, yend = height_ticks, size= line_thickness) +
    annotate("segment",x = .75, xend = .75, y = -height_ticks, yend = height_ticks, size= line_thickness) +
    annotate("segment",x = 1, xend = 1, y = -height_ticks, yend = height_ticks, size= line_thickness) +
    geom_point(size = dot_size) + 
    scale_x_continuous(limits=c(1, 0), expand = c(0.01, 0.01), trans = "reverse") + 
    scale_y_continuous(limits = c(-.1,.1), expand = c(0,0)) +
    geom_text(x=0, y = -2*height_ticks, label="0", color = "black", size = font_size) + 
    geom_text(x=-0.25, y = -2*height_ticks, label="0.25", color = "black", size = font_size) + 
    geom_text(x=-.5, y = -2*height_ticks, label="0.5", color = "black", size = font_size) + 
    geom_text(x=-.75, y = -2*height_ticks, label="0.75", color = "black", size = font_size) + 
    geom_text(x=-1, y = -2*height_ticks, label="1", color = "black", size = font_size) + 
    geom_text(x=-.5, y = -4*height_ticks, label="Relative Rank", color = "black", size = font_size_label) + 
    #scale_color_manual(values = unname(colours)) + 
    scale_colour_discrete(name  ="Method") + 
    ylab(ICD_Code) + 
    theme(panel.background = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(), 
          legend.title = element_text(size = 11), 
          axis.title.y = element_text(angle = 0, vjust = .5, hjust = -.5, size = 12)) 
}



labels <- data.frame(
  method = c("a",
             "chi2", 
             "chi2Yates", 
             "GPS", 
             "GPS025", 
             "GPS05", 
             "ICAlt", 
             "ICAlt025", 
             "ICAlt05", 
             "ICOrig", 
             "ICOrig025", 
             "ICOrig05", 
             "lbinomial",
             "highest_lambda", 
             "midRFET", 
             "ppoisson", 
             "PRR", 
             "PRR025",
             "PRR05", 
             "Q", 
             "Q025", 
             "Q05", 
             "RFET", 
             "ROR", 
             "ROR025", 
             "ROR05", 
             "RRR"),
  method_label = c("# reports",
                   "$\\chi^2$",
                   "$\\chi^2_{Yates}$",
                   "EBGM",
                   "$EB_{025}$",
                   "$EB_{05}$",
                   "$IC^{alternative}$",
                   "$IC^{alternative}_{025}$",
                   "$IC^{alternative}_{05}$",
                   "$IC^{original}$",
                   "$IC^{original}_{025}$",
                   "$IC^{original}_{05}$",
                   "$\\Lambda_{binomial}$",
                   "LASSO",
                   "midRFET",
                   "$p_{Poisson}$",
                   "PRR",
                   "$PRR_{025}$",
                   "$PRR_{05}$",
                   "$Q$",
                   "$Q_{025}$",
                   "$Q_{05}$",
                   "RFET",
                   "ROR",
                   "$ROR_{025}$",
                   "$ROR_{05}$",
                   "RRR")
)



# Define UI ----
ui <- fluidPage( withMathJax(),
  tagList(tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  )),
  
  #password protection ---
   # div(
   #   class = "login",
   #   uiOutput("uiLogin"),
   #   textOutput("pass"),
   #   tags$head(tags$style("#pass{color: red;"))
   # ),
  
  uiOutput("UIafterLogin")
)

# Define server logic ----
server <- function(input, output, session) {
  
  # # Password protection ---
   #USER <- reactiveValues(Logged = FALSE , session = session$user) 
   #source("www/login.R",  local = TRUE)
  # 
   output$UIafterLogin <- renderUI({
    # if (USER$Logged) { 
       navbarPage("Louis mega Simulation Study",
                  tabMain,
                  tabData,
                  tabMean,
                  tabPlots,
                  tabICDs
                  #outcome,
                  #tabCorrelation,
                  #Cohort,
                  #tabAbout
                  )
   #}
   })
   #This shows the data you have currently clicked on
   
   output$Data <- renderDataTable({

       inFile <- input$file1
  
         if (input$dat == "R"){
           return(Riva)
           
         }
         if (input$dat == "A"){
           return(Apix)
           
         }
         if (input$dat == "E"){
           return(Edox)
           
         }
         if (input$dat == "D"){
           return(Dabi)
           
         }
       
       
       
       

         if (input$dat == "Y")
           
         {
           if (is.null(inFile)){
             showNotification("You have to put in Data before",
                              duration = 4, 
                              closeButton = TRUE,
                              type = "error")} else {
                                
       return(customData <<- read.csv(inFile$datapath, header = TRUE))
           }
         }
    })
   
   
   
   observeEvent(input$do, {
     # if(Clicker1 == FALSE){
     

     Clicker1 <<- TRUE 
     if (input$dat == "R"){
       BigData <<- Riva
       
       
     }
     if (input$dat == "A"){
       BigData <<- Apix
       
     }
     if (input$dat == "E"){
       BigData <<- Edox
       
     }
     if (input$dat == "D"){
       BigData <<- Dabi
       
     }
     if (input$dat == "Y")
     {
       BigData <<- customData
       if(input$desc == TRUE){
         Description <<- TRUE
       } else if (input$desc == FALSE){
         Description <<- FALSE
       }
     }
     
     # Split <- gsub("\\..*","",names(BigData))
     # BigName <<- list()
     # for (i in 1:length(Split)) {
     #   if (!(Split[i] %in% BigName)) {
     #     BigName <- c(BigName, Split[i])
     #   }
     # }
     if (Description == TRUE) {
       BigNames <- names(BigData)[3:length(BigData)]
     } else if (Description == FALSE) {
       BigNames <- names(BigData)[2:length(BigData)]
     }
     shinyalert("Thank you!", "Your data has been succesfully been loaded." , type = "success")
      # } else if(Clicker1 == TRUE){
      #    shinyalert("Error", "You already have data loaded", type = "error")
      # }
     
     
     output$moreControls <- renderUI({
       tagList(
         p("Please Chose the methods you want to create the Bordacount for"),
         checkboxGroupInput("checkGroup", label = h3("Methods"), 
                            choices = BigNames),
         p("")

       )
       
     })
   })
   
   setValues1 <- eventReactive(input$rnks, {
     methods <- input$checkGroup
     checkmethod <<- TRUE
    values <- list(methods)
     })
   

  
   
output$ranks <- renderTable({
    vars <- setValues1()
    methods <- vars [[1]]
    
    if (is.null(BigData )) {
      shinyalert("No Datainput yet", type = "error")
    return()
      }
    if (length(methods) == 0 ) {
      shinyalert("You have not selected any method to proceed", type = "error")
    return()
      }
    
shinyalert("Creating Borda Count", type = "success")
    Borda <- BigData
    Borda$Bordarank  <- 0
    Borda$Bordavalue <- 0

    
    
    
if(Description == TRUE){
  for (i in 1:length(BigData[,1])) {
    Borda$Bordavalue[i] <- sum(BigData[i,methods]) 
  }
}
if(Description == FALSE){
  for (i in 1:length(BigData[,1])) {
    Borda$Bordavalue[i] <- sum(BigData[i,methods]) 
  }
}
 
    
    
        Borda <- Borda[order(Borda$Bordavalue),]
 #  
        Borda$Bordarank <- rank(Borda$Bordavalue)
        Borda <- subset(Borda, select = -Bordavalue)
        

 #    tmp1 <- Borda$Bordavalue[1]
 #    tmp2 <- Borda$Bordavalue[2]
 # for (i in 1:length(BigData[,1])) {
 #  if (tmp1 == tmp2) {
 #    Borda$Bordarank[i]   <- (i+i+1)/2
 #    Borda$Bordarank[i+1] <- (i+i+1)/2
 #  }
 #   
 #   tmp1 <- Borda$Bordavalue[i]
 #   tmp2 <- Borda$Bordavalue[i + 1]
 #   if (Borda$Bordarank[i] == 0){
 #   Borda$Bordarank[i] <- i
 #   }
 # }   
    Borda <<- Borda
    CreateBorda <<- TRUE
return(Borda)
    
  
  })



 


observeEvent(input$init, {
  if (CreateBorda == FALSE) {
    shinyalert("You dont have any data yet!", type = "error")
    # showModal(
    #   modalDialog(
    #     title = 'Error',
    #     p('Hello world!')
    #   )
    # )
  } else {
    shinyjs::runjs("document.getElementById('downloadData').click();")
  }
})
 
 output$downloadData <- downloadHandler(filename = function() {paste("data-", Sys.Date(), ".csv", sep="")},
                                        content = function(file) {write.csv(Borda, file)})


 
 kenButton <- eventReactive(input$ken, {
  return()
 })
 
 
 output$Kendall <- renderPlot({
   kenButton()
   if (is.null(BigData)){shinyalert("You have no data", type = "error")
     return()
     }
   if(CreateBorda == FALSE){
     shinyalert("You haven't created the borda yet", type = "warning")
     }
  
   n <- length(Borda[1,])
   dat <- Borda[,1:(n-1)]
   dat <- dat[,-2]
   ken_data <- dat %>% arrange(ICD)
   
   # cor_data <- tibble(
   #   BCPNN = filter(data, method == "ICA")$rank, 
   #   LGPS = filter(data, method == "LGPS")$rank,
   #   RF = filter(data, method == "RF")$rank, 
   #   LASSO = filter(data, method == "LASSO")$rank#, 
   #   #Borda = filter(data, method == "Borda")$rank
   # )
   # 
    kendall_tau <- cor(ken_data[,2:(n-2)], method = 'kendall')
   # 
   # # Get lower triangle of the correlation matrix
   get_lower_tri <- function(cormat){
     cormat[upper.tri(cormat)] <- NA
     return(cormat)
   }

   cormat <- get_lower_tri(kendall_tau)


   # Melt the correlation matrix
   library(reshape2)
   melted_cormat <- melt(cormat, na.rm = TRUE)
   # Heatmap
   library(ggplot2)
   p <- ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
     geom_tile(color = "white")+
     scale_fill_gradient2(low = "white", high = "#1763AA", #mid = "white",
                          midpoint = 0, limit = c(0,1), space = "Lab",
                          name="Kendall's tau") +
     geom_text(aes(label = round(value, 2))) +
     theme_minimal()+
     theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                      hjust = 1))+
     xlab("") +
     ylab("") +
     coord_fixed()
   p

 })

 
 
 ICD_Button <- eventReactive(input$goIcd, {
   return()
 })

output$ICDs <- renderPlot({
  ICD_Button()
 if (is.null(BigData)){shinyalert("You have no data", type = "error")
   return()
 }
 if(CreateBorda == FALSE){
   shinyalert("You haven't created the borda yet", type = "warning")
 }
 
  text <- input$Code
  codes <- gsub(" ", "", text)
  #codes <- strsplit(codes, ",")
  z <- gsub(",", "|",codes)
  #z <- codes[[1]]
  #z <<- t(z)
  

  
  
  Borda$relative_rank <- Borda$Bordarank/length(Borda[,1])
  

  
  
  hits <- grep(z, Borda$ICD, ignore.case = TRUE)
  Borda2 <<- Borda[hits,]





  
  if (input$label_check == FALSE){
    if(input$Hide == TRUE){
    X <- ggplot(Borda2 , aes(x = relative_rank, y = 0, label = ICD)) +
      # create axis
      annotate("segment", x = 0, xend = 1, y = 0, yend = 0, size = .5) +
      geom_point(size = 3, color = "blue", alpha = 0.5) + #, position=position_jitter(width=0.1,height=00.1)) +
      #geom_text(position=position_jitter(height=0.02)) +
      scale_x_continuous(limits=c(1, 0), expand = c(0.01, 0.01), trans = "reverse") +
      scale_y_continuous(limits = c(-.1,.1), expand = c(0,0)) +
      geom_text(x=0, y=-.02, label="0", color = "black") +
      geom_text(x=-1, y=-.02, label="1", color = "black") +
      theme(panel.background = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank())
    } else if(input$Hide == FALSE){
      X <- ggplot(Borda2 , aes(x = relative_rank, y = 0, label = ICD)) +
        # create axis
        annotate("segment", x = 0, xend = 1, y = 0, yend = 0, size = .5) +
        geom_point(size = 3, color = "blue", alpha = 0.5) + #, position=position_jitter(width=0.1,height=00.1)) +
        geom_text(position=position_jitter(height=0.02)) +
        scale_x_continuous(limits=c(1, 0), expand = c(0.01, 0.01), trans = "reverse") +
        scale_y_continuous(limits = c(-.1,.1), expand = c(0,0)) +
        geom_text(x=0, y=-.02, label="0", color = "black") +
        geom_text(x=-1, y=-.02, label="1", color = "black") +
        theme(panel.background = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              axis.title = element_blank())
    }
  }
  
  else if (input$label_check == TRUE) {
    if(input$Hide == TRUE){
      X <- ggplot(Borda2 , aes(x = relative_rank, y = 0, label = ICD)) +
        # create axis
        annotate("segment", x = 0, xend = 1, y = 0, yend = 0, size = .5) +
        geom_point(size = 3, color = "blue", alpha = 0.5) + #, position=position_jitter(width=0.1,height=00.1)) +
        #geom_text(position=position_jitter(height=0.02)) +
        annotate("segment", x = 0,xend = 0, y=-0.01,yend=0.01, size= .5) +
        annotate("segment",x = .25, xend = .25, y =-0.01,yend=0.01, size= .5) +
        annotate("segment",x = .5, xend = .5, y =-0.01,yend=0.01, size= .5) +
        annotate("segment",x = .75, xend = .75, y =-0.01,yend=0.01, size= .5) +
        annotate("segment",x = 1, xend = 1, y =-0.01,yend=0.01, size= .5) +
        #geom_text(aes(label = x), col="white") +
        scale_x_continuous(limits=c(1, 0), expand = c(0.01, 0.01), trans = "reverse") +
        #scale_x_continuous(limits = c(1,20)) +
        scale_y_continuous(limits = c(-.1,.1), expand = c(0,0)) +
        #geom_text(data = labels, mapping = aes(label = text, x = x, y = 0))+
        geom_text(x=0, y=-.02, label="0", color = "black") +
        geom_text(x=-0.25, y=-.02, label="0.25", color = "black") +
        geom_text(x=-.5, y=-.02, label="0.5", color = "black") +
        geom_text(x=-.75, y=-.02, label="0.75", color = "black") +
        geom_text(x=-1, y=-.02, label="1", color = "black") +
        geom_text(x=-.5, y=-.04, label="Relative Rank", color = "black") +
        #scale_color_manual(values = unname(colours)) +
        theme(panel.background = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              axis.title = element_blank())
    
    }    
    if(input$Hide == FALSE){
     X <- ggplot(Borda2 , aes(x = relative_rank, y = 0, label = ICD)) +
      # create axis
      annotate("segment", x = 0, xend = 1, y = 0, yend = 0, size = .5) +
      geom_point(size = 3, color = "blue", alpha = 0.5) + #, position=position_jitter(width=0.1,height=00.1)) +
      geom_text(position=position_jitter(height=0.02)) +
      annotate("segment", x = 0,xend = 0, y=-0.01,yend=0.01, size= .5) +
      annotate("segment",x = .25, xend = .25, y =-0.01,yend=0.01, size= .5) +
      annotate("segment",x = .5, xend = .5, y =-0.01,yend=0.01, size= .5) +
      annotate("segment",x = .75, xend = .75, y =-0.01,yend=0.01, size= .5) +
      annotate("segment",x = 1, xend = 1, y =-0.01,yend=0.01, size= .5) +
      #geom_text(aes(label = x), col="white") +
      scale_x_continuous(limits=c(1, 0), expand = c(0.01, 0.01), trans = "reverse") +
      #scale_x_continuous(limits = c(1,20)) +
      scale_y_continuous(limits = c(-.1,.1), expand = c(0,0)) +
      #geom_text(data = labels, mapping = aes(label = text, x = x, y = 0))+
      geom_text(x=0, y=-.02, label="0", color = "black") +
      geom_text(x=-0.25, y=-.02, label="0.25", color = "black") +
      geom_text(x=-.5, y=-.02, label="0.5", color = "black") +
      geom_text(x=-.75, y=-.02, label="0.75", color = "black") +
      geom_text(x=-1, y=-.02, label="1", color = "black") +
      geom_text(x=-.5, y=-.04, label="Relative Rank", color = "black") +
      #scale_color_manual(values = unname(colours)) +
      theme(panel.background = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank())
  
    }
  }
  plot(X)
  
 })

output$downloadPlot <- downloadHandler(
  filename = function(){paste("input$ICDs",'.png',sep='')},
  content = function(file){
    ggsave(file)
  }
)
}
# Run the app ----
shinyApp(ui = ui, server = server)
