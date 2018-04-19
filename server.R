require(shiny)
#require(shinyjs)

#TOREMOVE: for all plotfunctions
source("./helpers/plotfunction.R")
source("./helpers/helpers.R")

options(shiny.maxRequestSize=1000*1024^2)


shinyServer(function(input, output , session) {
  
  value <- reactiveValues()
  
  data <- NULL

  backcounter <- 0
  
  plotfun_Env <- new.env()
  
  plotfun <- NULL
  
  value$selected <- NULL
  
  counter <- NULL
  
  called <- 0
  
  backbuffer <- vector()
  
  observeEvent(input$file , {
    
   value$data <<- try(read.csv(input$file$datapath , header = input$import.header , sep = input$import.sep , quote = input$import.quote , stringsAsFactors = F , row.names = NULL , na.strings = c("" , "NA")))
   
  #column is called JTarget due to the fact that R has a nasty autocomplition for subsetting lists with $ and to avoid to break it JTarget is Choosen as uniqe
   called <<- 0
   if(is.null(value$data$JTarget)){
   
   value$data <- cbind(value$data , JTarget = rep(NA , times = nrow(value$data)) )
   value$selected <- sample(1:nrow(value$data) , 1)
   counter <<- 0
   }else{
     
     remaining <- as.numeric(rownames(value$data[is.na(value$data$JTarget),]))
     value$selected <- sample(remaining , 1)
     counter <<- nrow(value$data)-length(remaining)
     
     
   }
   
   
  
  })
  
  output$progress <- renderUI({
    
    if(is.null(value$data)){
      
      return(NULL)
      
    }else{
      
      return(paste(counter , "/" ,  nrow(value$data)))
      
    }
    
    
  })

 
  
  output$plotout <- renderPlot({
    validate(need(!is.null(value$data) , "No data set selected"),
             #TOREMOVE: is needed if upload plotfun should be activated
             need((length(value$selected) > 0 & !is.null(value$selected)), "Everything done")
             #,need(!is.null(plotfun) , "No plotfunction available")
             )
    called <<- called+1
    #the secound argument is here specific for the data
    #TOREMOVE: is needed if upload plotfun should be activated and replace the other plotfun
    #plotfun(data = value$data ,value$selected = value$selected)
    plot_CurvePlot(data = value$data ,selected = value$selected , called = called )
    print(value$selected)
  })
  
  

  
observeEvent(input$decision,{
  #TOREMOVE: is needed if upload plotfun should be activated and replace the other plotfun
  #req(!is.null(plotfun))
  req(length(value$selected) > 0)
 
  print(value$selected)

  if(!is.null(value$data)){

    if(input$decision[1] == 39){

      value$data[value$selected , "JTarget"] <- TRUE
      remaining <- as.numeric(rownames(value$data[is.na(value$data[, "JTarget"]),]))
      
      
      if(length(remaining) == 0){
        
        value$selected <- integer(0)
        counter <<- counter+1
        
      }else{
        value$selected <- sampleCurveTinder(remaining , 1)
        counter <<- counter+1
      }

    }else if(input$decision[1] == 37){
      
      value$data[value$selected , "JTarget"] <- FALSE
      
      remaining <- as.numeric(rownames(value$data[is.na(value$data[, "JTarget"]),]))
      

      if(length(remaining) == 0){
        
        value$selected <- integer(0)
        counter <<- counter+1
        
      }else{
      value$selected <- sampleCurveTinder(remaining , 1)
      counter <<- counter+1
      }

    }
    
    if(backcounter != 0){
      
      counter <<- counter-1
    }else{
    
    if(length(backbuffer) <10){
      
      backbuffer <<- c(value$selected , backbuffer)
      
    }else{
      
      backbuffer <<- c( value$selected , backbuffer[-10] )
      
    }
    
    }
   
      backcounter <<- 0

  
  }
  
  
  output$Save <- downloadHandler(filename = paste0(strsplit(input$file$name , split = "." , fixed = T)[[1]][-length(strsplit(input$file$name , split = "." , fixed = T)[[1]])] , "_judged.csv") , content = function(file){
    
    write.csv(value$data , file = file, row.names = F)
    
  })
  
})
  
  observeEvent( input$back , {
    
    
    backcounter <<- backcounter + 1
    
    if(backcounter > 10){
      
      backcounter <<- 10
      
    }
    
    if(backcounter == 1){
    
    backbuffer <<- backbuffer[-1]

    }
    
    tmp <- backbuffer[backcounter]
    
    if(!is.na(tmp)){
      
      value$selected <- tmp
      
    }
    
    print(backbuffer)
    
  })
  
  







  
  
 
observe({
  
  f <- input$plotScript$datapath
  
  if(!is.null(f)){
    rm(list = ls(envir = plotfun_Env) ,envir =  plotfun_Env)
    source(f , local = plotfun_Env)
    
    
  }
  
  if(length(grep("^plot_" , ls(envir = plotfun_Env))) == 1){
    
    plotfun <<- plotfun_Env[[ (grep("^plot_" , ls(envir = plotfun_Env) , value = T))]]
  }
  
  

  
  output$plotfuncode <- renderPrint({
    
    validate(need(!is.null(plotfun), message = "No plot function avaiable!\n"))
    
    for(n in ls(envir = plotfun_Env)){
      
      print(plotfun_Env[[n]])
      
    }
    
    
  })
  
  
})

  
  
  
  
  
  
  
  
  
})