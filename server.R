require(shiny)

#TOREMOVE: for all plotfunctions
#source("./helpers/plotfunction.R")
source("./helpers/helpers.R")

options(shiny.maxRequestSize=1000*1024^2)


shinyServer(function(input, output , session) {
  
  card_swipe <- callModule(shinyswipr, "plot_swiper")
  
  value <- reactiveValues()
  
  data <- NULL

  backcounter <- 0
  
  plotfun_Env <- new.env()
  
  value$plotfun <- NULL
  
  value$selected <- NULL
  
  counter <- NULL
  
  called <- 0
  
  backbuffer <- vector()
  
  value$y <- NULL
  
  value$x <- NULL
  
  observeEvent(input$file , {
    
   value$data <<- try(read.csv(input$file$datapath , header = input$import.header , sep = input$import.sep , quote = input$import.quote , stringsAsFactors = F , row.names = NULL , na.strings = c("" , "NA")))
   
  #column is called Class due to the fact that R has a nasty autocomplition for subsetting lists with $ and to avoid to break it Class is Choosen as uniqe
   called <<- 0
   
   value$y <<- colnames(value$data)[apply(value$data , 2 ,is.numeric)]
   
   value$x <<- colnames(value$data)
   
   if(is.null(value$data$Class)){
   
   value$data <- cbind(value$data , Class = rep(NA , times = nrow(value$data)) )
   value$selected <- sample(1:nrow(value$data) , 1)
   counter <<- 0
   }else if(all(!is.na(value$data$Class))){
     
     counter <<- nrow(value$data)
     
     
   }else{
     
     remaining <- as.numeric(rownames(value$data[is.na(value$data$Class),]))
     value$selected <- sample(remaining , 1)
     counter <<- nrow(value$data)-length(remaining)
     
     
   }
   
   backcounter <<- 0
   backbuffer <<- value$selected
  
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
             #TOREMOVE: is needed if upload value$plotfun should be activated
             need((length(value$selected) > 0 & !is.null(value$selected)), "Everything done")
             ,need(!is.null(value$plotfun) , "No plotfunction available")
             )
    called <<- called+1
    #the secound argument is here specific for the data
    #TOREMOVE: is needed if upload value$plotfun should be activated and replace the other value$plotfun
    value$plotfun(data = value$data ,selected = value$selected , called = called)
    #plot_CurvePlot(data = value$data ,selected = value$selected , called = called )
  })
  
  

  
observeEvent( card_swipe() , {
  #TOREMOVE: is needed if upload value$plotfun should be activated and replace the other value$plotfun
  req(!is.null(value$plotfun))
  req(length(value$selected) > 0)
 

  if(!is.null(value$data)){

    if( card_swipe() == "right"){

      value$data[value$selected , "Class"] <- TRUE
      remaining <- as.numeric(rownames(value$data[is.na(value$data[, "Class"]),]))
      
      
      if(length(remaining) == 0){
        
        value$selected <- integer(0)
        counter <<- counter+1
        
      }else{
        value$selected <- sampleCurveTinder(remaining , 1)
        counter <<- counter+1
      }

    }else if(card_swipe() == "left"){
      
      value$data[value$selected , "Class"] <- FALSE
      
      remaining <- as.numeric(rownames(value$data[is.na(value$data[, "Class"]),]))
      

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
    }
   
    if(!any(value$selected == backbuffer)){
     
    if(length(backbuffer) <10){
      
      backbuffer <<- c(value$selected , backbuffer)
      
    }else{
      
      backbuffer <<- c( value$selected , backbuffer[-10] )
      
    }
    
    }
   
      backcounter <<- 0
      
    
  }

  
})
  

observe({
  
  req(!is.null(value$data))

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
    

  })
  
  
  observeEvent( input$decision , {
    #TOREMOVE: is needed if upload value$plotfun should be activated and replace the other value$plotfun
    req(!is.null(value$plotfun))
    req(length(value$selected) > 0)
    
    
    if(!is.null(value$data)){
      
      if( input$decision[1] == 39){
        
        value$data[value$selected , "Class"] <- TRUE
        remaining <- as.numeric(rownames(value$data[is.na(value$data[, "Class"]),]))
        
        
        if(length(remaining) == 0){
          
          value$selected <- integer(0)
          counter <<- counter+1
          
        }else{
          value$selected <- sampleCurveTinder(remaining , 1)
          counter <<- counter+1
        }
        
      }else if(input$decision[1] == 37){
        
        value$data[value$selected , "Class"] <- FALSE
        
        remaining <- as.numeric(rownames(value$data[is.na(value$data[, "Class"]),]))
        
        
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
      }
      
      if(!any(value$selected == backbuffer)){
        
        if(length(backbuffer) <10){
          
          backbuffer <<- c(value$selected , backbuffer)
          
        }else{
          
          backbuffer <<- c( value$selected , backbuffer[-10] )
          
        }
        
      }
      
      backcounter <<- 0
      
      
    }

  })
  






  
  
 
observe({
  
  f <- input$plotScript$datapath
  
  if(!is.null(f)){
    rm(list = ls(envir = plotfun_Env) ,envir =  plotfun_Env)
    source(f , local = plotfun_Env)
    
    
  }
  
  if(length(grep("^plot_" , ls(envir = plotfun_Env))) == 1){
    
    value$plotfun <<- plotfun_Env[[ (grep("^plot_" , ls(envir = plotfun_Env) , value = T))]]
  }
  
  

  
  output$plotfuncode <- renderPrint({
    
    validate(need(!is.null(value$plotfun), message = "No plot function avaiable!\n"))
    
    for(n in ls(envir = plotfun_Env)){
      
      print(plotfun_Env[[n]])
      
    }
    
    
  })
  
  
  output$plotfunctionSelector <- renderUI({

      list(
      selectInput("yValue" , label = "Select a y value" , choices = value$y , multiple = F), 
      br(),
      selectInput("xValue" , label = "Select x values" , choices = value$x , multiple = T))
      
  })
  
  
})



  
  
  
  
  
  
  
  
  
})