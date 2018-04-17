require(plotrix)
require(drc)
# load("Z://users_files/Florian/Master/R_project/10DoseKB/actuel Data/data_suzan_new_try_raw.RData")
# 
# data <- data[1:3,]

transformData <- function(data , patterns = c("Reporter.intensity.corrected.") ){
  
  colnames(data) <- gsub("\\.DMSO", ".0nM", colnames(data))
  
  conc <- extractConc(data , "^Reporter.intensity.corrected.")
  
  tdata <- data.frame(concentration = conc)
  #tdata <- data.frame(uniqueIdentifier = rep(data[,"uniqueIdentifier"] , times = dim(tdata)[1]) , tdata)
  
  for(idx in patterns){
    
    subdata <- data[,grep(idx , names(data))]
    
    names(subdata) <- extractConc(subdata , idx)
    
    subdata <- t(subdata)
    
    subdata <- subdata[match(tdata$concentration , as.numeric(rownames(subdata))),]
    
    
    
    tdata <- data.frame(tdata ,  subdata)
    
    names(tdata)[dim(tdata)[2]] <- gsub("^" , "" , idx , fixed = T)
    
    
  }
  
  return(tdata)
}

extractConc <- function(data , pattern){
  
  conc <- grep(paste0(pattern , "*nM") , names(data) , value = T)
  conc <- as.numeric(gsub("nM" , "" , gsub(pattern , replacement = "" , x = conc)))
  
  return(conc)
}




plot_CurvePlot <- function(data , selected ,  pattern = c("Reporter.intensity.corrected.") , called)
{
  if(called == 1){
  histData <<- apply(data[,grep("Reporter.intensity.corrected.|Intensity" , names(data))] , 1 , function(x)
  {
    res <- x["Intensity"]*(max(x[grep("Reporter.intensity.corrected." , names(x))] , na.rm = T)/sum(x[grep("Reporter.intensity.corrected." , names(x))] , na.rm = T))
  
    return(res)
  })
  }
  data <- data[selected , ]
  
  origData <- transformData(data , pattern)
  
  
  histLine <-  max(origData[,"Reporter.intensity.corrected."] , na.rm = T)/ sum(origData[, "Reporter.intensity.corrected."] , na.rm = T)
  
  histLine <- histLine*data[,"Intensity"]
  
  origData[,pattern] <- origData[,pattern] / origData[ origData$concentration == 0, pattern ]
  
  fit <- try(drm(Reporter.intensity.corrected. ~ concentration ,  data=origData, fct=LL.4()) , silent = T)
  
  

  
  if(class(fit) != "try-error"){
   coefficien <- fit$coefficients
   
   names(coefficien) <- c("Slope" , "Bottom" , "Top" , "Inflection")
  
   estimate <- summary(fit)$coefficients["e:(Intercept)" , c("Estimate" , "Std. Error")]
    
    plotCurve <- T
    
   
  }else{
    
    plotCurve <- F
    
    
  }

  
  
  
  origData <- origData[order(origData$concentration),]
  
  
  
  
  #target <- as.character(unique(origData$uniqueIdentifier))
  
  
  
  
  
  
  if(!is.null(histData)){
  
  layout(matrix( nrow = 1 , ncol = 2 , c(1,2) , byrow = T) , widths = c(60,40) )
  }
  
  
  
  par(cex = 1)
  
  par(mar = c( 5.1 , 4.1, 4.1, 0.5))
  
  newdose <- origData$concentration[!is.na(origData[,grep(pattern , names(origData))])]
  
  intensity <- origData[!is.na(origData[,pattern]),grep(pattern , names(origData))]
  
  
  
  newdose[newdose == 0] <- min( newdose[newdose != 0] )/10
  
  
  
  nobservations <- length(origData$concentration)
  
  xaxisObser <- 1:length(origData$concentration)
  
  
  xDiscrete <- sapply(origData$concentration , function(x)
  {
    
    if(x == 0)
    {
      return("DMSO")  
    } else{
      return(paste(x , "nM" , sep = " "))
    }
  })
  
  
  
  #TOFIX: is the plot generated right or not , then check the arrowbar
  plot(y = intensity , x = newdose , log = "x", xaxt = "n" , xlab = "" , ylab = "" , pch = 19 , ylim = c(0 , max(intensity , na.rm = T)))
  
  title(xlab = "Concentration [nM]" , ylab = pattern)
  
  abline(h = 0.5 , lty = 2)
  abline(h = 2 , lty = 2)
  
  #title(main = paste(target , "prob:" , data[1,"prob.TRUE"] , sep = " ") ,  adj =0 , font = 2 , cex.main = 1.2 , line = 2.5)
  
  axis(1 , at = newdose ,labels = as.character(origData$concentration[!is.na(origData[,grep(pattern , names(origData))])]))
  #mtext(paste("", paste("Observed:" , data[1,"truth"] , "Predicted:" , data[1, "response"] ), sep=""), side=3, adj=0, line=1.0, cex=1, font=2)
  axis.break(axis = 1 , breakpos = 2*min(newdose))
  
  if(plotCurve)
  {
    curve(CurveFunc(x ,coefficien), from = 2*min(newdose) , add = T)
    
    y <- (coefficien["Top"]-coefficien["Bottom"])/2 + coefficien["Bottom"]
    x <- estimate["Estimate"]
    sd <- estimate["Std. Error"]

    distance <- log10(x + sd) - log10(x)
    xmin <- 10^(log10(x) - distance)
    xmax <- x + sd

    arrows( xmin ,y , xmax , y , code = 3 , angle = 90 , length = 0.05)

    rsqu <- rsquare(fit)
    
    legend("bottomleft" ,  legend = c(paste("b =" , round(coefficien["Bottom"] , digits = 2),sep = " ") ,
                                      paste("t =" , round(coefficien["Top"] , digits = 2),sep = " ") , 
                                      paste("s =" , round(coefficien["Slope"] , digits = 2) , sep = " ") , 
                                      paste("i =" , round(coefficien["Inflection"] , digits = 2) , sep = " "), 
                                      paste("R2 =" , round(rsqu , digits = 2) , sep = " ")
    ) , bty = "n" , cex = 0.9)
    
    
    
    
    
    
  }
  
  
  if(!is.null(histData)){
    
    hist(log10(histData) , xlab = "log10(Intensity)" , main = NULL , breaks = 50)
   
    if(!is.infinite(histLine)){
      abline(v = log10(histLine) , col = "red")
    }
    
  }
  
  
  
  
  
  
  
}


CurveFunc <- function(x , parameter)
{
  
  bottom <- as.numeric(parameter["Bottom"])
  top <- as.numeric(parameter["Top"])
  slope <- as.numeric(parameter["Slope"])
  inflection <- as.numeric(parameter["Inflection"])
  
  
  x <- (bottom+(top-bottom)/(1 +exp(slope*(log(x) - log(inflection)))))
  
  return(x)
  
}

rsquare <- function(curve)
{
  meanobs <- mean(curve$origData$Reporter.intensity.corrected.)
  sst <- sum((curve$origData$Reporter.intensity.corrected. - meanobs)^2)
  predictobs <- sapply(curve$additionalInformation$origData$dose , function(dose) curve$curve[[1]](dose))
  ssr <- sum((curve$predres[,2])^2)
  
  return(1 - ssr/sst)
  
}


