require(plotrix)

transformData <- function(data , patterns = c("Peptides.", "Unique.peptides.", "^Intensity.", "^LFQ.intensity.", "MS.MS.Count.", "Normalized.Intensity.", "Normalized.LFQ.intensity.") ){
  
  colnames(data) <- gsub("\\.DMSO", ".0nM", colnames(data))
  
  conc <- extractConc(data , "^Normalized.Intensity.")
  
  tdata <- data.frame(concentration = conc)
  tdata <- data.frame(uniqueIdentifier = rep(data[,"uniqueIdentifier"] , times = dim(tdata)[1]) , tdata)
  
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
  

plot_CurvePlot <- function(data , selected , called ,  pattern = c("Normalized.Intensity.") )
{
  
  data <- data[selected , ]
  
  
  paraNames <- grep("Bottom|Top|Slope|Inflection" , x = names(data) , value = T)
  
  paramCurve <- data[,grep("Bottom|Top|Slope|Inflection" , x = names(data))]
  names(paramCurve) <- paraNames
  
  
    plotCurve <- T
  
  if(any(is.na(paramCurve))){
    
    plotCurve <- F
    
  }
  
  origData <- transformData(data , c("Peptides.", "Unique.peptides.", "^Intensity.", "^LFQ.intensity.", "MS.MS.Count.", "Normalized.Intensity.", "Normalized.LFQ.intensity."))
  

  
  origData <- origData[order(origData$concentration),]
  
   
  
  
  target <- as.character(unique(origData$uniqueIdentifier))
  
  
  layout(matrix( nrow = 1 , ncol = 2 , c(1,2) , byrow = T) , widths = c(60,40) )
  
 
  
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
  
  
  plot(y = intensity , x = newdose , log = "x", xaxt = "n" , xlab = "" , ylab = "" , pch = 19 , ylim = c(0 , max(intensity , na.rm = T)))
  
  title(xlab = "Concentration [nM]" , ylab = pattern)
  
  axis(1 , at = newdose ,labels = as.character(origData$concentration[!is.na(origData[,grep(pattern , names(origData))])]))
 
  axis.break(axis = 1 , breakpos = 2*min(newdose))
  
  if(plotCurve)
  {
    CurveFunc(5 , paramCurve)
    curve(CurveFunc(x , paramCurve), from = 2*min(newdose) , add = T)
  }
 
  
  #Observation Plot
  
  plot(x = c(xaxisObser , xaxisObser) , y = c(origData$MS.MS.Count. , origData$Unique.peptides.) , axes = F , xlab = "" , ylab = "", pch = rep(c(19,20),each = nobservations) , col = rep(c("black" , "red") , each = nobservations) )
  
  axis(1, at = 1:nobservations , labels = xDiscrete , las = 2)
  axis(2)
  
  mtext("Observations" , side = 3 , line = 0.0 , adj = 0.5 , font = 2 , cex = par("cex"))
  title(ylab = "MSMS Count", line = 3.0 , col = "black")
  mtext("Unique peptides" , line = 2.0 , side = 2 , col = "red" , cex = par("cex"))
  
  
  
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

