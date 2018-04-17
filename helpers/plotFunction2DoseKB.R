TUMmauve <-"#69085A"
TUMviolet <-"#0F1B5F"
TUMblackblue <- "#003359"
TUMdarkblue <- "#005293"
TUMblue <- "#0073CF"
TUMlightblue <- "#64A0C8"
TUMciel <- "#98C6EA"
TUMturk <- "#00778A"
TUMgreen <- "#007C30"
TUMlightgreen <- "#679A1D"
TUMolive <- "#A2AD00"
TUMbeige <- "#DAD7CB"
TUMyellow <- "#FFDC00"
TUMgold <- "#F9BA00"
TUMorange <- "#E37222"
TUMbrick <- "#D64C13"
TUMred <- "#C4071B"
TUMblood <- "#9C0D16"

pre_cols <- c(TUMmauve, TUMviolet, TUMblackblue, TUMdarkblue, TUMblue, TUMlightblue, TUMciel, TUMturk, TUMgreen, TUMlightgreen, TUMolive, TUMbeige, TUMyellow, TUMgold, TUMorange, TUMbrick, TUMred, TUMblood)


root <- ""
# see and of script for function calls


plotIntensityBar <- function(data, ints = c("LFQ.intensity" , "Intensity" )) {
  
  idxs <- grep(paste("Median.Normalized.", ints[1], sep=""), colnames(data))
  labs <- gsub("DMSO", "0", gsub("nM", "", sapply(strsplit(colnames(data)[idxs], "\\."), function(d) d[length(d)])))
  dat <- matrix(NA, length(ints), length(labs))
  rownames(dat) <- ints
  colnames(dat) <- labs
  vdat <- dat
  qua <- dat
  for (i in 1:length(ints)) {
    idxs <- grep(paste("Median.Normalized.", ints[i], sep=""), colnames(data))
    dat[i,] <- unlist(data[,idxs])
    jdxs <- grep(paste("Var.Normalized.", ints[i], sep=""), colnames(data))
    vdat[i,] <- sqrt(unlist(data[,jdxs]))
    idxs <- grep(paste("Quantile.", ints[i], sep=""), colnames(data))
    qua[i,] <- unlist(data[,idxs]) #c(qua, unlist(data[,idx]))
  }
  
  
  dat <- dat[,order(as.numeric(colnames(dat)))]
  vdat <- vdat[,order(as.numeric(colnames(vdat)))]
  vdat[vdat==0] <- NA
  qua <- qua[,order(as.numeric(colnames(qua)))]
  dat[!is.finite(dat)] <- 0
  yr <- range(c(0, max(dat, na.rm=T), max(max(dat, na.rm=T) + max(min(max(dat*2, na.rm=T), max(abs(vdat), na.rm=T), na.rm=T), na.rm=T), 1)), na.rm=T)
  barc <- barplot(dat, beside=T, ylim=yr, ylab="Relative intensity", xlab="Concentration [nM]", las=1, col=c(TUMdarkblue, TUMorange))
  mtext(paste("", paste(data$uniqueIdentifier[1] , "prob:" , data[1,"prob.TRUE"]), sep=""), side=3, adj=0, line=2.5, cex=1, font=2)
  mtext(paste("", paste("Observed:" , data[1,"truth"] , "Predicted:" , data[1, "response"] ), sep=""), side=3, adj=0, line=1.0, cex=1, font=2)
  #title("Intensity")
  aco <- character(length(ints))
  aco[1] <- "#000000"
  aco[2] <- TUMblood
  arrows(barc, dat - vdat, barc, dat + vdat, lwd=1.5, angle=90, code=3, length=0.05, col=aco)
  
  text(barc, yr[2]*0.05, paste(round(qua*100, digits=0), "%", sep=""), cex=0.7, font=2)
  
  # prob <- unlist(data$Target.probability)
  # tar <- unlist(data$Target.annotation)
  
  # par(xpd=T)
  # 
  # if ((tar >= 5) | (tar == -10 & prob > 0.5)) {
  #   ec <- unlist(data$Estimated.EC50)
  #   if (ec < 0) {
  #     ec <- 0
  #   }
  #   ndigits <- -min(floor(log10(ec)), 0)
  #   
  #   if (length(ints)>1) {
  #     x <- barc[length(ints),length(labs)]
  #   } else {
  #     x <- barc[length(labs),1]
  #   }
  #   text(x, yr[2]*1.0, paste("Estimated EC50: ", round(ec, digits=ndigits), "nM", sep=""), pos=2)
  #   text(x, yr[2]*0.9, paste("Target probability: ", round(prob * 100, digits=0), "%", sep=""), pos=2)
  # }
  # 
  # par(xpd=F)
}


plot_plotOne <- function(data, selected, ints = c("LFQ.intensity" , "Intensity" ), disablePar=F) {
  
  
  data <- data[selected , ]
  
  if (!disablePar) {
    par(mfrow=c(1,2))
  }
  
  plotIntensityBar(data, ints = ints)
  plotObservations(data)
}

plotObservations <- function(data, types = c("Peptides", "Unique.peptides", "MS.MS.count")) {
  
  idxs <- grep(paste(types[1], ".*nM", sep=""), colnames(data))
  labs <- c(0, gsub("nM", "", sapply(strsplit(colnames(data)[idxs], "\\."), function(d) d[length(d)])))
  dat <- matrix(NA, length(types), length(labs))
  rownames(dat) <- types
  colnames(dat) <- labs
  for (i in 1:length(types)) {
    idxs <- grep(paste(types[i], ".*DMSO.*", sep=""), colnames(data))
    dat[i,1] <- median(unlist(data[,idxs]), na.rm=T)
    idxs <- grep(paste(types[i], ".*nM", sep=""), colnames(data))
    dat[i,2:(length(labs))] <- unlist(data[,idxs])
  }
  dat <- dat[,order(as.numeric(colnames(dat)))]
  
  cols <- pre_cols[1:length(types)]
  pchs <- 1:length(types)
  
  yr <- range(c(0, range(dat)*1.5))
  plot(-10, -10, xlab="Concentration [nM]", ylab="Counts", ylim=yr, xlim=c(1, length(labs)), xaxt="n", bty="n")
  #title("Observations")
  axis(1, at=1:length(labs), labels=colnames(dat), lwd=2, lwd.ticks=1, cex.lab=1, cex.axis=1, font.axis=1, las=1)
  for (i in 1:length(types)) {
    lines(1:length(labs), dat[i,], col=cols[i], pch=pchs[i], type="b", lwd=2)
  }
  legend("topright", legend=types, col=cols, pch=pchs, lty=1, lwd=2)
  
}

colDataAndNN <- function(data , NeighbourResult)
{
  results <- lapply(NeighbourResult , function(x, data){
    
    tmp <- data[rownames(x),]
    tmp <- cbind(tmp , x[,c("truth" , "response" , "prob.TRUE")])
    
  } , data = data)
  
  return(results)
}

plotNN <- function(data ,filename , directory){
 
  pdf(file = paste(directory , filename , ".pdf" , sep = "")  , paper = "a4r" , width = 0 , height = 0)
  
   for(n in 1:length(data)){

     par(mfrow=c(2, 4))
     
     
    for(i in 1:nrow(data[[n]])){
      
      plotIntensityBar(data[[n]][i,])
      plotObservations(data[[n]][i,])
      
    }
    
     while(!par('page')) plot.new()
    
  }
  
  dev.off()
  
}

## work#
#searchspace has to be data which is used for perdiction


# neighbors <- nearestNeighbors(uniqueIdentifier =  , searchspace =  , prediction = prediction , targetColumn = "Target" , nNeighbor = 10000 , normalize = T)
# 
# # data has to be the full data without subset of anything
# 
# plotdata <- colDataAndNN(data , NeighbourResult = neighbors)
# 
# plotNN(plotdata , directory = root)

