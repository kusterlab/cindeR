plot_DefaultPlot <- function(data , selected , called , yValue)
{
  
  data <- data[selected , ]
  
  barplot(unlist(data[,yValue]), las = 2)
  
}
