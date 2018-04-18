
#avoids the behavior of sample on a single value
sampleCurveTinder <- function (x, size, replace = FALSE, prob = NULL) 
{
  if(length(x) == 0){
    
    return(integer(0))
    
  }
  
  
  if (missing(size)) 
    size <- length(x)
  x[sample.int(length(x), size, replace, prob)]
  
}