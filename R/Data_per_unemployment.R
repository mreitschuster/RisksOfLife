#' Data Type - per_unemployment
#' 
#' verifies wether the object has the expected format and fullfills some boundary conditions.
#' 
#' @param per_unemployment put in the object you want to check.
#' @return none the code will just throw an error if the object is not as expected.  
#' @export 
#' 


Val_Data_per_unemployment <- function(per_unemployment)
{
    # let's check if the input is the expected format
    if (!(ncol(per_unemployment)==2 & nrow(per_unemployment)==101)){stop('Error: the input per_unemployment does not have the expected dimensions 2x2.')}
    if (!isTRUE(all.equal(colnames(per_unemployment),c('Probability','Severity')))){stop('Error: the input per_unemployment seems to have wrong colnames.')}
    
  return(0)
}

