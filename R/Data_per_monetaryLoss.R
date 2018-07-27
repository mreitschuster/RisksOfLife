#' Data Type - per_monetaryLoss
#' 
#' verifies wether the object has the expected format and fullfills some boundary conditions.
#' 
#' @param per_monetaryLoss put in the object you want to check.
#' @return none the code will just throw an error if the object is not as expected.  
#' @export 
#' 

Val_Data_per_monetaryLoss <- function(per_monetaryLoss)
{
    # let's check if the input is the expected format
    if (!ncol(per_monetaryLoss)==2 ){stop('Error: the input per_monetaryLoss does not have the expected dimensions 2x2.')}
    if (!isTRUE(all.equal(colnames(per_monetaryLoss),c('Probability','Severity')))){stop('Error: the input per_monetaryLoss seems to have wrong colnames.')}
    
  return(0)
}

