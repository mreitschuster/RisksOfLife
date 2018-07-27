#' Data Type - per_StockLoss
#' 
#' verifies wether the object has the expected format and fullfills some boundary conditions.
#' 
#' @param per_StockLoss put in the object you want to check.
#' @return none the code will just throw an error if the object is not as expected.  
#' @export 
#' 


Val_Data_per_StockLoss <- function(per_StockLoss)
{
    # let's check if the input is the expected format
    if (!(ncol(per_StockLoss)==2 & nrow(per_StockLoss)==101)){stop('Error: the input per_StockLoss does not have the expected dimensions 2x2.')}
    if (!isTRUE(all.equal(colnames(per_StockLoss),c('Probability','Severity')))){stop('Error: the input per_StockLoss seems to have wrong colnames.')}
    
  return(0)
}

