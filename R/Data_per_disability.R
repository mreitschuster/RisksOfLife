#' Data Type - per_disability
#' 
#' verifies wether the object has the expected format and fullfills some boundary conditions.
#' 
#' @param per_disability put in the object you want to check.
#' @return none the code will just throw an error if the object is not as expected.  
#' @export 
#' 


Val_Data_per_disability <- function(per_disability)
{
    # let's check if the input is the expected format
    if (!(ncol(per_disability)==2 & nrow(per_disability)==101)){stop('Error: the input per_disability does not have the expected dimensions 2x2.')}
    if (!isTRUE(all.equal(colnames(per_disability),c('Probability','Severity')))){stop('Error: the input per_disability seems to have wrong colnames.')}
    
  return(0)
}

