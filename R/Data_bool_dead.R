#' Data Type - bool_dead
#' 
#' verifies wether the object has the expected format and fullfills some boundary conditions.
#' 
#' @param bool_dead put in the object you want to check.
#' @return none the code will just throw an error if the object is not as expected.  
#' @export 
#' 

Val_Data_bool_dead <- function(bool_dead)
{
    # let's check if the input is the expected format
    if (!(ncol(bool_dead)==2 & nrow(bool_dead)==2)){stop('Error: the input bool_dead does not have the expected dimensions 2x2.')}
    if (!isTRUE(all.equal(colnames(bool_dead),c('Probability','Severity')))){stop('Error: the input bool_dead seems to have wrong colnames.')}
    if (!isTRUE(all.equal(rownames(bool_dead),c("not dead", "dead")))){stop('Error: the input bool_dead seems to have wrong rownames.')}
  
  return(0)
}
