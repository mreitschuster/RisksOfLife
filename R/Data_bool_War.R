#' Data Type - bool_war
#' 
#' verifies wether the object has the expected format and fullfills some boundary conditions.
#' 
#' @param bool_war put in the object you want to check.
#' @return none the code will just throw an error if the object is not as expected.  
#' @export 
#' 

Val_Data_bool_War <- function(bool_war)
{
    # let's check if the input is the expected format
    if (!(ncol(bool_war)==2 & nrow(bool_war)==2)){stop('Error: the input bool_war does not have the expected dimensions 2x2.')}
    if (!isTRUE(all.equal(colnames(bool_war),c('Probability','Severity')))){stop('Error: the input bool_war seems to have wrong colnames.')}
    if (!isTRUE(all.equal(rownames(bool_war),c('no War','War')))){stop('Error: the input bool_war seems to have wrong rownames.')}
  
  return(0)
}
