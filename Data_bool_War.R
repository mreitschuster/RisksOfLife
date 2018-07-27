# Risk Of Life
# Implementation of the validation for the data type bool_war
# written by Markus Reitschuster on 24.7.18

Val_Data_bool_War <- function(bool_war)
{
    # let's check if the input is the expected format
    if (!(ncol(bool_war)==2 & nrow(bool_war)==2)){stop('Error: the input bool_war does not have the expected dimensions 2x2.')}
    if (!isTRUE(all.equal(colnames(bool_war),c('Probability','Severity')))){stop('Error: the input bool_war seems to have wrong colnames.')}
    if (!isTRUE(all.equal(rownames(bool_war),c('no War','War')))){stop('Error: the input bool_war seems to have wrong rownames.')}
  
  return(0)
}
