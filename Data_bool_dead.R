# Risk Of Life
# Implementation of the validation for the data type bool_dead
# written by Markus Reitschuster on 24.7.18

Val_Data_bool_dead <- function(bool_dead)
{
    # let's check if the input is the expected format
    if (!(ncol(bool_dead)==2 & nrow(bool_dead)==2)){stop('Error: the input bool_dead does not have the expected dimensions 2x2.')}
    if (!isTRUE(all.equal(colnames(bool_dead),c('Probability','Severity')))){stop('Error: the input bool_dead seems to have wrong colnames.')}
    if (!isTRUE(all.equal(rownames(bool_dead),c("not dead", "dead")))){stop('Error: the input bool_dead seems to have wrong rownames.')}
  
  return(0)
}
