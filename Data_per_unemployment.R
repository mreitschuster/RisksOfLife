# Risk Of Life
# Implementation of the validation for the data type per_unemployment
# written by Markus Reitschuster on 24.7.18

Val_Data_per_unemployment <- function(per_unemployment)
{
    # let's check if the input is the expected format
    if (!(ncol(per_unemployment)==2 & nrow(per_unemployment)==101)){stop('Error: the input per_unemployment does not have the expected dimensions 2x2.')}
    if (!isTRUE(all.equal(colnames(per_unemployment),c('Probability','Severity')))){stop('Error: the input per_unemployment seems to have wrong colnames.')}
    
  return(0)
}

