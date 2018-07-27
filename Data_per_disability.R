# Risk Of Life
# Implementation of the validation for the data type per_disability
# written by Markus Reitschuster on 24.7.18

Val_Data_per_disability <- function(per_disability)
{
    # let's check if the input is the expected format
    if (!(ncol(per_disability)==2 & nrow(per_disability)==101)){stop('Error: the input per_disability does not have the expected dimensions 2x2.')}
    if (!isTRUE(all.equal(colnames(per_disability),c('Probability','Severity')))){stop('Error: the input per_disability seems to have wrong colnames.')}
    
  return(0)
}

