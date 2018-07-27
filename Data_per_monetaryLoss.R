# Risk Of Life
# Implementation of the validation for the data type per_monetaryLoss
# written by Markus Reitschuster on 24.7.18

Val_Data_per_monetaryLoss <- function(per_monetaryLoss)
{
    # let's check if the input is the expected format
    if (!ncol(per_monetaryLoss)==2 ){stop('Error: the input per_monetaryLoss does not have the expected dimensions 2x2.')}
    if (!isTRUE(all.equal(colnames(per_monetaryLoss),c('Probability','Severity')))){stop('Error: the input per_monetaryLoss seems to have wrong colnames.')}
    
  return(0)
}

