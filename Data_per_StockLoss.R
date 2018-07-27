# Risk Of Life
# Implementation of the validation for the data type per_StockLoss
# written by Markus Reitschuster on 24.7.18

Val_Data_per_StockLoss <- function(per_StockLoss)
{
    # let's check if the input is the expected format
    if (!(ncol(per_StockLoss)==2 & nrow(per_StockLoss)==101)){stop('Error: the input per_StockLoss does not have the expected dimensions 2x2.')}
    if (!isTRUE(all.equal(colnames(per_StockLoss),c('Probability','Severity')))){stop('Error: the input per_StockLoss seems to have wrong colnames.')}
    
  return(0)
}

