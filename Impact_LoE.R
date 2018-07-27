# Risk Of Life
# Implementation of the impact "Loss of Earnings"
# input='bool_dead', output='data_monetaryLoss'

Impact_LoE <- function(input_name,input)
{
  if (input_name=='bool_dead'){
    bool_dead=input
    x=Val_Data_bool_dead(bool_dead)
  } else if (input_name=='none'){
  } else {
    stop('Error: unknown input.')
  }
  # assuming loss is 100'€ if dead. 0 if alive
  cost_of_death=1e5
  data_monetaryLoss=bool_dead
  dimnames(data_monetaryLoss) = list(c("no loss", "loss"),c("Probability", "Severity"))
  data_monetaryLoss[,2]=bool_dead[,2]*cost_of_death
  return(data_monetaryLoss)
}
