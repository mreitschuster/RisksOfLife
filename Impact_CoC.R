# Risk Of Life
# Implementation of the impact "Cost of Care"
# input='per_disability', output='data_monetaryLoss'

Impact_CoC <- function(input_name,input)
{
  if (input_name=='per_disability'){
    per_disability=input
    x=Val_Data_per_disability(per_disability)
  } else if (input_name=='none'){
  } else {
    stop('Error: unknown input.')
  }
  
  # assuming CoC is linear in disability
  costs_per_disability_degree=1e4
  data_monetaryLoss=per_disability

  data_monetaryLoss[,2]=per_disability[,2]*costs_per_disability_degree
  return(data_monetaryLoss)
}
