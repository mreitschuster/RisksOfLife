#' Event - Accident
#' 
#' calculates the probability of an accident.
#' 
#' @param input_name currently only 'Data_none' and 'Data_bool_War' are supported. 
#' @param input e.g. bool_war
#' @return per_disability  
#' @export 
#' 
Event_Accident <- function(input_name,input)
{
  
  if (input_name=='Data_bool_War'){
    bool_war=input
    x=Val_Data_bool_War(bool_war)
  } else if (input_name=='Data_none'){
  } else {
    stop('Error: unknown input.')
  }
  
  

  
  probability_of_an_accident=0.005 # per year, per person. Derived from German Accident statistic (disability degree ultimately >1). This means only accident that leave a permanent disability
  share_of_small_accidents=0.9
  share_of_medium_accidents=0.08
  share_of_severe_accidents=0.02
  
  x0=c(1-probability_of_an_accident,0)
  x1=cbind(data.matrix(probability_of_an_accident*rep(share_of_small_accidents/10,10)),data.matrix(1:10))
  x2=cbind(data.matrix(probability_of_an_accident*rep(share_of_medium_accidents/40,40)),data.matrix(11:50))
  x3=cbind(data.matrix(probability_of_an_accident*rep(share_of_severe_accidents/50,50)),data.matrix(51:100))
  per_disability=rbind(x0,x1,x2,x3)
  colnames(per_disability) = c("Probability", "Severity")
  # with severity currently we have only 2 options. 0 and 1. But there could also be a severity measure for the war.
  
  if (input_name=='bool_war'){
    multiplicator_prob_when_war=10
    per_disability[,1]=per_disability[,1]*multiplicator_prob_when_war
    per_disability[1,1]=1-(sum(per_disability[,1])-per_disability[1,1]) # renormalization. the increased probabilities of disabilities must be coming from somewhere - from the 0 disability
  }
  
  return(per_disability)
}

#' Event - Accident
#' 
#' build the nodes and edges in the graph object necessary to include the Loss Aggregator
#' @param Rgraph The Rgraph object (package specific object to save the graph)
#' @return Rgraph
#' @export 
#' 
build_graph_Event_Accident<-function(Rgraph){
  Rgraph=rbind(Rgraph,c('Risk','Data_none','Event_Accident',T))
  Rgraph=rbind(Rgraph,c('Event_War','Data_bool_War','Event_Accident',F))
  return(Rgraph)
}