#' Event - Death
#' 
#' calculates the probability of death.
#' 
#' @param input_name currently only 'Data_none' and 'Data_per_disability' are supported. should include bool_war in the future
#' @param input e.g. per_disability
#' @return bool_dead  
#' @export 
#' 

Event_Death <- function(input_name,input)
{

  if (input_name=='Data_per_disability'){
    per_disability=input
    x=Val_Data_per_disability(per_disability)
  } else if (input_name=='Data_none'){
  } else {
    stop('Error: unknown input.')
  }
  
  # TODO missing check if correct input format
  # TODO missing input war
  # TODO add age, gender and country dependency
  # TODO finalize input per_disability
  
  probability_of_death=0.01 # per year per person as found roughly inhttps://www-genesis.destatis.de/genesis/online/data;jsessionid=4352733D7BFB1168A85B7DF1BEFFA586.tomcat_GO_1_2?operation=abruftabelleBearbeiten&levelindex=1&levelid=1532254986512&auswahloperation=abruftabelleAuspraegungAuswaehlen&auswahlverzeichnis=ordnungsstruktur&auswahlziel=werteabruf&selectionname=12613-0001&auswahltext=&werteabruf=Value+retrieval
  bool_dead=matrix(c(1- probability_of_death, probability_of_death,0,1),nrow=2,ncol=2)
  dimnames(bool_dead) = list(c("not dead", "dead"),c("Probability", "Severity"))
  # with severity currently we have only 2 options. 0 and 1. But there could also be a severity measure for the war.
  
  if (input_name=='per_disability'){
    multiplicator_prob_when_disability=2 # very simplistic mode, assuming death probability is doubled, when person has a disability >0
    
    # matrix multiplication, this now reflect the case that there is a disability
    mult_matrix=matrix(c(1,0,1-multiplicator_prob_when_disability,multiplicator_prob_when_disability),nrow = 2)
    bool_dead[,'Probability']=mult_matrix %*% data.matrix(bool_dead[,'Probability']) 
    
    # now we need to adjust that disability >0 itself has a probaility
    bool_dead[1,'Probability']=per_disability[1,'Probability'] + sum(per_disability[2:101,'Probability']) * bool_dead[1,'Probability']
    bool_dead[2,'Probability']= sum(per_disability[2:101,'Probability']) * bool_dead[2,'Probability']
    }
  return(bool_dead)
  
}

#' Event - Death
#' 
#' build the nodes and edges in the graph object necessary to include the Loss Aggregator
#' @param Rgraph The Rgraph object (package specific object to save the graph)
#' @return Rgraph
#' @export 
#' 
build_graph_Event_Death<-function(Rgraph){
  Rgraph=rbind(Rgraph,c('Risk','Data_none','Event_Death',T))
  Rgraph=rbind(Rgraph,c('Event_Accident','Data_per_disability','Event_Death',T))
  Rgraph=rbind(Rgraph,c('Event_Illness','Data_per_disability','Event_Death',F))
  Rgraph=rbind(Rgraph,c('Event_War','Data_bool_War','Event_Death',F))
  return(Rgraph)
}