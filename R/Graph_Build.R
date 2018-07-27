#' Build the Neo4j data used for RisksOfLife
#' 
#' This will hopefully be replaced by a load functionality on existing graphs, together with a more intuitive way to create the graphs.
#' 
#' @param graph The graph object representing the neo4j database as produced by Rneo4j
#' @return nothing
#' @export 
#' 
Graph_Build <- function(graph){
    clear(graph, input=FALSE)
    
    
    
   
    
    
    # we define the dataobject, which will be handed over between events and impacts
    addConstraint(graph, "DataObject", "name") 
    data_bool_War=createNode(graph, "DataObject", name="bool_War",description="",active=F, code='Data_bool_War.R')
    data_per_disability=createNode(graph, "DataObject", name="per_disability",description="",active=T, code='Data_per_disability.R')
    data_per_unemployment=createNode(graph, "DataObject", name="per_unemployment",description="",active=F, code='Data_per_unemployment.R')
    data_bool_dead=createNode(graph, "DataObject", name="bool_dead",description="",active=T, code='Data_bool_dead.R')
    data_per_StockLoss=createNode(graph, "DataObject", name="per_StockLoss",description="",active=F, code='Data_bool_War.R')
    data_monetaryLoss=createNode(graph, "DataObject", name="monetaryLoss",description="",active=T, code='Data_per_monetaryLoss.R')
    data_none=createNode(graph, "DataObject", name="none",description="",active=T, code='Data_none.R')
    
    # risk
    # this reflects what you know before something happens
    addConstraint(graph, "Risk", "name") #  nodes need to have a unqiue identifier
    risk=createNode(graph, "Risk", name="Risk",active=T, code='')
      createRel(risk, "hasOutput",data_none,active=F)
    
    # events
    # they reflect events that happen and they can reference to other events depicting causal relationships
    addConstraint(graph, "Event", "name") 
    ev_Accident=createNode(graph, "Event", name="Accident",description="",active=T, code='Event_Accident.R')
    ev_Illness=createNode(graph, "Event", name="Illness",description="",active=F, code='')
    ev_Death=createNode(graph, "Event", name="Death",description="",active=T, code='Event_Death.R')
    ev_Unemployment=createNode(graph, "Event", name="Unemployment",description="Unemployment, involuntary reduction of pay",active=F, code='')
    ev_StockCrash=createNode(graph, "Event", name="Stock Market Crash",description="",active=F, code='')
    ev_War=createNode(graph, "Event", name="War",description="War in the country you are living in.",active=F, code='')
    
    # impact
    # this is the step where we translate the different severity measures to monetary impact
    addConstraint(graph, "Impact", "name") 
    im_LoE=createNode(graph, "Impact", name="Loss Of Earnings",active=T,  code='Impact_LoE.R')
    im_CoC=createNode(graph, "Impact", name="Cost of Care",active=T, code='Impact_CoC.R')
    im_stockdeval=createNode(graph, "Impact", name="Stock devaluation",active=F,code='')
    
    # loss
    addConstraint(graph, "Loss", "name") 
    loss=createNode(graph, "Loss", name="Monetary Loss",active=T, code='lossAggregator.R')
      createRel(data_monetaryLoss, "isInputFor",loss,active=F)      
    
    # relationships between risk and event
    createRel(risk, "causes",ev_Accident,active=T)
    createRel(risk, "causes",ev_Illness,active=F)
    createRel(risk, "causes",ev_Death,active=T)
    createRel(risk, "causes",ev_Unemployment,active=F)
    createRel(risk, "causes",ev_StockCrash,active=F)
    createRel(risk, "causes",ev_War,active=F)
    
    # relationships between events
    # also we add the input and output data objects    
    createRel(ev_War, "causes",ev_StockCrash,active=F)
    createRel(ev_War, "causes",ev_Death,active=F)
    createRel(ev_War, "causes",ev_Accident,active=F)
      createRel(data_none, "isInputFor",ev_War,active=F)
      createRel(ev_War, "hasOutput",data_bool_War,active=F)
      
    createRel(ev_StockCrash, "causes",ev_Unemployment,active=F)
      createRel(data_none, "isInputFor",ev_StockCrash,active=F)
      createRel(data_bool_War, "isInputFor",ev_StockCrash,active=F)
      createRel(ev_StockCrash, "hasOutput",data_per_StockLoss,active=F)    
      
    createRel(ev_Illness, "causes",ev_Unemployment,active=F)
    createRel(ev_Illness, "causes",ev_Death,active=F)
      createRel(data_none, "isInputFor",ev_Illness,active=F)
      createRel(data_per_unemployment, "isInputFor",ev_Illness,active=F)
      createRel(ev_Illness, "hasOutput",data_per_disability,active=F)    
      
    createRel(ev_Unemployment, "causes",ev_Illness,active=F)
      createRel(data_none, "isInputFor",ev_Unemployment,active=F)
      createRel(data_per_disability, "isInputFor",ev_Unemployment,active=F)
      createRel(data_per_StockLoss, "isInputFor",ev_Unemployment,active=F)
      createRel(ev_Unemployment, "hasOutput",data_per_unemployment,active=F) 
      
    createRel(ev_Accident, "causes",ev_Unemployment,active=T)
    createRel(ev_Accident, "causes",ev_Death,active=T)
      createRel(data_none, "isInputFor",ev_Accident,active=F)
      createRel(data_bool_War, "isInputFor",ev_Accident,active=F)
      createRel(ev_Accident, "hasOutput",data_per_disability,active=F) 
      
      createRel(data_none, "isInputFor",ev_Death,active=F) 
      createRel(data_per_disability, "isInputFor",ev_Death,active=F) 
      createRel(data_bool_War, "isInputFor",ev_Death,active=F) 
      createRel(ev_Death, "hasOutput",data_bool_dead,active=F) 


    # relationships between event and impact
    createRel(ev_Death, "causes",im_LoE,active=T)
    createRel(ev_Unemployment, "causes",im_LoE,active=F)
    createRel(ev_Accident, "causes",im_CoC,active=T)
    createRel(ev_Illness, "causes",im_CoC,active=F)
    createRel(ev_StockCrash, "causes",im_stockdeval,active=F)
    
    
    # relationships between impact and loss
    createRel(im_LoE, "causes",loss,active=T)
      createRel(data_per_unemployment, "isInputFor",im_LoE,active=F) 
      createRel(data_bool_dead, "isInputFor",im_LoE,active=F) 
      createRel(im_LoE, "hasOutput",data_monetaryLoss,active=F) 
    createRel(im_CoC, "causes",loss,active=T)
      createRel(data_per_disability, "isInputFor",im_CoC,active=F) 
      createRel(im_CoC, "hasOutput",data_monetaryLoss,active=F)     
    createRel(im_stockdeval, "causes",loss,active=F)
      createRel(data_per_StockLoss, "isInputFor",im_stockdeval,active=F) 
      createRel(im_stockdeval, "hasOutput",data_monetaryLoss,active=F)       
    


    
}