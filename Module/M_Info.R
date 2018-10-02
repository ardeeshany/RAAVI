
M_InfoUI <- function(id){

  ns <- NS(id)
  
  forceNetworkOutput(ns("network3d"))

}




M_Info <- function(input,output,session){
  
data(MisLinks)
data(MisNodes)  
  
output$network3d <- renderForceNetwork({
  # Plot
  forceNetwork(Links = MisLinks, Nodes = MisNodes,
               Source = "source", Target = "target",
               Value = "value", NodeID = "name",
               Group = "group", opacity = 0.8,height = "1em")
})
  
}