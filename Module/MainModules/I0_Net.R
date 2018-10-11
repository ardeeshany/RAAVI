I0_NetUI <- function(id) {
  
  ns <- NS(id)
  tabPanel(title =div(class="tabPanel--font-size","ساختار تیمی"),
  visNetworkOutput(ns("network3d"))
  )
  #visNetworkOutput(ns("network3d"))
  
}


I0_Net<- function(input,output,session){
  
  
  ardalan <- "ardalan.jpg"
  logo <- "logo.svg"
  #   #path_to_images <- "https://raw.githubusercontent.com/datastorm-open/datastorm-open.github.io/master/visNetwork/data/img/indonesia/"
  #   
  #   
  #   
  nodes <- data.frame(id = 1:10,group=c("raavi",rep("user",9))
                      #shape = c("circularImage"),
                      #image = c(ardalan,rep(logo,9)),
                      #image = paste0(ardalan, 1:4, ".jpg"),
                      #label = c("اردلان میرشانی",rep("",9))
  )
  
  links <- data.frame(from = c(rep(1,9),4,3,3), to = c(2:10,2,4,2))
  #   
  #   
  #   
  output$network3d <- renderVisNetwork({
    
    visNetwork(nodes, links,main = "ساختار تیمی راوی") %>%
      visPhysics(stabilization = FALSE) %>%
      visNodes(size = 50) %>%
      visEdges(length = 100,hidden = FALSE,color="#9C9A40",smooth = FALSE) %>%
      visGroups(groupname = "user", shape = "icon", 
                icon = list(code = "f007", size = 75,color="black")) %>%
      visGroups(groupname = "raavi", shape="circularImage",
                image=ardalan) %>%
      visLayout(randomSeed = 2) %>%
      visInteraction(navigationButtons = TRUE) %>%
      visHierarchicalLayout()
      #visPhysics(minVelocity = 0.1,maxVelocity = 50,)
      #visPhysics(timestep = 0.2) %>%
      #visLayout(randomSeed = 2)
    
    
    
    
    # forceNetwork(Links = MisLinks, Nodes = MisNodes,
    #              Source = "source", Target = "target",
    #              Value = "value", NodeID = "name",
    #              Group = "group", opacity = 1)
  })
  
  
}