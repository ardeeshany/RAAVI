
M_InfoUI <- function(id){

  ns <- NS(id)
  
  fluidRow(
  column(12,offset = 3,
    #align="center",
    widgetUserBox(
    title = div(style="font-size:130%; color: white; text-align:left;","اردلان میرشانی"),
    subtitle = div(style="font-size:130%; color: white; text-align:left;","موسس راوی"),
    type = NULL,
    src = "ardalan.jpg",
    background = TRUE,
    backgroundUrl = "https://images.pexels.com/photos/531880/pexels-photo-531880.jpeg?auto=compress&cs=tinysrgb&h=350",
    closable = FALSE,
    div(style="font-size:130%; text-align:right;",
        "دکتری آمار و تحلیل داده"
        ),
    footer = div(style="text-align:right;",
      list(socialButton(
      url = "https://github.com/ardeeshany",
      type = "github"
    ),
    socialButton(
      url = " https://www.linkedin.com/in/ardalan-mirshani-ab7b5476/",
      type = "linkedin"
    )))
   
  )))
  # fluidRow(
  #   visNetworkOutput(ns("network3d"))
  # )
  
  # widgetUserBox(
  #   title = "Nadia Carmichael",
  #   subtitle = "lead Developer",
  #   type = 2,
  #   src = "https://adminlte.io/themes/AdminLTE/dist/img/user7-128x128.jpg",
  #   color = "yellow",
  #   "Some text here!",
  #   footer = "The footer here!"
  # )

  #visNetworkOutput(ns("network3d"))

}




M_Info <- function(input,output,session){

   ardalan <- "ardalan.jpg"
   logo <- "logogrey.svg"
#   #path_to_images <- "https://raw.githubusercontent.com/datastorm-open/datastorm-open.github.io/master/visNetwork/data/img/indonesia/"
#   
#   
#   
  nodes <- data.frame(id = 1:4,
                      shape = c("circularImage"),
                      image = c(logo,ardalan,logo,logo)
                      #image = paste0(ardalan, 1:4, ".jpg"),
  )
                      #label =c("","اردلان میرشانی","",""))

  links <- data.frame(from = c(2,4,3,3,3), to = c(1,2,4,2,1))
#   
#   
#   
output$network3d <- renderVisNetwork({

  visNetwork(nodes, links, width="100%", label= c("","asd","fds","ewrf"),height="400%",background = "grey",
             color = c("darkred", "grey", "orange", "darkblue")) %>%
    visPhysics(stabilization = FALSE) %>%
    visNodes(shapeProperties = list(useBorderWithImage = TRUE)) %>%
    visLayout(randomSeed = 2)




  # forceNetwork(Links = MisLinks, Nodes = MisNodes,
  #              Source = "source", Target = "target",
  #              Value = "value", NodeID = "name",
  #              Group = "group", opacity = 1)
})
  
}