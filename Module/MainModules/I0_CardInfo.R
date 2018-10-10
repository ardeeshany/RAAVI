CardInfoUI <- function(id) {
  
  ns <- NS(id)
  
  tabPanel(title =div(class="tabPanel--font-size","اطلاعات فردی"),
  column(12,offset = 3,
           uiOutput(ns("card_ardalan"))
  ),
  actionButton("sendSignal", "Press 3x to Send"),
  textOutput("my_button"),
  textInput("subject", "Subject:", value="")
  
           )

  
}


CardInfo <- function(input,output,session){
  
  
  output$card_ardalan <- renderUI({
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
                 list(
                   socialButton(
                   url = "https://github.com/ardeeshany",
                   type = "github"
                 ),
                 socialButton(
                   url = " https://www.linkedin.com/in/ardalan-mirshani-ab7b5476/",
                   type = "linkedin"
                 ),
                 
                 appButton(
                   url = "http://google.com",
                   label = "Inbox",
                   icon = "fa fa-envelope",
                   enable_badge = FALSE
                 )
                 ))
    
  )
  })
  
  
}