
I0_CardUI <- function(id) {
  
  ns <- NS(id)


  tabPanel(title =div(class="tabPanel--font-size center","تیم راوی"),
           icon = icon("vcard",class="tabPanel-icon"),
        
          div(class="info-card",
           uiOutput(ns("card_ardalan")))
          
          )
  
  
}


I0_Card <- function(input,output,session){
  
  
  
  output$card_ardalan <- renderUI({
    widgetUserBox(width = 8,
    title = div(style="font-size:120%; color: white; text-align:left;","اردلان میرشانی"),
    subtitle = div(style="font-size:120%; color: white; text-align:left;","موسس راوی"),
    type = NULL,
    src = "ardalan.jpg",
    color = "aqua-active",
    #backgroundUrl = "https://images.pexels.com/photos/531880/pexels-photo-531880.jpeg?auto=compress&cs=tinysrgb&h=350",
    closable = FALSE,
    div(style="font-size:130%; text-align:right;",
        "دکتری آمار و تحلیل داده"
    ),
    footer = div(style="text-align:right;",
                 list(
                socialButton(
                     url = " https://www.linkedin.com/in/ardalan-mirshani-ab7b5476/",
                     type = "linkedin"
                   ),   
                socialButton(
                   url = "https://github.com/ardeeshany",
                   type = "github"
                 ),
                a(actionButton(inputId = "email1", label = "", 
                                icon = icon("envelope", lib = "font-awesome"),
                                style="background-color:#485167 ; color : white"),
                   href="mailto:ardeeshany@gmail.com")
                 ))
    
  )
    
  })


  
  
  
  
}
