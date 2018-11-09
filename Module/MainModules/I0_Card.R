
I0_CardUI <- function(id) {
  
  ns <- NS(id)


  tabPanel(title = div(class="tabPanel--font-size center",uiOutput(ns("title"))),
           icon = icon("vcard",class="inline tabPanel-icon"),
        
          div(class="info-card",
           uiOutput(ns("card_ardalan")))
          
          )
  
  
}


I0_Card <- function(input,output,session,l){

  
  output$title <- renderUI({
    if(l()=="pr"){
      title1 = "تیم راوی"
    }else{
      title1 = "Our Team"
    }
   return(title1)
  })
  
  output$card_ardalan <- renderUI({
    
    if(l()=="pr"){
      title = "اردلان میرشانی"
      subtitle = "موسس راوی"
      text = "دکتری آمار و تحلیل داده"
    }else{
      title = "Ardalan Mirshani"
      subtitle = "Raavi Designer"
      text = "Shiny Developer"
    }
    
    
    A <- widgetUserBox(width = 8,
    title = div(style="font-size:120%; color: white; text-align:left;",title),
    subtitle = div(style="font-size:120%; color: white; text-align:left;",subtitle),
    type = NULL,
    src = "ardalan.jpg",
    color = "aqua-active",
    #backgroundUrl = "https://images.pexels.com/photos/531880/pexels-photo-531880.jpeg?auto=compress&cs=tinysrgb&h=350",
    closable = FALSE,
    div(style="font-size:130%; text-align:right;",
        text
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
    
    
    return(A)
    
    })


}
