LoginUI <- function(id){
  
  ns <- NS(id)
  
  textInput(ns("userID"),label = "userID")
  textInput(ns("username"),label = "username")
  textInput(ns("userhash"),label = "userhash")

  includeScript("get_user_info.js") 
  
}


Login <- function(input,output,session){
  
  userID <- reactive({ input$userID }) 
  username <- reactive({ input$username }) 
  userhash <- reactive({input$userhash })
  hashkey <- "RAAVI"
  redirecturl <- "https://raaviman.com"
  md5hash <- reactive({ md5(paste0(hashkey,username(),userID())) })
  
  observeEvent(input$userhash,{
    if(userhash() != md5hash()) {
      session$sendCustomMessage("redirecturl", redirecturl)
    } 
  })

  
}