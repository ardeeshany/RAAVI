LoginUI <- function(id){
  
  ns <- NS(id)
  
  
  tabPanel(title = div(class="tabPanel--font-size center",
                       "لاگین"),
           icon = icon("group",class="tabPanel-icon"),
           
           # tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #485167}")), 
           #        
           
           fluidRow(            
  textInput(ns("userID"),label = "userID"),
  textInput(ns("username"),label = "username"),
  textInput(ns("userhash"),label = "userhash"),
  includeScript("www/get_user_info.js") 
           )

  )
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