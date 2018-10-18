I0_ContactUI <- function(id){
  
  ns <- NS(id)
  
  tabPanel(title=div(class="tabPanel--font-size center","ارتباط با ما"),
           icon = icon("envelope",class="tabPanel-icon"),
    div(class="box__contact--general",
    box(status = "primary",collapsible = FALSE, width=9,
    uiOutput(ns("error")),
    div(class="mailR right",
        
        div(class="Bnaz",
        textAreaInput(ns("name"), "نام و نام خانوادگی", value=""),
        textAreaInput(ns("from"), "آدرس ایمیل", value=""),
        textAreaInput(ns("subject"), "موضوع", value=""),
        textAreaInput(ns("message"),label = "پیام خود را اینجا وارد کنید"))),
     div(style="margin-left:86%;",
      actionButton(ns("send"), "ارسال ایمیل", class="action-button--color--yellow"))
          )))
  
}

I0_Contact <- function(input,output,server){
  
  info1 <- utils::read.table("Module/.gmail_private", header=FALSE, stringsAsFactors=FALSE)
  info <- as.list(info1[,2])
  
  output$error <- renderUI({""})
  
  observeEvent(input$send,{
    if(input$message==""){
      output$error <- renderUI({ div(class="red error__size error--mleft__message", "پیام وارد نشده است") })
    }
    if(input$subject==""){
      output$error <- renderUI({ div(class="red error__size error--mleft", "موضوع وارد نشده است") })
    }
    if(input$from==""){
      output$error <- renderUI({ div(class="red error__size error--mleft","ایمیل وارد نشده است")})
    }
    if(input$name==""){
      output$error <- renderUI({ div(class="red error__size error--mleft__name","نام و نام خوانوادگی وارد نشده است") })
    }
    if(input$message!="" && input$subject!="" && input$name!="" && input$from!=""){
      
      output$error <- renderUI({""})
      subject <- isolate(input$subject)
      body <- isolate(paste("ایمیل : " , "\n",input$from,"\n",
                            "نام و نام خانوادگی : ","\n", input$name,"\n",
                            "موضوع : " ,"\n", input$subject , "\n",
                            "پیام : ", "\n", input$message,sep=""))
      
      send.mail(from = info[[1]], to = info[[3]], subject = subject, body = body, 
                #smtp = list(host.name = "aspmx.l.google.com", port = 25),
                smtp = list(host.name = "smtp.gmail.com", port = 465, 
                            user.name=info[[1]], passwd=info[[2]], ssl=TRUE),
                encoding = "utf-8",
                authenticate = TRUE,
                send = TRUE)
      
      output$error <- renderUI({ div(class="green2 error__size error--mleft__message",
                                     "پیام شما ارسال شد")})
    }
    
  })
  
  
}