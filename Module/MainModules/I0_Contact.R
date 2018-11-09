I0_ContactUI <- function(id){
  
  ns <- NS(id)
  
  tabPanel(title=div(class="tabPanel--font-size center",uiOutput(ns("contact"))),
           icon = icon("envelope",class="tabPanel-icon"),
    div(class="box__contact--general",
    box(status = "primary",collapsible = FALSE, width=9,
    uiOutput(ns("error")),
    uiOutput(ns("email"))
    
    
    
    
          )))
  
}

I0_Contact <- function(input,output,session,l){
  
  ns <- session$ns
  
  output$contact <- renderUI({
    if(l()=="pr"){
      contact = "ارتباط با ما"
    }else{
      contact = "Contact Us"
    }
    return(contact)
  })
  
  
  
  info1 <- utils::read.table("Module/.gmail_private", header=FALSE, stringsAsFactors=FALSE)
  info <- as.list(info1[,2])
  
  output$error <- renderUI({""})
  
  output$email <- renderUI({
    
    if(l()=="pr"){
      name = "نام و نام خانوادگی"
      email_address = "آدرس ایمیل"
      subject = "موضوع"
      message = "پیام خود را اینجا وارد کنید"
      submit = "ارسال پیام"
    }else{
      name = "First and Last Name"
      email_address = "Email Address"
      subject = "Subject"
      message = "Write your messgae here ... "
      submit = "send"
    }
    
    
    
    
    A <- div(div(class="mailR right",
    div(class="Bnaz",
            textAreaInput(ns("name"), name, value=""),
            textAreaInput(ns("from"), email_address, value=""),
            textAreaInput(ns("subject"), subject, value=""),
            textAreaInput(ns("message"),label = message))),
    
    div(style="margin-left:87%;",
        actionButton(ns("send"), submit, class="action-button--color--yellow")))
  
    return(A)
    
    
    })
  
  
  
  
  observeEvent(input$send,{
    
    
    if(l()=="pr"){
      no_message = "پیام وارد نشده است" ;
      no_subject = "موضوع وارد نشده است"
      no_email = "ایمیل وارد نشده است"
      no_name = "نام و نام خوانوادگی وارد نشده است"
      yes_submit = "پیام شما ارسال شد"
    }else{
      no_message = "The message is empty" 
      no_subject = "The Subject is empty"
      no_email = "The email address is empty"
      no_name = "The first and last name is empty"
      yes_submit = "Your message was sent successfully"
    }
    

    if(input$message==""){
      output$error <- renderUI({ div(class="red error__size error--mleft__message", no_message) })
    }
    if(input$subject==""){
      output$error <- renderUI({ div(class="red error__size error--mleft", no_subject) })
    }
    if(input$from==""){
      output$error <- renderUI({ div(class="red error__size error--mleft",no_email)})
    }
    if(input$name==""){
      output$error <- renderUI({ div(class="red error__size error--mleft__name",no_name) })
    }
    if(input$message!="" && input$subject!="" && input$name!="" && input$from!=""){
      
      output$error <- renderUI({""})
      subject <- isolate(input$subject)
      body <- isolate(paste("ایمیل : " , "\n",input$from,"\n","\n","\n",
                            "نام و نام خانوادگی : ","\n", input$name,"\n","\n","\n",
                            "موضوع : " ,"\n", input$subject , "\n","\n","\n",
                            "پیام : ", "\n", input$message,sep=""))
      
      send.mail(from = info[[1]], to = info[[3]], subject = subject, body = body, 
                #smtp = list(host.name = "aspmx.l.google.com", port = 25),
                smtp = list(host.name = "smtp.gmail.com", port = 465, 
                            user.name=info[[1]], passwd=info[[2]], ssl=TRUE),
                encoding = "utf-8",
                authenticate = TRUE,
                send = TRUE)
      
      output$error <- renderUI({ div(class="green2 error__size error--mleft__message",
                                     yes_submit)})
    }
    
  })
  
  
}