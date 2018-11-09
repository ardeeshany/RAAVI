#############################
#
# Header
#
#############################

### For Dynamic content with dropdownMenuOutput
# messageData=matrix(NA,3,2)
# colnames(messageData)=c("from","message")
# messageData[,1]=1:3
# messageData[,2]=c("test1","test2","test3")
###

HeaderUI <- function(id){
  
  ns <- NS(id)
  
  # dashboardHeader(title =  tags$a(href='http://github.com/ardeeshany',
  #                               tags$img(src='logo.png',height='60',width='80')),
  
 
  # tags$head(tags$style(".main-header .navbar{ margin-left: 0px !important;}"))
  # tags$head(tags$style(".navbar-custom-menu, .main-header .navbar-right {float: left !important;}"))
  
  dashboardHeader(
                      #enable_rightsidebar = TRUE,
                      #rightSidebarIcon = "info",                    
    
    # Thick Title
    tags$li(class = "dropdown",
            tags$style(".main-header .logo {height: 2.5em;}")
            ),
    
    #tags$head(tags$link(href = "custom.css", rel = "stylesheet")),

    title =  uiOutput(ns("title")),
      
      ### long Title
                 titleWidth = 85,
    
    # tags$li(class = "dropdown",style="color:white, text-align:center,margin-top:-30%",
    #         radioGroupButtons(inputId = ns("language"),size = "xs",
    #                   choices = c("English"="en","Farsi"="pr"),
    #                   selected = "pr",label = "")),
    
                ### Messages menus ; not render in server function, a message menu needs values for from and message.
    
                 uiOutput(ns("message"))
                 
    



                ### Notification menus ; not render in server function, a text notification
                # dropdownMenu(type = "notifications",
                #              notificationItem(
                #                text = "5 new users today",
                #                icon("users")
                #              ),
                #              notificationItem(
                #                text = "12 items delivered",
                #                icon("truck"),
                #                status = "success"
                #              ),
                #              notificationItem(
                #                text = "Server load at 86%",
                #                icon = icon("exclamation-triangle"),
                #                status = "warning"
                #              )
                # ),
                # 
                ### Tasks menus ; not render in server function, a progress bar and a text label. 
                # dropdownMenu(type = "tasks",
                #              taskItem(value = 90, color = "green",
                #                       "وارد کردن داده"
                #              ),
                #              taskItem(value = 90, color = "blue",
                #                       "روند کلاس"
                #              ),
                #              taskItem(value = 90, color = "yellow",
                #                       "روند دانش آموزان"
                #              ),
                #              taskItem(value = 90, color = "red",
                #                       "گروه بندی"
                #              ),
                #              taskItem(value = 90, color = "olive",
                #                       "پیشرفت"
                #              ),
                #              taskItem(value = 70, color = "lime",
                #                       "فیلتر"
                #              )
                #             
                #             
                # )
                
                ### Dynamic content: instead of using dropdownMenu, use dropdownMenuOutput
                ### it needs to be rendered in server function              
                #dropdownMenuOutput(ns("messageMenu"))
)

}

Header <- function(input,output,session,l){
  
  
  output$title <- renderUI({
    
    if(l()=="pr"){
    A <- helpText(div(style=" color : #9C9A40; text-align:center ;font-size: 120%; font-weight: bold;font-family:'dast_nevis'",
                 "راوی"))
    }else{
      A <- helpText(div(style=" color : #9C9A40; text-align:center ;font-size: 110%; font-weight: bold;font-family:'dast_nevis'",
                        "Raavi"))
    }    
    return(A)
    
  })
  
  
  
  output$message <- renderUI({


    if(l()=="pr"){
      raavi = "راوی"
      version = "ورژن ۰.۱"
      title1 = "موسس"
      name = "اردلان میرشانی"
      style = "text-align:right"
      time = "۲۰۱۸-۱۱-۰۱"
      width = 150
    }else{
      raavi = "Raavi"
      version = "Version 1.0"
      title1 = "designer"
      name = "Ardalan Mirshani"
      style = "text-align:left"
      time = "2018-11-01"
      width=160
    }

    A <- div(style="margin-left:-32%; padding-top:15%;",
      dropdown(icon = icon("life-ring"),size = "sm",right = T,width = width,status = "primary",
               animate = T,style="material-circle",label = "کلیات",
        icon("life-ring"),
        div(class="inline",style="font-weight:bold",raavi),
        br(),
        version,
        br(),br(),
        icon("user"),
        div(class="inline",style="font-weight:bold",title1),
        br(),
        div(style="font-weight:bold",name),
        br(),
        icon("clock-o"),
        time
                        ))
      # dropdownMenu(type = "messages",
      # messageItem(
      #              from = raavi,
      #              message = version ,
      #              icon = icon("life-ring")
      #            ),
      #            # messageItem(
      #            #   from = "New User",
      #            #   message = "How do I register?",
      #            #   icon = icon("question")
      #            # ),
      #            messageItem(
      #              from = title1,
      #              message = name,
      #              icon = icon("user")
      #              #time = time
      #            ))
      

    return(A)

  })
  
  
  # output$message <- renderUI({
  #   
  #   div(
  #                messageItem(
  #                  from = "راوی",
  #                  message = "ورژن ۱.۰" ,
  #                  icon = icon("life-ring")
  #                ),
  #                # messageItem(
  #                #   from = "New User",
  #                #   message = "How do I register?",
  #                #   icon = icon("question")
  #                # ),
  #                messageItem(
  #                  from = "اردلان میرشانی",
  #                  message = "موسس راوی",
  #                  icon = icon("user"),
  #                  time = "2018-11-01"
  #                ))
  #   
  # })
  
  
  
  # output$messageMenu <- renderMenu({
  #   # Code to generate each of the messageItems here, in a list. This assumes
  #   # that messageData is a data frame with two columns, 'from' and 'message'.
  #   msgs <- apply(messageData, 1, function(row) {
  #     messageItem(from = row[["from"]], message = row[["message"]])
  #   })
  #   
  #   # This is equivalent to calling:
  #   #   dropdownMenu(type="messages", msgs[[1]], msgs[[2]], ...)
  #   dropdownMenu(type = "messages", .list = msgs)
  # })
  }

