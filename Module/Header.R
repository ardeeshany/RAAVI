#############################
#
# Header
#
#############################

### For Dynamic content with dropdownMenuOutput
messageData=matrix(NA,3,2)
colnames(messageData)=c("from","message")
messageData[,1]=1:3
messageData[,2]=c("test1","test2","test3")
###

HeaderUI <- function(id){
  
  ns <- NS(id)
  
  # dashboardHeader(title =  tags$a(href='http://github.com/ardeeshany',
  #                               tags$img(src='logo.png',height='60',width='80')),
  
 
  
  dashboardHeader(
    
    # Thick Title
    tags$li(class = "dropdown",
            tags$style(".main-header .logo {height: 4em;}")
            ),
    
    #tags$head(tags$link(href = "custom.css", rel = "stylesheet")),
   
    
    
    title =  helpText(div(style="margin-top:10%; color : #9C9A40; text-align:center ;font-size: 280%; font-weight: bold;font-family:'dastnevis';",
                                        "راوی"),logo_blue_gradient,theme = "blue_gradient"),
  
    
                ### long Title
                 titleWidth = 180,
                
                
                ### Messages menus ; not render in server function, a message menu needs values for from and message.
                dropdownMenu(type = "messages",
                             messageItem(
                               from = "دپارتمان فروش",
                               message = "در این ماه مشکلی موجود نمی باشد"
                             ),
                             messageItem(
                               from = "New User",
                               message = "How do I register?",
                               icon = icon("question"),
                               time = "13:45"
                             ),
                             messageItem(
                               from = "Support",
                               message = "The new server is ready.",
                               icon = icon("life-ring"),
                               time = "2014-12-01"
                             )
                ),
                
                ### Notification menus ; not render in server function, a text notification
                dropdownMenu(type = "notifications",
                             notificationItem(
                               text = "5 new users today",
                               icon("users")
                             ),
                             notificationItem(
                               text = "12 items delivered",
                               icon("truck"),
                               status = "success"
                             ),
                             notificationItem(
                               text = "Server load at 86%",
                               icon = icon("exclamation-triangle"),
                               status = "warning"
                             )
                ),
                
                ### Tasks menus ; not render in server function, a progress bar and a text label. 
                dropdownMenu(type = "tasks", badgeStatus = "success",
                             taskItem(value = 90, color = "green",
                                      "Documentation"
                             ),
                             taskItem(value = 17, color = "blue",
                                      "Project X"
                             ),
                             taskItem(value = 75, color = "yellow",
                                      "Server deployment"
                             ),
                             taskItem(value = 80, color = "red",
                                      "Overall project"
                             )
                ),
                
                ### Dynamic content: instead of using dropdownMenu, use dropdownMenuOutput
                ### it needs to be rendered in server function              
                dropdownMenuOutput(ns("messageMenu"))
)

}

Header <- function(input,output,session){
  
  output$messageMenu <- renderMenu({
    # Code to generate each of the messageItems here, in a list. This assumes
    # that messageData is a data frame with two columns, 'from' and 'message'.
    msgs <- apply(messageData, 1, function(row) {
      messageItem(from = row[["from"]], message = row[["message"]])
    })
    
    # This is equivalent to calling:
    #   dropdownMenu(type="messages", msgs[[1]], msgs[[2]], ...)
    dropdownMenu(type = "messages", .list = msgs)
  })
}

