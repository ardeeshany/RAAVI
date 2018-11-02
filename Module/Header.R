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
  
 
  
  dashboardHeader(
                      #enable_rightsidebar = TRUE,
                      #rightSidebarIcon = "info",                    
    
    # Thick Title
    tags$li(class = "dropdown",
            tags$style(".main-header .logo {height: 2.5em;}")
            ),
    
    #tags$head(tags$link(href = "custom.css", rel = "stylesheet")),
   
    
    
    title =  helpText(div(style=" color : #9C9A40; text-align:center ;font-size: 120%; font-weight: bold;font-family:'dastnevis';",
                                        "راوی")),
  
    
                ### long Title
                 titleWidth = 85,
                
                ### Messages menus ; not render in server function, a message menu needs values for from and message.
                dropdownMenu(type = "messages",
                             messageItem(
                               from = "راوی",
                               message = "ورژن ۱.۰۱",
                               icon = icon("life-ring")
                             ),
                             # messageItem(
                             #   from = "New User",
                             #   message = "How do I register?",
                             #   icon = icon("question")
                             # ),
                             messageItem(
                               from = "موسس",
                               message = "اردلان میرشانی",
                               icon = icon("user"),
                               time = "2018-11-01"
                             )
                )
                
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

Header <- function(input,output,session){
  
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

