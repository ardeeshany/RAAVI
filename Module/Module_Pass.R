# 
# 
# USER <- reactiveValues(Logged = Logged)
# 
# # # # How to generate the Password
# # # library(digest)
# # # digest('1',  serialize = FALSE)
# 
# PASSWORD <- data.frame(Brukernavn = c("1","pachiras"),
#                        Passord = c("c4ca4238a0b923820dcc509a6f75849b","81b073de9370ea873f548e31b8adc081"))
# 
# 
# passwdInput <- function(inputId, label) {
#   tagList(
#     br(),
#     tags$div(style="text-align: right;",
#     tags$label(label)),
#     tags$input(id = inputId, type="password", value="",size="48px",src="logo.png")
#     
#   )
# }
# 
# LognInput <- function(inputId, label) {
#   tagList(
#     br(),
#     tags$div(style="text-align: right;",
#     tags$label(label)),
#     tags$input(id = inputId, type="text", value="",size="48px")
#     
#   )
# }
# 
# 
# output$uiLogin <- renderUI({ 
# 
#   if (USER$Logged == FALSE) {
#     wellPanel(
#       #textInput("userName", "نام کاربری : "),
#       LognInput("userName", "نام کاربری "),
#       br(),
#       passwdInput("passwd", "کلمه عبور "),
#       br(),
#       br(),
#       tags$div(style=" text-align: left;font-weight: bold;",
#       actionButton("Login", "ورود",width = "100%")
#       )
#     )
#   }
#   
# })
# 
# output$pass <- renderText({  
#   if (USER$Logged == FALSE) {
#     if (!is.null(input$Login)) {
#       if (input$Login > 0) {
#         Username <- isolate(input$userName)
#         Password <- isolate(input$passwd)
#         Id.username <- which(PASSWORD$Brukernavn == Username)
#         Id.password <- which(PASSWORD$Passord    == Password)
#         if (length(Id.username) > 0 & length(Id.password) > 0) {
#           if (Id.username == Id.password) {
#             USER$Logged <- TRUE
#             output$txt <- renderUI(
#               tags$div(style="color:green;font-size:150%;text-align: center",
#              "به راوی خوش آمدید",
#              br(),
#              br(),
#              tags$div(style="color:DodgerBlue;font-size:200%;text-align: center;
#                       border-style: dotted; border-width: 3px;background-color: lightblue;",
#              "  راوی، روایتگر قصه های شماست  ")
#               )
#             )
#           } 
#         } else  {
#           
#           output$txt <- renderUI(tags$div( style = "color:red; font-family: 'Times New Roman';
#                                            text-align: right;font-weight: bold;",
#            "نام کاربری یا کلمه عبور اشتباه است"))
#         
#           }
#       } 
#     }
#   }
# })
