
BodyUI <- function(id){
  
ns <- NS(id)

# tags$head(
#   tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
# )

dashboardBody(
#tags$head(tags$script(HTML("$('body').addClass('sidebar-mini');"))),

theme_RAAVI(font="dast_nevis"),
uiOutput(ns("theme")),
#ColorUI("mode_color"),  
  # tags$head( 
  #   tags$style(HTML(".main-sidebar { color: #f4b943; }")) 
  # )
tags$head(tags$link(href = "https://fonts.googleapis.com/css?family=Reem+Kufi|El+Messiri|Lalezar|Scheherazade|Amiri", rel = "stylesheet")),

 tags$head(
   tags$style(
       HTML("@font-face { font-family: dast_nevis; src: url(dastnevis.ttf);}")
  )
 ),

#tags$head(tags$link(href = "/IranNastaliq.ttf", rel = "stylesheet")),

#tags$head(tags$link(href = "IranNastaliq.ttf", rel = "stylesheet")), 

tags$head(tags$link(href = "custom.css", rel = "stylesheet")), 
#tags$head(tags$style(".main-header .navbar{ margin-left: 0px !important;}")),
#tags$head(tags$style(".navbar-custom-menu, .main-header .navbar-right {float: left !important;}")),
# tags$head(
#   tags$link(rel="stylesheet", type = "text/css",
#             href = "IranNastaliq")
# ),
#tags$head( includeCSS(path = "www/style.css") ),

useShinyjs(),
#inlineCSS(appCSS),

# Loading message
div( id = ns("loading-content"),
  div(class="loading",
      "...راوی در حال بارگذاری اطلاعات است ")
),


hidden(
  div(
    id = ns("app-content"),
tabItems(
#### Login ####      
      
#PassUI("mod_pass")
## Login module;
      
# tabItem(tabName = "Login",
# 
# tagList(
#   tags$head(
#     tags$link(rel="stylesheet", type="text/css",href="style.css"),
#     tags$script(type="text/javascript", src = "md5.js"),
#     tags$script(type="text/javascript", src = "passwdInputBinding.js")
#   )
# ),
# 
# div(class = "login",
#     style="text-align:right; font-weight:bold",
#     box(height = "500%",
#     #title = "راوی، روایتگر قصه های شما",
#     status = "primary",width = "250%",
#     uiOutput("uiLogin"),
#     textOutput("pass"),
#     uiOutput("txt"))
# )
# ),
#### Login ####  

tabItem(tabName = "Class", M3_ClassUI(ns("cls"))),

# tabItem(tabName = "C12",M1_ClassUI(ns("c12"))), 
# tabItem(tabName = "C11",M1_ClassUI(ns("c11"))), 
# tabItem(tabName = "C10",M1_ClassUI(ns("c10"))), 
#tabItem(tabName="Student", M_StudentUI(ns("student"),names_all)),
tabItem(tabName = "Summary", M_SummaryUI(ns("summary"))),
tabItem(tabName = "Info", I1_InfoUI(ns("info")))
       ),
tags$head(includeHTML(("www/google-analytics.html")))    
)
))
}

Body <- function(input,output,session, outputadrs="RAAVI/RAAVI-Released/DATA/Test",l){
  
  
  
  output$theme <- renderUI({
    if(l()=="pr")
    A <- theme_RAAVI(font="dast_nevis")
    else
    A <- theme_RAAVI(font="Gabriola")
    
    return(A)
  })
  
          callModule(M_Summary,"summary",l)
          callModule(I1_Info,"info",l)
          callModule(M3_Class,"cls",outputDir = outputadrs,class="0",level="0",course="0",font_plot = "dast_nevis",l)
  
          hide(id = "loading-content", anim = TRUE, animType = "fade")    
          show("app-content")
          
  # vals <- callModule(M1_Class,"c12",outputcls = sprintf("%s/12",outputadrs),class="12") 
  #         callModule(M1_Class,"c11",outputcls = sprintf("%s/11",outputadrs),class="11")
  #         callModule(M1_Class,"c10",outputcls = sprintf("%s/10",outputadrs),class="10")
  #        callModule(M_Student,"student",Vals=vals)    

#### Login ####  
  #USER1 <- callModule(Pass,"mod_pass")
  #source('Module/Module_Pass.R',local = TRUE)
  # observe({
  # 
  #   
  #   if (USER$Logged == TRUE) {   
#### Login ####    
  


#}  
  
}
