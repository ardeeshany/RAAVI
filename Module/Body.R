
appCSS <- "
#loading-content {
position: absolute;
background: #000000;
opacity: 0.9;
z-index: 100;
left: 0;
right: 0;
height: 100%;
text-align: center;
color: #FFFFFF;
}"


BodyUI <- function(id,theme){
  
ns <- NS(id)

# tags$head(
#   tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
# )



dashboardBody(
#tags$head(tags$script(HTML("$('body').addClass('sidebar-mini');"))),
theme,

#ColorUI("mode_color"),  
  # tags$head( 
  #   tags$style(HTML(".main-sidebar { color: #f4b943; }")) 
  # )
#tags$head(tags$link(href = "https://fonts.googleapis.com/css?family=Reem+Kufi|El+Messiri|Lalezar|Scheherazade", rel = "stylesheet")),
#tags$head(tags$link(href = "/IranNastaliq.ttf", rel = "stylesheet")),

#tags$head(tags$link(href = "IranNastaliq.ttf", rel = "stylesheet")), 

tags$head(tags$link(href = "custom.css", rel = "stylesheet")), 

# tags$head(
#   tags$link(rel="stylesheet", type = "text/css",
#             href = "IranNastaliq")
# ),
#tags$head( includeCSS(path = "www/style.css") ),


useShinyjs(),
inlineCSS(appCSS),

# Loading message
div(
  id = ns("loading-content"),
  div(style="text-align:center",
  h2("اپلیکیشن راوی در حال باز شدن است"))
),


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
}

Body <- function(input,output,session, outputadrs="RAAVI/RAAVI-Released/DATA/Test"){
  
          hide(id = "loading-content", anim = TRUE, animType = "fade")    
          #show("app-content")
  
          callModule(M_Summary,"summary")
          callModule(I1_Info,"info")
          callModule(M3_Class,"cls",outputDir = outputadrs,class="0",level="0",course="0",font_plot = "dastnevis")
  
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
