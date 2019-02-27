# 
# appCSS <- "
# #loading-content {
# position: absolute;
# background: #000000;
# opacity: 0.9;
# z-index: 100;
# left: 0;
# right: 0;
# height: 100%;
# text-align: center;
# color: #FFFFFF;
# }"


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

tags$head(tags$style(HTML("@font-face { font-family: dastnevis; src: url(dastnevis.ttf);}") )), 

tags$head(tags$style(HTML("@font-face { font-family: main_font; src: url(IRANSansDNLight.ttf);}") )),

tags$head(tags$style(HTML("@font-face { font-family: IRANSansDN; src: url(IRANSansDN.ttf);}") )),

# tags$head(
#   tags$link(rel="stylesheet", type = "text/css",
#             href = "IranNastaliq")
# ),
#tags$head( includeCSS(path = "www/style.css") ),


# useShinyjs(),
# inlineCSS(appCSS),
# 
# # Loading message
# div(
#   id = ns("loading-content"),
#   div(style="text-align:center",
#   h2("اپلیکیشن راوی در حال باز شدن است"))
# ),


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

#tabItem(tabName = "Class", M3_ClassUI(ns("cls"))),
#M0_LoadUI(ns("Load")),
#M0_BoxUI(ns("Box")),
#M0_ScatterUI(ns("Scatter")),     
#M0_HistUI(ns("Hist")),
#M0_ProgUI(ns("Progress")),
#M0_CatUI(ns("Category"))
tabItem(tabName = "Load", M0_LoadUI(ns("load"))),
tabItem(tabName = "Box", M0_BoxUI(ns("box"))),
tabItem(tabName = "Scatter", M0_ScatterUI(ns("scatter"))),
tabItem(tabName = "Hist", M0_HistUI(ns("hist"))),
tabItem(tabName = "Progress", M0_ProgUI(ns("progress"))),
tabItem(tabName = "Category", M0_CatUI(ns("category")))
# tabItem(tabName = "C12",M1_ClassUI(ns("c12"))), 
# tabItem(tabName = "C11",M1_ClassUI(ns("c11"))), 
# tabItem(tabName = "C10",M1_ClassUI(ns("c10"))), 
#tabItem(tabName="Student", M_StudentUI(ns("student"),names_all)),
#tabItem(tabName = "Summary", M_SummaryUI(ns("summary"))),
#tabItem(tabName = "Info", I1_InfoUI(ns("info")))
       ),
tags$head(includeHTML(("www/google-analytics.html")))
)
}

Body <- function(input,output,session, 
                 class="0",level="0",course="0",
                 outputDir="RAAVI/RAAVI-Released/DATA/Test",font_plot = "IRANSansDN"){
  
          #hide(id = "loading-content", anim = TRUE, animType = "fade")    
          #show("app-content")
  
          #callModule(M_Summary,"summary")
          #callModule(M0_Cat,"category")
          #callModule(I1_Info,"info")
          #callModule(M3_Class,"cls",outputDir = outputadrs,class="0",level="0",course="0")
  
          
          #callModule(M0_Box,"Box",vals,font_plot)
          #callModule(M0_Hist,"Hist",vals,font_plot)
          #callModule(M0_Scatter,"Scatter",vals,font_plot)
          callModule(M0_Box,"box",vals,font_plot)
          callModule(M0_Cat,"category",vals,font_plot)
          callModule(M0_Scatter,"scatter",vals,font_plot)
          callModule(M0_Hist,"hist",vals,font_plot)
          callModule(M0_Prog,"progress",vals,font_plot)
          #callModule(M0_Prog,"Progress",vals,font_plot)
          vals <- callModule(M0_Load,"load",outputDir)
          
          # V <- reactive({
          #   M <- tidyr::gather(cbind(name=vals[["names"]],vals[["now"]],class=class,level=level,course=course),date,grade,-name,-class,-level,-course)
          #   return(M)
          # })
          
          
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
