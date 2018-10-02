## Sidebar ##
SidebarUI <- function(id){
  
  ns <- NS(id)

  dashboardSidebar(
    width = 180,
    
    # helpText(div(style="margin-top:10%; color : AliceBlue; text-align:center ;font-size: 380%; font-weight: bold;font-family:'dastnevis';",
    #              "راوی")),

    sidebarMenu(id = ns("menu1"),          
              
              
      #sidebarSearchForm(textId = "searchText", buttonId = "searchButton",label = "Search..."),

      #menuItem("ورود", tabName="Login", icon = icon("sign-in")),
      
      div(style="margin-top:1em;",br()),
      
      div(style="font-family:'dastnevis'; font-size:1.4em; margin-top:25%; margin-left:5%;text-align:center;",
          menuItem(div(style=" color:AliceBlue; ","معرفی"), tabName= "Summary",icon = icon("list-ul")),
          div(style="margin-top:25%;",
          menuItem(div(style=" color:AliceBlue; ", "کلاس"),  tabName= "Class",icon  =icon("sitemap"))),
          div(style="margin-top:120%;",
              menuItem(div(style=" color:AliceBlue; ", "ارتباط با ما"),  tabName= "Info"))
          ),
      
      div(style="text-align:center;",
      tags$img(src='logogrey.svg')
      )
      # div(style="font-family:'dastnevis'; font-size:1.5em; margin-top:5%; margin-left:5%;",
      #     menuItem("معرفی", tabName= "Summary",icon = icon("list-ul")),
      #     div(style="margin-top:5%;",
      #     menuItem("کلاس",  tabName= "Class",icon =icon("sitemap")))
      #     )
      
      
      
      # menuItem("کلاس",tabname= "Class", icon = icon("sitemap"),tabName = "Ref",
      #          menuSubItem("دوازدهم", tabName = "C12"),
      #          menuSubItem("یازدهم", tabName = "C11"),
      #          menuSubItem("دهم", tabName = "C10"))
      #menuItem("دانش آموز", tabName = "Student", icon = icon("users","lg")),
      #menuItem("دبیر", tabName = "Teacher", icon = icon("user-secret","fa-lg")),
      #menuItem("Source code", icon = icon("file-code-o"),       # link to external content
      #                 href = "https://github.com/ardeeshany/"),

      ### Dynamic item : Instead of menuItem, use menuItemOutput
      ### It needs a render function in server function
       #menuItemOutput(ns("menuitem")),
      #  tags$head(tags$style(HTML('.shiny-server-account { display: none; }'))),

      # The dynamically-generated user panel
      #uiOutput(ns("userpanel"))
        
  ))
}

Sidebar <- function(input,output,session){
  
  # output$menuitem <- renderMenu({
  #   sidebarMenu(
  #     menuItem("Menu item", icon = icon("calendar"))
  #   )
  # })
  
  # output$userpanel <- renderUI({
  #   # session$user is non-NULL only in authenticated sessions
  #   if (!is.null(session$user)) {
  #     sidebarUserPanel(
  #       span("Logged in as ", session$user),
  #       subtitle = a(icon("sign-out"), "Logout", href="__logout__"))
  #   }
  # })
  
  #return(list())
}