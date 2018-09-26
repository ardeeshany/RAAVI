## Sidebar ##
SidebarUI <- function(id){
  
  ns <- NS(id)

  dashboardSidebar(
    width = 200,
    
    helpText(div(style="text-align:center; color: white ;font-size: 350%; font-weight: bold;",
                 "راوی")),
    
    sidebarMenu(id = ns("menu1"),          
              
              
      #sidebarSearchForm(textId = "searchText", buttonId = "searchButton",label = "Search..."),

      menuItem("ورود", tabName="Login", icon = icon("sign-in")),
      menuItem("خلاصه", tabName= "Summary", icon = icon("list-ul")),
      menuItem("کلاس",tabname= "Class", icon = icon("sitemap"),tabName = "Ref",
               menuSubItem("دوازدهم", tabName = "C12"),
               menuSubItem("یازدهم", tabName = "C11"),
               menuSubItem("دهم", tabName = "C10")),
      menuItem("دانش آموز", tabName = "Student", icon = icon("users","lg")),
      menuItem("دبیر", tabName = "Teacher", icon = icon("user-secret","fa-lg")),
      menuItem("Source code", icon = icon("file-code-o"),       # link to external content
                       href = "https://github.com/ardeeshany/"),

      ### Dynamic item : Instead of menuItem, use menuItemOutput
      ### It needs a render function in server function
       menuItemOutput(ns("menuitem")),
      #  tags$head(tags$style(HTML('.shiny-server-account { display: none; }'))),

      # The dynamically-generated user panel
      uiOutput(ns("userpanel"))
        
  ))
}

Sidebar <- function(input,output,session){
  
  # output$menuitem <- renderMenu({
  #   sidebarMenu(
  #     menuItem("Menu item", icon = icon("calendar"))
  #   )
  # })
  
  output$userpanel <- renderUI({
    # session$user is non-NULL only in authenticated sessions
    if (!is.null(session$user)) {
      sidebarUserPanel(
        span("Logged in as ", session$user),
        subtitle = a(icon("sign-out"), "Logout", href="__logout__"))
    }
  })
  
  return(list())
}