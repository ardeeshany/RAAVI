RightSidebarUI <- function(id){

  ns <- NS(id)
  
  
  rightSidebar(
  background = "light",
  rightSidebarTabContent(
    id = 1,
    title = div(style="text-align:right;","درباره ی راوی"),
    icon = "envelope",
    active = TRUE,
    div(class="color__raavi", style="text-align:right","راوی")
  )
  
  )
  
}

RightSidebar <- function(inut,output,session){}