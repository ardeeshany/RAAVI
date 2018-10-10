

ColorUI<- function(id){
  
  ns <- NS(id)

tags$head(tags$style(HTML('
        /* logo */
                          .skin-blue .main-header .logo {
                          background-color: #f4b943 ;
                          }
                          
        /* logo when hovered; chnage color when mouseover on it*/
                          .skin-blue .main-header .logo:hover {
                          background-color: darkgoldenrod ;
                          }
                          
        /* navbar (rest of the header) */
                          .skin-blue .main-header .navbar {
                          background-color: darkcyan ;
                          }   
                          
                          
        /* main sidebar */
                          .skin-blue .main-sidebar {
                           background-color: darkolivegreen ;
                                   }
                          
                          
                           .skin-blue .main-sidebar .sidebar .sidebar-menu .treeview-menu {
                           background-color: darkolivegreen;
                                    }



                          
        /* active selected tab in the sidebarmenu */
                          .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                          background-color: teal;
                          }
                          

                          
        /* other links in the sidebarmenu when hovered */
                          .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                          background-color: teal;
                          }
                          

        /* other links in the sidebarmenu */
                          .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                          #        background-color: darkcyan;
                          color: white;
                          }

                          
        /* toggle button when hovered  */                    
                          .skin-blue .main-header .navbar .sidebar-toggle:hover{
                          background-color: #ff69b4;
                          }
                          
                          
                          
                          ')))
}

Color <- function(input,output,session){}