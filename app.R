#    159.65.170.96
#  sudo su - -c "R -e \"install.packages('devtools', repos='http://cran.rstudio.com/')\""
#  sudo su - -c "R -e \"devtools::install_github('daattali/shinyjs')\""


# lib <- c("shiny","stats","openxlsx","ConvCalendar","shinydashboard","graphics",
#          "reshape2","plotly","ggplot2","rhandsontable","tidyr","stringr","extrafont",
#          "dashboardthemes","shinydashboardPlus","mailR")
# # "rdrop2","rio","shiny.semantic",
# # "networkD3","visNetwork","profvis"
# 
# lapply(lib,library,character.only=TRUE)


# ## app.R ##f
library(shiny)
library(stats)
#library(XLConnect)
#library(xlsx)
#library(readxl)
library(openxlsx)
library(ConvCalendar)
library(shinydashboard)
#library(truncnorm)
library(graphics)
library(reshape2)
library(plotly)
library(ggplot2)
library(rhandsontable)
#library(rdrop2)
#library(rio)
library(tidyr)
library(stringr)
library(extrafont)
#library(shinyWidgets)
#library(shiny.semantic)
#library(shinythemes)
#library(networkD3)
library(dashboardthemes)
#library(visNetwork)
library(shinydashboardPlus)
library(mailR)
library(shinycssloaders)
#library(profvis)
#library(gridExtra)
library(DT)


#font_import()
Sys.setlocale(category = 'LC_ALL','en_US.UTF-8')



## modules ##
#source('Module/Extra/ui_color.R')

source('Module/Header.R')
source('Module/Sidebar.R')
#source('Module/RightSidebar.R')
source('Module/Body.R')
source('Module/theme.R')

#source('Module/Extra/Data_Import.R')
source('Module/Extra/ui_outside_functions.R')

#source('Module/M1_Class.R')
#source('Module/M2_Class.R')
source('Module/M3_Class.R')
  source('Module/MainModules/M0_Hist.R')
  source('Module/MainModules/M0_Scatter.R')
  source('Module/MainModules/M0_Box.R')
  source('Module/MainModules/M0_Category.R')
  source('Module/MainModules/M0_Progress.R')
  source('Module/MainModules/M0_Load.R')
#source('Module/M_Student.R')
  #source('Module/MainModules/M0_St_Scatter.R')
source('Module/M_Summary.R')
source('Module/I1_Info.R')
  #source('Module/MainModules/I0_Net.R')
  source('Module/MainModules/I0_Card.R')
  source('Module/MainModules/I0_Contact.R')

    ui <- dashboardPage(
      
      HeaderUI("mod_header"),                   
      SidebarUI("mod_sidebar"),
      BodyUI("mod_body",theme=theme_RAAVI)
      #rightsidebar = RightSidebarUI("mod_rightsidebar")
      
    )
    
    
    server <- function(input, output,session) {
      
      callModule(Header,"mod_header")
      callModule(Sidebar,"mod_sidebar")
      # callModule(RightSidebar,"mod_rightsidebar")
      callModule(Body,"mod_body")
    }
      
    
  shinyApp(ui=ui,server=server)
