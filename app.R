#  ssh root@159.65.170.96f
#  sudo su - -c "R -e \"install.packages('Cairo', repos='http://cran.rstudio.com/')\""
#  sudo su - -c "R -e \"devtools::install_github('s-u/rJava')\""

# lib <- c("shiny","stats","openxlsx","ConvCalendar","shinydashboard","graphics",
#          "reshape2","plotly","ggplot2","rhandsontable","tidyr","stringr","extrafont",
#          "dashboardthemes","shinydashboardPlus","mailR")
# # "rdrop2","rio","shiny.semantic",
# # "networkD3","visNetwork","profvis"
# 
# lapply(lib,library,character.only=TRUE)



# sudo su - -c "R -e \"install.packages('rJava', repos='http://cran.rstudio.com/')\""

# ## app.R ##f
library(shiny)
library(stats)
library(rmarkdown)
library(knitr)
library(webshot)
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
library(tinytex)
#library(shinyWidgets)
#library(shiny.semantic)
#library(shinythemes)
#library(networkD3)
library(dashboardthemes)
#library(visNetwork)
library(shinydashboardPlus)
#library(mailR)
library(shinycssloaders)
#library(profvis)
#library(gridExtra)
library(DT)
library(shinyWidgets)
library(shinyjs)
library(colourpicker)
library(openssl)
# library(Cairo)
# 
# CairoFonts(
#   regular="FreeSans:style=Medium",
#   bold="FreeSans:style=Bold",
#   italic="FreeSans:style=Oblique",
#   bolditalic="FreeSans:style=BoldOblique"
# )


# font_import(paths = "www/",prompt = FALSE)
# loadfonts()
Sys.setlocale(category = 'LC_ALL','en_US.UTF-8')
#options(shiny.usecairo=FALSE)

#font_import(pattern="dastnevis",prompt=FALSE)
#loadfonts()

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
  source('Module/MainModules/Report.R')
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
source('Module/Login.R')

    ui <- dashboardPage(
      
      HeaderUI("mod_header"),                   
      SidebarUI("mod_sidebar"),
      BodyUI("mod_body",theme=theme_RAAVI)
      #RightSidebarUI("mod_rightsidebar"),
      
      
    
      )
    
    
    
    server <- function(input, output,session) {
      
      callModule(Header,"mod_header")
      callModule(Sidebar,"mod_sidebar")
      #callModule(RightSidebar,"mod_rightsidebar")
      callModule(Body,"mod_body")
    }
      
    
  shinyApp(ui=ui,server=server)
  
  
  
  
  
  
  