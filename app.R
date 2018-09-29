## app.R ##f
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
library(rdrop2)
library(rio)
library(tidyr)
library(stringr)
library(extrafont)
#library(shinyWidgets)
#library(shiny.semantic)

#font_import()
Sys.setlocale(category = 'LC_ALL','en_US.UTF-8')

## modules ##
source('ui_color.R')

source('Module/Header.R')
source('Module/Sidebar.R')
source('Module/Body.R')

source('Data_Import.R')
source('ui_outside_functions.R')

source('Module/M1_Class.R')
source('Module/M2_Class.R')
source('Module/M3_Class.R')
  source('Module/MainModules/M0_Hist.R')
  source('Module/MainModules/M0_Scatter.R')
  source('Module/MainModules/M0_Box.R')
  source('Module/MainModules/M0_Category.R')
  source('Module/MainModules/M0_Progress.R')
  source('Module/MainModules/M0_Load.R')
source('Module/M_Student.R')
  source('Module/MainModules/M0_St_Scatter.R')
source('Module/M_Summary.R')


ui <- dashboardPage(

HeaderUI("mod_header"),                   
SidebarUI("mod_sidebar"),
BodyUI("mod_body")

)


server <- function(input, output,session) {

callModule(Header,"mod_header")
callModule(Sidebar,"mod_sidebar")
callModule(Body,"mod_body")
  
}


shinyApp(ui, server) #,enableBookmarking = "url")

