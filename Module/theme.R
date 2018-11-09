theme_RAAVI <- function(font){
  
  
  shinyDashboardThemeDIY(
  
  ### general
   appFontFamily = font
  ,appFontColor = "rgb(0,0,0)"
  #,bodyBackColor = "rgb(248,248,248)"
  ,bodyBackColor = "white"
  
  ### header
  ,logoBackColor = "#2E4A62"
  
  ,headerButtonBackColor = "#485167"
  ,headerButtonIconColor = "white"
  ,headerButtonBackColorHover = "rgb(238,238,238)"
  ,headerButtonIconColorHover = "#485167"
  
  ,headerBackColor = "#485167"
  ,headerBoxShadowColor = "white"
  ,headerBoxShadowSize = "2px 2px 2px"
  
  ### sidebar
  
  ,sidebarBackColor="#2E4A62"
  # ,sidebarBackColor = cssGradientThreeColors(
  #   direction = "down"
  #   ,colorStart = "rgb(20,97,117)"
  #   ,colorMiddle = "rgb(56,161,187)"
  #   ,colorEnd = "rgb(3,22,56)"
  #   ,colorStartPos = 0
  #   ,colorMiddlePos = 50
  #   ,colorEndPos = 100
  # )
  ,sidebarPadding = 0
  
  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = 0
  ,sidebarMenuBorderRadius = 0
  
  ,sidebarShadowRadius = "3px 5px 5px"
  ,sidebarShadowColor = "#aaaaaa"
  
  ,sidebarUserTextColor = "rgb(255,255,255)"
  
  ,sidebarSearchBackColor = "rgb(55,72,80)"
  ,sidebarSearchIconColor = "rgb(153,153,153)"
  ,sidebarSearchBorderColor = "rgb(55,72,80)"
  
  ,sidebarTabTextColor = "white"
  ,sidebarTabTextSize = 18
  ,sidebarTabBorderStyle =  "none none solid none"
  ,sidebarTabBorderColor = "rgb(35,106,135)"
  ,sidebarTabBorderWidth = 2
  
  ,sidebarTabBackColorSelected = "#485167"
  
  # ,sidebarTabBackColorSelected = cssGradientThreeColors(
  #   direction = "right"
  #   ,colorStart = "rgba(44,222,235,1)"
  #   ,colorMiddle = "rgba(44,222,235,1)"
  #   ,colorEnd = "rgba(0,255,213,1)"
  #   ,colorStartPos = 0
  #   ,colorMiddlePos = 30
  #   ,colorEndPos = 100
  # )
  #,sidebarTabTextColorSelected = "rgb(0,0,0)"
  ,sidebarTabTextColorSelected = "#9C9A40"
  ,sidebarTabRadiusSelected = "0px 0px 0px 0px"
  
  
  ,sidebarTabBackColorHover = "#2E4A62"
  # ,sidebarTabBackColorHover = cssGradientThreeColors(
  #   direction = "right"
  #   ,colorStart = "rgba(44,222,235,1)"
  #   ,colorMiddle = "rgba(44,222,235,1)"
  #   ,colorEnd = "rgba(0,255,213,1)"
  #   ,colorStartPos = 0
  #   ,colorMiddlePos = 30
  #   ,colorEndPos = 100
  # )
  ,sidebarTabTextColorHover = "white"
  ,sidebarTabBorderStyleHover = "none none solid none"
  ,sidebarTabBorderColorHover = "rgb(75,126,151)"
  ,sidebarTabBorderWidthHover = 4
  ,sidebarTabRadiusHover = "0px 0px 0px 0px"
  
  ### boxes
  ,boxBackColor = "rgb(255,255,255)"
  ,boxBorderRadius = 5
  ,boxShadowSize = "0px 1px 1px"
  ,boxShadowColor = "rgba(0,0,0,.1)"
  ,boxTitleSize = 16
  ,boxDefaultColor = "rgb(210,214,220)"
  ,boxPrimaryColor = "#578CA9"
  ,boxSuccessColor = "rgba(0,255,213,1)"
  ,boxWarningColor = "rgb(244,156,104)"
  ,boxDangerColor = "rgb(255,88,55)"
  
  ,tabBoxTabColor = "rgb(255,255,255)"
  ,tabBoxTabTextSize = 14
  ,tabBoxTabTextColor = "rgb(0,0,0)"
  ,tabBoxTabTextColorSelected = "rgb(0,0,0)"
  ,tabBoxBackColor = "rgb(255,255,255)"
  ,tabBoxHighlightColor = "rgba(44,222,235,1)"
  ,tabBoxBorderRadius = 5
  
  ### inputs
  ,buttonBackColor = "rgb(245,245,245)"
  ,buttonTextColor = "rgb(0,0,0)"
  ,buttonBorderColor = "rgb(200,200,200)"
  ,buttonBorderRadius = 5
  
  ,buttonBackColorHover = "rgb(235,235,235)"
  ,buttonTextColorHover = "rgb(100,100,100)"
  ,buttonBorderColorHover = "rgb(200,200,200)"
  
  ,textboxBackColor = "rgb(255,255,255)"
  ,textboxBorderColor = "rgb(200,200,200)"
  ,textboxBorderRadius = 5
  ,textboxBackColorSelect = "rgb(245,245,245)"
  ,textboxBorderColorSelect = "rgb(200,200,200)"
  
  ### tables
  ,tableBackColor = "rgb(255,255,255)"
  ,tableBorderColor = "rgb(240,240,240)"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
  
)}