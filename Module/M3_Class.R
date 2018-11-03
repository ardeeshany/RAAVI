M3_ClassUI <- function(id){

  ns <- NS(id)

  tabsetPanel(
    
    
     #tabPanel(title=div(class="tabPanel--font-size center","تحلیل را انتخاب کنید"),icon = icon("hand-o-right",class="tabPanel-icon")),
     M0_LoadUI(ns("Load")),
     M0_BoxUI(ns("Box")),
     M0_ScatterUI(ns("Scatter")),     
     M0_HistUI(ns("Hist")),
     M0_ProgUI(ns("Progress")),
     M0_CatUI(ns("Category"))
     
  )}


M3_Class <- function(input,output,session,outputDir,class,level,course,font_plot){

        callModule(M0_Box,"Box",vals,font_plot)
        callModule(M0_Hist,"Hist",vals,font_plot)
        callModule(M0_Scatter,"Scatter",vals,font_plot)
        callModule(M0_Cat,"Category",vals,font_plot)
        callModule(M0_Prog,"Progress",vals,font_plot)
vals <- callModule(M0_Load,"Load",outputDir)

V <- reactive({
  M <- tidyr::gather(cbind(name=vals[["names"]],vals[["now"]],class=class,level=level,course=course),date,grade,-name,-class,-level,-course)
  return(M)
})

return(V)

}
