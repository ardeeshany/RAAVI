M1_ClassUI <- function(id){

          ns <- NS(id)
          
          box(width = "130%",collapsible = TRUE,status = "primary",
              tabsetPanel(selected = NULL,

                          
                          tabPanel(title="گروه را انتخاب کنید",icon = icon("mail-forward")),
                          tabPanel(title="یک",M2_ClassUI(ns("c12_1"))),
                          tabPanel(title="دو",M2_ClassUI(ns("c12_2"))),
                          tabPanel(title="سه",M2_ClassUI(ns("c12_3")))

                                  ))}

M1_Class <- function(input,output,session,outputcls,class){

            callModule(M2_Class,"c12_1",outputlev = sprintf("%s/level1",outputcls),class,level="1")
    vals <- callModule(M2_Class,"c12_2",outputlev = sprintf("%s/level2",outputcls),class,level="2")
            callModule(M2_Class,"c12_3",outputlev = sprintf("%s/level3",outputcls),class,level="3")  

    return(vals)
}
