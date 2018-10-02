M1_ClassUI <- function(id,date,names){

  ns <- NS(id)

     box(width = "130%",collapsible = TRUE,status = "primary",
           
         tabsetPanel(selected = NULL,
                 
                 tabPanel(title="گروه را انتخاب کنید",icon = icon("mail-forward")),
                 tabPanel(title="یک"),
                 tabPanel(title="دو",M2_ClassUI(ns("mod_CP2"),date,names)),
                 tabPanel(title="سه",M2_ClassUI(ns("mod_CP3"),date,names))
                         
                                    ))}


M1_Class <- function(input,output,session,Data,date,names){
  
callModule(M2_Class,"mod_CP2",Data,date,names)
callModule(M2_Class,"mod_CP3",Data,date,names)  


}
