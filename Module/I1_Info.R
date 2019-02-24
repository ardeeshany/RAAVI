I1_InfoUI <- function(id){
  
  ns <- NS(id)
   tabsetPanel(
              I0_CardUI(ns("card")),
              I0_ContactUI(ns("contact"))
        )
  
}


I1_Info <- function(input,output,server){
  
  callModule(I0_Card,"card")
  callModule(I0_Contact,"contact")
  
  
}