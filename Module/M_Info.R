
M_InfoUI <- function(id){

  ns <- NS(id)
  
  # tabsetPanel(
  # NetInfoUI(ns("mod_netinfo")),
  # CardInfoUI(ns("mod_cardinfo"))
  # )

  CardInfoUI(ns("mod_cardinfo"))

}




M_Info <- function(input,output,session){

  #callModule(NetInfo,"mod_netinfo")
  callModule(CardInfo,"mod_cardinfo")
  
}