
M_SummaryUI <- function(id){

  ns <- NS(id)
  
tagList( 
 
#   fluidRow(
#   box(
#     title = "Box title", width = 6, status = "primary",
#     "Box content"
#   ),
#   box(
#     status = "warning", width = 6,
#     "Box content"
#   )
# ),


# layout based on column priority; "column" 
fluidRow(
  
  # column(width = 4,
  #        box(
  #          title = "Title 1", width = NULL, solidHeader = TRUE, status = "primary",
  #          "Box content"
  #        ),
  #        box(
  #          width = NULL, background = "black",
  #          "A box with a solid black background"
  #        )
  # ),
  # column(width = 8,offset = 2,
  #        div(style="margin-top:0%; text-align: center;font-size: 0.5em;",
  #        box(
  #          title = "", width = NULL, solidHeader = TRUE, status = "primary",
           # div(style="font-size: 5em; font-family:'dastnevis';margin-top:5%;padding-bottom: 5%;",
           # HTML(paste("دیتاهای شما قصه های زیادی برای گفتن دارند",
           #            div(style="margin-top:1%;"),
           #            div(style="color:red;","راوی"),
           #            div(style="display:inline-block;","آن قصه ها را برایتان می خواند"),
           #            sep="<br/>")))
            div(style="font-size: 2em; text-align:center; font-family:'dastnevis';margin-top:3%;", #padding-bottom: 5%;",
           "دیتاهای شما قصه های زیادی برای گفتن دارند",
           div(style="margin-top:1%;"),
           "آن قصه ها را برایتان می خواند",
           div(class="DimGray",style="font-size:1.5em;","راوی")
          #                 )
          # ))
         
         # box(
         #   title = "Title 5", width = NULL, background = "light-blue",
         #   "A box with a solid light-blue background"
         # )
  ),
  M_InfoUI(ns("info"))
  
  
  # column(width = 4,
  #        box(
  #          title = "Title 2", width = NULL, solidHeader = TRUE,
  #          "Box content"
  #        ),
  #        box(
  #          title = "Title 6", width = NULL, background = "maroon",
  #          "A box with a solid maroon background"
  #        )
  # )
)
## InfoBox and ValueBox         
#infoBoxOutput(ns("progressBox"),width = 3),
#valueBoxOutput(ns("approvalBox"),width = 3)
##         

)

}




M_Summary <- function(input,output,session){
 
  callModule(M_Info,"info")
  
  # infoBox
  # output$progressBox <- renderInfoBox({
  #   infoBox(
  #     "Progress", "80%" , icon = icon("list"),
  #     color = "purple", fill = FALSE ,width = 1
  #   )
  # })
  
  # valueBox    
  # output$approvalBox <- renderValueBox({
  #   valueBox(
  #     "70%", "Approval", icon = icon("thumbs-up", lib = "glyphicon"),
  #     color = "yellow",width = 2
  #   )
  # }) 
}