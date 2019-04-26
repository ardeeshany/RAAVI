ReportUI <- function(id){
  
  ns <- NS(id)
  
  fluidRow(

### First Column    
      
div(style="text-align:center;",
column(width = 2,
    br(),
    box(width="100%",
    #title = div(class="load__title--font-size","وارد کردن داده"),
    status="primary",
    wellPanel(
      fileInput(inputId = ns("f_new"),
                label = div(class="load__subtitle--font-size",'آپلود فایل'),
                buttonLabel = list(icon("file-excel-o")),
                placeholder = "ورود",
                width = "100%",
                accept=".xlsx")),
    
    wellPanel(
        div(style="align:center; tet-align:center;",
        actionBttn(inputId = ns("f_test"),style = "jelly",color = "warning",
                   label = div(class="action-button--widget","فایل نمونه"))))
    
    ))),

### Second Column    

div(style="text-align:center;",
column(width = 2, title = div(class="load__title--font-size","روند کلاس"),
   M0_BoxUI(ns("box")) 
   )),
   

### Third Column    

div(style="text-align:center;",
    column(width = 2, title = div(class="load__title--font-size","روند دانش آموزان"),
           M0_ScatterUI(ns("scatter")) 
    ))


)}


### Server

Report <- function(input,output,session){
  
  
  ns <- session$ns
  
  Date_US <- as.OtherDate(Sys.Date(),"persian")[1:3]
  Date_Persian = sprintf("%s-%s-%s",Date_US[3],Date_US[2],Date_US[1])
  
  
  values <- reactiveValues(tot=NULL)
  
  observeEvent(input$f_new,{
     D_new <- read.xlsx(input$f_new$datapath)
     
     if(dim(D_new)[1] > 20)
     D_new <- D_new[1:21,]
  
     if(dim(D_new)[2] > 20)
     D_new <- D_new[,1:21]
     
     values[["names"]] <- D_new[-1,1]
     values[["dates"]] <-colnames(D_new)[-1]
     
     A <- D_new[-1,-1]
     
     if(!is.null(dim(A))){
       for(i in 1:dim(A)[1]){
         for(j in 1:dim(A)[2]){
           A[i,j] <- persian.tonumber(A[i,j])}}}
     
     values[["now"]] <- data.matrix(A)
     
     })
  
  
  
  observeEvent(input$f_test,{
    D_new <- read.xlsx(file.path(getwd(),"www/Data.xlsx"))
    values[["names"]] <- D_new[-1,1]
    values[["dates"]] <-colnames(D_new)[-1]
    
    A <- D_new[-1,-1]
    
    if(!is.null(dim(A))){
      for(i in 1:dim(A)[1]){
        for(j in 1:dim(A)[2]){
          A[i,j] <- persian.tonumber(A[i,j])}}}
    
    values[["now"]] <- data.matrix(A)

  })

  persian <- "\u0660\u0661\u0662\u0663\u0664\u0665\u0666\u0667\u0668\u0669\u06F0\u06F1\u06F2\u06F3\u06F4\u06F5\u06F6\u06F7\u06F8\u06F9"
  english <- "01234567890123456789"
  persian.tonumber <- function(s) as.character(chartr(persian,english,s))
  
  callModule(M0_Box,"box",values,"IRANSansDN")
  callModule(M0_Scatter,"scatter",values,"IRANSansDN")
  
}
