ReportUI <- function(id){
  
  ns <- NS(id)
  
  fluidRow(

### First Column    
      
div(style="text-align:center;",
column(width = 3,
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
column(width = 3, title = div(class="load__title--font-size","روند کلاس"),
  
   M0_BoxUI(ns("box")) 
   #  wellPanel(
   #    br(),
   #    actionBttn(inputId = ns("Bx_Ac"),style = "jelly",color = "warning",
   #               label= div(class="action-button--widget","تحیل داده")),
   #   br(),
   #   div(style="align:center; text-align:center",
   #       downloadBttn(ns("report"),label = "گزارش روند کلاس",size = "sm")
   # ))
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
    #values[["now"]] <- D_new[,-1]
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
  
  
  Data <- reactive({
    if(!is.null(values)){
    M <- values[["now"]]
    rownames(M) <- values[["names"]]
    colnames(M) <- values[["dates"]]
    return(M)}
  })
  
  
  
  Reac_CP2M_Bx1 <- eventReactive(input$Bx_Ac, {
    
    # validate(
    #   need(!is.null(Data()),"هنوز داده ای وارد نشده است"), errorClass = "Hist_l"
    # )
    
    # min=which(colnames(Data())==input$Bx_SeI1)
    # max=which(colnames(Data())==input$Bx_SeI2)
    min=1
    max=dim(Data())[2]
    
    validate(
      need(min <= max,"زمان ابتدا نباید بعد از زمان انتها باشد"), errorClass = "Hist_l"
    )
    
    if(min < max){
      d <- Data()[,min:max]
      melt_Data_Bx <- melt(d)
      colnames(melt_Data_Bx) <- c("Student","Day","value")
      vec_ind <- min:max
    }
    if(min==max){
      d <- Data()[,max]
      melt_Data_Bx <- melt(as.matrix(d))
      colnames(melt_Data_Bx) <- c("Student","Day","value")
      melt_Data_Bx$Day <- input$Bx_SeI1    #factor(melt_Data_Bx$Day)
      vec_ind <- max
    }
    
    
    p1 <- ggplot(melt_Data_Bx , aes(x=Day,y=value,fill=Day))+ geom_boxplot() +
      labs(title = "روند کلاس در طول زمان", x ="",y="نمره",fill="تاریخ")+
      scale_x_discrete(labels=colnames(Data())[vec_ind])+
      theme(axis.text.x = element_text(size=11,colour="black",angle=60, hjust=1,vjust=.5),
            axis.text.y = element_text(size=12,colour="black"),
            axis.title=element_text(size=14,face="bold"),
            plot.title = element_text(size=14,face="bold"),
            legend.title = element_text(size=12,face="bold"),
            text=element_text(family=font_plot))
    
    p2 <- ggplot(melt_Data_Bx , aes(x=Day,y=value,fill=Day))+
      stat_summary(fun.y=mean, geom="point", size=3)+# shape=20, size=2, color="red", fill="red")+
      stat_summary(fun.data = mean_se, geom = "errorbar")+
      labs(title = "میانگین و میزان انحراف از آن", x ="",y="نمره",fill="تاریخ")+
      scale_x_discrete(labels=colnames(Data())[vec_ind])+
      theme(axis.text.x = element_text(size=11,colour="black",angle=60, hjust=1,vjust=.5),
            axis.text.y = element_text(size=12,colour="black"),
            axis.title=element_text(size=14,face="bold"),
            plot.title = element_text(size=14,face="bold"),
            legend.title = element_text(size=12,face="bold"),
            text=element_text(family=font_plot))
    
    return(list(p1=p1,p2=p2))
    
  })
  
  Reac_CP2M_Bx <- reactive({
    
    if(input$add_points){
      p1 <- Reac_CP2M_Bx1()$p1 + geom_jitter(width = 0.1,size=1.8)
    }else{
      p1 <- Reac_CP2M_Bx1()$p1
    }
    
    p2 <- Reac_CP2M_Bx1()$p2
    
    if(input$combine){
      color_m <- "#ffff4e"
      color_s <- "#fd1c00"
      p1 <- p1 + stat_summary(fun.y=mean, geom="point",size=1.35,color = color_m ,fill= color_m, shape=20)+# shape=20, size=2, color="red", fill="red")+
        stat_summary(fun.data = mean_se, geom = "errorbar",color=color_s)
      gg2 <- NULL
    }else{
      
      gg2 <- ggplotly(p2) %>% config(displaylogo = FALSE,collaborate = FALSE,modeBarButtonsToRemove = list(
        'zoom2d','pan2d','select2d','lasso2d','zoomIn2d','zoomOut2d',
        'sendDataToCloud',
        'autoScale2d',
        'hoverClosestCartesian',
        'hoverCompareCartesian'
      )) 
      
    }  
    
    
    gg1 <- ggplotly(p1) %>% config(displaylogo = FALSE,collaborate = FALSE,modeBarButtonsToRemove = list(
      'zoom2d','pan2d','select2d','lasso2d','zoomIn2d','zoomOut2d',
      'sendDataToCloud',
      'autoScale2d',
      'hoverClosestCartesian',
      if(input$combine)'hoverCompareCartesian'
    ))
    
    return(list(gg1=gg1,gg2=gg2))
    
  })
  
  out_ind <- reactiveValues(a=0)
  
  observeEvent(input$Bx_Ac, {
    out_ind$a = 1
  })
  
  
  
  
  
  output$output <- renderUI({
    
    if(out_ind$a==1){
      
      validate(need(!is.null(Data()),"هنوز داده ای وارد نشده است"),errorClass = "Hist_l")  
      
      A <- dropdown(
        div(style="text-align:right; font-size :110%; font-weight:bold;", ":تنظیمات نمودار"),         
        
        materialSwitch(inputId = ns("add_points"),label = "اضافه کردن نفرات", 
                       status = "danger", right = TRUE,value = FALSE),
        
        materialSwitch(inputId = ns("combine"),label = "تجمیع نمودارها",
                       status = "danger", right = TRUE,value = FALSE),
        
        circle = TRUE, status = "default", icon = icon("gear"),style = "unite",width = "38%")
      
      
      B <- div(style="text-align:right",downloadBttn(ns("download"),
                                                     label = "دانلود",size = "sm"))
      
      return(list(A,B,br()))
    }
    
  })
  
  
  output$output2 <- renderUI({
    
    if(!is.null(input$combine))
      if(out_ind$a==1){
        
        B1 <- withSpinner(plotlyOutput(ns("Bx1")),type=5,color = "#006E6D",size = 0.6)
        
        if(input$combine){
          M <- list(B1)
        }else{
          B2 <- withSpinner(plotlyOutput(ns("Bx2")),type=5,color = "#006E6D",size = 0.6)
          M <- list(B1,B2)
        }
        
        return(M)
        
      }
    
  })

  
  output$Bx1 <- renderPlotly(Reac_CP2M_Bx()$gg1)
  output$Bx2 <- renderPlotly(Reac_CP2M_Bx()$gg2)
  
  
  callModule(M0_Box,"box",values,"IRANSansDN")
  
  # output$report <- downloadHandler(
  #   
  #   filename = paste0("report",".pdf"),
  #   
  #   content=function(file){ 
  #     tempReport <- file.path(tempdir(),"report.Rmd")
  #     file.copy("report/report.Rmd",tempReport,overwrite = TRUE)
  #     tempImage <- file.path(tempdir(),"Logo.png")
  #     file.copy("report/Logo.png",tempImage,overwrite = TRUE)
  #     params <- list(n = Reac_CP2M_Bx())
  #     rmarkdown::render(tempReport,output_file = file,
  #                       params = params,
  #                       envir = new.env(parent = globalenv()))})  
  
  
  
}
