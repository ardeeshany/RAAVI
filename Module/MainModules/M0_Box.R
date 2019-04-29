
M0_BoxUI <- function(id,date,names){
  
  ns <- NS(id)
  
  # tabPanel(title =div(class="tabPanel--font-size center",
  #                      "روند کلاس"),
  #          icon=icon("archive",class="tabPanel-icon"),

#fluidRow(
 
div(style="text-align:center;",
#  column(width = 8, 
  br(),  
  box(width="100%",status="primary",              
   
  wellPanel(
                    # div(class="input-box--general",
                    #     uiOutput(ns("Bx_SeI1"))),
                    # 
                    # br(),
                    # div(class="input-box--general",
                    #     uiOutput(ns("Bx_SeI2"))),
                    #div(class="action-button--general action-button--mleft action-button--mtop",
                    
                    div(style="text-align:center",
                    actionBttn(inputId = ns("Bx_Ac"),style = "jelly",color = "warning",
                               label= div(class="action-button--widget","چهاردهم"))),
                    
                    br(),
                    uiOutput(ns("output"))
  )),


#column(width = 4,
br(),
box(status="primary",width="100%",collapsible = TRUE,collapsed = FALSE,
    uiOutput(ns("output2"))
  )
#)

)}



######################
#
# Server Logic
#
######################

M0_Box <- function(input,output,session,Vals,format_out,font_plot){
  
  ns <- session$ns  
  ch_opt <- list(content = c("<div> </div>"))
  
  persian <- "\u0660\u0661\u0662\u0663\u0664\u0665\u0666\u0667\u0668\u0669\u06F0\u06F1\u06F2\u06F3\u06F4\u06F5\u06F6\u06F7\u06F8\u06F9"
  english <- "01234567890123456789"
  persian.tonumber <- function(s) as.character(chartr(persian,english,s))
  
  Data <- reactive({
    M <- Vals[["now"]]
    rownames(M) <- Vals[["names"]]
    colnames(M) <- Vals[["dates"]]
    return(M)
  })
  

  # output$Bx_SeI1 <- renderUI({
  #   if(is.null(Data())) {
  #     ch <- ""
  #     ch_select <- ""
  #     pickerInput(inputId = ns("Bx_SeI1"),label = "زمان ابتدا",choices = ch,
  #                 selected =ch_select,
  #                 options = list(style = "btn"),
  #                 choicesOpt = ch_opt)
  #   }else{
  #     ch <- colnames(Data())
  #     ch_select <- colnames(Data())[1]
  #     pickerInput(inputId = ns("Bx_SeI1"),label = "زمان ابتدا",choices = ch,
  #                 selected =ch_select,
  #                 options = list(style = "btn"))
  #   }
  # })
  # 
  # 
  # output$Bx_SeI2 <- renderUI({
  #   if(is.null(Data())) {
  #     ch <- ""
  #     ch_select <- ""
  #     pickerInput(inputId = ns("Bx_SeI2"),label = "زمان انتها",choices = ch,
  #                 selected =ch_select,
  #                 options = list(style = "btn"),
  #                 choicesOpt = ch_opt)
  #   }else{
  #     ch <- colnames(Data())
  #     ch_select <- tail(colnames(Data()),1)
  #     pickerInput(inputId = ns("Bx_SeI2"),label = "زمان انتها",choices = ch,
  #                 selected =ch_select,
  #                 options = list(style = "btn"))
  #   }
  # })
  

  Reac_CP2M_Bx1 <- eventReactive(input$Bx_Ac, {
    
    # validate(
    #   need(!is.null(Data()),"هنوز داده ای وارد نشده است"), errorClass = "Hist_l"
    # )
    
    # min=which(colnames(Data())==input$Bx_SeI1)
    # max=which(colnames(Data())==input$Bx_SeI2)
    
    showtext_auto()
    min = 1
    max = dim(Data())[2]
    
    
    
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
        # stat_summary(fun.y=mean, colour="darkred", geom="point")+# shape=20, size=2, color="red", fill="red")+
        # stat_summary(fun.data = mean_se, geom = "errorbar")+
        labs(title = "روند کلاس در طول زمان", x ="",y="نمره",fill="تاریخ")+
        scale_x_discrete(labels=colnames(Data())[vec_ind])+
        #geom_jitter(width = 0.2)+
        theme(axis.text.x = element_text(size=11,colour="black",angle=60, hjust=1,vjust=.5),
              axis.text.y = element_text(size=12,colour="black"),
              axis.title=element_text(size=14,face="bold"),
              plot.title = element_text(size=14,face="bold"),
              legend.title = element_text(size=12,face="bold"),
              text=element_text(family=font_plot))
        
      p2 <- ggplot(melt_Data_Bx , aes(x=Day,y=value,fill=Day))+
        stat_summary(fun.y=mean, geom="point", size=3)+ # shape=20, size=2, color="red", fill="red")+
        stat_summary(fun.data = mean_se, geom = "errorbar")+
        labs(title = "میانگین و میزان انحراف از آن", x ="",y="نمره",fill="تاریخ")+
        scale_x_discrete(labels=colnames(Data())[vec_ind])+
        #geom_jitter(width = 0.2)+
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
  
  
  
  output$output <- renderUI({
    
    if(out_ind$a==1){
      
      validate(need(!is.null(Data()),"هنوز داده ای وارد نشده است"),errorClass = "Hist_l")  
      
      A <- dropdown(
        div(style="text-align:right; font-size :80%; font-weight:bold;", ":تنظیمات نمودار"),         
        
        div(style="text-align:left; font-size :80%;",
        materialSwitch(inputId = ns("add_points"),label = "نفرات", width = "60%",
                       status = "danger", right = TRUE,value = FALSE),
        
        materialSwitch(inputId = ns("combine"),label = "تجمیع",width = "60%",
                       status = "danger", right = TRUE,value = FALSE)),
        
        circle = TRUE, status = "default", icon = icon("gear"),style = "unite",width = "200%")
      
      B <- div(style="text-align:center",downloadBttn(ns("download"),
                                                     label = "گزارش",size = "sm"))
      
      return(list(A,br(),B))
    }
    
  })
  

  out_ind <- reactiveValues(a=0)
  
  observeEvent(input$Bx_Ac, {
    out_ind$a = 1
  })
  
  observeEvent(Data(),{
    out_ind$a = 0
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

  output$download <- downloadHandler(
    

    filename = function(){
      paste("نمودار", sep = '.', switch(format_out(),HTML = 'html', PDF = 'pdf', Word = 'docx'))
      # paste("salam.pdf")
      },
    content=function(file){
      withProgress(message = "... گزارش در حال ساخته شدن است",
                   min = 0,max = 100,value = 72, {
                     # tempReport <- file.path(tempdir(),"box.Rmd")
                     # file.copy("report/box.Rmd",tempReport,overwrite = TRUE)
      params <- list(n = Reac_CP2M_Bx()$gg1,m=Reac_CP2M_Bx()$gg2,mainfont=font_plot)
      rmarkdown::render("report/box.Rmd",output_format = switch(format_out(),PDF = pdf_document(), HTML = html_document(), Word = word_document()),
      #rmarkdown::render(tempReport,output_format = switch(format_out(),PDF = pdf_document(), HTML = html_document(), Word = word_document()),
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))
      })})
  
  
}