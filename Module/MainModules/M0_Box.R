
M0_BoxUI <- function(id,date,names){
  
  ns <- NS(id)

  tabPanel(title =div(class="tabPanel--font-size center",
                       "روند کلاس"),
           icon=icon("archive",class="tabPanel-icon"),

fluidRow(
 
div(style="text-align:center;",
  column(width = 2, 
  br(),  
  box(width="100%",status="primary",              
   
  wellPanel(
                    div(class="input-box--general",
                        uiOutput(ns("Bx_SeI1"))),

                    br(),
                    div(class="input-box--general",
                        uiOutput(ns("Bx_SeI2"))),
                    #div(class="action-button--general action-button--mleft action-button--mtop",
                    
                    br(),
                    
                    actionBttn(inputId = ns("Bx_Ac"),style = "jelly",color = "warning",
                          label= div(class="action-button--widget","Run"))
                          #٫label = div(class="action-button--font-size",style="color:black","آنالیز"))
                                     #class="action-button--color--yellow")
           )))),

column(width = 10,
br(),  
box(status="primary",width="100%",collapsible = TRUE,collapsed = FALSE,
    uiOutput(ns("output")),     
    uiOutput(ns("output2"))       
  ))

)

)
}



######################
#
# Server Logic
#
######################

M0_Box <- function(input,output,session,Vals,font_plot){
  
  
  ns <- session$ns  
  
  ch_opt <- list(content = c("<div> </div>"))
  
  Data <- reactive({
    M <- Vals[["now"]]
    rownames(M) <- Vals[["names"]]
    colnames(M) <- Vals[["dates"]]
    return(M)
  })
  

  output$Bx_SeI1 <- renderUI({
    if(is.null(Data())) {
      ch <- ""
      ch_select <- ""
      pickerInput(inputId = ns("Bx_SeI1"),label = "First Exam",choices = ch,
                  selected =ch_select,
                  options = list(style = "btn"),
                  choicesOpt = ch_opt)
    }else{
      ch <- colnames(Data())
      ch_select <- colnames(Data())[1]
      pickerInput(inputId = ns("Bx_SeI1"),label = "First Exam",choices = ch,
                  selected =ch_select,
                  options = list(style = "btn"))
    }
  })
  
  
  output$Bx_SeI2 <- renderUI({
    if(is.null(Data())) {
      ch <- ""
      ch_select <- ""
      pickerInput(inputId = ns("Bx_SeI2"),label = "Last Exam",choices = ch,
                  selected =ch_select,
                  options = list(style = "btn"),
                  choicesOpt = ch_opt)
    }else{
      ch <- colnames(Data())
      ch_select <- tail(colnames(Data()),1)
      pickerInput(inputId = ns("Bx_SeI2"),label = "Last Exam",choices = ch,
                  selected =ch_select,
                  options = list(style = "btn"))
    }
  })
  

  Reac_CP2M_Bx1 <- eventReactive(input$Bx_Ac, {
    
    # validate(
    #   need(!is.null(Data()),"هنوز داده ای وارد نشده است"), errorClass = "Hist_l"
    # )
    
    min=which(colnames(Data())==input$Bx_SeI1)
    max=which(colnames(Data())==input$Bx_SeI2)

    validate(
      need(min <= max,"The second date should be happened after the first date"), errorClass = "Hist_l"
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
        labs(title = "Box Plot of the exams within the time", x ="",y="Scores",fill="Date")+
        scale_x_discrete(labels=colnames(Data())[vec_ind])+
        #geom_jitter(width = 0.2)+
        theme(axis.text.x = element_text(size=11,colour="black",angle=60, hjust=1,vjust=.5),
              axis.text.y = element_text(size=12,colour="black"),
              axis.title=element_text(size=14,face="bold"),
              plot.title = element_text(size=14,face="bold"),
              legend.title = element_text(size=12,face="bold"),
              text=element_text(family=font_plot))
        
      p2 <- ggplot(melt_Data_Bx , aes(x=Day,y=value,fill=Day))+
        stat_summary(fun.y=mean, geom="point", size=3)+# shape=20, size=2, color="red", fill="red")+
        stat_summary(fun.data = mean_se, geom = "errorbar")+
        labs(title = "Mean and Standard Deviation", x ="",y="Scores",fill="Date")+
        scale_x_discrete(labels=colnames(Data())[vec_ind])+
        #geom_jitter(width = 0.2)+
        theme(axis.text.x = element_text(size=11,colour="black",angle=60, hjust=1,vjust=.5),
              axis.text.y = element_text(size=12,colour="black"),
              axis.title=element_text(size=14,face="bold"),
              plot.title = element_text(size=14,face="bold"),
              legend.title = element_text(size=12,face="bold"),
              text=element_text(family=font_plot))
      
     # gg1 <- ggplotly(p1) %>% config(displaylogo = FALSE,collaborate = FALSE,
     #                              modeBarButtonsToRemove = list(
     #                                'zoom2d','pan2d','select2d','lasso2d','zoomIn2d','zoomOut2d',
     #                                'sendDataToCloud',
     #                                'autoScale2d',
     #                                'hoverClosestCartesian',
     #                                'hoverCompareCartesian'
     #                              ))
     # 
     # gg2 <- ggplotly(p2) %>% config(displaylogo = FALSE,collaborate = FALSE,
     #                                modeBarButtonsToRemove = list(
     #                                  'zoom2d','pan2d','select2d','lasso2d','zoomIn2d','zoomOut2d',
     #                                  'sendDataToCloud',
     #                                  'autoScale2d',
     #                                  'hoverClosestCartesian',
     #                                  'hoverCompareCartesian'
     #                                ))
     
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
      if(input$combine)                                                                             'hoverCompareCartesian'
    ))
    
    return(list(gg1=gg1,gg2=gg2))
    
  })
  
  
  
  
  out_ind <- reactiveValues(a=0)
  
  observeEvent(input$Bx_Ac, {
    out_ind$a = 1
  })
  

  
  

  output$output <- renderUI({
    
    if(out_ind$a==1){
      
    validate(need(!is.null(Data()),"The Data has not been imported yet!"),errorClass = "Hist_l")  
    
    A <- dropdown(
        div(style="text-align:right; font-size :110%; font-weight:bold;", "Settings : "),         

        materialSwitch(inputId = ns("add_points"),label = "Adding individuals", 
                       status = "danger", right = TRUE,value = FALSE),
        
        materialSwitch(inputId = ns("combine"),label = "Combining plots",
                       status = "danger", right = TRUE,value = FALSE),
        
        circle = TRUE, status = "default", icon = icon("gear"),style = "unite",width = "38%")
    
      
    B <- div(style="text-align:right",downloadBttn(ns("download"),
                                                   label = "Download",size = "sm"))
    
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

  
  output$download <- downloadHandler(
    filename = paste0("Class",".html"),
    content=function(file){ 
      
      tempReport <- file.path(tempdir(),"box.Rmd")
      file.copy("report/box.Rmd",tempReport,overwrite = TRUE)
      tempImage <- file.path(tempdir(),"Logo.png")
      file.copy("report/Logo.png",tempImage,overwrite = TRUE)
      params <- list(n = Reac_CP2M_Bx()$gg1,m=Reac_CP2M_Bx()$gg2)
      rmarkdown::render(tempReport,output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))
      #pdf(file,width=7,height=5) 
      #ggsave(filename = file,plot = React_DT2(),device = cairo_pdf)
      #export(p = ggplotly(React_DT2()),file = file)
      #htmlwidgets::saveWidget(widget = ggplotly(React_DT2()),file = file)
      #webshot::webshot(sprintf("file://%s", file),file = file,selector="#htmlwidget_container")
      #plotly_IMAGE(x = ggplotly(React_DT2()),out_file = file,format = "jpeg")
      #orca(ggplotly(React_DT2()),file)
      #dev.off() 
    }
  )
  
  
  
}