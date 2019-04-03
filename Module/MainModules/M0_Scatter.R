M0_ScatterUI <- function(id){

   ns <- NS(id)

  tabPanel(title = div(class="tabPanel--font-size center",
                       "روند دانش آموزان"),
           icon=icon("line-chart",class="tabPanel-icon"),
###

fluidRow(

div(style="text-align:center;",
column(width = 2, 
br(),  
box(width="100%",status="primary",  


    wellPanel(uiOutput(ns("St_ChG"))),
               
    wellPanel(             
               
           div(style="text-align:left;",
                prettyToggle(shape = "round",value = T,
                  inputId = ns("St_Mean"),
                  label_on = "میانگین", 
                  icon_on = icon("check"),
                  status_on = "primary",
                  status_off = "default", 
                  label_off = "میانگین"
                )),
                
           div(style="text-align:left;",
                prettyToggle(shape = "round",value = T,
                  inputId = ns("St_chbI"),
                  label_on = "تجمیع نمودارها", 
                  icon_on = icon("check"),
                  status_on = "primary",
                  status_off = "default", 
                  label_off = "تجمیع نمودارها"
                ))

                ),

            wellPanel(      
              
               
               div(style="text-align:left;",
                prettyRadioButtons(
                  inputId = ns('St_rb'),inline = T,
                  label = div(class="check-box--font-size","نوع تخمین"),
                  choices = c("خطی"="lm","غیرخطی"="loess"),
                  selected = "loess"
                ))
                
                
                
                ),
                
            wellPanel(      
                        actionBttn(inputId = ns("St_Ac"),style = "jelly",color = "warning",
                                     label = div(class="action-button--widget","در طول زمان"))
                                   
            )
                        



))),

column(width = 10,
br(),  
box(status="primary",width="100%",collapsible = TRUE,collapsed = FALSE,
    uiOutput(ns("output"))
      ))
))

}







######################
#
# Server Logic
#
######################

M0_Scatter <- function(input,output,session,Vals,font_plot){

  ns <- session$ns
  
    Data <- reactive({
    M <- Vals[["now"]]
    rownames(M) <- Vals[["names"]]
    colnames(M) <- Vals[["dates"]]
    return(M)
  })

    
  output$St_ChG <- renderUI({

    dropdown(label = div(style="font-size:72%; color:black; font-weight:bold;","لیست دانش آموزان"),
    
    div(style="text-align:left;",  
    checkboxInput(ns('St_all'),
    div(style="text-align:left;,color: #607D8B;",
    'تمام / هیچ'))),
             
      div(style="text-align:left;",
      checkboxGroupInput(inputId = ns("St_ChG"), label = "", choices = c(rownames(Data())))
      ),
      style = "unite", icon = div(style="color:black;",icon("user-circle-o")),
      status = "warning", width = "168%"
       # animate = animateOptions(
       #   enter = animations$fading_entrances$fadeInLeftBig,
       #   exit = animations$fading_exits$fadeOutRightBig
       # )
    )
    })

  observeEvent(input$St_all,{
    updateCheckboxGroupInput(
      session, 'St_ChG', choices = rownames(Data()),
      selected = if (input$St_all) rownames(Data())
    )
  })


  Reac_CP2_Sc <- eventReactive(input$St_Ac, {
    
    # validate(
    #   need(!is.null(Data()),"هنوز داده ای وارد نشده است"),errorClass = "Hist_l"
    # )
    
    validate(
      need((input$St_Mean==TRUE)||!is.null(input$St_ChG),
           "حداقل باید یک نمودار را انتخاب کنید"),errorClass = "scatter_min")
    
    Mean <- apply(Data(),2,mean)
    
    if(input$St_Mean==TRUE){
      if(is.null(input$St_ChG)==TRUE){
        d <- t(Mean)
        rownames(d) <- "میانگین"
      }else{
        d <- as.data.frame(Data()[which(rownames(Data()) %in% input$St_ChG),,drop=FALSE])
        d <- rbind(d,Mean)
        rownames(d) <- c(rownames(d)[-length(rownames(d))],"میانگین")
      }
      
      colnames(d) <- 1:dim(d)[2]
      
    }
    else{
      d <- as.data.frame(Data()[which(rownames(Data()) %in% input$St_ChG),,drop=FALSE])
      colnames(d) <- 1:dim(d)[2]
    }
    
    melt_Data_St <- melt(as.matrix(d))
    colnames(melt_Data_St) <- c("Student","Day","value")
    
    m <- lm(value ~ Day, melt_Data_St)

    text <- coef(m)[1]

    if(input$St_chbI == FALSE){
      
      if(length(unique(melt_Data_St[,1]))==1){
        x_angle = 60
        x_size = 11
      }else{
        x_angle = 90
        x_size = 8
      }
      
      p <- ggplot(melt_Data_St, aes(Day, value)) + geom_point(aes(color = Student)) +
        stat_smooth(aes(color = Student),method = input$St_rb) +
        #facet_wrap( ~ Student,as.table = FALSE,ncol=3)+
        # theme(axis.line = element_line(colour = "darkblue", size = 1, linetype = "solid"),
        #       axis.text.x  = element_text(face="bold",angle=45, vjust=0.5, size=5)) +
        theme(axis.text.x = element_text(size=x_size,colour="black",angle=x_angle, hjust=1,vjust=.5),
              axis.text.y = element_text(size=12,colour="black"),
              axis.title=element_text(size=14,face="bold"),
              plot.title = element_text(size=14,face="bold"),
              legend.title = element_text(size=12,face="bold"),
              text=element_text(family=font_plot))+
        labs(title="روند دانش آموزان در طول زمان",
             color="دانش آموزان") +
        #scale_x_discrete(name ="", limits=colnames(Data())) +
        #annotate('text',x = 9,y = 18,label= text())+
        xlab("زمان") + ylab("نمره")
    }
    else{

      p <- ggplot(melt_Data_St, aes(Day, value)) + geom_point(aes(color = Student)) +
        stat_smooth(aes(color = Student),method = input$St_rb,se=FALSE) +
        # theme(axis.line = element_line(colour = "darkblue", size = 1, linetype = "solid"),
        #       axis.text.x  = element_text(face="bold",angle=45, vjust=0.5, size=5)) +
        theme(axis.text.x = element_text(size=11,colour="black",angle=60, hjust=1,vjust=.5),
              axis.text.y = element_text(size=12,colour="black"),
              axis.title=element_text(size=14,face="bold"),
              plot.title = element_text(size=14,face="bold"),
              legend.title = element_text(size=12,face="bold"),
              text=element_text(family=font_plot))+
        labs(title="روند دانش آموزان در طول زمان",
             color="دانش آموزان") +
        #scale_x_discrete(name ="", limits=colnames(Data())) +
        #annotate('text',x = 9,y = 18,label= text())+
        #geom_text(aes(color=Student),position = position_dodge(width = 1),label = text(), parse = TRUE)+
        xlab("زمان") + ylab("نمره")
    }

    return(list(p=p,melt=melt_Data_St))
    #gg <- ggplotly(p)
    #gg

  })
  
  f <- list(
    #family = "Courier New, monospace",
    size = 16,
    weight = 'bold',
    color = "black")
  
  text_scatter <- list(

    text = "روند دانش آموزان در طول زمان",

    font=f,
    xref = "paper",
    yref = "paper",
    yanchor = "bottom",
    xanchor = "bottom",
    align = "center",
    x = 0.5,
    y = 1,
    showarrow = FALSE
  )
  
  Reac_out <- reactive({
    
  
    colnum <- input$number_col
    height <- input$height
    #width <- input$width
    
    if(isolate(!input$St_chbI)){
      if(input$add_date){
      p <- Reac_CP2_Sc()$p + facet_wrap( ~ Student,as.table = FALSE,ncol=colnum,scales = "free",switch = "both")+
        scale_x_discrete(name ="", limits=colnames(Data()))
      
      A <- ggplotly(p)   %>% 
        layout(height = height, autosize=TRUE) %>% config(displaylogo = FALSE,collaborate = FALSE,
                                                          modeBarButtonsToRemove = list(
                                                            'zoom2d','pan2d','select2d','lasso2d','zoomIn2d','zoomOut2d',
                                                            'sendDataToCloud',
                                                            'autoScale2d',
                                                            'hoverClosestCartesian',
                                                            'hoverCompareCartesian'
                                                          )) #,annotations = text_scatter)

      
    }else{
      p <- Reac_CP2_Sc()$p + facet_wrap( ~ Student,as.table = FALSE,ncol=colnum,scales = "free",switch = "both")+ #,ncol=colnum,)+
        scale_x_discrete(name ="", limits=1:ncol(Data()))
    
      A <- ggplotly(p)  %>% 

        layout(height = height,autosize=TRUE) %>% config(displaylogo = FALSE,collaborate = FALSE,
                                                         modeBarButtonsToRemove = list(
                                                           'zoom2d','pan2d','select2d','lasso2d','zoomIn2d','zoomOut2d',
                                                           'sendDataToCloud',
                                                           'autoScale2d',
                                                           'hoverClosestCartesian',
                                                           'hoverCompareCartesian'
                                                         )) #,annotations = text_scatter)
        

    }}else{
      p <- Reac_CP2_Sc()$p+
        scale_x_discrete(name ="", limits=colnames(Data()))
      
      #A <- ggplotly(p) %>% layout(annotations = text_scatter)
      
      A <- ggplotly(p)  %>% 

        layout(height = height, autosize=TRUE) %>% config(displaylogo = FALSE,collaborate = FALSE,
                                                          modeBarButtonsToRemove = list(
                                                            'zoom2d','pan2d','select2d','lasso2d','zoomIn2d','zoomOut2d',
                                                            'sendDataToCloud',
                                                            'autoScale2d',
                                                            'hoverClosestCartesian',
                                                            'hoverCompareCartesian'
                                                          )) #,annotations = text_scatter)

  
    }

    return(A)
    
  })
  
  
  
  out_ind <- reactiveValues(a=0)
  
  observeEvent(input$St_Ac, {
    out_ind$a = 1
  })
  
  # observeEvent(input$St_Ac,{
  #   # pdf("plot.pdf")
  #   # Reac_out()
  #   # dev.off()
  #   if(!input$St_chbI){
  #   #dir <- file.path(getwd(),"/www")
  #   #cairo_pdf("www/plot2.pdf",family = font_plot)
  #   #Reac_out()
  #   #dev.off()
  #   #ggsave(plot =Reac_out(),filename = "www/plot2.pdf", device = cairo_pdf)
  #   # cairo_pdf(filename = "www/plot.pdf")
  #   # #pdf("www/plot.pdf")
  #   # Reac_out()
  #   # dev.off()
  #     }
  # })
  
  output$St <- renderPlotly(Reac_out())
  
  output$output <- renderUI({

    if(out_ind$a==1){
    
      validate(need(!is.null(Data()),"هنوز داده ای وارد نشده است"),errorClass = "Hist_l")
      
      A <- dropdown(
        div(style="text-align:right; font-size :110%; font-weight:bold;", ":تنظیمات نمودار"),         
        div(style="text-align:left",
              noUiSliderInput(ns("height"),label = "ارتفاع نمودار",     
                      inline = T, min = 50,max = 2000,value = 450,step = 50,tooltips = F,
                      width = "100%",color = "#578CA9")),

      materialSwitch(inputId = ns("add_date"),label = "تاریخ", 
                 status = "danger", right = TRUE,value = FALSE),
      numericInput(ns("number_col"),min = 1,max = 20,value = 3,label = "تعداد ستون ها",width = "100%"),
      circle = TRUE, status = "default", icon = icon("gear"),style = "unite",width = "38%")
      
      
      B <- div(style="text-align:right",downloadBttn(ns("download"),
                                                     label = "دانلود",size = "sm"))
      
       #if(isolate(input$St_chbI)){
       C <- withSpinner(plotlyOutput(ns("St"),height = isolate(input$height)),type=5,color = "#006E6D",size = 0.6)
       #else{
       #C <- tags$iframe(src="plot2.pdf")}   #style="height:600px; width:100%"   
       #C <- withSpinner(plotOutput(ns("St"),height = input$ratio),type=5,color = "#006E6D",size = 0.6)}
      #C <- plotlyOutput(ns("St"),height = input$ratio)
      
      M <- list(A,B,br(),C)
      
      return(M)
    }
    })

  
  output$download <- downloadHandler(
    filename = paste0("روند دانش آموزان",".html"),
    content=function(file){ 
      
      tempReport <- file.path(tempdir(),"scatter.Rmd")
      file.copy("report/scatter.Rmd",tempReport,overwrite = TRUE)
      tempImage <- file.path(tempdir(),"logogrey.svg")
      file.copy("report/logogrey.svg",tempImage,overwrite = TRUE)
      params <- list(n = Reac_out())
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
