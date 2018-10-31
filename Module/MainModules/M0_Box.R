
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
                          label= div(class="action-button--widget","در طول زمان"))
                          #٫label = div(class="action-button--font-size",style="color:black","آنالیز"))
                                     #class="action-button--color--yellow")
           )))),

column(width = 10,
br(),  
box(status="primary",width="100%",collapsible = TRUE,collapsed = FALSE,

           withSpinner(plotlyOutput(ns("Bx")),type=5,color = "#006E6D",size = 0.6)
  ))

))
}



######################
#
# Server Logic
#
######################

M0_Box <- function(input,output,session,Vals){
  
  
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
      pickerInput(inputId = ns("Bx_SeI1"),label = "زمان ابتدا",choices = ch,
                  selected =ch_select,
                  options = list(style = "btn"),
                  choicesOpt = ch_opt)
    }else{
      ch <- colnames(Data())
      ch_select <- colnames(Data())[1]
      pickerInput(inputId = ns("Bx_SeI1"),label = "زمان ابتدا",choices = ch,
                  selected =ch_select,
                  options = list(style = "btn"))
    }
  })
  
  
  output$Bx_SeI2 <- renderUI({
    if(is.null(Data())) {
      ch <- ""
      ch_select <- ""
      pickerInput(inputId = ns("Bx_SeI2"),label = "زمان انتها",choices = ch,
                  selected =ch_select,
                  options = list(style = "btn"),
                  choicesOpt = ch_opt)
    }else{
      ch <- colnames(Data())
      ch_select <- tail(colnames(Data()),1)
      pickerInput(inputId = ns("Bx_SeI2"),label = "زمان انتها",choices = ch,
                  selected =ch_select,
                  options = list(style = "btn"))
    }
  })
  

  Reac_CP2M_Bx <- eventReactive(input$Bx_Ac, {
    
    validate(
      need(!is.null(Data()),"هنوز داده ای وارد نشده است"), errorClass = "Hist_l"
    )
    
    min=which(colnames(Data())==input$Bx_SeI1)
    max=which(colnames(Data())==input$Bx_SeI2)

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


      p <- ggplot(melt_Data_Bx , aes(x=Day,y=value,fill=Day)) + geom_boxplot(outlier.size=6) +
        stat_summary(fun.y=mean, geom="point", shape=20, size=2, color="red", fill="red")+
        labs(title = "روند کلاس در طول زمان", x ="",y="نمره",fill="تاریخ")+
        scale_x_discrete(labels=colnames(Data())[vec_ind])+
        theme(axis.text.x = element_text(size=11,colour="black",angle=60, hjust=1,vjust=.5),
              axis.text.y = element_text(size=12,colour="black"),
              axis.title=element_text(size=14,face="bold"),
              plot.title = element_text(size=14,face="bold"),
              legend.title = element_text(size=12,face="bold"),
              text=element_text(family="dastnevis"))
        

     gg <- ggplotly(p)
     gg
  })
  
  output$Bx <- renderPlotly(Reac_CP2M_Bx())
}