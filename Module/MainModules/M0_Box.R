M0_BoxUI <- function(id,date,names){
  
  ns <- NS(id)
  
        
  tabPanel(title ="نمودار جعبه ای",
           
           fluidRow(
             
             column(1,
                    div(style="display:inline-block; width: 150px; margin-top: 15px;",
                        uiOutput(ns("Bx_SeI1")))
             ),
             column(1, offset=1,
                    div(style="display:inline-block; width: 150px; margin-top: 15px;",
                        uiOutput(ns("Bx_SeI2")))
             ),
             column(1, offset=1,
                    div(style="display:inline-block; margin-top: 40px;",
                        actionButton(inputId = ns("Bx_Ac"),label = "آنالیز"))
             )
           ),
           plotlyOutput(ns("Bx")),icon=icon("square")
  )
}







######################
#
# Server Logic
#
######################

M0_Box <- function(input,output,session,Vals){
  
  
  ns <- session$ns  
  
  Data <- reactive({
    M <- Vals[["now"]]
    rownames(M) <- Vals[["names"]]
    colnames(M) <- Vals[["dates"]]
    return(M)
  })
  
  
  output$Bx_SeI1 <- renderUI({
    selectInput(inputId = ns("Bx_SeI1"),label = "زمان ابتدا",choices = colnames(Data()),selected = tail(colnames(Data()),4)[1])
  })
  
  
  output$Bx_SeI2 <- renderUI({
    selectInput(inputId = ns("Bx_SeI2"),label = "زمان انتها",choices =  colnames(Data()),selected = tail( colnames(Data()),1))
  })
  
  #Mean <- reactive({ apply(Data(),2,mean) })
  
  melt_Data_Bx <- reactive({
    min=which(colnames(Data())==input$Bx_SeI1)
    max=which(colnames(Data())==input$Bx_SeI2)
    
    if(min < max){
      d <- as.data.frame(Data()[,min:max])
    }
    if(min==max){
      d <- as.data.frame(Data()[,max])
    }
    if(min > max){
      d <- as.data.frame(Data())
    }
    d <- melt(as.matrix(d))
    colnames(d) <- c("Student","Day","value")
    d$Day <- factor(d$Day)
    d
  })
  
  Reac_CP2M_Bx <- eventReactive(input$Bx_Ac, {
    min=which(colnames(Data())==input$Bx_SeI1)
    max=which(colnames(Data())==input$Bx_SeI2)
    if(min <=max){
      p <- ggplot(melt_Data_Bx() , aes(x=Day,y=value,fill=Day)) + geom_boxplot(outlier.size=6) +
        stat_summary(fun.y=mean, geom="point", shape=20, size=2, color="red", fill="red")+
        labs(title = "Grade", x = "Date")+
        scale_x_discrete(labels=colnames(Data())[which(colnames(Data())==input$Bx_SeI1):which(colnames(Data())==input$Bx_SeI2)])+
        theme(axis.text.x = element_text(size=10,colour="black",angle=90, hjust=1,vjust=.5)) }
    else{
      p <- ggplot(melt_Data_Bx() , aes(x=Day,y=value,fill=Day)) + geom_boxplot(outlier.size=6) +
        stat_summary(fun.y=mean, geom="point", shape=20, size=2, color="red", fill="red")+
        labs(title = "Grade", x = "Date")+
        scale_x_discrete(labels=colnames(Data()))+
        theme(axis.text.x = element_text(size=10,colour="black",angle=90, hjust=1,vjust=.5))
    }
    gg <- ggplotly(p)
    gg
  })
  
  output$Bx <- renderPlotly(Reac_CP2M_Bx())
}