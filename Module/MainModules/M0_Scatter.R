M0_ScatterUI <- function(id){

   ns <- NS(id)

  tabPanel(title = div(class="tabPanel--font-size center","دانش آموزان"),
           icon=icon("line-chart",class="tabPanel-icon"),

### For Error Message
           tags$head(
             tags$style(HTML("
                             .shiny-output-error-validation {
                             color: red;
                             font-size: 16%;
                             margin-top: 10%;
                             margin-left: 10%;
                             font-weight:bold;
                             }
                             "))
             ),
###


            fluidRow(

             column(1,
                    div(class="dropdown--margin-top",
                    dropdownButton(
                      label = div(class="dropdown__title","دانش آموزان مورد نظر را انتخاب کنید"), 
                      status = "default", width = "130%",
                          checkboxInput(ns('St_all'), 
                           div(class="dropdown__text",style="color:red;",'تمام / هیچ')),
                      div(class="dropdown__text",
                      uiOutput(ns("St_ChG"))
                               )))

             ),
             
             
             column(1,offset = 2,
                    div(class="action-button--mtop__scatter action-button--mleft__scatter",
                        actionButton(inputId = ns("St_Ac"),
                                     label = div(class="action-button--font-size","آنالیز"),
                                     class="action-button--color--yellow",
                                     width="180%")
                        )
             ),
             
             column(1,offset=1,
                    div(class="check-box--general check-box--mleft check-box--mtop",
                        checkboxInput(ns('St_Mean'),
                                      div(class="check-box--font-size","میانگین")))
             ),

             column(1,offset=1,
                    div(class="check-box--general check-box--mleft check-box--mtop",
                        checkboxInput(inputId = ns("St_chbI"),
                          label =  div(class="check-box--font-size","تجمیع نمودارها"),
                           value = FALSE))
             ),

             column(1,offset=1,
                    div(class="check-box--general check-box--mtop radio-button--mleft",
                        radioButtons(ns('St_rb'), inline = FALSE,
                                 div(class="check-box--font-size","نوع تخمین"),
                       choices = c("خطی"="lm","غیرخطی"="loess")))
             )
             


           ),



           plotlyOutput(ns("St"))
  )
}







######################
#
# Server Logic
#
######################

M0_Scatter <- function(input,output,session,Vals){

  ns <- session$ns



  
    Data <- reactive({

      
    M <- Vals[["now"]]
    # print(class(M[1,1]))
    # print(M[1,1])
    # print(persian.tonumber(M[1,1]))
    # M[1,1] <- persian.tonumber(M[1,1])
    #print("########### Scat")
    rownames(M) <- Vals[["names"]]
    colnames(M) <- Vals[["dates"]]
    return(M)
  })

  output$St_ChG <- renderUI({
    checkboxGroupInput(inputId = ns("St_ChG"), label = "", choices = c(rownames(Data())))
  })


  Mean <- reactive({ apply(Data(),2,mean) })

  observe({
    updateCheckboxGroupInput(
      session, 'St_ChG', choices = rownames(Data()),
      selected = if (input$St_all) rownames(Data())
    )
  })

  melt_Data_St <- reactive({

    validate(
      need((input$St_Mean==TRUE)||!is.null(input$St_ChG),"حداقل باید یک نمودار را انتخاب کنید")
    )

    if(input$St_Mean==TRUE){
      if(is.null(input$St_ChG)==TRUE){
        d <- t(as.matrix(Mean()))
        rownames(d) <- "میانگین"
      }else{
        d <- as.data.frame(Data()[which(rownames(Data()) %in% input$St_ChG),,drop=FALSE])
        d <- rbind(d,Mean())
        rownames(d) <- c(rownames(d)[-length(rownames(d))],"میانگین")
      }

      colnames(d) <- 1:dim(d)[2]

    }else{
      d <- as.data.frame(Data()[which(rownames(Data()) %in% input$St_ChG),,drop=FALSE])
      colnames(d) <- 1:dim(d)[2]
    }

    d <- melt(as.matrix(d))
    colnames(d) <- c("Student","Day","value")
    print(d)
    return(d)
  })



  Reac_CP2_Sc <- eventReactive(input$St_Ac, {

    m <- reactive({lm(value ~ Day, melt_Data_St())})

    text <- reactive({ coef(m())[1] })

    if(input$St_chbI == FALSE){
      p <- ggplot(melt_Data_St(), aes(Day, value)) + geom_point(aes(color = Student)) +
        stat_smooth(aes(color = Student),method = input$St_rb) +
        facet_wrap( ~ Student,as.table = FALSE, scales = "free_x")+
        theme(axis.line = element_line(colour = "darkblue", size = 1, linetype = "solid"),
              axis.text.x  = element_text(face="bold",angle=45, vjust=0.5, size=5)) +
        labs(title="تحلیل زمانی دانش آموزان",color="دانش آموزان") +
        scale_x_discrete(name ="تاریخ امتحان", limits=colnames(Data())) +
        annotate('text',x = 9,y = 18,label= text())+
        xlab("زمان") + ylab("نمره")
    }
    else{
      p <- ggplot(melt_Data_St(), aes(Day, value)) + geom_point(aes(color = Student)) +
        stat_smooth(aes(color = Student),method = input$St_rb,se=FALSE) +
        theme(axis.line = element_line(colour = "darkblue", size = 1, linetype = "solid"),
              axis.text.x  = element_text(face="bold",angle=45, vjust=0.5, size=5)) +
        labs(title="تحلیل زمانی دانش آموزان",color="دانش آموزان") +
        scale_x_discrete(name ="تاریخ امتحان", limits=colnames(Data())) +
        annotate('text',x = 9,y = 18,label= text())+
        #geom_text(aes(color=Student),position = position_dodge(width = 1),label = text(), parse = TRUE)+
        xlab("زمان") + ylab("نمره")
    }


    gg <- ggplotly(p)
    gg

  })

  output$St <- renderPlotly(Reac_CP2_Sc())

}
