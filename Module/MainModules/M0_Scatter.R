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
              
              #wellPanel(

    
    wellPanel(
      uiOutput(ns("St_ChG"))
    ),
               
    

                
                
                
      
                    # dropdownButton(
                    #   label = div(class="dropdown__title","دانش آموزان"), 
                    #   status = "default", #width = "130%",
                    #   checkboxInput(ns('St_all'), 
                    #   div(class="dropdown__text",style="color: #607D8B ;",
                    #       'تمام / هیچ')),
                    #   div(class="dropdown__text",
                    #   uiOutput(ns("St_ChG"))
                    #            ))),
             
             # ),
    
            wellPanel(             
               
                # div(class="check-box--general",
                #     checkboxInput(inputId = ns("St_Mean"),
                #        label=div(class="check-box--font-size","میانگین"),value = TRUE)),
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
                
                # div(class="check-box--general",
                #     checkboxInput(inputId = ns("St_chbI"),
                #                   label = div(class="check-box--font-size","تجمیع نمودارها"),value = TRUE))
                
                
                ),

            wellPanel(      
              
                # div(class="check-box--general",
                #     radioButtons(ns('St_rb'), inline = F,
                #                  div(class="check-box--font-size","نوع تخمین"),
                #                  choices = c("خطی"="lm","غیرخطی"="loess"),selected = "loess"
                #     ))
                
                
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

   withSpinner(plotlyOutput(ns("St")),type=5,color = "#006E6D",size = 0.6)

  ))

))

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
    rownames(M) <- Vals[["names"]]
    colnames(M) <- Vals[["dates"]]
    return(M)
  })

    
  output$St_ChG <- renderUI({

    # pickerInput(inputId = ns("St_ChG"),
    #             label = '',
    #             choices = c(rownames(Data())),
    #             options = list(`style` = "btn-info"))
    dropdown(label = div(style="font-size:72%; color:black; font-weight:bold;","لیست دانش آموزان"),
    
    div(style="text-align:left;",  
    checkboxInput(ns('St_all'),
    div(style="text-align:left;,color: #607D8B;",
    'تمام / هیچ'))),
             
      div(style="text-align:left;",
      checkboxGroupInput(inputId = ns("St_ChG"), label = "", choices = c(rownames(Data())))
      ),
      style = "unite", icon = div(style="color:black;",icon("user-circle-o")),
      status = "warning", width = "150%"
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

    validate(
      need(!is.null(Data()),"هنوز داده ای وارد نشده است"),errorClass = "Hist_l"
    )
    
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
      
    }else{
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
        facet_wrap( ~ Student,as.table = FALSE, scales = "free_x")+
        # theme(axis.line = element_line(colour = "darkblue", size = 1, linetype = "solid"),
        #       axis.text.x  = element_text(face="bold",angle=45, vjust=0.5, size=5)) +
        theme(axis.text.x = element_text(size=x_size,colour="black",angle=x_angle, hjust=1,vjust=.5),
              axis.text.y = element_text(size=12,colour="black"),
              axis.title=element_text(size=14,face="bold"),
              plot.title = element_text(size=14,face="bold"),
              legend.title = element_text(size=12,face="bold"),
              text=element_text(family="dastnevis"))+
        labs(title="روند دانش آموزان در طول زمان",
             color="دانش آموزان") +
        scale_x_discrete(name ="", limits=colnames(Data())) +
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
              text=element_text(family="dastnevis"))+
        labs(title="روند دانش آموزان در طول زمان",
             color="دانش آموزان") +
        scale_x_discrete(name ="", limits=colnames(Data())) +
        #annotate('text',x = 9,y = 18,label= text())+
        #geom_text(aes(color=Student),position = position_dodge(width = 1),label = text(), parse = TRUE)+
        xlab("زمان") + ylab("نمره")
    }


    gg <- ggplotly(p)
    gg

  })

  output$St <- renderPlotly(Reac_CP2_Sc())

}
