M0_ProgUI <- function(id){
  
  ns <- NS(id)
  
  tabPanel(title = "پیشرفت", icon=icon("arrow-circle-up"),
           
           ### For Error Message          
           tags$head(
             tags$style(HTML("
                             .shiny-output-error-validation {
                             color: red;
                             font-size: 16px;
                             margin-top: 10px;
                             margin-left: 10px;
                             }
                             "))
             ),
           ###
           
           
           fluidRow(                   
             
             
             column(2, offset=1,
                    div(style="display:inline-block; margin-top:40px;",
                        actionButton(inputId = ns("Pr_AC1"),label = "پیشرفت دانش آموزان"))
             ),  
             
             #######################       
             column(1,
                    div(style="display:inline-block; margin-top: 33px; margin-left: 530px",
                        uiOutput(ns("Pr_numI"))),
                    div(style="margin-top: 9.5px;margin-left: 545px",
                        actionButton(inputId = ns("Pr_AC2"),label = "پیشرفت زیرگروه ها",width = "150px",icon=icon("arrow-right")))
             ),
             
             column(1,
                    div(style="display:inline-block; margin-top: 33px;margin-left: 540px",
                        numericInput(ns("Pr_bin2"),label = "تعداد گروه",min = 1,value = 1,width = "85px"))
             )
             
           ),
           
           
           plotlyOutput(ns("Pr"))
           
             )
  }







######################
#
# Server Logic
#
######################

M0_Prog <- function(input,output,session,Vals){
  
  
  ns <- session$ns  
  
  Data <- reactive({
    M <- Vals[["now"]]
    rownames(M) <- Vals[["names"]]
    colnames(M) <- Vals[["dates"]]
    return(M)
  })
  
  
   output$Pr_numI <- renderUI({
    numericInput(ns("Pr_numI"),label = "میانگین وزنی",min = 1,max=length(colnames(Data())),value = 1,width = "85px")
  })
  
  ## Variable (like trigger) for selecting which React_DT want to show in output
  var = reactiveValues(a = TRUE)
  
  observeEvent(input$Pr_AC1, {
    var$a = TRUE
  })
  
  observeEvent(input$Pr_AC2, {
    var$a = FALSE
  })
  ##

 
  
  ##  
  
  
  React_Pr1 <- eventReactive(input$Pr_AC1, {
    
    Mean <- apply(Data(),2,mean)
    
    Data_tot1 <- rbind(Data(),Mean)
    rownames(Data_tot1) <- c(rownames(Data()),"میانگین")
    fit_tot1 <- rep(list("NA"),dim(Data_tot1)[1])
    slope1 <- rep(NA,dim(Data_tot1)[1])
    colnames(Data_tot1) <- 1:dim(Data_tot1)[2]
    for(i in 1:dim(Data_tot1)[1]){
      d1 <- Data_tot1[i,]
      d1 <- melt(as.matrix(d1))
      colnames(d1) <- c("Student","Day","value")
      fit_tot1[[i]] <- lm(value~Day, data=d1)
      slope1[i] <- coef(fit_tot1[[i]])[2]
    }
    slope1 <- as.data.frame(slope1)
    slope1$names <- rownames(Data_tot1)
    slope1$clr <- "green"
    colnames(slope1) <- c("sl","names","clr")
    slope1[which(slope1$sl>0),3] <- "red" 
    
    slope1$ann <- "black"
    slope1$ann[dim(slope1)[1]] <- "violetred"
    colnames(slope1) <- c("sl","names","clr","ann")
    slope1 <- slope1[order(slope1$sl,decreasing = T),]
    
    
    s1 <- ggplot(slope1,aes(x = reorder(names,sl),y = sl))+
      geom_bar(stat="identity",aes(fill = factor(clr, labels = c("پسرفت","پیشرفت"))),color="black")+
      labs(fill="")+
      geom_text(data=slope1,aes(x = names,y = sl,label=round(sl,3)),vjust=0)+
      ylab("شیب پیشرفت خطی") + xlab("")
      coord_flip()
    
    
    
    gg1 <- ggplotly(s1)
    gg1
    
    })
  
  #########
  
  React_Pr2 <-eventReactive(input$Pr_AC2, {
    
    ## For input$Pr_bin2==1  
    # Mean <- apply(Data(),2,mean)
    # Mean2 <- Mean
    # Mean2 <- melt(as.matrix(Mean2))[,-2]
    # colnames(Mean2) <- c("Day","value")
    # Mean2$Day <- 1:length(colnames(Data()))
    # fit_tot2 <- lm(value~Day, data=Mean2)
    # slope2 <- coef(fit_tot2)[2]
    # 
    # slope2 <- as.data.frame(slope2)
    # slope2$names <- 1
    # slope2$clr <- "green"
    # colnames(slope2) <- c("sl","names","clr")
    
    
    # if(slope2$sl>=0){
    #   leg.lab="پیشرفت"
    #   leg.col="#00BFC4"
    # }
    # else{
    #   leg.lab="پسرفت" 
    #   leg.col="#F8766D"
    # }
    # s2 <-ggplot(slope2,aes(x=names, y = sl))+
    #   geom_bar(stat="identity",fill=leg.col,color="black")+
    #   labs(fill="")+  # legend title
    #   geom_text(data=slope2,aes(x = names,y = sl,label=round(sl,3)),vjust=0)+
    #   ylab("شیب پیشرفت خطی") + xlab("")+
    #   theme(axis.text.x=element_text(face = "bold.italic", color = "blue", size = 8),
    #         axis.text.y=element_text(colour="blue", size=10, face="bold"))
      #coord_flip()  
    
    
    # generated weights   
    if(input$Pr_numI==1){
      gr <- rep(1,dim(Data())[2])
      }
    else{
      gr <- as.numeric(cut(1:dim(Data())[2],breaks = input$Pr_numI,labels = 1:input$Pr_numI))
      }
    #   
    d <- as.data.frame(apply(Data(),1,function(x){weighted.mean(x,gr)}))
    d$names <- rownames(Data())
    colnames(d) <- c("mean.w","names")
    d <- d[order(d$mean.w,decreasing = T),]
    
    # if(input$Pr_bin2==1){
    #   gg <- ggplotly(s2)
    #   #gg<-s2
    # }
    # else{
      names_ch <- rep(list("NA"),input$Pr_bin2)
      Data_T <- rep(list("NA"),input$Pr_bin2)
      ind <- split(1:dim(d)[1], ceiling(seq_along(1:dim(d)[1])/(dim(d)[1]/input$Pr_bin2)))
      
      for(i in 1:input$Pr_bin2){
        names_ch[[i]] <- d$names[ind[[i]]]
        Data_T[[i]] <- Data()[rownames(Data()) %in% names_ch[[i]],]
      }
      
      
      fit_tot <- rep(list("NA"),length(Data_T))
      slope <- rep(NA,length(Data_T))  
      
      for(i in 1:length(Data_T)){  
        Mean <- apply(Data_T[[i]],2,mean)
        Mean <- melt(as.matrix(Mean))[,-2]
        colnames(Mean) <- c("Day","value")
        Mean$Day <- 1:length(colnames(Data()))
        fit_tot[[i]] <- lm(value~Day, data=Mean)
        slope[i] <- coef(fit_tot[[i]])[2]
      }
      
      slope <- as.data.frame(slope)
      slope$names <- 1:input$Pr_bin2
      slope$clr <- "green"
      colnames(slope) <- c("sl","names","clr")
      slope[which(slope$sl>0),3] <- "red"
      slope <- slope[dim(slope)[1]:1,] 
      
      
      ylab_names <- rep(list("NA"),input$Pr_bin2)
      
      for(i in 1:input$Pr_bin2){
        ylab_names[[i]] <- paste(names_ch[[i]],sep = "\n")
      }
    
     
      if(input$Pr_bin2==1){
       if(slope$sl>=0){
         lab="پیشرفت"
         col="#00BFC4"
       }
       else{
         lab="پسرفت"
         col="#F8766D"
       }
      }else{
        lab <- c("پسرفت","پیشرفت")
        col <- c("#F8766D","#00BFC4")
      }
       
      
   s <- ggplot(slope,aes(x=reorder(names,1:input$Pr_bin2), y = sl))+
        geom_bar(stat="identity",aes(fill=factor(clr,labels = lab)),color="black")+
        scale_fill_manual(values=col)+    # filling geom_bar with colors
        labs(fill="")+  # legend title
        geom_text(data=slope,aes(x = reorder(names,1:input$Pr_bin2),y = sl,label=round(sl,3)),vjust=0)+
        ylab("شیب پیشرفت خطی") + xlab("")+
        scale_x_discrete(labels= ylab_names[input$Pr_bin2:1])+
        theme(axis.text.x=element_text(face = "bold.italic", color = "blue", size = 8),
              axis.text.y=element_text(colour="blue", size=8, face="bold"))
        #coord_flip()  
      
  
      gg <- ggplotly(s)
      #gg <- s
    #}
    
    return(gg)
    
  })  
  
  
  ### selecting which React_DT want to show in output
  React_out <- reactive({
    
    if(var$a==TRUE){        # Does not need have () for input$x .... I mean, input$x() is wrong.
      return(React_Pr1())   # return is important here. Without it does not work
    }
    
    if(var$a==FALSE){
      return(React_Pr2())
    }
    
  })
  ###
  
  output$Pr <- renderPlotly(React_out())  
  
}

