M0_ProgUI <- function(id){
  
  ns <- NS(id)
  
  tabPanel(title = div(class="tabPanel--font-size center","پیشرفت"),
           icon=icon("arrow-circle-up",class="tabPanel-icon"),
           
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
            
             column(1,
                    div(class="numeric-box--general__Pr",
                        uiOutput(ns("Pr_numI")))),
             
             column(1,
                    div(class="numeric-box--general__Pr",
                        uiOutput(ns("Pr_bin2")))),
             
             column(1,
                    div(class="action-button--general--left action-button--mtop__Pr action-button--mleft__Pr",
                        actionButton(inputId = ns("DT_AC3"),
                            label = div(class="action-button--font-size", "گروه بندی میانگین وزنی"),
                            class="action-button--color--yellow"
                        ))),
                            #icon=icon("arrow-right")))),
             column(1,offset = 1,
                    div(class="action-button--general--left action-button--mtop__Pr action-button--mleft__Pr",
                        actionButton(inputId = ns("Pr_AC2"),
                            label = div(class="action-button--font-size","پیشرفت گروهی"),
                              class="action-button--color--yellow"
                        ))),
                            #icon=icon("arrow-right")))),
             
             
             ########################################
 
             column(1,offset = 5,
                    div(class="action-button--general--left",
                        style="margin-left:30%; margin-top:80%;",
                        actionButton(inputId = ns("Pr_AC1"),
                            label = div(class="action-button--font-size","پیشرفت فردی"),
                            class="action-button--color--yellow"))
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
    selectInput(ns("Pr_numI"),label = "میانگین وزنی",choices = 1:ncol(Data()),selected = 1)
  })
  
  output$Pr_bin2 <- renderUI({
    selectInput(ns("Pr_bin2"),label = "تعداد گروه",choices = 1:nrow(Data()),selected = 1)
  })
  
  
  
  ## Variable (like trigger) for selecting which React_DT want to show in output
  var = reactiveValues(a = 1)
  
  observeEvent(input$Pr_AC1, {
    var$a = 1
  })
  
  observeEvent(input$Pr_AC2, {
    var$a = 2
  })
  
  observeEvent(input$DT_AC3, {
    var$a = 3
  })
  
 
##  
  
  
  React_Pr1 <- eventReactive(input$Pr_AC1, {

    Mean <- apply(Data(),2,mean)
    Data_tot1 <- rbind(Data(),Mean)
    rownames(Data_tot1) <- c(rownames(Data()),"میانگین")
    
    fit_tot1 <- rep(list("NA"),dim(Data_tot1)[1])
    slope1 <- rep(NA,dim(Data_tot1)[1])
    colnames(Data_tot1) <- 1:dim(Data_tot1)[2]
    
    for(i in 1:(dim(Data_tot1)[1])){
      d1 <- Data_tot1[i,]
      d1 <- reshape2::melt(as.matrix(d1))
      d1[,1] <- rownames(Data_tot1)[i]
      d1[,2] <- 1:dim(Data_tot1)[2]
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
    #slope1 <- slope1[order(slope1$sl,decreasing = T),]
    
    
    if(length(unique(slope1[,3]))==1){
      if(slope1[,3]=="red"){
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
    
    
    
    s1 <- ggplot(slope1,aes(x = reorder(names,sl),y = sl))+
      geom_bar(data = slope1,stat="identity",aes(fill=factor(clr,labels = lab)),colour="black")+
      labs(fill="")+
      geom_text(data=slope1,aes(x = names,y = sl,label=round(sl,3)),vjust=0)+
      ylab("شیب پیشرفت خطی") + xlab("")
      #coord_flip()
    
    gg1 <- ggplotly(s1)
    gg1
    
    })
  
  #########
  
  React_Pr2 <-eventReactive(input$Pr_AC2, {
    
   
    bin2 <- as.numeric(input$Pr_bin2)
    numI <- as.numeric(input$Pr_numI)
 
    # generated weights   
    if(numI==1){
      gr <- rep(1,dim(Data())[2])
      }
    else{
      gr <- as.numeric(cut(1:dim(Data())[2],breaks = numI,labels = 1:numI))
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
      names_ch <- rep(list("NA"),bin2)
      Data_T <- rep(list("NA"),bin2)
      ind <- split(1:nrow(Data()), ceiling(seq_along(1:nrow(Data()))/(nrow(Data())/bin2)))
      
      for(i in 1:bin2){
        names_ch[[i]] <- d$names[ind[[i]]]
        Data_T[[i]] <- Data()[rownames(Data()) %in% names_ch[[i]],]
        if(length(names_ch[[i]])==1){
          Data_T[[i]] <- t(Data_T[[i]])
        }
        }
      
      
      fit_tot <- rep(list("NA"),length(Data_T))
      slope <- rep(NA,length(Data_T))  
      

      for(i in 1:length(Data_T)){  
        Mean <- apply(Data_T[[i]],2,mean)
        Mean <- melt(as.matrix(Mean))[,-2]
        colnames(Mean) <- c("Day","value")
        Mean$Day <- 1:ncol(Data())
        fit_tot[[i]] <- lm(value~Day, data=Mean)
        slope[i] <- coef(fit_tot[[i]])[2]
      }
      
      slope <- as.data.frame(slope)
      slope$names <- 1:bin2
      slope$clr <- "green"
      colnames(slope) <- c("sl","names","clr")
      slope[which(slope$sl>0),3] <- "red"
      #slope <- slope[dim(slope)[1]:1,] 
      
      
      ylab_names <- rep(list("NA"),bin2)
      
      for(i in 1:bin2){
        ylab_names[[i]] <- paste(names_ch[[i]],sep = "\n")
      }
    
       
      if(length(unique(slope[,3]))==1){
        if(slope[,3]=="red"){
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
      
      
      
      
   slope <- slope[order(slope$sl,decreasing = F),]    
        
   s <- ggplot(slope,aes(x=reorder(names,1:bin2), y = sl))+
        geom_bar(stat="identity",aes(fill=factor(clr,labels = lab)),color="black")+
        scale_fill_manual(values=col)+    # filling geom_bar with colors
        labs(fill="")+  # legend title
        geom_text(data=slope,aes(x = reorder(names,1:bin2),y = sl,label=round(sl,3)),vjust=0)+
        ylab("شیب پیشرفت خطی") + xlab("")+
        scale_x_discrete(labels= ylab_names[bin2:1])+
        theme(axis.text.x=element_text(face = "bold.italic", color = "blue", size = 8),
              axis.text.y=element_text(colour="blue", size=8, face="bold"))
        #coord_flip()  
      
      gg <- ggplotly(s)
      #gg <- s
    #}
    
    return(gg)
    
  })  
  
  
  
  #########
  
  React_DT3 <-eventReactive(input$DT_AC3, {
    
    
    bin2 <- as.numeric(input$Pr_bin2)
    numI <- as.numeric(input$Pr_numI)
    
    if(numI==1)
      gr <- rep(1,dim(Data())[2])
    else
      gr <- as.numeric(cut(1:dim(Data())[2],breaks = numI,labels = 1:numI))
    
    
    d <- as.data.frame(apply(Data(),1,function(x){weighted.mean(x,gr)}))
    d$names <- rownames(Data())
    colnames(d) <- c("mean.w","names")
    d <- d[order(d$mean.w,decreasing = T),]
    
    cc1 <- colorRampPalette(c("sienna3","khaki3","turquoise3"))(bin2)
    
    if(bin2==1)
      d$clr <-cc1
    else
      d$clr <- as.vector(cut(1:dim(Data())[1],breaks = bin2,labels = cc1))
    
    p <- ggplot(d,aes(x = reorder(names,mean.w),y = mean.w))+
      geom_bar(stat="identity",aes(fill = clr),color="black")+
      geom_text(data=d,aes(x = names,y = mean.w,label=round(mean.w,2)),vjust=0)+
      labs(title ="میانگین وزنی", x = "", y = "میانگین وزنی")+
      scale_fill_manual(values=cc1)
    
    gg <- ggplotly(p)
    
    gg  
    
  })
  
  
  
  
  ### selecting which React_DT want to show in output
  React_out <- reactive({
    
    if(var$a==1){        # Does not need have () for input$x .... I mean, input$x() is wrong.
      return(React_Pr1())   # return is important here. Without it does not work
    }
    
    if(var$a==2){
      return(React_Pr2())
    }
    
    if(var$a==3){
      return(React_DT3())
    }
    
  })
  ###
  
  output$Pr <- renderPlotly(React_out())  
  
}

