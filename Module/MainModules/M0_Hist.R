
M0_HistUI <- function(id){
  
  ns <- NS(id)
  
  ################
  #### Histogram
  ################
   
  tabPanel(title = div(class="tabPanel--font-size center",
                      "گروه بندی"),
            icon = icon("group",class="tabPanel-icon"),
           
           
           fluidRow(            
             
             column(1,
                    div(class="input-box--general",
                        uiOutput(ns("Hg_SeI")))
             ),
             
             column(1,
                    div(class="input-box--general",style="margin-left:90%",
                        uiOutput(ns("Hg_bin")))
             ),
             column(1,
                    div(class="action-button--general action-button--mleft action-button--mtop",
                        actionButton(inputId = ns("Hg_Ac"),
                        label = div(class="action-button--font-size","آنالیز"),
                        class="action-button--color--yellow",
                        width="130%"))
             ),
             column(1,offset = 2,
                    div(class="check-box--general check-box--mtop__hist check-box--font-size",
                        checkboxInput(inputId = ns("density"),label = "توزیع",value = TRUE))
             )
             
           ),
           withSpinner(plotlyOutput(ns("Hg")),type=5,color = "#006E6D",size = 0.6),
           tags$div(
             tags$table(
               withSpinner( DT::dataTableOutput(ns("Gr_N")),type=5,color = "#006E6D",size = 0.4)
           ))

  )
        
}



M0_Hist <- function(input,output,session,Vals){
  
   ns <- session$ns  
  
    Data <- reactive({
      M <- Vals[["now"]]
      rownames(M) <- Vals[["names"]]
      colnames(M) <- Vals[["dates"]]
      return(M)
    })
    
    
    output$Hg_SeI <- renderUI({
      selectInput(inputId = ns("Hg_SeI"),label = "زمان",choices = colnames(Data()),selected =length(colnames(Data())))
    })
    
    
    output$Hg_bin <- renderUI({
      selectInput(inputId = ns("Hg_bin"),label = "دسته",choices = 1:20,selected = 5)
    })
    

  Reac_Hg <- eventReactive(input$Hg_Ac, {
   
    validate(
      need(!is.null(Data()),"هنوز داده ای وارد نشده است"),errorClass = "box"
    )
    
    d <- as.data.frame(Data()[,input$Hg_SeI])
    melt_Data_Hg <- melt(as.matrix(d))
    melt_Data_Hg[,1] <- rownames(Data())
    melt_Data_Hg[,2] <- input$Hg_SeI
    colnames(melt_Data_Hg) <- c("Student","Day","value")
    

    
    
    p <- ggplot(melt_Data_Hg,aes(value)) + 
         geom_histogram(aes(y = round(..count../(0.01*sum(..count..)),1)),bins=input$Hg_bin,
                        colour="black", fill="#009688",alpha=0.9)+
         labs(title ="هیستوگرام", x = "نمره", y = "فراوانی")+
         theme(axis.text.x = element_text(size=11,colour="black",angle=0, hjust=1,vjust=.5),
             axis.text.y = element_text(size=12,colour="black"),
             axis.title=element_text(size=14,face="bold"),
             plot.title = element_text(size=14,face="bold"),
             legend.title = element_text(size=12,face="bold"),
             text=element_text(family="dastnevis"))
    
    
    if(input$density==TRUE){
      p <- p + geom_density(aes(y=100*..density..),alpha=0.3, fill="#9B2335",colour="#9B2335",lwd=1.5)+
        stat_bin(aes(label=round(..count../(0.01*sum(..count..)),1)),geom="text",
                 bins = input$Hg_bin,color="white", size=5,vjust=0.5)
    }else{
      p <- p + stat_bin(aes(label=round(..count../(0.01*sum(..count..)),1)),geom="text",
                 bins = input$Hg_bin,color="white", size=5,vjust=0.5)
    }
    
    
    gg <- ggplotly(p)
    

### number of groups == #bins    
        

    count <- ggplot_build(p)$data[[1]]$count
    splt=split(sort(melt_Data_Hg[,3]), rep(1:length(count), count))
    
     group_names <- as.data.frame(matrix(NA,max(count),length(splt)))
     colnames(group_names) <- sapply(1:length(splt), function(x){paste0("گروه",x)})
     rownames(group_names) <- 1:max(count)
    
    for(i in 1:length(splt)){
      group_names[1:length(splt[[i]]),i] <- melt_Data_Hg[melt_Data_Hg[,3] %in% splt[[i]],1]
    }
 
    #group_names[is.na(group_names)] <- ""
    
    
### number of groups: between each two zero, we consider one group    
    
    # if(0 %in% count){
    #   
    # }
    # else{
    #   group_compct <- rownames(Data)
    # }
    # 
    # 
    # splt=split(sort(melt_Data_Hg()[,3]), rep(1:length(count), count))
    # 
    # group_names <- as.data.frame(matrix(NA,max(count),length(splt)))
    # colnames(group_names) <- 1:length(splt)
    # rownames(group_names) <- 1:max(count)
    # for(i in 1:length(splt)){
    #   colnames(group_names)[i] <- paste0("گروه",i)
    # }
    # for(i in 1:length(splt)){
    #   group_names[1:length(splt[[i]]),i] <- melt_Data_Hg()[melt_Data_Hg()[,3] %in% splt[[i]],1]
    # }
    # 
    # group_names[is.na(group_names)] <- ""
    
    
    
    
    out <- list(gg=gg,group_names=group_names)
    
    return(out)

    })
  

  
  
  
  # p <- ggplot(melt_Data_Hg,aes(value)) + geom_histogram(aes(y=..density..),bins=12,colour="black", fill="forestgreen",alpha=0.9)+
  #   geom_density(aes(y=..density..),alpha=0.4, fill="#FF6666",colour="firebrick2",lwd=1.5)+
  #   labs(title ="هیستوگرام", x = "نمره", y = "فرکانس")
  # splt=split(sort(melt_Data_Hg[,3]), rep(1:length(count), count))
  # 
  # c=1
  # z_id <- which(count==0)
  # count_cmpct <- rep(NA,length(z_id)+1)
  # splt_cmpct <- rep(NA,length(z_id)+1)                   
  # for(i in 1:(length(z_id)+1)){
  #   
  #   count_cmpct[i] <- length(c:(z_id[i]-1))
  #   splt_cmpct[i] <- sum(count[c:(z_id[i]-1)])
  #   
  #   if(i!=(length(z_id)+1)) c=z_id[i]+1
  # }
  # 
  # cumsum(count)
  
  
  
  
  
  output$Hg <- renderPlotly(Reac_Hg()$gg)
  
  output$Gr_N <- DT::renderDataTable(Reac_Hg()$group_names,
                              options = list(
                              pageLength = 10, orderClasses = TRUE,
                              searching = FALSE, paging = FALSE
                                     ))
  
  
}
