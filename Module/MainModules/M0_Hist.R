
M0_HistUI <- function(id){
  
  ns <- NS(id)
  
  ################
  #### Histogram
  ################
  tabPanel(title = div(class="tabPanel--font-size","هیستوگرام"),
           icon=icon("user-circle"),
           
           fluidRow(            
             
             column(1,
                    div(class="input-box--general",
                        uiOutput(ns("Hg_SeI")))
             ),
             
             column(1,
                    div(class="input-box--general",style="margin-left:50%",
                        uiOutput(ns("Hg_bin")))
             ),
             column(1,
                    div(class="action-button--general action-button--mleft action-button--mtop",
                        actionButton(inputId = ns("Hg_Ac"),
                        label = div(class="action-button--font-size","آنالیز"),
                        class="action-button--color--yellow",
                        width="130%"))
             )
           ),
           
           plotlyOutput(ns("Hg")), 
           tags$div(
             tags$table(
               DT::dataTableOutput(ns("Gr_N"))
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
      selectInput(inputId = ns("Hg_SeI"),label = "زمان",choices = colnames(Data()),selected =tail(colnames(Data()),n=1))
    })
    
    # output$Hg_bin <- renderUI({
    #   div(style="font-size=220% ;",
    #   numericInput(inputId = ns("Hg_bin"),label = "بین",min = 1,max =100,value = 5))
    # })
    
    output$Hg_bin <- renderUI({
      selectInput(inputId = ns("Hg_bin"),label = "دسته",choices = 1:20,selected = 5)
    })
    
    
    melt_Data_Hg <- reactive({
    d <- as.data.frame(Data()[,input$Hg_SeI])
    d <- melt(as.matrix(d))
    d[,1] <- rownames(Data())
    d[,2] <- input$Hg_SeI
    colnames(d) <- c("Student","Day","value")
    d
  })
  
  
  
  Reac_Hg <- eventReactive(input$Hg_Ac, {
   
    p <- ggplot(melt_Data_Hg(),aes(value)) + geom_histogram(aes(y=..density..),bins=input$Hg_bin,colour="black", fill="forestgreen",alpha=0.9)+
    geom_density(aes(y=..density..),alpha=0.4, fill="#FF6666",colour="firebrick2",lwd=1.5)+
    labs(title ="هیستوگرام", x = "نمره", y = "فرکانس")
    
    gg <- ggplotly(p)
    

### number of groups == #bins    
        
    count <- ggplot_build(p)$data[[1]]$count
    splt=split(sort(melt_Data_Hg()[,3]), rep(1:length(count), count))
    
    group_names <- as.data.frame(matrix(NA,max(count),length(splt)))
    colnames(group_names) <- 1:length(splt)
    rownames(group_names) <- 1:max(count)
    for(i in 1:length(splt)){
      colnames(group_names)[i] <- paste0("گروه",i)
    }
    for(i in 1:length(splt)){
      group_names[1:length(splt[[i]]),i] <- melt_Data_Hg()[melt_Data_Hg()[,3] %in% splt[[i]],1]
    }
    
    group_names[is.na(group_names)] <- ""
    
    
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
