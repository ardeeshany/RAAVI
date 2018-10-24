
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
                        checkboxInput(inputId = ns("density"),label = "توزیع",value = FALSE))
             )
             
           ),
           
            withSpinner(plotlyOutput(ns("Hg")),type=5,color = "#006E6D",size = 0.6),
           
           radioButtons(inputId = ns("table"),label = "",choices = c("گروه"="G","دسته"="D"),
                        selected = "G",inline = TRUE),
           
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
    melt_Data_Hg <- melt_Data_Hg[order(melt_Data_Hg$value,decreasing = F),]
    
    
    f <-ggplot(melt_Data_Hg,aes(value))+
      geom_histogram(aes(y = round(..count../(0.01*sum(..count..)),1)),bins=input$Hg_bin)
    
    count <- ggplot_build(f)$data[[1]]$count
    splt=split(sort(melt_Data_Hg[,3]), rep(1:length(count), count))

    cum_count <- cumsum(count)
    dup_count <- unique(cum_count[duplicated(cum_count)])
    gr_num <- length(dup_count) + 1
    
    gr_names <- rep(list(NA),gr_num)

    cc1 <- rev(colorRampPalette(c("#00BFC4","#D8AE47","#F7786B"))(gr_num))
    
    melt_Data_Hg$clr <- NA
    
    if(gr_num==1){
      gr_names <- melt_Data_Hg[,1]
      melt_Data_Hg[,4] <- cc1[1]
    }else{
    if(gr_num==2){
      gr_names[[1]] <- melt_Data_Hg[1:dup_count[1],1]
      gr_names[[2]] <- melt_Data_Hg[(dup_count[1]+1):tail(cum_count,1),1]
      melt_Data_Hg[1:dup_count[1],4] <- cc1[1]
      melt_Data_Hg[(dup_count[1]+1):tail(cum_count,1),4] <- cc1[2]
    }else{
    gr_names[[1]] <- melt_Data_Hg[1:dup_count[1],1]
    melt_Data_Hg[1:dup_count[1],4] <- cc1[1]
    for(i in 2:(gr_num-1)){
    gr_names[[i]] <- melt_Data_Hg[((dup_count[i-1]+1):dup_count[i]),1]
    melt_Data_Hg[(dup_count[i-1]+1):dup_count[i],4] <- cc1[i]
    }
    gr_names[[gr_num]] <- melt_Data_Hg[(dup_count[length(dup_count)]+1):tail(cum_count,1),1]
    melt_Data_Hg[(dup_count[length(dup_count)]+1):tail(cum_count,1),4] <- tail(cc1,1)
}}

    color_count <- rep(cc1[1],length(count))
    if(length(count) > 1){
    a <- 1
    for(i in 2:length(count)){
      if(count[i] == 0 && count[i-1] !=0){
        a <- a+1
      }
      color_count[i] <- cc1[a]
    }
    if(length(which(count==0))>0)
    color_count <- color_count[-which(count==0)]
    }

    
    group_names <- as.data.frame(matrix(NA,max(count),length(splt)))
    colnames(group_names) <- sapply(1:length(splt), function(x){paste("دسته",x)})
    rownames(group_names) <- 1:max(count)
    
    for(i in 1:length(splt)){
      group_names[1:length(splt[[i]]),i] <- melt_Data_Hg[melt_Data_Hg[,3] %in% splt[[i]],1]
    }     
    
    if(gr_num==1){
      Gr_names <- as.data.frame(gr_names)
      colnames(Gr_names) <- "گروه ۱"
    }else{
      Gr_names <- as.data.frame(matrix(NA,nrow = max(sapply(gr_names,length)),ncol = gr_num ))
      colnames(Gr_names) <- sapply(1:gr_num, function(x){paste("گروه",x)})
      for(i in 1:gr_num){
        Gr_names[1:length(gr_names[[i]]),i] = gr_names[[i]]
      }}
    
    leg <- rev(apply(Gr_names,2,FUN=function(x){
      length(x[which(x!='NA')])/tail(cum_count,1)
    }))
    
    # m=0
    # lab_legend <- sapply(100*leg, function(x){
    #   paste0(x,"%")
    # })
    
    lab_legend <- paste("گروه",
          length(leg):1,":   ","%",round(100*leg,0))
    
    # lab_legend <- sapply(1:gr_num, function(x){
    #   paste("گروه",x)
    #   })
    
    p <- ggplot(melt_Data_Hg,aes(value)) + 
         geom_histogram(aes(y = round(..count../(0.01*sum(..count..)),1),
                        fill=factor(clr,labels=lab_legend)), bins=input$Hg_bin,
                        colour="black",alpha=0.9)+
         labs(title ="هیستوگرام", x = "نمره", y = "فراوانی")+
         theme(axis.text.x = element_text(size=11,colour="black",angle=0, hjust=1,vjust=.5),
             axis.text.y = element_text(size=12,colour="black"),
             axis.title=element_text(size=14,face="bold"),
             plot.title = element_text(size=14,face="bold"),
             legend.title = element_text(size=12,face="bold"),
             legend.text=element_text(size=12),
             text=element_text(family="dastnevis"))+
             scale_fill_manual(values=rev(cc1),guide = guide_legend(reverse = TRUE,title = "",size=20))
    
    
    
    
    
    lab_hist <- function(x){
      if(x==0) return("")
      else return(paste0(x,"%"))
      }
  
    if(input$density==TRUE){
      p <- p + geom_density(aes(y=100*..density..),alpha=0.3, fill="#9B2335",colour="#9B2335",lwd=1.5)+
        stat_bin(aes(label=lapply(round(..count../(0.01*sum(..count..)),1),FUN= lab_hist)),geom="text",
                 bins = input$Hg_bin,color="black", size=4.5)
    }else{
      p <- p + stat_bin(aes(label=lapply(round(..count../(0.01*sum(..count..)),1),FUN=lab_hist)),geom="text",
                 bins = input$Hg_bin,color="black", size=4.5)
    }
    
    
    gg <- ggplotly(p)
    
##################
    
     #  group_names <- as.data.frame(matrix(NA,max(count),length(splt)))
     #  colnames(group_names) <- sapply(1:length(splt), function(x){paste("دسته",x)})
     #  rownames(group_names) <- 1:max(count)
     # 
     # for(i in 1:length(splt)){
     #   group_names[1:length(splt[[i]]),i] <- melt_Data_Hg[melt_Data_Hg[,3] %in% splt[[i]],1]
     # }     
     #  
     #  if(gr_num==1){
     #    Gr_names <- as.data.frame(gr_names)
     #    colnames(Gr_names) <- "گروه ۱"
     #  }else{
     #  Gr_names <- as.data.frame(matrix(NA,nrow = max(sapply(gr_names,length)),ncol = gr_num ))
     #  colnames(Gr_names) <- sapply(1:gr_num, function(x){paste("گروه",x)})
     #  for(i in 1:gr_num){
     #    Gr_names[1:length(gr_names[[i]]),i] = gr_names[[i]]
     #  }}
      
    out <- list(gg=gg,group_names=group_names,gr_names=Gr_names,color_count = color_count ,cc1 = cc1)
    
    return(out)

    })
  
  
  D_Table <- reactive({


    n <- dim(Reac_Hg()$group_names)[2]
    column <- 1:n
    color <- rep('black',n)
    backColor <- Reac_Hg()$color_count
    font <- rep('bold',n)

    DT <- datatable(Reac_Hg()$group_names,
              options = list(
                pageLength = 10, orderClasses = TRUE,
                searching = FALSE, paging = FALSE
                # initComplete = DT::JS(
                #   "function(settings, json) {",
                #   "$(this.api().table().header()).css({'background-color': 'gray', 'color': 'black'});",
                #   "}")
              ))

    for(i in 1:n){
      DT <- DT %>%
        formatStyle(column[i],  color = color[i], backgroundColor = backColor[i], fontWeight = font[i])
    }

    return(DT)

  })
  
  
  
  
  D_Table2 <- reactive({

    n <- dim(Reac_Hg()$gr_names)[2]
    column <- 1:n
    color <- rep('black',n)
    backColor <- Reac_Hg()$cc1
    font <- rep('bold',n)

    DT <- datatable(Reac_Hg()$gr_names,
                    options = list(
                      pageLength = 10, orderClasses = TRUE,
                      searching = FALSE, paging = FALSE
                      # initComplete = DT::JS(
                      #   "function(settings, json) {",
                      #   "$(this.api().table().header()).css({'background-color': 'gray', 'color': 'black'});",
                      #   "}")
                    ))

    for(i in 1:n){
      DT <- DT %>%
        formatStyle(column[i],  color = color[i], backgroundColor = backColor[i], fontWeight = font[i])
    }

    return(DT)

  })
  
  
  
  output$Hg <- renderPlotly(Reac_Hg()$gg)
  
  
  React_out <- reactive({
    
    if(input$table=="D"){
      return(D_Table())
    }else{
      return(D_Table2())
    }
    
  })
  
  output$Gr_N <- DT::renderDataTable( React_out() )
  
  
}
