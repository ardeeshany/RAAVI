
M0_HistUI <- function(id){
  
  ns <- NS(id)
  
  ################
  #### Histogram
  ################
  
  tabPanel(title = div(class="tabPanel--font-size center",
                       "گروه بندی"),
           icon = icon("group",class="tabPanel-icon"),
   
           
    tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #485167}")), 
           
                   
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
                    div(class="action-button--general action-button--mleft__Hist1 action-button--mtop",
                        actionButton(inputId = ns("Hg_Ac"),
                                     label = div(class="action-button--font-size","آنالیز"),
                                     class="action-button--color--yellow",
                                     width="130%"))
             ),             
             column(1,offset = 2,
                    div(class="check-box--general check-box--mtop__hist check-box--font-size",
                        style="margin-left:-65%;",
                        checkboxInput(inputId = ns("density"),label = "توزیع",value = FALSE))
             ),


             
             
####################################
      
     
      column(1,offset = 2,
           div(class="check-box--general check-box--mtop__hist check-box--font-size",
               style="margin-left:-90%;",
           checkboxInput(inputId = ns("density_r"),label = "توزیع",value = FALSE))),


      column(1,
             div(class="numeric-box--general__Pr",
                 style="margin-left:-150%;",
                 uiOutput(ns("Pr_numI")))),
      
      column(1,
             div(class="numeric-box--general__Pr",
                 style="margin-left:-148%;",
                 uiOutput(ns("Pr_bin2")))),
      
      column(1,
             div(class="action-button--general--left action-button--mtop__Pr action-button--mleft__Hist",
                 style="margin-left:-146%;",
                 actionButton(inputId = ns("DT_AC3"),
                              label = div(class="action-button--font-size", "گروه بندی میانگین وزنی"),
                              class="action-button--color--yellow"
                 ))),
       fluidRow(
          column(2,offset = 8,
             div(class="inline",
             style="size:30%; text-align:right;  margin-top: -40%;margin-left:10%;",
             sliderInput(inputId = ns("slider_width"),min = 15,max = 75,value = 35,label=""))))

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
    selectInput(inputId = ns("Hg_SeI"),label = "تاریخ",choices = colnames(Data()),selected =length(colnames(Data())))
  })
  

  output$Hg_bin <- renderUI({
    ch <- length(unique(Data()[,input$Hg_SeI]))
    selectInput(inputId = ns("Hg_bin"),label = "تعداد گروه",choices = 1:ch,selected = min(2,ch))
  })
  
  
  
  
  Reac_Hg <- eventReactive(input$Hg_Ac, {
    
    validate(
      need(!is.null(Data()),"هنوز داده ای وارد نشده است"),errorClass = "Hist_l"
    )
    
    d <- as.data.frame(Data()[,input$Hg_SeI])
    melt_Data_Hg <- melt(as.matrix(d))
    melt_Data_Hg[,1] <- rownames(Data())
    melt_Data_Hg[,2] <- input$Hg_SeI
    colnames(melt_Data_Hg) <- c("Student","Day","value")
    melt_Data_Hg <- melt_Data_Hg[order(melt_Data_Hg$value,decreasing = F),]
    
    
    gr_num <- 1
    c <- 0
    count <- dim(melt_Data_Hg)[1]
    bin2 <- input$Hg_bin
    
    if(bin2==1) {
      c <- 1
    }else{
      while(gr_num < bin2){
        c <- c+1
        f <-ggplot(melt_Data_Hg,aes(value))+
          geom_histogram(aes(y = round(..count../(0.01*sum(..count..)),1)),bins=c)
        count <- ggplot_build(f)$data[[1]]$count
        cum_count <- cumsum(count)
        dup_count <- unique(as.vector(cum_count[duplicated(cum_count)]))
        gr_num <- length(dup_count) + 1
      }
    }
    
    splt=split(sort(melt_Data_Hg[,3]), rep(1:length(count), count))
    cc1 <- rev(colorRampPalette(c("#00BFC4","#D8AE47","#F7786B"))(gr_num))
    
    melt_Data_Hg$clr <- NA
    gr_names <- rep(list(NA),gr_num)

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
      length(x[which(x!='NA')])/dim(melt_Data_Hg)[1]
    }))
    
    # m=0
    # lab_legend <- sapply(100*leg, function(x){
    #   paste0(x,"%")
    # })
    
    lab_legend <- paste("گروه",
                        length(leg):1,":   ","%",round(100*leg,1))
    
    # lab_legend <- sapply(1:gr_num, function(x){
    #   paste("گروه",x)
    #   })
    
    p <- ggplot(melt_Data_Hg,aes(value)) + 
      geom_histogram(aes(y = round(..count../(0.01*sum(..count..)),1),
                         fill=factor(clr,labels=lab_legend)), bins=c,
                     colour="black",alpha=0.9)+
      labs(title ="", x = "نمره", y = "فراوانی")+
      theme(axis.text.x = element_text(size=11,colour="black",angle=0, hjust=1,vjust=.5),
            axis.text.y = element_text(size=12,colour="black"),
            axis.title=element_text(size=14,face="bold"),
            plot.title = element_text(size=14,face="bold"),
            legend.title = element_text(size=12,face="bold"),
            legend.text=element_text(size=12),
            text=element_text(family="dastnevis"))+
      scale_fill_manual(values=rev(cc1),guide = guide_legend(reverse = TRUE,title = "",size=20))
    
    
    gg <- ggplotly(p)
    
    out <- list(p=p,gg=gg,group_names=group_names,gr_names=Gr_names,color_count = color_count ,cc1 = cc1,c=c)
    
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
  
  
  table_ind <- reactiveValues(a=1)
  

  observeEvent(input$Hg_Ac, {
    table_ind$a = 1
  })
  
  observeEvent(input$DT_AC3, {
    table_ind$a = 2
  })
  
  
  
  React_out_table_l <- reactive({
    
    if(input$table=="D"){
      return(D_Table())
    }else{
      return(D_Table2())
    }
    
  })

  

  
##################################################  
##################################################
##################################################  
##################################################  
##################################################  
##################################################  
##################################################  
##################################################
##################################################  
##################################################  
##################################################  
##################################################   
  
  
  
  output$Pr_numI <- renderUI({
    if(is.null(Data())) ch <- ""
    else ch <- 1:ncol(Data())
    selectInput(ns("Pr_numI"),label = "میانگین وزنی",choices = ch )
  })
  
  
  group_mean <- reactive({
    
    numI <- as.numeric(input$Pr_numI)
    
    if(numI==1)
      gr <- rep(1,dim(Data())[2])
    else
      gr <- as.numeric(cut(1:dim(Data())[2],breaks = numI,labels = 1:numI))
    
    d <- as.data.frame(apply(Data(),1,function(x){weighted.mean(x,gr)}))
    d$names <- rownames(Data())
    colnames(d) <- c("mean.w","names")
    d <- d[order(d$mean.w,decreasing = T),]
    d[,1] <- round(d[,1],2)
    return(d)
  })
  
  
  
  ch_bin <- eventReactive(input$Pr_numI,{
    d <- group_mean()
    return(1:length(unique(d[,1])))
  })
  
  output$Pr_bin2 <- renderUI({
    if(is.null(Data())) ch <- ""
    else{
      ch <- ch_bin()
    }
    selectInput(ns("Pr_bin2"),label = "تعداد گروه",choices = ch,selected = min(2,length(ch)))
  })  
  
  
####################################  

React_DT3 <-eventReactive(input$DT_AC3, { 
  
  validate(
    need(!is.null(Data()),"هنوز داده ای وارد نشده است"),errorClass = "Hist_r"
  )
  
  bin2 <- as.numeric(input$Pr_bin2)
  numI <- as.numeric(input$Pr_numI)
  
  melt_Data_Hg <- as.data.frame(group_mean())[,2:1]
  colnames(melt_Data_Hg) <- c("Student","value")
  melt_Data_Hg <- melt_Data_Hg[order(melt_Data_Hg$value,decreasing = F),]
  
  gr_num <- 1
  c <- 0
  count <- dim(melt_Data_Hg)[1]
  
  if(bin2==1) {
    c <- 1
  }else{
    while(gr_num < bin2){
      c <- c+1
      f <-ggplot(melt_Data_Hg,aes(value))+
        geom_histogram(aes(y = round(..count../(0.01*sum(..count..)),1)),bins=c)
      count <- ggplot_build(f)$data[[1]]$count
      cum_count <- cumsum(count)
      dup_count <- unique(as.vector(cum_count[duplicated(cum_count)]))
      gr_num <- length(dup_count) + 1
    }
  }
  splt=split(sort(melt_Data_Hg[,2]), rep(1:length(count), count))
  cc1 <- rev(colorRampPalette(c("#00BFC4","#D8AE47","#F7786B"))(gr_num))
  
  melt_Data_Hg$clr <- NA
  gr_names <- rep(list(NA),gr_num)
  
  if(gr_num==1){
    gr_names <- melt_Data_Hg[,1]
    melt_Data_Hg[,3] <- cc1[1]
  }else{
    if(gr_num==2){
      gr_names[[1]] <- melt_Data_Hg[1:dup_count[1],1]
      gr_names[[2]] <- melt_Data_Hg[(dup_count[1]+1):tail(cum_count,1),1]
      melt_Data_Hg[1:dup_count[1],3] <- cc1[1]
      melt_Data_Hg[(dup_count[1]+1):tail(cum_count,1),3] <- cc1[2]
    }else{
      gr_names[[1]] <- melt_Data_Hg[1:dup_count[1],1]
      melt_Data_Hg[1:dup_count[1],3] <- cc1[1]
      for(i in 2:(gr_num-1)){
        gr_names[[i]] <- melt_Data_Hg[(dup_count[i-1]+1):dup_count[i],1]
        melt_Data_Hg[(dup_count[i-1]+1):dup_count[i],3] <- cc1[i]
      }
      gr_names[[gr_num]] <- melt_Data_Hg[(dup_count[length(dup_count)]+1):tail(cum_count,1),1]
      melt_Data_Hg[(dup_count[length(dup_count)]+1):tail(cum_count,1),3] <- tail(cc1,1)
    }}
  
  

  
  ##############################    
  ##############################      
  ##############################      
  
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
  
  
  
  
  ##################
  
  group_names <- as.data.frame(matrix(NA,max(count),length(splt)))
  colnames(group_names) <- sapply(1:length(splt), function(x){paste("دسته",x)})
  rownames(group_names) <- 1:max(count)
  
  for(i in 1:length(splt)){
    group_names[1:length(splt[[i]]),i] <- melt_Data_Hg[melt_Data_Hg[,2] %in% splt[[i]],1]
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
  
  
  
  
  lab_legend_bar <- sapply(1:gr_num, function(x){
    paste("گروه",x)
  })
  


  gg_bar <- ggplot(melt_Data_Hg,aes(x = reorder(Student,value),y = value))+
    geom_bar(stat="identity",aes(fill=factor(clr,labels=lab_legend_bar)),color="black")+
    geom_text(data=melt_Data_Hg,aes(x = Student,y = value, label= value),
              position = position_stack(vjust = 0.5),color="black",size=4.5)+
    labs(title ="", x = "", y = "")+
    theme(axis.text.x = element_text(size=12,colour="black",angle=0, hjust=1,vjust=.5),
          axis.text.y = element_text(size=9,colour="black"),
          axis.title=element_text(size=14,face="bold"),
          plot.title = element_text(size=14,face="bold"),
          legend.title = element_text(size=12,face="bold"),
          legend.text=element_text(size=12),
          text=element_text(family="dastnevis"))+
    scale_fill_manual(aes(breaks=clr),values=rev(cc1),guide = guide_legend(title = "",size=20))+
    coord_flip()
  
  

  
  gg_bar <- ggplotly(gg_bar) %>% layout(annotations = text_bar)
  
  ########
  
  leg_hist <- rev(apply(Gr_names,2,FUN=function(x){
    length(x[which(x!='NA')])/dim(melt_Data_Hg)[1]
  }))
  
  lab_legend_hist <- paste("گروه",
                           length(leg_hist):1,":   ","%",round(100*leg_hist,1))
  

  gg_hist0 <- ggplot(melt_Data_Hg,aes(value)) +
    geom_histogram(aes(y = round(..count../(0.01*sum(..count..)),1),
                       fill=factor(clr,labels=lab_legend_hist)), bins=c,
                   colour="black",alpha=0.9)+
    labs(title ="", x = "نمره", y = "")+
    theme(axis.text.x = element_text(size=11,colour="black",angle=0, hjust=1,vjust=.5),
          axis.text.y = element_text(size=12,colour="black"),
          axis.title=element_text(size=14,face="bold"),
          plot.title = element_text(size=14,face="bold"),
          legend.title = element_text(size=12,face="bold"),
          legend.text=element_text(size=12),
          text=element_text(family="dastnevis"))+
    scale_fill_manual(aes(breaks=clr),values=rev(cc1),guide = guide_legend(title = "",size=20))
  
  
  out <- list(gg_hist0=gg_hist0,gg_bar=gg_bar,group_names=group_names,gr_names=Gr_names,color_count = color_count ,cc1 = cc1,c=c,gr_num=gr_num)
  
  return(out) 
  
})
  
  
  D_Table_r <- reactive({
    
    
    n <- dim(React_DT3()$group_names)[2]
    column <- 1:n
    color <- rep('black',n)
    backColor <- React_DT3()$color_count
    font <- rep('bold',n)
    
    DT <- datatable(React_DT3()$group_names,
                    options = list(
                      pageLength = 10, orderClasses = TRUE,
                      searching = FALSE, paging = FALSE
                    ))
    
    for(i in 1:n){
      DT <- DT %>%
        formatStyle(column[i],  color = color[i], backgroundColor = backColor[i], fontWeight = font[i])
    }
    
    return(DT)
    
  })
  
  
  
  
  D_Table2_r <- reactive({
    
    n <- dim(React_DT3()$gr_names)[2]
    column <- 1:n
    color <- rep('black',n)
    backColor <- React_DT3()$cc1
    font <- rep('bold',n)
    
    DT <- datatable(React_DT3()$gr_names,
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
  
  React_out_table_r <- reactive({
    
    if(input$table=="D"){
      return(D_Table_r())
    }else{
      return(D_Table2_r())
    }
    
  })
  
  
  # React_GrCat <- reactive({
  #   A <- subplot(React_DT3()$gg_bar,React_DT3()$gg_hist,shareX = FALSE,shareY = FALSE, 
  #                widths=c(input$slider_width/100,1-input$slider_width/100),
  #                titleX=TRUE) %>% layout(showlegend = FALSE)
  #   return(A)
  #   
  # })  
  
  lab_hist <- function(x){
    if(x==0) return("")
    else return(paste0(x,"%"))
  }
  
  f <- list(
    #family = "Courier New, monospace",
    size = 16,
    weight = 'bold',
    color = "black")
  
  text_bar <- list(
    text = "میانگین وزنی نمره دانش آموزان در طول زمان",
    font=f,
    xref = "paper",
    yref = "paper",
    yanchor = "bottom",
    xanchor = "center",
    align = "center",
    x = 0.5,
    y = 1,
    showarrow = FALSE
  )
  
  text_hist_date <- list(
    text = "فراوانی نمره ها در یک زمان مشخص",
    font=f,
    xref = "paper",
    yref = "paper",
    yanchor = "bottom",
    xanchor = "center",
    align = "center",
    x = 0.5,
    y = 1,
    showarrow = FALSE
  )
  
  
  text_hist_mean <- list(
    text = "فراوانی میانگین وزنی نمره ها در طول زمان",
    font=f,
    xref = "paper",
    yref = "paper",
    yanchor = "bottom",
    xanchor = "center",
    align = "center",
    x = 0.5,
    y = 1,
    showarrow = FALSE
  )
  
  Reac_Hg_final <- reactive({

    bin <- Reac_Hg()$c

    if(input$density){
      
      p <- Reac_Hg()$p + geom_density(aes(y=100*..density..),alpha=0.3, fill="#9B2335",colour="#9B2335",lwd=1.5)+
        stat_bin(aes(label=lapply(round(..count../(0.01*sum(..count..)),1),FUN= lab_hist)),geom="text",
                 bins = bin ,color="black", size=4.5)
      
      return(ggplotly(p) %>% layout(annotations = text_hist_date) )
    }else{
      p <- Reac_Hg()$p + stat_bin(aes(label=lapply(round(..count../(0.01*sum(..count..)),1),FUN=lab_hist)),geom="text",
                        bins = bin,color="black", size=4.5)
      return(ggplotly(p) %>% layout(annotations = text_hist_date) )
    }
  })
  
  
  React_GrCat_final <- reactive({
    
    
    if(input$density_r){
      
      p <- React_DT3()$gg_hist0 + geom_density(aes(y=100*..density..),alpha=0.3, fill="#9B2335",colour="#9B2335",lwd=1.5)+
        stat_bin(aes(label=lapply(round(..count../(0.01*sum(..count..)),1),FUN= lab_hist)),geom="text",
                 bins = React_DT3()$c,color="black", size=4.5)
      
      p <- ggplotly(p) %>% layout(annotations = text_hist_mean) 
      
      A <- subplot(React_DT3()$gg_bar, p,
                     widths=c(input$slider_width/100,1-input$slider_width/100),
                     titleX=TRUE)  %>% layout(showlegend = FALSE) 

      return(A)

    }else{
      p <- React_DT3()$gg_hist0 + stat_bin(aes(label=lapply(round(..count../(0.01*sum(..count..)),1),FUN=lab_hist)),geom="text",
                                             bins = React_DT3()$c,color="black", size=4.5)
      
      p <- ggplotly(p) %>% layout(annotations = text_hist_mean)
      
      A <- subplot(React_DT3()$gg_bar, p,
                   widths=c(input$slider_width/100,1-input$slider_width/100),
                   titleX=TRUE)  %>% layout(showlegend = FALSE) 
      
      return(A)

    }
  })
  
  
  
  React_out <- reactive({
    
    if(table_ind$a==1){        # Does not need have () for input$x .... I mean, input$x() is wrong.
      return(Reac_Hg_final())   # return is important here. Without it does not work
    }
    
    if(table_ind$a==2){
      return(React_GrCat_final())
    }
    
  }) 

  output$Hg <- renderPlotly(React_out())
  
  
  React_out_table <- reactive({
    
    if(table_ind$a==1){        # Does not need have () for input$x .... I mean, input$x() is wrong.
      return(React_out_table_l())   # return is important here. Without it does not work
    }
    
    if(table_ind$a==2){
      return(React_out_table_r())
    }
    
  })
  
  output$Gr_N <- DT::renderDataTable( React_out_table() )
  
  
}
