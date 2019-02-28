
M0_HistUI <- function(id){
  
  ns <- NS(id)
  
  ################
  #### Histogram
  ################
  
  
  tabPanel(title = div(class="tabPanel--font-size center",
                       "گروه بندی"),
           icon = icon("group",class="tabPanel-icon"),
           
    # tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #485167}")), 
    #        
                   
fluidRow(            
             
div(style="text-align:center;",
column(width = 2, 
br(),  
box(width="100%",status="primary",     
    
  wellPanel( 
  
                    div(class="input-box--general",
                        uiOutput(ns("Hg_SeI"))),
             

                    div(class="input-box--general",
                        uiOutput(ns("Hg_bin"))),

                        actionBttn(inputId = ns("Hg_Ac"),style = "jelly",color = "warning",
                                     label = div(class="action-button--widget","Run - one exam"))
                                     

),
             
             
####################################
      
 wellPanel( 

             div(class="numeric-box--general__Pr",
                 uiOutput(ns("Pr_numI"))),

             div(class="numeric-box--general__Pr",
                 uiOutput(ns("Pr_bin2"))),


                 actionBttn(inputId = ns("DT_AC3"),style = "jelly",color = "warning",
                              label = div(class="action-button--widget", "Run - all time"))
                              #class="action-button--color--yellow")

)

))),
           
column(width = 10,
br(),  
box(status="primary",width="100%",collapsible = TRUE,collapsed = FALSE,

    uiOutput(ns("Hg_full"))
    
           ))           
  

))

}



M0_Hist <- function(input,output,session,Vals,font_plot){
  
  ns <- session$ns  
  
  Data <- reactive({
    M <- Vals[["now"]]
    rownames(M) <- Vals[["names"]]
    colnames(M) <- Vals[["dates"]]
    return(M)
  })
  
  # A <- reactive({
  #   
  #   if(input$Pr_numI != ""){
  #     a <- input$Pr_numI
  #   }else{
  #     a <- 1
  #   }
  #   return(a)
  #   
  # })
  
  
  group_mean <- reactive({


    # if(!is.null(Data())){
    if(!is.null(input$Pr_numI)){
    if(input$Pr_numI != ""){
     numI <- as.numeric(input$Pr_numI)
     }else{
       numI <- 1
     }
    #numI=A()
    if(numI==1)
      gr <- rep(1,dim(Data())[2])
    else
      gr <- as.numeric(cut(1:dim(Data())[2],breaks = numI,labels = 1:numI))
    
    d <- as.data.frame(apply(Data(),1,function(x){weighted.mean(x,gr)}))
    d$names <- rownames(Data())
    colnames(d) <- c("mean.w","names")
    d <- d[order(d$mean.w,decreasing = T),]
    d[,1] <- round(d[,1],2)
    return(d)}
    # else{
    #   return(NULL)
    # }


  })
  
  
  
  ch_bin <- reactive({

    print("group_mean")
    print(is.null(group_mean()))
    print("group_mean2")
    print(group_mean())
    d <- group_mean()
    return(length(unique(d[,1])))

    })
  
  
  output$Hg_SeI <- renderUI({
    
    if(is.null(Data())) {
      ch <- ""
      ch_select <- ""
      pickerInput(inputId = ns("Hg_SeI"),label = "Date",choices = ch,
                  selected =ch_select,
                  options = list(style = "btn"),
                  choicesOpt = ch_opt)
    }else{
      ch <- colnames(Data())
      ch_select <- tail(ch,1)
      pickerInput(inputId = ns("Hg_SeI"),label = "Date",choices = ch,
                  selected =ch_select,
                  options = list(style = "btn"))
    }

  
    })
  

  output$Hg_bin <- renderUI({
    
  if(is.null(Data())) {
      ch <- ""
      ch_select <- ""
      pickerInput(ns("Hg_bin"),label = "Number of groups",choices = ch,selected = ch_select,
                  options = list(style = "btn"),
                  choicesOpt = ch_opt)
    }else{
      # print("***group_mean***")
      # print(is.null(group_mean()))
      # print("***ch_bin***")
      # print(ch_bin())
      #if(!is.null(group_mean())){
      ch <- 1:min(ch_bin(),5)
      ch_select <- min(2,ch_bin())
      pickerInput(ns("Hg_bin"),label = "Number of groups",choices = ch,selected = ch_select,
                  options = list(style = "btn"))
    
      #}
      }

    
  })
  
  
  
  
  Reac_Hg <- eventReactive(input$Hg_Ac, {
    
    # validate(
    #   need(!is.null(Data()),"هنوز داده ای وارد نشده است"),errorClass = "Hist_l"
    # )
    
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
    colnames(group_names) <- sapply(length(splt):1, function(x){paste("batch",x)})
    rownames(group_names) <- 1:max(count)
    
    for(i in 1:length(splt)){
      group_names[1:length(splt[[i]]),i] <- melt_Data_Hg[melt_Data_Hg[,3] %in% splt[[i]],1]
    }     
    
    if(gr_num==1){
      Gr_names <- as.data.frame(gr_names)
      colnames(Gr_names) <- "group 1"
    }else{
      Gr_names <- as.data.frame(matrix(NA,nrow = max(sapply(gr_names,length)),ncol = gr_num ))
      colnames(Gr_names) <- sapply(gr_num:1, function(x){paste("group",x)})
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
    
    lab_legend <- paste("group",
                        1:length(leg),":   ","%",round(100*leg,1))
    
    # lab_legend <- sapply(1:gr_num, function(x){
    #   paste("گروه",x)
    #   })
    lab_legend_bar <- sapply(1:gr_num, function(x){
      paste("group",x)
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
            text=element_text(family=font_plot))+
      scale_fill_manual(aes(breaks=clr),values=rev(cc1),guide = guide_legend(title = "",size=20))+
      coord_flip()
    
    
    
    
    gg_bar <- ggplotly(gg_bar) %>% layout(annotations = text_bar1) %>% config(displaylogo = FALSE,collaborate = FALSE,
                                                                              modeBarButtonsToRemove = list(
                                                                                'zoom2d','pan2d','select2d','lasso2d','zoomIn2d','zoomOut2d',
                                                                                'sendDataToCloud',
                                                                                'autoScale2d',
                                                                                'hoverClosestCartesian',
                                                                                'hoverCompareCartesian'
                                                                              ))
    
    
    
    
    p <- ggplot(melt_Data_Hg,aes(value)) + 
      geom_histogram(aes(y = round(..count../(0.01*sum(..count..)),1),
                         fill=factor(clr,labels=lab_legend)), bins=c,
                     colour="black",alpha=0.9)+
      labs(title ="", x = "Scores", y = "Frequency")+
      theme(axis.text.x = element_text(size=11,colour="black",angle=0, hjust=1,vjust=.5),
            axis.text.y = element_text(size=12,colour="black"),
            axis.title=element_text(size=14,face="bold"),
            plot.title = element_text(size=14,face="bold"),
            legend.title = element_text(size=12,face="bold"),
            legend.text=element_text(size=12),
            text=element_text(family=font_plot))+
      scale_fill_manual(values=rev(cc1),guide = guide_legend(reverse = TRUE,title = "",size=20))
    
    
    gg <- ggplotly(p) %>% config(displaylogo = FALSE,collaborate = FALSE,
                                 modeBarButtonsToRemove = list(
                                   'zoom2d','pan2d','select2d','lasso2d','zoomIn2d','zoomOut2d',
                                   'sendDataToCloud',
                                   'autoScale2d',
                                   'hoverClosestCartesian',
                                   'hoverCompareCartesian'
                                 ))
    
    
    
    
    
    out <- list(p=p,gg=gg,gg_bar=gg_bar,group_names=group_names,gr_names=Gr_names,color_count = color_count ,cc1 = cc1,c=c)
    
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
                      searching = FALSE, paging = FALSE,
                      columnDefs = list(list(className = 'dt-right', targets = "_all"))
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
                      searching = FALSE, paging = FALSE,
                      columnDefs = list(list(className = 'dt-right', targets = "_all"))
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
  
  
  table_ind <- reactiveValues(a=0)
  
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
  
  
  ch_opt <- list(content = c("<div> </div>"))
  
  output$Pr_numI <- renderUI({
    if(is.null(Data())){
      ch <- ""
      pickerInput(ns("Pr_numI"),label = "Weighted mean",choices = ch,
                  selected=1,
                  options = list(style = "btn"),
                  choicesOpt = ch_opt)
    } else{
      ch <- 1:ncol(Data())
      pickerInput(ns("Pr_numI"),label = "Weighted mean",choices = ch,
                  selected=1,
                  options = list(style = "btn"))
    } 

  })
  

  
  output$Pr_bin2 <- renderUI({
    if(is.null(Data())) {
      ch <- ""
      ch_select <- ""
      pickerInput(ns("Pr_bin2"),label = "Number of groups",choices = ch,selected = ch_select,
                  options = list(
                    style = "btn"),
                  choicesOpt = ch_opt)
    }else{
      ch <- 1:min(ch_bin(),5)
      ch_select <- min(2,ch_bin())
      pickerInput(ns("Pr_bin2"),label = "Number of groups",choices = ch,selected = ch_select,
                  options = list(
                    style = "btn"))
    }

  })
  
  
####################################  

React_DT3 <-eventReactive(input$DT_AC3, { 
  
  # validate(
  #   need(!is.null(Data()),"هنوز داده ای وارد نشده است"),errorClass = "Hist_r"

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
  colnames(group_names) <- sapply(length(splt):1, function(x){paste("batch",x)})
  rownames(group_names) <- 1:max(count)
  
  for(i in 1:length(splt)){
    group_names[1:length(splt[[i]]),i] <- melt_Data_Hg[melt_Data_Hg[,2] %in% splt[[i]],1]
  }
  
  
  if(gr_num==1){
    Gr_names <- as.data.frame(gr_names)
    colnames(Gr_names) <- "group 1"
  }else{
    Gr_names <- as.data.frame(matrix(NA,nrow = max(sapply(gr_names,length)),ncol = gr_num ))
    colnames(Gr_names) <- sapply(gr_num:1, function(x){paste("group",x)})
    for(i in 1:gr_num){
      Gr_names[1:length(gr_names[[i]]),i] = gr_names[[i]]
    }}
  
  
  
  
  lab_legend_bar <- sapply(1:gr_num, function(x){
    paste("group",x)
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
          text=element_text(family=font_plot))+
    scale_fill_manual(aes(breaks=clr),values=rev(cc1),guide = guide_legend(title = "",size=20))+
    coord_flip()
  
  

  
  gg_bar <- ggplotly(gg_bar) %>% layout(annotations = text_bar) %>% config(displaylogo = FALSE,collaborate = FALSE,
                                                                           modeBarButtonsToRemove = list(
                                                                             'zoom2d','pan2d','select2d','lasso2d','zoomIn2d','zoomOut2d',
                                                                             'sendDataToCloud',
                                                                             'autoScale2d',
                                                                             'hoverClosestCartesian',
                                                                             'hoverCompareCartesian'
                                                                           ))
  
  ########
  
  leg_hist <- rev(apply(Gr_names,2,FUN=function(x){
    length(x[which(x!='NA')])/dim(melt_Data_Hg)[1]
  }))
  
  lab_legend_hist <- paste("group",
                           1:length(leg_hist),":   ","%",round(100*leg_hist,1))
  

  gg_hist0 <- ggplot(melt_Data_Hg,aes(value)) +
    geom_histogram(aes(y = round(..count../(0.01*sum(..count..)),1),
                       fill=factor(clr,labels=lab_legend_hist)), bins=c,
                   colour="black",alpha=0.9)+
    labs(title ="", x = "Scores", y = "")+
    theme(axis.text.x = element_text(size=11,colour="black",angle=0, hjust=1,vjust=.5),
          axis.text.y = element_text(size=12,colour="black"),
          axis.title=element_text(size=14,face="bold"),
          plot.title = element_text(size=14,face="bold"),
          legend.title = element_text(size=12,face="bold"),
          legend.text=element_text(size=12),
          text=element_text(family=font_plot))+
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
                      searching = FALSE, paging = FALSE,
                      columnDefs = list(list(className = 'dt-right', targets = "_all"))
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
                      searching = FALSE, paging = FALSE,
                      columnDefs = list(list(className = 'dt-right', targets = "_all"))
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
    text = "Weighted mean of students over time",
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
  
  text_bar1 <- list(
    text = "Student scores",
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
    text = "Frequency of scores in one date",
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
    text = "Frequency of weighted means over time",
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
      
      p <- ggplotly(p) %>% layout(annotations = text_hist_date) %>% config(displaylogo = FALSE,collaborate = FALSE,
                                                                           modeBarButtonsToRemove = list(
                                                                             'zoom2d','pan2d','select2d','lasso2d','zoomIn2d','zoomOut2d',
                                                                             'sendDataToCloud',
                                                                             'autoScale2d',
                                                                             'hoverClosestCartesian',
                                                                             'hoverCompareCartesian'
                                                                           ))
      
      A <- subplot(Reac_Hg()$gg_bar, p,
                   widths=c(input$slider_width/100,1-input$slider_width/100),
                   titleX=TRUE)  %>% layout(showlegend = FALSE) 
      
     return(A)
      
    }else{
      p <- Reac_Hg()$p + stat_bin(aes(label=lapply(round(..count../(0.01*sum(..count..)),1),FUN=lab_hist)),geom="text",
                        bins = bin,color="black", size=4.5)
      p <- ggplotly(p) %>% layout(annotations = text_hist_date) %>% config(displaylogo = FALSE,collaborate = FALSE,
                                                                           modeBarButtonsToRemove = list(
                                                                             'zoom2d','pan2d','select2d','lasso2d','zoomIn2d','zoomOut2d',
                                                                             'sendDataToCloud',
                                                                             'autoScale2d',
                                                                             'hoverClosestCartesian',
                                                                             'hoverCompareCartesian'
                                                                           ))
      
      A <- subplot(Reac_Hg()$gg_bar, p,
                   widths=c(input$slider_width/100,1-input$slider_width/100),
                   titleX=TRUE)  %>% layout(showlegend = FALSE) 
     return(A)
      
    }
  })
  
  
  React_GrCat_final <- reactive({
    
    
    if(input$density){
      
      p <- React_DT3()$gg_hist0 + geom_density(aes(y=100*..density..),alpha=0.3, fill="#9B2335",colour="#9B2335",lwd=1.5)+
        stat_bin(aes(label=lapply(round(..count../(0.01*sum(..count..)),1),FUN= lab_hist)),geom="text",
                 bins = React_DT3()$c,color="black", size=4.5)
      
      p <- ggplotly(p) %>% layout(annotations = text_hist_mean) %>% config(displaylogo = FALSE,collaborate = FALSE,
                                                                           modeBarButtonsToRemove = list(
                                                                             'zoom2d','pan2d','select2d','lasso2d','zoomIn2d','zoomOut2d',
                                                                             'sendDataToCloud',
                                                                             'autoScale2d',
                                                                             'hoverClosestCartesian',
                                                                             'hoverCompareCartesian'
                                                                           ))
      
      A <- subplot(React_DT3()$gg_bar, p,
                     widths=c(input$slider_width/100,1-input$slider_width/100),
                     titleX=TRUE)  %>% layout(showlegend = FALSE) 

      return(A)

    }else{
      p <- React_DT3()$gg_hist0 + stat_bin(aes(label=lapply(round(..count../(0.01*sum(..count..)),1),FUN=lab_hist)),geom="text",
                                             bins = React_DT3()$c,color="black", size=4.5)
      
      p <- ggplotly(p) %>% layout(annotations = text_hist_mean) %>% config(displaylogo = FALSE,collaborate = FALSE,
                                                                           modeBarButtonsToRemove = list(
                                                                             'zoom2d','pan2d','select2d','lasso2d','zoomIn2d','zoomOut2d',
                                                                             'sendDataToCloud',
                                                                             'autoScale2d',
                                                                             'hoverClosestCartesian',
                                                                             'hoverCompareCartesian'
                                                                           ))
      
      A <- subplot(React_DT3()$gg_bar, p,
                   widths=c(input$slider_width/100,1-input$slider_width/100),
                   titleX=TRUE)  %>% layout(showlegend = FALSE) 
      
      return(A)

    }
  })
  
  


  output$Hg_1 <- renderPlotly(Reac_Hg_final())
  output$Hg_2 <- renderPlotly(React_GrCat_final())
  

  React_out_table <- reactive({
    
    if(table_ind$a==1){        # Does not need have () for input$x .... I mean, input$x() is wrong.
      return(React_out_table_l())   # return is important here. Without it does not work
    }
    
    if(table_ind$a==2){
      return(React_out_table_r())
    }
    
  })
  
   output$Gr_N_1 <- DT::renderDataTable( React_out_table_l() )
   output$Gr_N_2 <- DT::renderDataTable( React_out_table_r() )
  
  #output$Gr_N_1 <- renderRHandsontable( React_out_table_l() )
  #output$Gr_N_2 <- renderRHandsontable( React_out_table_r() )
  
  
  output$Hg_full <- renderUI({
    
    
    if(table_ind$a==1){
       
       
      validate(
        need(!is.null(Data()),"No Data has been imported yet!"),errorClass = "Hist_l"
      )
      
      A <- dropdown(
        div(style="text-align:right; font-size :110%; font-weight:bold;", "Settings : "),
        div(style="text-align:left",
            noUiSliderInput(inputId = ns("slider_width"),
                            label =div(style="font-size:80%;","scale of plots"),tooltips = F,
                            inline = T, min = 15,max = 75,value = 35,step = 1,
                            width = "100%",color = "#578CA9")),
        materialSwitch(inputId = ns("density"), label = "density function", status = "danger", right = TRUE),
        circle = TRUE, status = "default", icon = icon("gear"),style = "unite",width = "25%"
      ) 
      
      B <- div(style="text-align:right",downloadBttn(ns("download"),
                                                     label = "Download",size = "sm"))
      
      C <- withSpinner(plotlyOutput(ns("Hg_1")),type=5,color = "#006E6D",size = 0.6)
       
      D <- dropdown(
        div(style="text-align:right; font-size :110%; font-weight:bold;", "Settings : "),
        radioGroupButtons(inputId = ns("table"),label = "",choices = c("group"="G","batch"="D"),
                          selected = "G",individual = TRUE,
                          checkIcon = list(
                            yes = tags$i(class = "fa fa-circle", 
                                         style = "color: steelblue"),
                            no = tags$i(class = "fa fa-circle-o", 
                                        style = "color: steelblue"))),
        circle = TRUE, status = "default", icon = icon("gear"),style = "unite", width = "25%")
       
      E <- DT::dataTableOutput(ns("Gr_N_1"))
      #E <- rHandsontableOutput(ns("Gr_N_1"))  
       
      out <- list(A,B,br(),C,D,E)
      
      return(out)
       
    }
    
    if(table_ind$a==2){
      
    validate(
      need(!is.null(Data()),"No Data has been imported yet!"),errorClass = "Hist_l"
    )  
    
   
    A <- dropdown(
        div(style="text-align:right; font-size :110%; font-weight:bold;", "Settings : "),
        div(style="text-align:left",
            noUiSliderInput(inputId = ns("slider_width"),
                            label =div(style="font-size:80%;","scale of plots"),tooltips = F,
                            inline = T, min = 15,max = 75,value = 35,step = 1,
                            width = "100%",color = "#578CA9")),
        materialSwitch(inputId = ns("density"), label = "density function", status = "danger", right = TRUE),
        circle = TRUE, status = "default", icon = icon("gear"),style = "unite",width = "25%"
      )
      
    B <- div(style="text-align:right",downloadBttn(ns("download"),
                                                   label = "Download",size = "sm"))
         
    C <- withSpinner(plotlyOutput(ns("Hg_2")),type=5,color = "#006E6D",size = 0.6)
    
    
    D <- dropdown(
      div(style="text-align:right; font-size :110%; font-weight:bold;", "Settings : "),
      radioGroupButtons(inputId = ns("table"),label = "",choices = c("group"="G","batch"="D"),
                   selected = "G",individual = TRUE,
                   checkIcon = list(
                     yes = tags$i(class = "fa fa-circle", 
                                  style = "color: steelblue"),
                     no = tags$i(class = "fa fa-circle-o", 
                                 style = "color: steelblue"))),
      circle = TRUE, status = "default", icon = icon("gear"),style = "unite", width = "25%"
    )
    
    E <- DT::dataTableOutput(ns("Gr_N_2"))
    #E <- rHandsontableOutput(ns("Gr_N_2"))

    
    out <- list(A,B,br(),C,D,E)
    
    return(out)
    
    }
    
  })
  
  
  output$download <- downloadHandler(
    filename = paste0("Category",".html"),
    content=function(file){ 
      
      tempReport <- file.path(tempdir(),"hist.Rmd")
      file.copy("report/hist.Rmd",tempReport,overwrite = TRUE)
      tempImage <- file.path(tempdir(),"Logo.png")
      file.copy("report/Logo.png",tempImage,overwrite = TRUE)
      if(table_ind$a==1)
      params <- list(n = Reac_Hg_final(),m= React_out_table_l())
      if(table_ind$a==2)
      params <- list(n = React_GrCat_final(),
                     m= React_out_table_r())
      rmarkdown::render(tempReport,output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))
    }
  )
  
  
}
