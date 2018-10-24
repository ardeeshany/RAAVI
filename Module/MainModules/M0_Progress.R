M0_ProgUI <- function(id){
  
  ns <- NS(id)
  
  tabPanel(title = div(class="tabPanel--font-size center",
                       "پیشرفت"),
           icon=icon("arrow-circle-up",class="tabPanel-icon"),
           
           tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #485167}")),         
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
    fluidRow(
    
    column(2,
    div(class="inline",style="text-align:center;",
    sliderInput(inputId = ns("slider_width"),min = 15,max = 75,value = 30,label="مقیاس نمودارها"))),
    
    column(1,offset = 1,
    div(class="inline check-box--general check-box--font-size", style="margin-top:48%; margin-left:40%;size:50%",
        checkboxInput(inputId = ns("density"),label = "توزیع",value = TRUE)))
    
    ),
    br(), 
    fluidRow(
    withSpinner(plotlyOutput(ns("Pr")),type=5,color = "#006E6D",size = 0.6)
    ),
    fluidRow(
        uiOutput(ns("table")),
      
      tags$div(
        tags$table(
          withSpinner( DT::dataTableOutput(ns("Gr_N")),type=5,color = "#006E6D",size = 0.4)
        ))
    )
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
    if(is.null(Data())) ch <- ""
    else ch <- 1:ncol(Data())
    selectInput(ns("Pr_numI"),label = "میانگین وزنی",choices = ch )
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

  

  
  
  ## Variable (like trigger) for selecting which React_DT should be shown in output
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

    validate(
      need(!is.null(Data()),"هنوز داده ای وارد نشده است"),errorClass = "Pr_right"
    )
    
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
    
    
    
    s1 <- ggplot(slope1,aes(x = reorder(names,sl),y = 100*sl))+
      geom_bar(data = slope1,stat="identity",aes(fill=factor(clr,labels = lab)),colour="black")+
      labs(title="پیشرفت فردی دانش آموزان",x="",y="درصد پیشرفت خطی",fill="")+
      geom_text(data=slope1,aes(x = names,y = 100*sl,label= lapply(round(100*sl,1),FUN=function(x){paste0(x,"%")})),
                position = position_stack(vjust = 0.5),color="black",size=4.5)+
      theme(axis.text.x = element_text(size=11,colour="black",angle=0, hjust=1,vjust=.5),
            axis.text.y = element_text(size=12,colour="black"),
            axis.title=element_text(size=14,face="bold"),
            plot.title = element_text(size=14,face="bold"),
            legend.title = element_text(size=12,face="bold"),
            legend.text=element_text(size=12),
            text=element_text(family="dastnevis"))
      #coord_flip()
    
    gg1 <- ggplotly(s1)
    gg1
    
    })
  
  #########
  
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
  
  
  
  
  
  
  React_Pr2 <-eventReactive(input$Pr_AC2, {
    
    
    validate(
      need(!is.null(Data()),"هنوز داده ای وارد نشده است"),errorClass = "Pr"
    )
   
    
    bin2 <- as.numeric(input$Pr_bin2)
    numI <- as.numeric(input$Pr_numI)
    
      d <- group_mean()
      names_ch <- rep(list("NA"),bin2)
      Data_T <- rep(list("NA"),bin2)
      if(bin2==1)
      ind <- list(1:nrow(Data()))
      else
      ind <- split(1:nrow(Data()), as.vector(cut(1:dim(Data())[1],breaks = bin2,labels = 1:bin2)))
    
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
        
   s <- ggplot(slope,aes(x=reorder(names,1:bin2), y = round(100*sl,1)))+
        geom_bar(stat="identity",aes(fill=factor(clr,labels = lab)),color="black")+
        scale_fill_manual(values=col)+    # filling geom_bar with colors
        labs(title="پیشرفت گروهی دانش آموزان",x="",y="درصد پیشرفت خطی",fill="")+
        geom_text(data=slope,aes(x = reorder(names,1:bin2),y = round(100*sl,1),
              label= lapply(round(100*sl,1),FUN=function(x){paste0(x,"%")})),
              position = position_stack(vjust = 0.5),color="black",size=4.5)+
           scale_x_discrete(labels= ylab_names[bin2:1])+
           theme(axis.text.x = element_text(size=11,colour="black",angle=0, hjust=1,vjust=.5),
           axis.text.y = element_text(size=12,colour="black"),
           axis.title=element_text(size=14,face="bold"),
           plot.title = element_text(size=14,face="bold"),
           legend.title = element_text(size=12,face="bold"),
           legend.text=element_text(size=12),
           text=element_text(family="dastnevis"))
        
      gg <- ggplotly(s)

    
    return(gg)
    
  })  
  
  
  
  #########
  
  React_DT3 <-eventReactive(input$DT_AC3, {
    
    
    validate(
      need(!is.null(Data()),"هنوز داده ای وارد نشده است"),errorClass = "Pr"
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
     
     
     
    lab_legend <- sapply(1:gr_num, function(x){
      paste("گروه",x)
    })
    

    gg_bar <- ggplot(melt_Data_Hg,aes(x = reorder(Student,value),y = value))+
      geom_bar(stat="identity",aes(fill=factor(clr,labels=lab_legend)),color="black")+
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
    
    
    gg_bar <- ggplotly(gg_bar) %>% layout(annotations = text_bar)
    
########
    
    gg_hist <- ggplot(melt_Data_Hg,aes(value)) +
      geom_histogram(aes(y = round(..count../(0.01*sum(..count..)),1),
                    fill=factor(clr,labels=lab_legend)), bins=c,
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
    

    
    text_hist <- list(
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


    lab_hist <- function(x){
      if(x==0) return("")
      else return(paste0(x,"%"))
    }

    if(gr_num > 3){
      size_text <- 3
    }else{
      size_text <- 4.8
    }
    
    if(input$density==TRUE){
      gg_hist <- gg_hist + geom_density(aes(y=100*..density..),alpha=0.3, fill="#9B2335",colour="#9B2335",lwd=1.5)+
        stat_bin(aes(label=lapply(round(..count../(0.01*sum(..count..)),1),FUN= lab_hist)),geom="text",
                 bins = c,color="black", size=size_text)
    }else{
      gg_hist <- gg_hist + stat_bin(aes(label=lapply(round(..count../(0.01*sum(..count..)),1),FUN=lab_hist)),geom="text",
                       bins = c,color="black", size=size_text)
    }


    gg_hist <- ggplotly(gg_hist) %>% layout(annotations = text_hist) 
    
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
    
    
    
    
##############################    
##############################      
##############################      
    
    Data_T <- rep(list("NA"),gr_num)
    for(i in 1:gr_num){
      Data_T[[i]] <- Data()[rownames(Data()) %in% Gr_names[,i],]
      if(length(which(Gr_names[,i]!="NA"))==1){
        Data_T[[i]] <- t(Data_T[[i]])
      }
    }

    fit_tot <- rep(list("NA"),gr_num)
    slope <- rep(NA,gr_num)  
    
    
    for(i in 1:gr_num){  
      Mean <- apply(Data_T[[i]],2,mean)
      Mean <- melt(as.matrix(Mean))[,-2]
      colnames(Mean) <- c("Day","value")
      Mean$Day <- 1:ncol(Data())
      fit_tot[[i]] <- lm(value~Day, data=Mean)
      slope[i] <- coef(fit_tot[[i]])[2]
    }
    
    slope <- as.data.frame(slope)
    slope$names <- 1:gr_num
    slope$clr <- "green"
    colnames(slope) <- c("sl","names","clr")
    slope[which(slope$sl>0),3] <- "red"

    ylab_names <- rep(list("NA"),gr_num)

    for(i in 1:gr_num){
      ylab_names[[i]] <- paste(Gr_names[which(Gr_names[,i]!="NA"),i],sep = "\n")
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
    
    gg_slope_gr <- ggplot(slope,aes(x=reorder(names,1:gr_num), y = round(100*sl,1)))+
      geom_bar(stat="identity",aes(fill=factor(clr,labels = lab)),color="black")+
      scale_fill_manual(values=col)+    # filling geom_bar with colors
      labs(title="پیشرفت گروهی دانش آموزان",x="",y="درصد پیشرفت خطی",fill="")+
      geom_text(data=slope,aes(x = reorder(names,1:bin2),y = round(100*sl,1),
                               label= lapply(round(100*sl,1),FUN=function(x){paste0(x,"%")})),
                position = position_stack(vjust = 0.5),color="black",size=4.5)+
      scale_x_discrete(labels= ylab_names[bin2:1])+
      theme(axis.text.x = element_text(size=11,colour="black",angle=0, hjust=1,vjust=.5),
            axis.text.y = element_text(size=12,colour="black"),
            axis.title=element_text(size=14,face="bold"),
            plot.title = element_text(size=14,face="bold"),
            legend.title = element_text(size=12,face="bold"),
            legend.text=element_text(size=12),
            text=element_text(family="dastnevis"))
    
    gg_slope_gr <- ggplotly(gg_slope_gr)
    
    
    
    out <- list(gg_bar=gg_bar,gg_hist=gg_hist, gg_slope_gr = gg_slope_gr, group_names=group_names,gr_names=Gr_names,color_count = color_count ,cc1 = cc1)
    
    return(out)    
    
  })
  
  

  D_Table <- reactive({
    
    
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
  
  
  
  
  D_Table2 <- reactive({
    
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
  
React_out_table <- reactive({
    
    if(input$table=="D"){
      return(D_Table())
    }else{
      return(D_Table2())
    }
    
  })
  
  output$Gr_N <- NULL
  
  observeEvent(input$DT_AC3,{
  output$Gr_N <- DT::renderDataTable( React_out_table() )
  })
  
  observeEvent(input$Pr_AC1,{
    output$Gr_N <- NULL
  })
  
  output$table <- renderUI({
    A <- radioButtons(inputId = ns("table"),label = "",choices = c("گروه"="G","دسته"="D"),
                 selected = "G",inline = TRUE)
  })
  
  React_GrCat <- reactive({
    A <- subplot(React_DT3()$gg_bar,React_DT3()$gg_hist,React_DT3()$gg_slope_gr,shareX = FALSE,shareY = FALSE,
                 titleX=TRUE) %>% layout(showlegend = FALSE)
    return(A)

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
      return(React_GrCat())
    }
    
  })
  ###
  
  output$Pr <- renderPlotly(React_out())  
  
}

