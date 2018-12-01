M0_ProgUI <- function(id){
  
  ns <- NS(id)
  
  tabPanel(title = div(class="tabPanel--font-size center",
                       "پیشرفت"),
           icon=icon("arrow-circle-up",class="tabPanel-icon"),
           
           #tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #485167}")),         

           
fluidRow(
div(style="text-align:center;",
column(width = 2, 
br(),  
box(width="100%",status="primary",
            
    wellPanel(

                    div(class="numeric-box--general__Pr",
                        uiOutput(ns("Pr_numI"))),

                    div(class="numeric-box--general__Pr",
                        uiOutput(ns("Pr_bin2"))),
                    
                    #div(class="action-button--general--left", #action-button--mtop__Pr action-button--mleft__Pr",
                        actionBttn(inputId = ns("DT_AC3"),style = "jelly",color = "warning",
                              label = div(class="action-button--widget","پیشرفت گروهی"))
              ),

########################################

   wellPanel(

                    #div(class="action-button--general--left",
                       #style="margin-left:-40%; margin-top:80%;",
                        actionBttn(inputId = ns("Pr_AC1"),style = "jelly",color = "warning",
                            label = div(class="action-button--widget","پیشرفت فردی"))
             
           )
  ))),

column(width = 10,
br(),
box(status="primary",width="100%",collapsible = TRUE,collapsed = FALSE,
    uiOutput(ns("output"))       
    )
     ))

)
             
  }







######################
#
# Server Logic
#
######################

M0_Prog <- function(input,output,session,Vals,font_plot){
  
  
  ns <- session$ns  
  
  ch_opt <- list(content = c("<div> </div>"))
  
  Data <- reactive({
    M <- Vals[["now"]]
    rownames(M) <- Vals[["names"]]
    colnames(M) <- Vals[["dates"]]
    return(M)
  })
  

  output$Pr_numI <- renderUI({
    if(is.null(Data())){
      ch <- ""
      pickerInput(ns("Pr_numI"),label = "میانگین وزنی",choices = ch,
                  selected=1,
                  options = list(style = "btn"),
                  choicesOpt = ch_opt)
    } else{
      ch <- 1:ncol(Data())
      pickerInput(ns("Pr_numI"),label = "میانگین وزنی",choices = ch,
                  selected=1,
                  options = list(style = "btn"))
    } 
    
  })
  
  
  ch_bin <- reactive({
    d <- group_mean()
    return(length(unique(d[,1])))
  })
  
  output$Pr_bin2 <- renderUI({
    if(is.null(Data())) {
      ch <- ""
      ch_select <- ""
      pickerInput(ns("Pr_bin2"),label = "تعداد گروه",choices = ch,selected = ch_select,
                  options = list(
                    style = "btn"),
                  choicesOpt = ch_opt)
    }else{
      ch <- 1:min(ch_bin(),5)
      ch_select <- min(2,ch_bin())
      pickerInput(ns("Pr_bin2"),label = "تعداد گروه",choices = ch,selected = ch_select,
                  options = list(style = "btn"))
    }
    
  })
  
  
  ## Variable (like trigger) for selecting which React_DT should be shown in output
  var = reactiveValues(a = 1)
  
  observeEvent(input$Pr_AC1, {
    var$a = 1
  })
  
  # observeEvent(input$Pr_AC2, {
  #   var$a = 2
  # })
  
  observeEvent(input$DT_AC3, {
    var$a = 3
  })
  
 
##  
  
  
  React_Pr1 <- eventReactive(input$Pr_AC1, {

    validate(
      need(!is.null(Data()),"هنوز داده ای وارد نشده است"),errorClass = "Hist_l"
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
        col="#fa8880"
      }
    }else{
      lab <- c("پسرفت","پیشرفت")
      col <- c("#fa8880","#00BFC4")
    }
    
    
    
    s1 <- ggplot(slope1,aes(x = reorder(names,sl),y = 100*sl))+
      geom_bar(data = slope1,stat="identity",aes(fill=factor(clr,labels = lab)),colour="black")+
      labs(title="پیشرفت فردی دانش آموزان",x="",y="درصد پیشرفت خطی",fill="")+
      geom_text(data=slope1,aes(x = names,y = 100*sl,label= lapply(round(100*sl,1),FUN=function(x){paste0(x,"%")})),
                position = position_stack(vjust = 0.5),color="black",size=4.5)+
      theme(axis.text.x = element_text(size=11,colour="black",angle=0, hjust=1,vjust=.5),
            axis.text.y = element_text(size=9,colour="black"),
            axis.title=element_text(size=14,face="bold"),
            plot.title = element_text(size=14,face="bold"),
            legend.title = element_text(size=12,face="bold"),
            legend.text=element_text(size=12),
            text=element_text(family=font_plot))+
            scale_fill_manual(values=col)+
            coord_flip()
    
    gg1 <- ggplotly(s1) %>% config(displaylogo = FALSE,collaborate = FALSE,
                                   modeBarButtonsToRemove = list(
                                     'zoom2d','pan2d','select2d','lasso2d','zoomIn2d','zoomOut2d',
                                     'sendDataToCloud',
                                     'autoScale2d',
                                     'hoverClosestCartesian',
                                     'hoverCompareCartesian'
                                   ))
    gg1
    
    })
  
  #########
  
  group_mean <- reactive({
    
    if(!is.null(input$Pr_numI)){
      if(input$Pr_numI != ""){
        numI <- input$Pr_numI
      }else{
        numI <- 1
      }
    
    if(numI==1)
      gr <- rep(1,dim(Data())[2])
    else
      gr <- as.numeric(cut(1:dim(Data())[2],breaks = numI,labels = 1:numI))
    
    d <- as.data.frame(apply(Data(),1,function(x){weighted.mean(x,gr)}))
    d$names <- rownames(Data())
    colnames(d) <- c("mean.w","names")
    #d <- d[order(d$mean.w,decreasing = T),]
    d[,1] <- round(d[,1],2)
    return(d)}
  
    })
  
  
  #########
  
  React_DT3 <-eventReactive(input$DT_AC3, {
    
    
    validate(
      need(!is.null(Data()),"هنوز داده ای وارد نشده است"),errorClass = "Hist_l"
    )
    
    bin2 <- as.numeric(input$Pr_bin2)
    numI <- as.numeric(input$Pr_numI)
    
    melt_Data_Hg <- as.data.frame(group_mean())[,2:1]
    colnames(melt_Data_Hg) <- c("Student","value")
    melt_Data_Hg <- melt_Data_Hg[order(melt_Data_Hg$value,decreasing = F),]
      
    gr_num <- 1
    c <- 1
    count <- dim(melt_Data_Hg)[1]
   
    if(bin2 > 1){
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

     splt=split(melt_Data_Hg[,2], rep(1:length(count), count))
     cc1 <- rev(colorRampPalette(c("#00BFC4","#D8AE47","#F7786B"))(gr_num))

     melt_Data_Hg$clr <- NA
     gr_names <- rep(list(NA),gr_num)


     #? break/cut insted of for(){}
     if(gr_num==1){
       gr_names <- melt_Data_Hg[,1]
     }else{
       if(gr_num==2){
         gr_names[[1]] <- melt_Data_Hg[1:dup_count[1],1]
         gr_names[[2]] <- melt_Data_Hg[(dup_count[1]+1):tail(cum_count,1),1]
       }else{
          gr_names[[1]] <- melt_Data_Hg[1:dup_count[1],1]
          for(i in 2:(gr_num-1)){
            gr_names[[i]] <- melt_Data_Hg[(dup_count[i-1]+1):dup_count[i],1]
          }
          gr_names[[gr_num]] <- melt_Data_Hg[(dup_count[length(dup_count)]+1):tail(cum_count,1),1]
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
    
    
    f <- list(
      #family = "Courier New, monospace",
      size = 16,
      weight = 'bold',
      color = "black")
    
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


    
    ##################
    
    # group_names <- as.data.frame(matrix(NA,max(count),length(splt)))
    # colnames(group_names) <- sapply(length(splt):1, function(x){paste("دسته",x)})
    # rownames(group_names) <- 1:max(count)

    # for(i in 1:length(splt)){
    #   group_names[1:length(splt[[i]]),i] <- melt_Data_Hg[melt_Data_Hg[,2] %in% splt[[i]],1]
    # }


    if(gr_num==1){
      Gr_names <- as.data.frame(gr_names)
      #colnames(Gr_names) <- "گروه ۱"
    }else{
      Gr_names <- as.data.frame(matrix(NA,nrow = max(sapply(gr_names,length)),ncol = gr_num ))
      #colnames(Gr_names) <- sapply(gr_num:1, function(x){paste("گروه",x)})
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

    #slope$clr <- melt_Data_Hg[,3]
     #slope$clr <- "green"
     slope$clr <- NA
     colnames(slope) <- c("sl","names","clr")
     #slope[which(slope$sl>0),3] <- "red"

    if(gr_num==1){
      colnames(Gr_names) <- "گروه ۱"
    }else{
      Gr_names <- Gr_names[,order(slope$sl,decreasing = F)]
      colnames(Gr_names) <- sapply(gr_num:1, function(x){paste("گروه",x)})
      }

     ylab_names <- rep(list("NA"),gr_num)
     
     for(i in 1:gr_num){
       ylab_names[[i]] <- paste(Gr_names[which(Gr_names[,i]!="NA"),i],collapse = " , ")
     }
    
    ylab_gr <- sapply(gr_num:1, function(x){
      paste("گروه",x)
    })
    
    
    ylab_full <- rep(list("NA"),gr_num)

    if(gr_num==1){
      ylab_full <- "گروه ۱ : تمام دانش آموزان"
    }else{
      for(i in 1:gr_num){
        if(length(Gr_names[which(Gr_names[,i]!="NA"),i]) > 5) 
          ylab_full[[i]] <- paste(ylab_gr[i])
        else
          ylab_full[[i]] <- paste(ylab_gr[i],":  ",ylab_names[[i]])
      }}
    

    #YLAB <- gsub(,paste(ylab_names,collapse = "--"))
    #print("YLAB")
    #print(ylab_names,15)
    
    # if(length(unique(slope[,3]))==1){
    #   if(slope[,3]=="red"){
    #     lab="پیشرفت"
    #     col="#00BFC4"
    #   }
    #   else{
    #     lab="پسرفت"
    #     col="#F8766D"
    #   }
    # }else{
    #   lab <- c("پسرفت","پیشرفت")
    #   col <- c("#F8766D","#00BFC4")
    # }
    
    #ylab_names <- ylab_names[order(slope$sl,decreasing = F)]
    slope <- slope[order(slope$sl,decreasing = F),]    
    
    for(i in 1:gr_num){
      slope[i,2] <- ylab_full[[i]]
    }

    
    g <- c("#00BFC4","#7ce1e9","#9ee9ef","#bff1f4","#e1f8fa")
    r <- c("#fa8880","#fbaba5","#fcbdb8","#fee0de","#fef2f1")
    #r <- rev(c("blue","pink","red","yellow","white"))
    
    slope[,3] <- rev(g[1:gr_num])
    ind_neg <- which(slope[,1] < 0)
    
    if(length(ind_neg) > 0){
    slope[1:length(ind_neg),3] <- (r[1:length(ind_neg)])
    }
    
    lab_r <- lapply(1:length(slope[,1]),FUN=function(x){paste("گروه",x,": پسرفت")})
    lab_g <- lapply(1:length(slope[,1]),FUN=function(x){paste("گروه",x,": پیشرفت")})
    

    lab_legend_bar <- lab_g[1:gr_num]
    if(length(ind_neg) > 0)
    lab_legend_bar[(gr_num-length(ind_neg)+1):gr_num] <- lab_r[(gr_num-length(ind_neg)+1):gr_num]
    
    lab_legend_bar <- lab_legend_bar
    
    print(slope)
    print(lab_legend_bar)
    
    #r <- c("red","blue","green","yellow","black")
    # length_slp <- (length(which(slope$sl >= 0)))
    # length_sln <- (length(which(slope$sl < 0)))
    # 
    # col <- rev(g[1:gr_num])
    # if(length_sln > 0){
    # col[1:length_sln] <- r[1:length_sln]
    # }
    # 
    # slope[,3] <- col
    # 
    # 

     
     
    # #print(lab_legend_bar)
     # if(length_sln > 0)
     # lab_legend_bar[(length_sln+1):gr_num] <- lab_r[(length_sln+1):gr_num]
    
    #print(lab_legend_bar)
    
    
    
    
    #lab <- rev(lab)
    
    #ylab_names <- 1:gr_num
    #levels(birds$effect) <- gsub(" ", "\n", levels(birds$effect))

    # print("YLAB")
    # print(ylab_names)
    
    # print(slope)
    # print(col)
   
    # label_legend <- rep(NA,gr_num)
    # for(i in 1:gr_num){
    #   if(slope[i,1] >= 0)
    #     label_legend <- paste("گروه",i,"پیشرفت")
    #   else
    #     label_legend <- paste("گروه",i,"پسرفت")
    # }

    
    # col <- rev(slope[,3])
    # col[length(which(slope$sl < 0 )):length(slope[,3])] <- rev(col[length(which(slope$sl < 0 )):length(slope[,3])])
    
    col <- rev(slope$clr)
    #names(col) <- 1:length(col)
    gg_slope_gr <- ggplot(slope,aes(x=reorder(names,sl),y = round(100*sl,1)))+
      geom_bar(stat="identity",fill=slope$clr,color="black")+ #  aes(fill=factor(clr,labels=lab_legend_bar)),color="black")+
      #scale_fill_manual(values = factor(col))+    # filling geom_bar with colors
      #scale_fill_brewer(palette="blues")+
      labs(title="پیشرفت گروهی دانش آموزان",x="",y="درصد پیشرفت خطی",fill="")+
      geom_text(data=slope,aes(x = reorder(names,round(100*sl,1)),y = round(100*sl,1),
                               label= lapply(round(100*sl,1),FUN=function(x){paste0(x,"%")})),
                position = position_stack(vjust = 0.5),color="black",size=4.5)+
      #scale_x_discrete(labels=ylab_full)+
      theme(axis.text.x = element_text(size=11,colour="black",angle=0, hjust=1,vjust=.5),
            axis.text.y = element_text(size=9,colour="black"),
            axis.title=element_text(size=14,face="bold"),
            plot.title = element_text(size=14,face="bold"),
            legend.title = element_text(size=12,face="bold"),
            legend.text=element_text(size=12),
            text=element_text(family=font_plot))+
            #aes(stringr::str_wrap(YLAB, 15))+
      #aes(fill = clr)+
      #scale_fill_manual(values= slope$clr,guide = guide_legend(title = "",size=20))+
      coord_flip()
    
    gg_slope_gr <- ggplotly(gg_slope_gr) %>% config(displaylogo = FALSE,collaborate = FALSE,
                                                    modeBarButtonsToRemove = list(
                                                      'zoom2d','pan2d','select2d','lasso2d','zoomIn2d','zoomOut2d',
                                                      'sendDataToCloud',
                                                      'autoScale2d',
                                                      'hoverClosestCartesian',
                                                      'hoverCompareCartesian'
                                                    ))
    
    
    
    out <- list(sl = slope$sl ,gg_slope_gr = gg_slope_gr,gr_names=Gr_names, color = slope$clr)
    
    return(out)    
    
  })
  
  

  #  D_Table <- reactive({
  #    
  #   n <- dim(React_DT3()$group_names)[2]
  #   column <- 1:n
  #   color <- rep('black',n)
  #   backColor <- rep("#00BFC4",n)
  #   backColor[which(React_DT3()$sl < 0)] <- "#F8766D" 
  #   #backColor <- React_DT3()$color_count
  #   font <- rep('bold',n)
  # 
  #   DT <- datatable(React_DT3()$group_names,
  #                   options = list(
  #                     pageLength = 10, orderClasses = TRUE,
  #                     searching = FALSE, paging = FALSE
  #                   ))
  # 
  #   for(i in 1:n){
  #     DT <- DT %>%
  #       formatStyle(column[i],  color = color[i], backgroundColor = backColor[i], fontWeight = font[i])
  #   }
  # 
  #   return(DT)
  # 
  # })



  D_Table2 <- reactive({

    n <- dim(React_DT3()$gr_names)[2]
    column <- 1:n
    color <- rep('black',n)
    backColor <- React_DT3()$color
    # backColor <- rep("#00BFC4",n)
    # if(length(which(React_DT3()$sl < 0)) > 0 )
    # backColor[which(React_DT3()$sl < 0)] <-  "#F8766D"
    #backColor <- React_DT3()$cc1
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


  
  

# React_out_table <- reactive({
# 
#     if(input$table=="D"){
#       return(D_Table())
#     }else{
#       return(D_Table2())
#     }
# 
#   })
  
  output$Gr_N <- NULL

  observeEvent(input$DT_AC3,{
  output$Gr_N <- DT::renderDataTable( D_Table2() )
  })

  observeEvent(input$Pr_AC1,{
    output$Gr_N <- NULL
  })

  # output$table <- renderUI({
  #   A <- radioButtons(inputId = ns("table"),label = "",choices = c("گروه"="G","دسته"="D"),
  #                selected = "G",inline = TRUE)
  # })
  
  React_GrCat <- reactive({
    A <- subplot(React_DT3()$gg_slope_gr,shareX = FALSE,shareY = FALSE,
                 titleX=TRUE) %>% layout(showlegend = TRUE)
    return(A)

  })
  
  
  
  ### selecting which React_DT want to show in output
  React_out <- reactive({
    
    if(var$a==1){        # Does not need have () for input$x .... I mean, input$x() is wrong.
      return(React_Pr1())   # return is important here. Without it does not work
    }
    
    if(var$a==3){
      return(React_GrCat())
    }
    
  })
  ###
  
  output$Pr <- renderPlotly(React_out())
 
  
  out_ind <- reactiveValues(a=0)
  
  observeEvent(input$DT_AC3, {
    out_ind$a = 1
  })
  
  observeEvent(input$Pr_AC1, {
    out_ind$a = 2
  })
  
  output$output <- renderUI({
    
    if(out_ind$a==1){
    
    A <- div(style="text-align:right",downloadBttn(ns("download"),
                                   label = "دانلود",size = "sm"))
      
    B <- withSpinner(plotlyOutput(ns("Pr")),type=5,color = "#006E6D",size = 0.6)
    
    C <- withSpinner( DT::dataTableOutput(ns("Gr_N")),type=5,color = "#006E6D",size = 0.4)
    
    M <- list(A,B,C)
    
    return(M)
    }
    if(out_ind$a==2){
      
      A <- div(style="text-align:right",downloadBttn(ns("download"),
                                     label = "دانلود",size = "sm"))
      
      B <- withSpinner(plotlyOutput(ns("Pr")),type=5,color = "#006E6D",size = 0.6)
      
      M <- list(A,B)
      
      return(M)
    }
    
    
  })
  
  
  
  output$download <- downloadHandler(
    filename = paste0("پیشرفت",".html"),
    content=function(file){ 
      
      tempReport <- file.path(tempdir(),"progress.Rmd")
      file.copy("report/progress.Rmd",tempReport,overwrite = TRUE)
      tempImage <- file.path(tempdir(),"logogrey.svg")
      file.copy("report/logogrey.svg",tempImage,overwrite = TRUE)
      if(out_ind$a==1)
        params <- list(n = React_out(),m= D_Table2())
      if(out_ind$a==2)
        params <- list(n = React_out(),m=NULL)

      rmarkdown::render(tempReport,output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))
    }
  )
  
  
  
   
}

