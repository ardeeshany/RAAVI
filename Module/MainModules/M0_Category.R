
M0_CatUI <- function(id){
  
  ns <- NS(id)
  
  tabPanel(title = div(class="tabPanel--font-size center",
                       "فیلتر"),
           icon = icon("filter",class="tabPanel-icon"),
                     
           fluidRow(                
             
div(style="text-align:center;",
column(width = 2, 
br(),  
box(width="100%",status="primary",   
    
    # wellPanel(
    # 
    #     uiOutput(ns("DT_bin")),
    # 
    #     br(),
    # 
    #     div(style="text-align:left;",
    #         prettyRadioButtons(
    #           inputId = ns('DT_chb1'),inline = T,
    #           label = "تنظیمات جدول",
    #           choices = c("نام گروه"="name","نمره"="grade"),
    #           selected = "grade"
    #         )),
    # 
    #     actionBttn(inputId = ns("DT_AC1"),style = "jelly",color = "warning",
    #     label = div(class="action-button--widget","گروه بندی"))
    # 
    # ),
  
      
    wellPanel(
      
      noUiSliderInput(inputId = ns("DT_sl"),label = "",
                      color = "#578CA9",
                      orientation = "vertical",
                      direction = "rtl",height = "270px",
                      min = 0,max = 20,value = c(0,20),step = 0.25), 

            actionBttn(inputId = ns("DT_AC2"),style = "jelly",color = "warning",
            label = div(class="action-button--widget", "جدول نمرات"))
             
          )
    
))),


column(width = 10, 
br(),  
box(width="100%",status="primary",
    
  uiOutput(ns("full_out"))


))


)
)

}








######################
#
# Server Logic
#
######################

# Group_name_iso <- rep("NA",20)
# for(i in 1:20){
#   Group_name_iso[i] = paste("گروه",i,sep="")
# }

M0_Cat <- function(input,output,session,Vals,font_plot){

  ns <- session$ns  
  
  ch_opt <- list(content = c("<div> </div>"))
  
  Data <- reactive({
    M <- Vals[["now"]]
    rownames(M) <- Vals[["names"]]
    colnames(M) <- Vals[["dates"]]
    return(M)
  })  
  
  
  
  
  # output$DT_bin <- renderUI({
  #   if(is.null(Data())) {
  #     ch <- ""
  #     ch_select <- ""
  #     pickerInput(inputId = ns("DT_bin"),label = "تعداد بخش" ,choices = ch,
  #                 selected =ch_select,
  #                 options = list(style = "btn"),
  #                 choicesOpt = ch_opt)
  #   }else{
  #     ch <- 1:5
  #     ch_select <- 1
  #     pickerInput(inputId = ns("DT_bin"),label = "تعداد بخش",choices = ch,
  #                 selected =ch_select,
  #                 options = list(style = "btn"))
  #   }
  # })
  
  
  
  
  
    
  # output$DT_numI <- renderUI({
  #   numericInput(ns("DT_numI"),label = "میانگین وزنی",min = 1,max=length(colnames(Data())),value = 1,width = "85px")
  # })
  
   # Mean <- reactive({ apply(Data(),2,mean)  })
   # 
   #  Group_name <- reactive({
   #    if(is.null())
   #    Group_name_iso[1:input$DT_bin]  
   #    })
  

      
    melt_Data_DT <- reactive({
    d <- as.data.frame(Data())
    d <- melt(as.matrix(d))
    
    # if(input$DT_bin==1)
    # d$Group <- as.vector(cut(d$value,c(0,20),labels = Group_name()))  
    # else
    # d$Group <- as.vector(cut(d$value,input$DT_bin,labels = Group_name()))
    # colnames(d) <- c("Student","Day","value","Group")
    
    colnames(d) <- c("Student","Day","value")
    d
  })


## Variable (like trigger) for selecting which React_DT want to show in output
#var = reactiveValues(a = 0)

# observeEvent(input$DT_AC1, {
#   var$a = 1
# })

# observeEvent(input$DT_AC2, {
#   var$a = 2
# })

# observeEvent(input$DT_AC3, {
#   var$a = 3
# })

##


# React_DT1 <-eventReactive(input$DT_AC1, {
# 
#  
#   validate(
#     need(!is.null(Data()),"هنوز داده ای وارد نشده است"),errorClass = "Cat_left"
#   )
#   
#   if(input$DT_chb1=="name"){
#     lab_m <- reactive(melt_Data_DT()$Group)
#   }
#   else{
#     lab_m <- reactive(round(melt_Data_DT()$value,2))
#   }  
#   
#     if(input$DT_bin==1){
#       
#       p <- ggplot(melt_Data_DT(), aes(Day,Student))+
#       geom_raster(aes(fill=cut(value,c(0,20),include.lowest = T)))+   # Color
#       geom_text(aes(label = lab_m() ))+ # Show values
#       labs(title ="نمودار دمایی", x = "زمان", y = "دانش آموزان")+
#       guides(fill=guide_legend(title="تقسیم بندی نمره ها"))+
#       scale_fill_manual(values="springgreen4") 
#     }else{
#       
#     cc1 <- reactive(colorRampPalette(c("sienna3","khaki3","palegreen4"))(input$DT_bin))
#     
#     p <- ggplot(melt_Data_DT(), aes(Day,Student))+
#       geom_raster(aes(fill=cut(value,input$DT_bin)))+   # Color
#       geom_text(aes(label= lab_m()))+ # Show values
#       labs(title ="نمودار دمایی", x = "زمان", y = "دانش آموزان")+
#       guides(fill=guide_legend(title="تقسیم بندی نمره ها"))+
#       scale_fill_manual(values=cc1())
#     }
#   
#     gg <- ggplotly(p,tooltip = ""           #for showing a subset on each point in plot
#     )
#     
#     
#     gg
#     
#   })



# React_DT2 <-eventReactive(input$DT_AC2,{
# 
#   validate(
#     need(!is.null(Data()),"هنوز داده ای وارد نشده است"), errorClass = "Hist_l"
#   )
#   
#   if((input$DT_sl[2] < min(melt_Data_DT()$value)) || (input$DT_sl[1] > max(melt_Data_DT()$value))){
#     lab <- reactive(paste("[",input$DT_sl[1],",",input$DT_sl[2],"]",sep=""))
#     p <- ggplot(melt_Data_DT(), aes(Day,Student))+
#       geom_raster(aes(fill=cut(value,c(-1,21),labels=lab())))+   # Color
#       geom_text(aes(label= round(value,2)))+ # Show values
#       labs(title ="نمره دانش آموزان در طول زمان", x = "", y = "")+
#       guides(fill=guide_legend(title="تقسیم بندی نمره ها"))+
#       scale_fill_manual(values="dimgrey")+
#       theme(axis.text.x = element_text(size=11,colour="black",angle=60, hjust=1,vjust=.5),
#             axis.text.y = element_text(size=12,colour="black"),
#             axis.title=element_text(size=14,face="bold"),
#             plot.title = element_text(size=14,face="bold"),
#             legend.title = element_text(size=12,face="bold"),
#             text=element_text(family="dastnevis"))
#   }
#   else{ if (input$DT_sl[1] == input$DT_sl[2]) {
#     lab <- reactive(c(paste("[0,",input$DT_sl[1],")",sep=""),paste(input$DT_sl[1]),paste("(",input$DT_sl[1],",",20,"]",sep="")))
#     p <- ggplot(melt_Data_DT(), aes(Day,Student))+
#       geom_raster(aes(fill=cut(value,c(input$DT_sl[2]-10^-4,input$DT_sl[2]),include.lowest = T,labels=lab)))+   # Color
#       geom_text(aes(label= round(value,2)))+ # Show values
#       labs(title ="نمودار دمایی", x = "زمان", y = "دانش آموزان")+
#       guides(fill=guide_legend(title="تقسیم بندی نمره ها"))+
#       scale_fill_manual(values=c("firebreack","dimgray"))+
#       theme(axis.text.x = element_text(size=11,colour="black",angle=60, hjust=1,vjust=.5),
#             axis.text.y = element_text(size=12,colour="black"),
#             axis.title=element_text(size=14,face="bold"),
#             plot.title = element_text(size=14,face="bold"),
#             legend.title = element_text(size=12,face="bold"),
#             text=element_text(family="dastnevis"))
#   }
#   
#   else{
#   
#     if(input$DT_sl[1]==0){
#           
#             if(input$DT_sl[2]==20){
#               lab <- reactive(c(paste("[0,20]",sep="")))
#               col <- "salmon"
#               p <- ggplot(melt_Data_DT(), aes(Day,Student))+
#               geom_raster(aes(fill=cut(value,c(-Inf,Inf),include.lowest = T,labels=lab())))+   # Color
#               geom_text(aes(label= round(value,2)))+ # Show values
#               labs(title ="نمودار دمایی", x = "زمان", y = "دانش آموزان")+
#               guides(fill=guide_legend(title="تقسیم بندی نمره ها"))+
#               scale_fill_manual(values=col)+
#               theme(axis.text.x = element_text(size=11,colour="black",angle=60, hjust=1,vjust=.5),
#                       axis.text.y = element_text(size=12,colour="black"),
#                       axis.title=element_text(size=14,face="bold"),
#                       plot.title = element_text(size=14,face="bold"),
#                       legend.title = element_text(size=12,face="bold"),
#                       text=element_text(family="dastnevis"))
#                                  }
#       
#              else{
#              lab <- reactive(c(paste("[0,",input$DT_sl[2],"]",sep=""),paste("[",input$DT_sl[2],",20]",sep=""))) 
#              col <- c("salmon","dimgrey")
#              p <- ggplot(melt_Data_DT(), aes(Day,Student))+
#                geom_raster(aes(fill=cut(value,c(-Inf,input$DT_sl[2],Inf),include.lowest = T,labels=lab())))+   # Color
#                geom_text(aes(label= round(value,2)))+ # Show values
#                labs(title ="نمودار دمایی", x = "زمان", y = "دانش آموزان")+
#                guides(fill=guide_legend(title="تقسیم بندی نمره ها"))+
#                scale_fill_manual(values=col)+
#                theme(axis.text.x = element_text(size=11,colour="black",angle=60, hjust=1,vjust=.5),
#                      axis.text.y = element_text(size=12,colour="black"),
#                      axis.title=element_text(size=14,face="bold"),
#                      plot.title = element_text(size=14,face="bold"),
#                      legend.title = element_text(size=12,face="bold"),
#                      text=element_text(family="dastnevis"))
#                  }
#    
#                             }
#     else{
#             if(input$DT_sl[2]==20){
#             lab <- reactive(c(paste("[0,",input$DT_sl[1],"]",sep=""),paste("[",input$DT_sl[1],",20]",sep=""))) 
#             col <- c("dimgrey","salmon")
#             if(input$DT_sl[1] <min(melt_Data_DT()$value))
#             col <- c("salmon") 
#             
#             p <- ggplot(melt_Data_DT(), aes(Day,Student))+
#               geom_raster(aes(fill=cut(value,c(-Inf,input$DT_sl[1],Inf),include.lowest = T,labels=lab())))+   # Color
#               geom_text(aes(label= round(value,2)))+ # Show values
#               labs(title ="نمودار دمایی", x = "زمان", y = "دانش آموزان")+
#               guides(fill=guide_legend(title="تقسیم بندی نمره ها"))+
#               scale_fill_manual(values=col)+
#               theme(axis.text.x = element_text(size=11,colour="black",angle=60, hjust=1,vjust=.5),
#                     axis.text.y = element_text(size=12,colour="black"),
#                     axis.title=element_text(size=14,face="bold"),
#                     plot.title = element_text(size=14,face="bold"),
#                     legend.title = element_text(size=12,face="bold"),
#                     text=element_text(family="dastnevis"))
#                                   }
#                else{
#                  if(input$DT_sl[1]<min(melt_Data_DT()$value)){
#                    lab <- reactive(c(paste("[",input$DT_sl[1],",",input$DT_sl[2],"]",sep=""),paste("(",input$DT_sl[2],",",20,"]",sep="")))  
#                    col <- c("salmon","dimgrey")
#                    p <- ggplot(melt_Data_DT(), aes(Day,Student))+
#                      geom_raster(aes(fill=cut(value,c(-Inf,input$DT_sl[2],Inf),include.lowest = T,labels=lab())))+   # Color
#                      geom_text(aes(label= round(value,2)))+ # Show values
#                      labs(title ="نمودار دمایی", x = "زمان", y = "دانش آموزان")+
#                      guides(fill=guide_legend(title="تقسیم بندی نمره ها"))+
#                      scale_fill_manual(values=col)+
#                      theme(axis.text.x = element_text(size=11,colour="black",angle=60, hjust=1,vjust=.5),
#                            axis.text.y = element_text(size=12,colour="black"),
#                            axis.title=element_text(size=14,face="bold"),
#                            plot.title = element_text(size=14,face="bold"),
#                            legend.title = element_text(size=12,face="bold"),
#                            text=element_text(family="dastnevis"))
#                    }
#                  else{
#                    lab <- reactive(c(paste("[0,",input$DT_sl[1],"]",sep=""),paste("[",input$DT_sl[1],",",input$DT_sl[2],"]",sep=""),paste("(",input$DT_sl[2],",",20,"]",sep="")))  
#                    col <- c("dimgrey","salmon","dimgrey")
#                    p <- ggplot(melt_Data_DT(), aes(Day,Student))+
#                      geom_raster(aes(fill=cut(value,c(-Inf,input$DT_sl[1],input$DT_sl[2],Inf),include.lowest = T,labels=lab())))+   # Color
#                      geom_text(aes(label= round(value,2)))+ # Show values
#                      labs(title ="نمودار دمایی", x = "زمان", y = "دانش آموزان")+
#                      guides(fill=guide_legend(title="تقسیم بندی نمره ها"))+
#                      scale_fill_manual(values=col)+ 
#                      theme(axis.text.x = element_text(size=11,colour="black",angle=60, hjust=1,vjust=.5),
#                            axis.text.y = element_text(size=12,colour="black"),
#                            axis.title=element_text(size=14,face="bold"),
#                            plot.title = element_text(size=14,face="bold"),
#                            legend.title = element_text(size=12,face="bold"),
#                            text=element_text(family="dastnevis"))
#                    }
#                
#                     }
#     }
#   }
#   }
#   
#   gg <- ggplotly(p
#                #,tooltip=c("y","x")           for showing a subset on each point in plot
#   )
#   
#   gg
#   
# })












React_DT2 <-eventReactive(input$DT_AC2,{
  
  # validate(
  #   need(!is.null(Data()),"هنوز داده ای وارد نشده است"), errorClass = "Hist_l"
  # )
  
  # min <- input$DT_sl[1]
  # max <- input$DT_sl[2]
  # val <- melt_Data_DT()$value
  # col_on <- "#55B4B0"
  # col_off <- "dimgrey"
  # eps <- 10^(-4)
  # if((max < min(melt_Data_DT()$value)) || (min > max(melt_Data_DT()$value))){
  #   
  #   lab <- reactive(paste("[",min,",",max,"]",sep=""))
  #   fill_col <- col_off
  #   val_range <- c(0-eps,20)
  # }
  # else{
  #   
  #   if(min == max){
  #   
  #    if(!is.null(which(Data()==min))){
  #    lab <- c(paste("[0,",min,")",sep=""),paste(min),paste("(",min,",",20,"]",sep=""))
  #    fill_col <- c(col_off,col_on,col_off)
  #    val_range <- c(-Inf,min-eps,min+eps,Inf)}
  #    else{
  #      fill_col <- col_off
  #      val_range <- c(-Inf,Inf)
  #     }
  #     
  #   }
  #   else{
  #     if(min==0){ 
  #        if(max==20){
  #        lab <- c(paste("[0,20]",sep=""))
  #        fill_col <- col_on
  #        val_range <- c(-Inf,Inf)}
  #       
  #        else{
  #         lab <- c(paste("[0,",max,"]",sep=""),paste("[",max,",20]",sep=""))
  #         fill_col <- c(col_on,col_off)
  #         val_range <- c(-Inf,max,Inf)}
  #       
  #                }
  #     else{
  #       if(max==20){
  #         lab <- c(paste("[0,",min,"]",sep=""),paste("[",min,",20]",sep=""))
  #         fill_col <- c(col_off,col_on)
  #         val_range <- c(-Inf,min-eps,Inf)
  #         
  #           if(min < min(melt_Data_DT()$value))
  #           fill_col <- c(col_on) 
  #         
  #         
  #                 }
  #       
  #       else{
  #         if(min<min(melt_Data_DT()$value)){
  #           lab <- c(paste("[",min,",",max,"]",sep=""),paste("(",max,",",20,"]",sep=""))
  #           fill_col <- c(col_on,col_off)
  #           val_range <- c(-Inf,max,Inf)
  #         }
  #         
  #         else{
  #           lab <- c(paste("[0,",min,"]",sep=""),paste("[",min,",",max,"]",sep=""),paste("(",max,",",20,"]",sep=""))
  #           fill_col <- c(col_off,col_on,col_off)
  #           val_range <- c(-Inf,min-eps,max,Inf)
  #           }
  #         
  #         }}}}
  
  

  #min_val < min(melt_Data_DT()$value)
  #max_val <- max(melt_Data_DT()$value)
  
  min <- input$DT_sl[1]
  max <- input$DT_sl[2]
  eps <- 10^(-4)
  title <- paste("[",min,",",max,"]", " : نمره ی دانش آموزان در بازه ی ")
  text_col <-input$color_text
  x_size <- 10
  y_size <- 9
  fill_col <- c(paste(input$color_bg),"#838487")
  text_size <- 4.6
  
  p <- ggplot(melt_Data_DT(), aes(Day,Student))+
    geom_raster(aes(fill=cut(value,c(min-eps,max),include.lowest = T)))+   # Color
    geom_text(aes(label= round(value,2)),color = text_col,size=text_size)+ # Show values
    labs(title = title , x = "", y = "")+
    #guides(fill=guide_legend(title=title_leg))+
    scale_fill_manual(values=fill_col)+ 
    theme(axis.text.x = element_text(size=x_size,colour="black",angle=60, hjust=1,vjust=.5),
          axis.text.y = element_text(size=y_size,colour="black"),
          axis.title=element_text(size=14,face="bold"),
          plot.title = element_text(size=14,face="bold"),
          legend.title = element_text(size=12,face="bold"),
          text=element_text(family=font_plot),
          legend.position="none")
  
  gg <- ggplotly(p) %>% config(displaylogo = FALSE,collaborate = FALSE,
                              modeBarButtonsToRemove = list(
                                'zoom2d','pan2d','select2d','lasso2d','zoomIn2d','zoomOut2d',
                                'sendDataToCloud',
                                'autoScale2d',
                                'hoverClosestCartesian',
                                'hoverCompareCartesian'
                              ))
  
  return(gg)
  
  # gg <- ggplotly(p
  #                #,tooltip=c("y","x")           for showing a subset on each point in plot
  # )
  # 
  # gg
  
})










# React_DT3 <-eventReactive(input$DT_AC3, {
#   
# if(input$DT_numI==1)
# gr <- rep(1,dim(Data())[2])
# else
# gr <- as.numeric(cut(1:dim(Data())[2],breaks = input$DT_numI,labels = 1:input$DT_numI))
# 
# 
# d <- as.data.frame(apply(Data(),1,function(x){weighted.mean(x,gr)}))
# d$names <- rownames(Data())
# colnames(d) <- c("mean.w","names")
# d <- d[order(d$mean.w,decreasing = T),]
# 
# cc1 <- colorRampPalette(c("sienna3","khaki3","turquoise3"))(input$DT_bin2)
# 
# if(input$DT_bin2==1)
# d$clr <-cc1
# else
# d$clr <- as.vector(cut(1:dim(Data())[1],breaks = input$DT_bin2,labels = cc1))
# 
# p <- ggplot(d,aes(x = reorder(names,mean.w),y = mean.w))+
#   geom_bar(stat="identity",aes(fill = clr),color="black")+
#   geom_text(data=d,aes(x = names,y = mean.w,label=round(mean.w,2)),vjust=0)+
#   labs(title ="میانگین وزنی", x = "", y = "میانگین وزنی")+
#   scale_fill_manual(values=cc1)
# 
# gg <- ggplotly(p)
# 
# gg  
#   
# })


output$DT <- renderPlotly(React_DT2())

table_ind <- reactiveValues(a=0)

observeEvent(input$DT_AC2, {
  table_ind$a = 1
})

output$full_out <- renderUI({
  
  
if(table_ind$a==1){
  
  validate(need(!is.null(Data()),"هنوز داده ای وارد نشده است"),errorClass = "Hist_l")
  
  A <- dropdown(
    
    div(class="right",
        colourpicker::colourInput(ns("color_bg"),label = div(style="font-size:80%;","انتخاب رنگ پس زمینه"),
                value = "#55B4B0",showColour = "background")),
    
    div(class="right",
        colourpicker::colourInput(ns("color_text"),label = div(style="font-size:80%;","انتخاب رنگ متن"),
                value =  "black",showColour = "background")),
                #palette = "limited" , allowedCols = c("black","white","#B93A32"))),
    
    circle = TRUE, status = "default", icon = icon("gear"),style = "unite",width = "18%"
  )
  
  B <- div(style="text-align:right",downloadBttn(ns("download"),
                label = "دانلود",size = "sm"))
  
  C <- withSpinner(plotlyOutput(ns("DT")),type=5,color = "#006E6D",size = 0.6)  
  
  
  
  out <- list(A,B,br(),C)
  return(out)
}
  
})


output$download <- downloadHandler(
  filename =  paste0("فیلتر",".html"),
  content=function(file){ 
    
    tempReport <- file.path(tempdir(),"filter.Rmd")
    file.copy("report/filter.Rmd",tempReport,overwrite = TRUE)
    tempImage <- file.path(tempdir(),"logogrey.svg")
    file.copy("report/logogrey.svg",tempImage,overwrite = TRUE)
    params <- list(n = ggplotly(React_DT2()) %>% config(displaylogo = FALSE,collaborate = FALSE,
                                                        modeBarButtonsToRemove = list(
                                                          'zoom2d','pan2d','select2d','lasso2d','zoomIn2d','zoomOut2d',
                                                          'sendDataToCloud',
                                                          'autoScale2d',
                                                          'hoverClosestCartesian',
                                                          'hoverCompareCartesian'
                                                        )))
    rmarkdown::render(tempReport,output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv()))
    #pdf(file,width=7,height=5) 
    #ggsave(filename = file,plot = React_DT2(),device = cairo_pdf)
    #export(p = ggplotly(React_DT2()),file = file)
    #htmlwidgets::saveWidget(widget = ggplotly(React_DT2()),file = file)
    #webshot::webshot(sprintf("file://%s", file),file = file,selector="#htmlwidget_container")
    #plotly_IMAGE(x = ggplotly(React_DT2()),out_file = file,format = "jpeg")
    #orca(ggplotly(React_DT2()),file)
    #dev.off() 
  }
)


}
