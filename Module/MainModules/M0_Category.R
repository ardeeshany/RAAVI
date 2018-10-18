
M0_CatUI <- function(id){
  
  ns <- NS(id)
  
  tabPanel(title = div(class="tabPanel--font-size center",
                       "فیلتر"),
           icon = icon("filter",class="tabPanel-icon"),
           
           
           fluidRow(                
             
             column(1,offset = 1,
                    
              div(class="numeric-box--general__cat",
              numericInput(inputId = ns("DT_bin"),
                          label = "تعداد دسته",
                          value=1)),
              
              div(class="action-button--general__cat", style="margin-top:15%;",
              actionButton(inputId = ns("DT_AC1"),
                    label = div(class="action-button--font-size","گروه بندی"),
                    class="action-button--color--yellow",
                    width = "130px")#,icon = icon("arrow-right"))
              )
             ),

             
             column(1,offset = 1,
                    div(class="check-box--general check-box--mtop__cat",
                      checkboxInput(inputId = ns("DT_chb1"),
                        label = div(class="check-box--font-size","نام گروه")))
             ),
             
     #######################        
             column(1,offset = 2,
                    div(class="slider--general",
                    sliderInput(inputId = ns("DT_sl"),label = "",
                      min = 0,max = 20,value = c(0,20),step = 0.5,width = "72%")),
                    div(class="action-button--general",    
                    style="margin-left:210%;",
                    actionButton(inputId = ns("DT_AC2"),
                    label = div(class="action-button--font-size inline action-button--text-mleft", "بازه ی مورد نظر"),
                    class="action-button--color--yellow",
                    width = "320%",
                    icon=icon("hand-o-up")))
             )

     #######################       
             # column(1,
             #        div(style="display:inline-block; margin-top: 33px; margin-left: 530px",
             #            uiOutput(ns("DT_numI"))),
             #        div(style="margin-top: 9.5px;margin-left: 545px",
             #            actionButton(inputId = ns("DT_AC3"),label = "طبقه بندی",width = "150px",icon=icon("arrow-right")))
             # ),
             # 
             # column(1,
             #        div(style="display:inline-block; margin-top: 33px;margin-left: 540px",
             #            numericInput(ns("DT_bin2"),label = "تعداد گروه",min = 1,value = 1,width = "85px"))
             # )

             
             
           ),                          

  withSpinner(plotlyOutput(ns("DT")),type=5,color = "#006E6D",size = 0.6)



)
}







######################
#
# Server Logic
#
######################

Group_name_iso <- rep("NA",20)
for(i in 1:20){
  Group_name_iso[i] = paste("گروه",i,sep="")
}

M0_Cat <- function(input,output,session,Vals){


  ns <- session$ns  
  
  Data <- reactive({
    M <- Vals[["now"]]
    rownames(M) <- Vals[["names"]]
    colnames(M) <- Vals[["dates"]]
    return(M)
  })  
  
    
  output$DT_numI <- renderUI({
    numericInput(ns("DT_numI"),label = "میانگین وزنی",min = 1,max=length(colnames(Data())),value = 1,width = "85px")
  })
  
   Mean <- reactive({ apply(Data(),2,mean)  })
  
    Group_name <- reactive({
      Group_name_iso[1:input$DT_bin]  
      })
  

      
    melt_Data_DT <- reactive({
    d <- as.data.frame(Data())
    d <- melt(as.matrix(d))
    
    if(input$DT_bin==1)
    d$Group <- as.vector(cut(d$value,c(0,20),labels = Group_name()))  
    else
    d$Group <- as.vector(cut(d$value,input$DT_bin,labels = Group_name()))
    
    colnames(d) <- c("Student","Day","value","Group")
    d
  })


## Variable (like trigger) for selecting which React_DT want to show in output
var = reactiveValues(a = 1)

observeEvent(input$DT_AC1, {
  var$a = 1
})

observeEvent(input$DT_AC2, {
  var$a = 2
})

# observeEvent(input$DT_AC3, {
#   var$a = 3
# })

##


React_DT1 <-eventReactive(input$DT_AC1, {

 
  validate(
    need(!is.null(Data()),"هنوز داده ای وارد نشده است")
  )
  
  if(input$DT_chb1==TRUE){
    lab_m <- reactive(melt_Data_DT()$Group)
  }
  else{
    lab_m <- reactive(round(melt_Data_DT()$value,2))
  }  
  
    if(input$DT_bin==1){
      
      p <- ggplot(melt_Data_DT(), aes(Day,Student))+
      geom_raster(aes(fill=cut(value,c(0,20),include.lowest = T)))+   # Color
      geom_text(aes(label = lab_m() ))+ # Show values
      labs(title ="نمودار دمایی", x = "زمان", y = "دانش آموزان")+
      guides(fill=guide_legend(title="تقسیم بندی نمره ها"))+
      scale_fill_manual(values="springgreen4") 
    }else{
      
    cc1 <- reactive(colorRampPalette(c("sienna3","khaki3","palegreen4"))(input$DT_bin))
    
    p <- ggplot(melt_Data_DT(), aes(Day,Student))+
      geom_raster(aes(fill=cut(value,input$DT_bin)))+   # Color
      geom_text(aes(label= lab_m()))+ # Show values
      labs(title ="نمودار دمایی", x = "زمان", y = "دانش آموزان")+
      guides(fill=guide_legend(title="تقسیم بندی نمره ها"))+
      scale_fill_manual(values=cc1())
    }
  
    gg <- ggplotly(p,tooltip = ""           #for showing a subset on each point in plot
    )
    
    
    gg
    
  })


React_DT2 <-eventReactive(input$DT_AC2, {

  validate(
    need(!is.null(Data()),"هنوز داده ای وارد نشده است")
  )
  
  if((input$DT_sl[2] <min(melt_Data_DT()$value)) || (input$DT_sl[1] > max(melt_Data_DT()$value))){
    lab <- reactive(paste("[",input$DT_sl[1],",",input$DT_sl[2],"]",sep=""))
    p <- ggplot(melt_Data_DT(), aes(Day,Student))+
      geom_raster(aes(fill=cut(value,c(-1,21),labels=lab())))+   # Color
      geom_text(aes(label= round(value,2)))+ # Show values
      labs(title ="نمودار دمایی", x = "زمان", y = "دانش آموزان")+
      guides(fill=guide_legend(title="تقسیم بندی نمره ها"))+
      scale_fill_manual(values="dimgrey") 
  }
  else{ if (input$DT_sl[1] == input$DT_sl[2]) {
    lab <- reactive(c(paste("[0,",input$DT_sl[1],")",sep=""),paste(input$DT_sl[1]),paste("(",input$DT_sl[1],",",20,"]",sep="")))
    p <- ggplot(melt_Data_DT(), aes(Day,Student))+
      geom_raster(aes(fill=cut(value,c(input$DT_sl[2]-10^-4,input$DT_sl[2]),include.lowest = T,labels=lab)))+   # Color
      geom_text(aes(label= round(value,2)))+ # Show values
      labs(title ="نمودار دمایی", x = "زمان", y = "دانش آموزان")+
      guides(fill=guide_legend(title="تقسیم بندی نمره ها"))+
      scale_fill_manual(values=c("firebreack","dimgray")) 
  }
  
  else{
  
    if(input$DT_sl[1]==0){
          
            if(input$DT_sl[2]==20){
              lab <- reactive(c(paste("[0,20]",sep="")))
              col <- "salmon"
              p <- ggplot(melt_Data_DT(), aes(Day,Student))+
              geom_raster(aes(fill=cut(value,c(-Inf,Inf),include.lowest = T,labels=lab())))+   # Color
              geom_text(aes(label= round(value,2)))+ # Show values
              labs(title ="نمودار دمایی", x = "زمان", y = "دانش آموزان")+
              guides(fill=guide_legend(title="تقسیم بندی نمره ها"))+
              scale_fill_manual(values=col) 
                                 }
      
             else{
             lab <- reactive(c(paste("[0,",input$DT_sl[2],"]",sep=""),paste("[",input$DT_sl[2],",20]",sep=""))) 
             col <- c("salmon","dimgrey")
             p <- ggplot(melt_Data_DT(), aes(Day,Student))+
               geom_raster(aes(fill=cut(value,c(-Inf,input$DT_sl[2],Inf),include.lowest = T,labels=lab())))+   # Color
               geom_text(aes(label= round(value,2)))+ # Show values
               labs(title ="نمودار دمایی", x = "زمان", y = "دانش آموزان")+
               guides(fill=guide_legend(title="تقسیم بندی نمره ها"))+
               scale_fill_manual(values=col)
                 }
   
                            }
    else{
            if(input$DT_sl[2]==20){
            lab <- reactive(c(paste("[0,",input$DT_sl[1],"]",sep=""),paste("[",input$DT_sl[1],",20]",sep=""))) 
            col <- c("dimgrey","salmon")
            if(input$DT_sl[1] <min(melt_Data_DT()$value))
            col <- c("salmon") 
            
            p <- ggplot(melt_Data_DT(), aes(Day,Student))+
              geom_raster(aes(fill=cut(value,c(-Inf,input$DT_sl[1],Inf),include.lowest = T,labels=lab())))+   # Color
              geom_text(aes(label= round(value,2)))+ # Show values
              labs(title ="نمودار دمایی", x = "زمان", y = "دانش آموزان")+
              guides(fill=guide_legend(title="تقسیم بندی نمره ها"))+
              scale_fill_manual(values=col)                      
                                  }
               else{
                 if(input$DT_sl[1]<min(melt_Data_DT()$value)){
                   lab <- reactive(c(paste("[",input$DT_sl[1],",",input$DT_sl[2],"]",sep=""),paste("(",input$DT_sl[2],",",20,"]",sep="")))  
                   col <- c("salmon","dimgrey")
                   p <- ggplot(melt_Data_DT(), aes(Day,Student))+
                     geom_raster(aes(fill=cut(value,c(-Inf,input$DT_sl[2],Inf),include.lowest = T,labels=lab())))+   # Color
                     geom_text(aes(label= round(value,2)))+ # Show values
                     labs(title ="نمودار دمایی", x = "زمان", y = "دانش آموزان")+
                     guides(fill=guide_legend(title="تقسیم بندی نمره ها"))+
                     scale_fill_manual(values=col) 
                   }
                 else{
                   lab <- reactive(c(paste("[0,",input$DT_sl[1],"]",sep=""),paste("[",input$DT_sl[1],",",input$DT_sl[2],"]",sep=""),paste("(",input$DT_sl[2],",",20,"]",sep="")))  
                   col <- c("dimgrey","salmon","dimgrey")
                   p <- ggplot(melt_Data_DT(), aes(Day,Student))+
                     geom_raster(aes(fill=cut(value,c(-Inf,input$DT_sl[1],input$DT_sl[2],Inf),include.lowest = T,labels=lab())))+   # Color
                     geom_text(aes(label= round(value,2)))+ # Show values
                     labs(title ="نمودار دمایی", x = "زمان", y = "دانش آموزان")+
                     guides(fill=guide_legend(title="تقسیم بندی نمره ها"))+
                     scale_fill_manual(values=col) 
                   }
               
                    }
    }
  }
  }
  
  gg <- ggplotly(p
               #,tooltip=c("y","x")           for showing a subset on each point in plot
  )
  
  gg
  
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



### selecting which React_DT want to show in output
React_out <- reactive({
  
  if(var$a==1){        # Does not need have () for input$x .... I mean, input$x() is wrong.
    return(React_DT1())   # return is important here. Without it does not work
  }
  
  if(var$a==2){
    return(React_DT2())
}
 
  # if(var$a==3){
  #   return(React_DT3())
  # }
   
})
###



output$DT <- renderPlotly(React_out())




}
