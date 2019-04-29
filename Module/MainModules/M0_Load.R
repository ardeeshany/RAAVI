M0_LoadUI <- function(id){
  
  ns <- NS(id)
  

  
  tabPanel(title = div(class="tabPanel--font-size center",
                       "وارد کردن داده"),
          icon=icon("download",class="tabPanel-icon"),
                  
           fluidRow(

### Left Panel - Add/Delete/Save   ----             
          # div(style="text-align:center;", 
          # column(width = 3,
          # br(),
          # box(width="200%",title = "ویرایش اطلاعات",status="primary",
          # 
          #       wellPanel(
          #       radioButtons(inputId = ns("up_rmv"),label = "",choices = c("آپلود","پاک"),selected = "آپلود",inline = TRUE),
          #       uiOutput(outputId = ns("f_upload"))),
          #       uiOutput(ns("message2"), inline=TRUE),
          #       br(),
          #      
          #      wellPanel(
          #        h3("ذخیره کردن دیتا"), 
          #        div(class='row', 
          #            textInput(inputId = ns("save_name"),label = "",value = Sys.Date(),
          #                      placeholder = "نام دیتا را وارد کنید"),
          #                actionButton(ns("save"), "ذخیره کردن دیتا در ابر"),
          #            
          #              uiOutput(ns("message"), inline=TRUE)
          #            
          #            # div(class="col-sm-6",
          #            #     radioButtons(ns("fileType"), "File type", c("R", "xlsx")))
          #        )
          #      )
          #      
          #    ))),
             
### Left Panel   ----     


div(style="text-align:center;",
column(width = 2,
br(),
box(width="100%",
    #title = div(class="load__title--font-size",""),
    status="primary",

    wellPanel(
      fileInput(inputId = ns("f_new"),
                label = div(class="load__subtitle--font-size",'آپلود فایل'),
                buttonLabel = list(icon("file-excel-o")),
                placeholder = "ورود",
                width = "100%",
                accept=".xlsx")
      ),
      

    # wellPanel(
    # 
    # div(class="input-box--general",
    #              numericInput(inputId = ns("num_row"),
    #               label =  "تعداد دانش آموز",min = 1,value = 1,step = 1)),
    # div(class="input-box--general", 
    #              numericInput(inputId = ns("num_col"),
    #               label = "تعداد امتحان",min = 1,value = 1,step = 1)),
    #   
    # actionBttn(inputId = ns("f_make"),style = "jelly",color = "warning",
    #            label = div(class="action-button--widget","ساختن فایل"))
    # ),
    
    
    # wellPanel(
    #       
    #       div(class="load--font-size_add",
    #           textAreaInput(inputId = ns("save_name"),label = "", value ="",
    #                         height = "2.3em",resize = "none",width = "100%",
    #                         placeholder ='نام فایل ذخیره')),
    #       
    #       #div(style="height:150%;",
    #           # downloadBttn(ns("downloadData"),style = "material-circle",color = "warning",
    #           #                div(class="action-button--widget","ذخیره کردن داده"))
    #          div(style="color:black", 
    #          downloadBttn(ns("downloadData"),color = "warning", 
    #           style ="jelly",size = "md",label=div(class="inline",style="font-size:68%;color:black;","ذخیره"))
    #          )            
    #       #uiOutput(ns("message"), inline=TRUE)
    #       
    # ),
    
      wellPanel(
        div(style="align:center; tet-align:center;",
        actionBttn(inputId = ns("f_test"),style = "jelly",color = "warning",
                   label = div(class="action-button--widget","فایل نمونه")))
        
        
        ),

    wellPanel(
      div(style="align:center; text-align:center",
          downloadBttn(ns("report"),label = "دانلود",size = "sm"))
    )

   ))),


### Right Panel   ----       
                 
               # div(style="text-align:center;",         
               # column(width = 9,
               # div(style="margin-top: 3%;",
               # box(status="primary",width="200%",collapsible = TRUE,collapsed = FALSE,
               # fluidRow(
               # column(width = 4,
               # div(style="display:inline-block;width:90%;",
               # uiOutput(outputId = ns("f_set")))),
               # column(width = 3,
               # div(style="display:inline-block;width:30%;margin-top:15%;",
               # actionButton(ns("cancel"), "Cancel last action")))),
               # rHandsontableOutput(ns("hot")),
               # br(),
               # 
               # 
               # box(collapsible = TRUE,title = "تغییر دیتا",collapsed = TRUE,width = "200%",status = "info",
               #     fluidRow(
               #       column(width = 3,
               #        wellPanel(div(style="width:100%;",
               #                    uiOutput(ns("ui_newcolname"))),
               #                div(style="display:inline-block;",
               #                    actionButton(ns("addcolumn"), "اضافه کردن ستون")))),
               #              #radioButtons(ns("newcolumntype"), "Type", c("integer", "double", "character")),
               #       
               #       column(width = 3,       
               #       wellPanel(div(style="width:100%;",
               #                    uiOutput(ns("ui_removecolname"))),
               #                div(style="display:inline-block;",
               #                    actionButton(ns("removecolumn"),"حذف کردن ستون")))),
               # 
               #       column(width = 3,
               #       wellPanel(div(style="display:inline-block;width:100%;",
               #                    uiOutput(ns("ui_newrowname"))),
               #                div(style="display:inline-block;",
               #                    actionButton(ns("addrow"),"اضافه کردن سطر")))),
               #              #radioButtons(ns("newrowtype"), "Type", c("integer", "double", "character")),
               #       column(width = 3,       
               #       wellPanel(div(style="display:inline-block;width:100%;height:60%;",
               #                    uiOutput(ns("ui_removerowname"))),
               #                div(style="display:inline-block;",
               #                    actionButton(ns("removerow"),"حذف کردن سطر"))))
               #       ))
               # ))))

### Right Panel   ----       



div(style="text-align:center;",         
    column(width = 10,
br(),
    box(status="primary",width="100%",collapsible = TRUE,collapsed = FALSE,
                   # fluidRow(
                   #   # column(width = 4,
                   #   #        div(style="display:inline-block;width:90%;",
                   #   #            uiOutput(outputId = ns("f_set")))),
                   #   
                   #   column(width = 3,offset = 9,
                   #          div(style="display:inline-block;width:40%;margin-top:5%;",
                   #              actionButton(ns("cancel"), "برگردان به قبل")))
                   #   
                   #   ),
                   
        
        

        
                 conditionalPanel(
                   condition = " input.hot == null ", ns=ns,
                   div(style="color:grey; font-size:140%;",br(), br(),
                  "هنوز فایلی وارد نشده است",br(),br(),
                  "فایل خود را از منوی سمت چپ وارد کرده یا فایل جدید بسازید",
                  br(),br(),br())
                                ),

        
                   div(class="data-table--general",
                       rHandsontableOutput(ns("hot")))
                   
                   #br(),
                   
                   
                   # box(collapsible = TRUE,title = "تغییر دیتا",collapsed = TRUE,width = "200%",status = "info",
                   #     fluidRow(
                   #       column(width = 3,
                   #          wellPanel(div(class="inline load_add",
                   #               uiOutput(ns("ui_newcolname"))),
                   #               div(style="margin-top:-1%",
                   #               actionBttn(ns("addcolumn"), 
                   #                          div(class="action-button--widget","اضافه ستون"),
                   #                          style = "jelly",color = "warning")))),
                   #       
                   #       column(width = 3,       
                   #              wellPanel(div(class="inline load_add",
                   #                uiOutput(ns("ui_removecolname"))),
                   #                div(style="margin-top:-6%",
                   #                actionBttn(ns("removecolumn"),
                   #                  div(class="action-button--widget","حذف ستون"),
                   #                  style = "jelly",color = "warning")))),
                   #       
                   #       column(width = 3,
                   #              wellPanel(div(class="inline load_add",
                   #                 uiOutput(ns("ui_newrowname"))),
                   #                 div(style="margin-top:-1%",
                   #                 actionBttn(ns("addrow"),
                   #                  div(class="action-button--widget","اضافه سطر"),
                   #                  style = "jelly",color = "warning")))),
                   #       
                   #       column(width = 3,       
                   #              wellPanel(div(class="inline load_add", 
                   #                  uiOutput(ns("ui_removerowname"))),
                   #                  div(style="margin-top:-6%",
                   #                  actionBttn(ns("removerow"),
                   #                    div(class="action-button--widget","حذف سطر"),
                   #                    style = "jelly",color = "warning"))))
                   #     ))
               )))



)
)
}


M0_Load <- function(input,output,session,outputDir){
  

  Date_US <- as.OtherDate(Sys.Date(),"persian")[1:3]
  Date_Persian = sprintf("%s-%s-%s",Date_US[3],Date_US[2],Date_US[1])
  
  

  output$report <- downloadHandler(
    filename = paste0("report",".pdf"),
    content=function(file){ 
      
      tempReport <- file.path(tempdir(),"report.Rmd")
      file.copy("report/report.Rmd",tempReport,overwrite = TRUE)
      tempImage <- file.path(tempdir(),"Logo.png")
      file.copy("report/Logo.png",tempImage,overwrite = TRUE)
      params <- list(n = 1)
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
  
  
  
  # saveData <- function(data,fileName){
  #   # Create a unique file name
  #   filePath <- file.path(tempdir(), sprintf("%s.xlsx",fileName)) # Write the data to a temporary file locally
  #   colnames(data)[1] <- "نام"
  #   A <- write.xlsx(x = data, file = filePath, row.names = FALSE)
  #  
  #   #drop_upload(filePath, path = outputDir, autorename = TRUE,mode = "add",dtoken = token)
  # }

   observeEvent(input$f_new,{
     D_new <- read.xlsx(input$f_new$datapath)
     
     if(dim(D_new)[1] > 20)
     D_new <- D_new[1:21,]
  
     if(dim(D_new)[2] > 20)
     D_new <- D_new[,1:21]
     
     
     values[["now"]] <- D_new[,-1]
     values[["names"]] <-D_new[,1]
     values[["dates"]] <- colnames(D_new)[-1]
     #saveData(D_new,input$f_name)
     })
  
  
  
  observeEvent(input$f_test,{
    D_new <- read.xlsx(file.path(getwd(),"www/Data.xlsx"))
    values[["now"]] <- D_new[,-1]
    values[["names"]] <- D_new[,1]
    values[["dates"]] <-colnames(D_new)[-1]
    #values[["now_numeric"]] <- D_new[,-1]
    #saveData(D_new,input$f_name)
  })

  # outvar <- reactiveValues(a=1)
  # 
  # observeEvent(input$hot,{
  #   outvar$a=2
  # })
  # 
  # 
  # React_out <- reactive({
  #   input$hot
  #   if(outvar$a==1){        # Does not need have () for input$x .... I mean, input$x() is wrong.
  #     return("هنوز فایلی وارد نشده است")  # return is important here. Without it does not work
  #   }
  #   
  #   if(outvar$a==2){
  #     return(
  #       div(class="data-table--general",
  #       rHandsontableOutput(session$ns("hot"))))
  #   }
  #   
  # }) 
  #output$Table <- renderUI({React_out()})
  

    
  observeEvent(input$f_make,{
    D_new <- data.frame("تاریخ آزمون"="",matrix("",nrow = input$num_row,ncol = input$num_col),stringsAsFactors = FALSE)
    colnames(D_new)[1] <- "تاریخ آزمون"
    for(i in 1:input$num_col) { 
      colnames(D_new)[i+1] <- paste("تاریخ ", i, sep = "")
    }
    for(i in 1:input$num_row) { 
      D_new[i,1] <- paste("نام ", i, sep = "")
    }

    if(input$num_col==1){
    values[["now"]] <-   cbind(rep(NA,input$num_row))
    }else{
    values[["now"]] <-   D_new[,-1]}
    values[["names"]] <- D_new[,1]
    values[["dates"]] <- colnames(D_new)[-1]
    #saveData(D_new,input$f_name)
  })
  
  # File <- reactive({
  #   input$save
  #   input$f_new
  #   #input$remove_f
  #   filesInfo <- drop_dir(outputDir,dtoken = token)
  #   filenames <- unlist(base::strsplit(filesInfo$name,"[.]"))[c(TRUE,FALSE)] # select odd elemnts (no after dot)
  #   filePaths <- filesInfo$path_display
  #   return(list(name=filenames,path=filePaths))
  # }) 
  
  #observeEvent(input$remove_f,{
    #ind <- which(File()$name==input$f_remove)
    #path <- file.path(outputDir, sprintf("%s.xlsx",input$f_remove))
    #path2 <- (drop_dir("/RAAVI/RAAVI/Data",dtoken=token)$path_display)
    # output$message2 <- renderUI({
    #    helpText(sprintf("فایل \"%s\" با موفقیت ذخیره شد", drop_exists(path2,dtoken = token)))
    # })

   
  # output$f_set <- renderUI({
  #    selectInput(inputId = session$ns("f_set"),label = "دیتا برای آنالیز",choices = File()$name)
  #    })

   # output$f_upload <- renderUI({
   #   #if(input$up_rmv=="آپلود"){
   #     #Date <- as.OtherDate(Sys.Date(),"persian")[1:3]
   #     #A <- textInput(inputId = session$ns("f_name"),label = "نام دیتا",
   #     #            value = sprintf("%s-%s-%s",Date[3],Date[2],Date[1]))
   #   #includeScript("progress.js")
   #   fileInput(inputId = session$ns("f_new"),
   #                    label = div(class="load__subtitle--font-size",'آپلود فایل'),
   #                    #buttonLabel = list(div(style="font-size:130%;","جستجو",icon("folder"))),
   #                    buttonLabel = list(icon("file-excel-o")),
   #                    placeholder = 'ورود داده',
   #                    width = "100%",
   #                    accept=".xlsx")
   #   # }else{
   #   #   A <- selectInput(inputId = session$ns("f_remove"),label = "نام دیتا",choices = File()$name) 
   #   #   B <- actionButton(session$ns("remove_f"), "پاک کردن")
   #   #   return(list(A,B))
   #   # }
   # })
   # 
  
  values <- reactiveValues(tot=NULL)

  
  # observeEvent(input$f_set,{
  #   ind <- which(File()$name==input$f_set)
  #   Temp <- file.path(tempdir(),"Test.xlsx")
  #   drop_download(path = File()$path[ind],local_path = Temp,overwrite = TRUE,dtoken = token)
  #   D <- read.xlsx(xlsxFile = Temp)
  #   values[["now"]] <- D[,-1]
  #   values[["names"]] <-D[,1]
  #   values[["dates"]] <- colnames(D)[-1]
  # })
  # D_new <- read.xlsx(file.path(getwd(),"www/Data.xlsx"))
  # now <- D_new[,-1]
  # dates <- colnames(D_new)[-1]
  # names <- D_new[,1]
  # 
  # r1 <- data.frame(cbind("تاریخ آزمون",t(dates)),stringsAsFactors = FALSE)
  # r2 <- data.frame(cbind(names,now),stringsAsFactors = FALSE)
  # names(r2) <- names(r1)
  # Tot <- rbind(r1,r2)
  # A <- rhandsontable(Tot, useTypes = TRUE, stretchH = "all",
  #               colHeaders = 1:dim(Tot)[2]  ,rowHeaders = NULL,search = TRUE) %>%
  #   #colHeaders = NULL  ,rowHeaders = NULL) %>%
  #   hot_cols(colWidths = 120, fixedColumnsLeft = 1,manualColumnMove = FALSE,manualColumnResize = FALSE) %>%
  #   hot_cols(renderer = "function(instance, td, row, col, prop, value, cellProperties) {
  #            Handsontable.renderers.TextRenderer.apply(this, arguments);
  #            if (col==0) {td.style.background = '#DCDCDC'; td.style.color = 'black';}
  #            if (row==0) {td.style.background = '#DCDCDC'; td.style.color = '#B93A32';}}") %>%        
  #   #manualColumnMove and manualColumnResize works when colHeaders is not NULL!
  #   hot_rows(rowHeights = 40, fixedRowsTop = 1) %>%
  #   hot_col(col = 1:dim(Tot)[2] , valign = "htMiddle") %>%
  #   hot_validate_numeric( cols = 2:dim(Tot)[2] , min = 0, max = 20, 
  #                         allowInvalid = TRUE) %>%
  #   # valign works when colHeaders is not NULL!
  #   hot_cell(1,1,readOnly=TRUE)
  # 
  
  persian <- "\u0660\u0661\u0662\u0663\u0664\u0665\u0666\u0667\u0668\u0669\u06F0\u06F1\u06F2\u06F3\u06F4\u06F5\u06F6\u06F7\u06F8\u06F9"
  english <- "01234567890123456789"
  persian.tonumber <- function(s) as.character(chartr(persian,english,s))
  
  # Changing the table, Save previous work Handsontable
  observe({
      if(!is.null(input$hot)){
      #values[["previous"]] <- isolate(values[["now"]]) # current table values
        hot_data = hot_to_r(input$hot)
        A <- hot_data[-1,-1]
        
        if(!is.null(dim(A))){
        for(i in 1:dim(A)[1]){
          for(j in 1:dim(A)[2]){
            A[i,j] <- persian.tonumber(A[i,j])
          }
        }
          }
        
        values[["now"]] <- data.matrix(A)
        
        values[["names"]] <- hot_data[-1,1]
        values[["dates"]] <- t(hot_data[1,-1])
        }  # DF is the r format of the table value
  })

  
  # D_new[,-1] <- data.matrix(D_new[,-1])
  # sapply(D_new,class)
  ## Add column
  output$ui_newcolname <- renderUI({
    div(class="load--font-size_add",
        textAreaInput(session$ns("newcolumnname"), label = "",
                      height = "2.4em",resize = "none",width = "100%",
                      value = Date_Persian))   # sprintf("newcol%s", 1+ncol(values[["now"]]))
  })
  
  observeEvent(input$addcolumn, {
    DF <- values[["now"]]
    values[["previous"]] <- DF
    newcolumn <- 0 #eval(parse(text=sprintf('%s(nrow(as.data.frame(DF)))', "integer" ))) #isolate(input$newcolumntype))))
    values[["now"]] <- setNames(cbind(DF, newcolumn), c(values[["dates"]], isolate(input$newcolumnname)))
    #values[["now"]] <- cbind(DF,0)
    values[["dates"]] <- c(values[["dates"]], isolate(input$newcolumnname))
    })

  
  ## Add row
  output$ui_newrowname <- renderUI({
    textAreaInput(session$ns("newrowname"), label = "",
                  height = "2.4em",resize = "none",width = "100%",
        placeholder = "نام دانش آموز")#sprintf("newrow%s", 1+nrow(values[["now"]])) )
  })
  
  observeEvent(input$addrow, {
    DF <- values[["now"]]
    values[["previous"]] <- DF
    values[["names"]] <- c(values[["names"]],input$newrowname)
    A <- rbind(DF,0)
    values[["now"]] <- A
  })
  
  
  
  ## remove column
  output$ui_removecolname <- renderUI({
    div(class="load--font-size_add",
    selectInput(session$ns("removecolnamelist"), "",choices = values[["dates"]] ))
  })
  
  observeEvent(input$removecolumn, {
    DF <- values[["now"]]
    values[["previous"]] <- DF
    count <- which(values[["dates"]]==input$removecolnamelist)
    A <- DF[,-count]
    values[["dates"]] <- values[["dates"]][-count]
    values[["now"]] <- A
    #values[["now"]] <- subset(DF, select=-c(input$removecolnamelist))
  })
   
  
  
  ## remove row
  output$ui_removerowname <- renderUI({
    div(class="load--font-size_add",
    selectInput(session$ns("removerownamelist"),"",choices = values[["names"]]))
  })
  
  observeEvent(input$removerow, {
    DF <- values[["now"]]
    values[["previous"]] <- DF
    count <- which(values[["names"]]==input$removerownamelist)
    A <- DF[-count,]
    values[["names"]] <- values[["names"]][-count]
    values[["now"]] <- A
  })
   

#        class(D_new)
#   sapply(D_new,class)
#        B <- D_new[-1,-1]
# class(B)  
# sapply(B,class)  
# C <- data.frame(lapply(C, as.character), stringsAsFactors=FALSE)
# class(C)
# sapply(C,class)
## Output  

  output$hot <- renderRHandsontable({
      if (!is.null(values[["now"]])){
      #values[["now"]] <- data.frame(lapply(values[["now"]], as.character), stringsAsFactors=FALSE)
      r1 <- data.frame(cbind("تاریخ آزمون",t(values[["dates"]])),stringsAsFactors = FALSE)
      r2 <- data.frame(cbind(values[["names"]],values[["now"]]),stringsAsFactors = FALSE)
      names(r2) <- names(r1)
      Tot <- rbind(r1,r2)
       rhandsontable(Tot, useTypes = TRUE, stretchH = "all",columnSorting=TRUE,
       colHeaders = 1:dim(Tot)[2]  ,rowHeaders = NULL,search = TRUE) %>%
       #colHeaders = NULL  ,rowHeaders = NULL) %>%
       hot_cols(colWidths = 120, fixedColumnsLeft = 1,manualColumnMove = TRUE,manualColumnResize = FALSE) %>%
       hot_cols(renderer = "function(instance, td, row, col, prop, value, cellProperties) {
          Handsontable.renderers.TextRenderer.apply(this, arguments);
            if (col==0) {td.style.background = '#DCDCDC'; td.style.color = 'black';}
            if (row==0) {td.style.background = '#DCDCDC'; td.style.color = '#B93A32';}}") %>%        
       #manualColumnMove and manualColumnResize works when colHeaders is not NULL!
       hot_rows(rowHeights = 40, fixedRowsTop = 1) %>%
       hot_col(col = 1:dim(Tot)[2] , valign = "htMiddle") %>%
       hot_validate_numeric( cols = 2:dim(Tot)[2] , min = 0, max = 20, 
                              allowInvalid = TRUE) %>%
      # valign works when colHeaders is not NULL!
       hot_cell(1,1,readOnly=TRUE)
      
      }
  })
  
## Save 

   # observeEvent(save_name,{
   #   output$message <- renderUI({
   #     div(style="text-align:right;",
   #         helpText(sprintf("فایل \"%s\" با موفقیت ذخیره شد", input$save_name)))
   #   })
   # })

   
   
    #observe({
    output$downloadData <- downloadHandler(
      filename = function() {
        if(!(input$save_name==""))
        paste(input$save_name, ".xlsx", sep = "")
        else
        paste(Date_Persian, ".xlsx", sep = "")  
      },
      content = function(file) {
        print("Ardal")
        #print(cbind(values[["names"]],values[["now"]]))
        finalDF <- cbind(values[["names"]],values[["now"]])
        colnames(finalDF) <- c("تاریخ آزمون",values[["dates"]])
        write.xlsx(finalDF, file, row.names = FALSE)
      }
    )
    #})
  
    
    
    
    
    
    
    
  ## Cancel last action    
  observeEvent(input$cancel, {
    if(!is.null(isolate(values[["previous"]]))) values[["now"]] <- isolate(values[["previous"]])
  })
  
  return(values)
  
  
  
}
