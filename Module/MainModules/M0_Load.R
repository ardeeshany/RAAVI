
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
column(width = 3,
br(),
box(width="200%",
    #title = div(class="load__title--font-size",""),
    status="primary",

    wellPanel(
        #radioButtons(inputId = ns("up_rmv"),label = "",choices = c("آپلود","پاک"),selected = "آپلود",inline = TRUE),
        uiOutput(outputId = ns("f_upload"))),
        #uiOutput(ns("message2"), inline=TRUE),
    wellPanel(
      #radioButtons(inputId = ns("up_rmv"),label = "",choices = c("آپلود","پاک"),selected = "آپلود",inline = TRUE),
      fluidRow(
      column(6,
             div(class="inline action-button--font-size__make",
                 numericInput(inputId = ns("num_row"),
                  label =  "تعداد دانش آموز",min = 1,value = 1,step = 1))),
      
      column(6,
             div(class="inline action-button--font-size__make", 
                 numericInput(inputId = ns("num_col"),
                  label = "تعداد امتحان",min = 1,value = 1,step = 1)))),
      
      uiOutput(outputId = ns("f_make"))
      #"این فایل را ذخیره کرده، تغییر داده و دوباره آپلود کنید"
    ),
    
    
    wellPanel(
      #h5("ذخیره کردن دیتا"),
      div(class='row',
          uiOutput(ns("save_name")),
          #actionButton(ns("save"), "ذخیره کردن دیتا"),
          div(style="height:150%;",
              downloadButton(ns("downloadData"),
                             div(class="action-button--font-size","ذخیره کردن داده"),
                             class="downlaod-button--general action-button--color--yellow")),
          uiOutput(ns("message"), inline=TRUE)
          #div(class="col-sm-6",
          #radioButtons(ns("fileType"), "File type", c("R", "xlsx")))
      )
    ),
    
      wellPanel(
        #radioButtons(inputId = ns("up_rmv"),label = "",choices = c("آپلود","پاک"),selected = "آپلود",inline = TRUE),
        uiOutput(outputId = ns("f_test"))
        #"این فایل را ذخیره کرده، تغییر داده و دوباره آپلود کنید"
        )
        #uiOutput(ns("message2"), inline=TRUE),

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
    column(width = 9,
           div(style="margin-top: 3%;",
               box(status="primary",width="200%",collapsible = TRUE,collapsed = FALSE,
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
                   
                   uiOutput(ns("Text")),
                  
                   
                   # conditionalPanel(
                   #   condition = " output.Table == 'NA'",
                   #   div(class="green2",style="font-size:120%;",
                   #       "فایل با موفقیت وارد شد",
                   #       br())
                   # ),
                   
                   
                   # if(is.null(input$hot)){
                   #   A <- div(style="color:grey; font-size:150%;",br(), br(),
                   #            "هنوز فایلی وارد نشده است",
                   #            br(),
                   #            "فایل خود را از منوی سمت چپ وارد کرده یا بسازید",
                   #            br(),br(),br())
                   # }
                   # else{
                   #   A <- ""
                   # }
                         
                   
                   div(class="data-table--general",
                       rHandsontableOutput(ns("hot"))),
                   # conditionalPanel(condition="is.null(input.Table)==TRUE",
                   #                  h2("هنوز فایلی وارد نشده است")),
                   # conditionalPanel(condition="is.null(input.Table)==FALSE",
                   # uiOutput(ns("Table"))),
                   
                   br(),
                   
                   
                   box(collapsible = TRUE,title = "تغییر دیتا",collapsed = TRUE,width = "200%",status = "info",
                       fluidRow(
                         column(width = 3,
                            wellPanel(div(style="width:100%;",
                                 uiOutput(ns("ui_newcolname"))),
                                 actionButton(ns("addcolumn"), "اضافه کردن ستون",
                                  class="action-button--color--yellow inline",
                                  style="margin-top:6%;"))),
                         #radioButtons(ns("newcolumntype"), "Type", c("integer", "double", "character")),
                         
                         column(width = 3,       
                                wellPanel(div(style="width:100%;",
                                  uiOutput(ns("ui_removecolname"))),
                                  actionButton(ns("removecolumn"),"حذف کردن ستون",
                                    class="action-button--color--yellow inline"))),
                         
                         column(width = 3,
                                wellPanel(div(style="display:inline-block;width:100%;",
                                   uiOutput(ns("ui_newrowname"))),
                                   actionButton(ns("addrow"),"اضافه کردن سطر",
                                     class="action-button--color--yellow inline",
                                     style="margin-top:6%;"))),
                         #radioButtons(ns("newrowtype"), "Type", c("integer", "double", "character")),
                         column(width = 3,       
                                wellPanel(div(style="display:inline-block;width:100%;height:60%;",
                                    uiOutput(ns("ui_removerowname"))),
                                    actionButton(ns("removerow"),"حذف کردن سطر",
                                      class="action-button--color--yellow inline",
                                      style="margin-top:1%;")))
                       ))
               ))))



))
}


M0_Load <- function(input,output,session,outputDir){
  
  Date_US <- as.OtherDate(Sys.Date(),"modpersian")[1:3]
  Date_Persian = sprintf("%s-%s-%s",Date_US[3],Date_US[2],Date_US[1])
  
  
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
     values[["now"]] <- D_new[,-1]
     values[["names"]] <-D_new[,1]
     values[["dates"]] <- colnames(D_new)[-1]
     #saveData(D_new,input$f_name)
     })
  
  
  
  observeEvent(input$f_test,{
    #D_new <- read.xlsx("/Users/ardalanmirshani/Dropbox/RAAVI/RAAVI-Released/www/Data.xlsx")
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
  
  
  output$Text <- renderUI({
    #print(is.null(input$hot))
    if(is.null(input$hot)){
      A <- div(style="color:grey; font-size:150%;",br(), br(),
               "هنوز فایلی وارد نشده است",
               br(),br(),
              "فایل خود را از منوی سمت چپ وارد کرده یا بسازید",
               br(),br(),br())
    }
    else{
      A <-  div(class="green2 right",style="font-size:120%;",
              "فایل با موفقیت وارد شد",
               br())
    }

    return(A)

  })
  
    
    # if(outvar$a==1){        # Does not need have () for input$x .... I mean, input$x() is wrong.
    #   return(h2("هنوز فایلی وارد نشده است"))   # return is important here. Without it does not work
    # }
    # 
    # if(outvar$a==2){
    #   return(div(class="data-table--general",
    #              rHandsontableOutput(session$ns("hot"))))
    # }
    
  


  
  
  output$f_make <- renderUI({
    
    actionButton(inputId = session$ns("f_make"),
                 label = div(class="action-button--font-size","ساختن فایل جدید"),
                 class="action-button--color--yellow")
    
  })

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

   output$f_upload <- renderUI({
     #if(input$up_rmv=="آپلود"){
       #Date <- as.OtherDate(Sys.Date(),"persian")[1:3]
       #A <- textInput(inputId = session$ns("f_name"),label = "نام دیتا",
       #            value = sprintf("%s-%s-%s",Date[3],Date[2],Date[1]))
     #includeScript("progress.js")
     div(style="padding-top:1%;",
     fileInput(inputId = session$ns("f_new"),
                      label = div(class="load__subtitle--font-size","آپلود کردن فایل جدید"),
                      #buttonLabel = list(div(style="font-size:130%;","جستجو",icon("folder"))),
                      buttonLabel = list(icon("folder")),
                      placeholder = "هنوز فایلی وارد نشده است",
                      width = "100%",
                      accept=".xlsx"))
     # }else{
     #   A <- selectInput(inputId = session$ns("f_remove"),label = "نام دیتا",choices = File()$name) 
     #   B <- actionButton(session$ns("remove_f"), "پاک کردن")
     #   return(list(A,B))
     # }
   })
   
   
   output$f_test <- renderUI({
     actionButton(inputId = session$ns("f_test"),
                  label = div(class="action-button--font-size","فایل جهت تست"),
                  class="action-button--color--yellow")
   })
   
  
  
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
       rhandsontable(Tot, useTypes = TRUE, stretchH = "all",
       colHeaders = 1:dim(Tot)[2]  ,rowHeaders = NULL,search = TRUE) %>%
       #colHeaders = NULL  ,rowHeaders = NULL) %>%
       hot_cols(colWidths = 120, fixedColumnsLeft = 1,manualColumnMove = FALSE,manualColumnResize = FALSE) %>%
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

  
  output$message <- renderUI({
      helpText(sprintf(""))
  })
  
## Save 

   output$save_name <- renderUI({
     div(class="load--font-size_add",
     textAreaInput(inputId = session$ns("save_name"),label = "", value ="",
               height = "2.4em",resize = "none",width = "100%",
               placeholder = "نام فایل ذخیره شده را وارد کنید"))
   })
   
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
        finalDF <- cbind(values[["names"]],values[["now"]])
        colnames(finalDF)[1] <- "تاریخ آزمون"
        colnames(finalDF)[-1] <- values[["dates"]]
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
