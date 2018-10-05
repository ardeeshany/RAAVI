
M0_LoadUI <- function(id){
  
  ns <- NS(id)
  
  
  tabPanel(title = div(class="tabPanel--font-size","وارد کردن دیتا"),
           icon=icon("download"),
       
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
    title = div(class="load__title--font-size","ویرایش اطلاعات"),
    status="primary",

      wellPanel(
        #radioButtons(inputId = ns("up_rmv"),label = "",choices = c("آپلود","پاک"),selected = "آپلود",inline = TRUE),
        uiOutput(outputId = ns("f_upload"))),
        #uiOutput(ns("message2"), inline=TRUE),

      wellPanel(
        #radioButtons(inputId = ns("up_rmv"),label = "",choices = c("آپلود","پاک"),selected = "آپلود",inline = TRUE),
        uiOutput(outputId = ns("f_make")),
        div(id="inright30","این فایل را ذخیره کرده، تغییر داده و دوباره آپلود کنید")),
        #uiOutput(ns("message2"), inline=TRUE),
    
      wellPanel(
        #h5("ذخیره کردن دیتا"),
        div(class='row',
        uiOutput(ns("save_name")),
        #actionButton(ns("save"), "ذخیره کردن دیتا"),
        div(style="height:150%;",
        downloadButton(ns("downloadData"),
                       div(class="action-button--font-size","ذخیره کردن دیتا"),
                       class="downlaod-button--general action-button--color--yellow")),
        uiOutput(ns("message"), inline=TRUE)
        #div(class="col-sm-6",
        #radioButtons(ns("fileType"), "File type", c("R", "xlsx")))
           )
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
    column(width = 9,
           div(style="margin-top: 3%;",
               box(status="primary",width="200%",collapsible = TRUE,collapsed = FALSE,
                   fluidRow(
                     # column(width = 4,
                     #        div(style="display:inline-block;width:90%;",
                     #            uiOutput(outputId = ns("f_set")))),
                     column(width = 3,offset = 9,
                            div(style="display:inline-block;width:40%;margin-top:5%;",
                                actionButton(ns("cancel"), "برگردان به قبل")))),
                   div(class="data-table--general",
                   rHandsontableOutput(ns("hot"))),
                   br(),
                   
                   
                   box(collapsible = TRUE,title = "تغییر دیتا",collapsed = TRUE,width = "200%",status = "info",
                       fluidRow(
                         column(width = 3,
                                wellPanel(div(style="width:100%;",
                                              uiOutput(ns("ui_newcolname"))),
                                          div(style="display:inline-block;",
                                              actionButton(ns("addcolumn"), "اضافه کردن ستون")))),
                         #radioButtons(ns("newcolumntype"), "Type", c("integer", "double", "character")),
                         
                         column(width = 3,       
                                wellPanel(div(style="width:100%;",
                                              uiOutput(ns("ui_removecolname"))),
                                          div(style="display:inline-block;",
                                              actionButton(ns("removecolumn"),"حذف کردن ستون")))),
                         
                         column(width = 3,
                                wellPanel(div(style="display:inline-block;width:100%;",
                                              uiOutput(ns("ui_newrowname"))),
                                          div(style="display:inline-block;",
                                              actionButton(ns("addrow"),"اضافه کردن سطر")))),
                         #radioButtons(ns("newrowtype"), "Type", c("integer", "double", "character")),
                         column(width = 3,       
                                wellPanel(div(style="display:inline-block;width:100%;height:60%;",
                                              uiOutput(ns("ui_removerowname"))),
                                          div(style="display:inline-block;",
                                              actionButton(ns("removerow"),"حذف کردن سطر"))))
                       ))
               ))))



))
}


M0_Load <- function(input,output,session,outputDir){
  
  Date_US <- as.OtherDate(Sys.Date(),"persian")[1:3]
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
  
  
  
  observeEvent(input$f_newzero,{
    #D_new <- read.xlsx("/Users/ardalanmirshani/Dropbox/RAAVI/RAAVI-Released/www/Data.xlsx")
   
    D_new <- read.xlsx(file.path(getwd(),"www/Data.xlsx"))
    values[["now"]] <- D_new[,-1]
    values[["names"]] <- D_new[,1]
    values[["dates"]] <-colnames(D_new)[-1]
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
     B <- fileInput(inputId = session$ns("f_new"),
                      label = div(class="load__subtitle--font-size","آپلود کردن فایل جدید"),
                      buttonLabel = div(style="font-size:130%;","جستجو"),
                      placeholder = "فایلی موجود نیست",
                      width = "100%")
       return(list(B))
     # }else{
     #   A <- selectInput(inputId = session$ns("f_remove"),label = "نام دیتا",choices = File()$name) 
     #   B <- actionButton(session$ns("remove_f"), "پاک کردن")
     #   return(list(A,B))
     # }
   })
   
   
   output$f_make <- renderUI({
     actionButton(inputId = session$ns("f_newzero"),
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
  
  
  # Changing the table, Save previous work Handsontable
  observe({
      if(!is.null(input$hot)){
      #values[["previous"]] <- isolate(values[["now"]]) # current table values
        hot_data = hot_to_r(input$hot)
        values[["now"]] <- data.matrix(hot_data[-1,-1])
        values[["names"]] <- hot_data[-1,1]
        values[["dates"]] <- t(hot_data[1,-1])
        }  # DF is the r format of the table value
  })

  ## Add column
  output$ui_newcolname <- renderUI({
    textInput(session$ns("newcolumnname"), "", value = Date_Persian)   # sprintf("newcol%s", 1+ncol(values[["now"]]))
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
    textInput(session$ns("newrowname"), "", placeholder = "نام دانش آموز")#sprintf("newrow%s", 1+nrow(values[["now"]])) )
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
    selectInput(session$ns("removecolnamelist"), "",choices = values[["dates"]] )
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
    selectInput(session$ns("removerownamelist"),"",choices = values[["names"]])
  })
  
  observeEvent(input$removerow, {
    DF <- values[["now"]]
    values[["previous"]] <- DF
    count <- which(values[["names"]]==input$removerownamelist)
    A <- DF[-count,]
    values[["names"]] <- values[["names"]][-count]
    values[["now"]] <- A
  })
   
  
## Output  
  output$hot <- renderRHandsontable({
      if (!is.null(values[["now"]])){
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
        # renderer = "function(instance, td, row, col, prop, value, cellProperties) {
                # Handsontable.renderers.TextRenderer.apply(this, arguments);
                # td.style.color = 'white';
                # td.style.background = 'grey';}
                # " )  %>%
       #manualColumnMove and manualColumnResize works when colHeaders is not NULL!
       hot_rows(rowHeights = 40, fixedRowsTop = 1) %>%
       hot_col(col = 1:dim(Tot)[2] , valign = "htMiddle") %>%
      # valign works when colHeaders is not NULL!
       hot_cell(1,1,readOnly=TRUE)
      
      
      
      }
  })
  
  # D_new <- read.xlsx(file.path(getwd(),"www/Data.xlsx"))
  # now <- D_new[,-1]
  # names <- D_new[,1]
  # dates <-colnames(D_new)[-1]
  # 
  # r1 <- data.frame(cbind("نام",t(dates)),stringsAsFactors = FALSE)
  # r2 <- cbind(names,now)
  # names(r2) <-  
  # names(r1) <-
  # Tot <- rbind(r1,r2)
  # 
  # colname <- "sssss"
  # DF <- now
  # pre <- DF
  # newcolumn <- 0 #eval(parse(text=sprintf('%s(nrow(as.data.frame(DF)))', "integer" ))) #isolate(input$newcolumntype))))
  # now <- setNames(cbind(DF, newcolumn), c(dates, colname))
  # #values[["now"]] <- cbind(DF,0)
  # dates <- c(dates, colname)
  # 
  # 
  # 
  
  
  
  
  
  
  output$message <- renderUI({
      helpText(sprintf(""))
  })
  
## Save 

  
   output$save_name <- renderUI({
     textInput(inputId = session$ns("save_name"),label = "",value = Date_Persian,
               placeholder = "نام دیتا را وارد کنید")
   })
  
    #observe({
    output$downloadData <- downloadHandler(
      filename = function() {
        paste(input$save_name, ".xlsx", sep = "")
      },
      content = function(file) {
        finalDF <- cbind(values[["names"]],values[["now"]])
        colnames(finalDF)[1] <- "تاریخ آزمون"
        colnames(finalDF)[-1] <- values[["dates"]]
        write.xlsx(finalDF, file, row.names = FALSE)
        output$message <- renderUI({
          div(style="text-align:right;",
              helpText(sprintf("فایل \"%s\" با موفقیت ذخیره شد", input$save_name)))
          
        })
      }
    )
    #})
  
  
  ## Cancel last action    
  observeEvent(input$cancel, {
    if(!is.null(isolate(values[["previous"]]))) values[["now"]] <- isolate(values[["previous"]])
  })
  
  return(values)
  
}
