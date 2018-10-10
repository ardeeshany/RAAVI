###########################
#
# Data Import
#
###########################

# read_excel_allsheets
# r_excel_class
# r_excel_names






## Read excel file including different sheets and return a list including the separate sheets information. 
library(readxl)    
read_excel_allsheets <- function(filename) {
  sheets <- readxl::excel_sheets(filename)    #return names of the sheets...ریاضی، فیزیک، شیمی
  x <-    lapply(sheets, function(X) readxl::read_excel(filename, sheet = X)) #read each sheet and save them as a sepratae files...list of 3 classes
  names(x) <- sheets
  x
}


## main function for reading from xlsx file (extract a specific sheet with its name or number in list)
r_excel_class <- function(filename,n){
  Data <- read_excel_allsheets(filename)
  
  if(is.numeric(n))
    D <- as.data.frame(Data[[n]]) 
  
  if(is.character(n))
    D <- as.data.frame(Data[[which(names(Data)==n)]])
  
  rownames(D) <- D[,1]
  D <- D[,-1]
  D
}


## Reading names from xlsx
r_excel_names <- function(filename){
  Data <- read_excel_allsheets(filename)
  Class_names <- names(Data) #Class names
  rownames <- unlist((Data[[1]])[,1] ,use.names = FALSE) #Student names
  date <- colnames(as.data.frame(Data[[1]]))[-1] #date of exams
  out <- list(Class_names,rownames,date)
  names(out) <- c("class","student","date") #output
  return(out)
}


###########################
#
# Data Labeling for each class and course
#
###########################


#! Inserting By hand
num_CP <- 3  # number of Class P
class_label <- c("M","P","C")
##


#   Insert NAMES: e.g. Class_P2, names_P2, date_P2
##Labeling "class", "names" and "dates" for each calss...
##  assign every sheet to its label (Class_P2)
##  assign student names in each class to its label (names_P2)
##  assign every grade dates to its label (date_P2)
for(i in 2:num_CP){
  nam <- paste("class_P", i, sep = "")
  Data_num <- paste("Module/Extra/Data_P", i, ".xlsx", sep = "")
  assign(nam, r_excel_names(Data_num)$class)
  
  nam <- paste("names_P", i, sep = "")
  Data_num <- paste("Module/Extra/Data_P", i, ".xlsx", sep = "")
  assign(nam, r_excel_names(Data_num)$student)
  
  nam <- paste("date_P", i, sep = "")
  Data_num <- paste("Module/Extra/Data_P", i, ".xlsx", sep = "")
  assign(nam, r_excel_names(Data_num)$date)
}


# Insert DATA: e.g. DP2M
##Insert all Class P data in format like DP3M : Data Pish 3, Math
for(i in 2:num_CP){
  for(j in 1:length(class_P3)){
    nam <- paste("DP", i,  class_label[j],  sep = "")
    Data_num <- paste("Module/Extra/Data_P", i, ".xlsx", sep = "")
    assign(nam, r_excel_class(Data_num,j))
  }
}


### 
names_all <- c(names_P2,names_P3)

################
# Outside variables
################

CP2M_Mean <- round(colMeans(DP2M),digits = 2)
CP3M_Mean <- round(colMeans(DP3M),digits = 2)
