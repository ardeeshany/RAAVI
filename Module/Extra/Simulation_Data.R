##############################
#
# Creating a xlsx file with multiple sheets from simulation the corresponding data in R
#
##############################


# set.seed(101)
# DP3M <- matrix(round(rtruncnorm(n=120, a=11, b=20, mean = 17, sd = 1),digits = 1),12,10)
# DP3P <- matrix(round(rtruncnorm(n=120, a=15, b=20, mean = 16, sd = 1),digits = 1),12,10)
# DP3C <- matrix(round(rtruncnorm(n=120, a=15, b=20, mean = 18, sd = 1),digits = 1),12,10)
# 
# DP2M <- matrix(round(rtruncnorm(n=120, a=15, b=20, mean = 17, sd = 2),digits = 1),12,10)
# DP2P <- matrix(round(rtruncnorm(n=120, a=15, b=20, mean = 16, sd = 1),digits = 1),12,10)
# DP2C <- matrix(round(rtruncnorm(n=120, a=15, b=20, mean = 18, sd = 0.5),digits = 1),12,10)
###
#names_En <- c("Ali Molaei","Bahram Heshmat","Cris","Dina Taghavi","Elham Pad","Farhad Minoei")

# names <- c("رضا مولایی",
#            "محمود وکیلی",
#            "علی شمس",
#            "رضا برهانی مرند",
#            "سینا وکیلی",
#            "رضا خوشخو",
#            "کسری نیک فرجام",
#            "سید محسن ابطحی",
#            "ندا اشرفی",
#            "اکرم سینایی",
#            "پوریا مقدسی",
#            "احمد رضا معین")
# 
# date_ymd <- c("1396-07-01","1396-07-05","1396-08-11","1396-08-23",
#               "1396-09-2","1396-09-12","1396-09-26","1396-10-07",
#               "1396-10-23","1396-10-25")
# 
# date_md <- c("07-01","07-05","08-11","08-23",
#           "09-2","09-12","09-26","10-07",
#           "10-23","10-25")
# 
# date <- date_md
# 
# rownames(DP3M) <- names
# colnames(DP3M) <- date
# 
# rownames(DP3P) <- names
# colnames(DP3P) <- date
# 
# rownames(DP3C) <- names
# colnames(DP3C) <- date
# 
# rownames(DP2M) <- names
# colnames(DP2M) <- date
# 
# rownames(DP2P) <- names
# colnames(DP2P) <- date
# 
# rownames(DP2C) <- names
# colnames(DP2C) <- date
# 
# wb <- createWorkbook()
# 
# addWorksheet(wb, "ریاضی")
# writeData(wb, sheet = "ریاضی", x = DP2M,colNames = TRUE,rowNames = TRUE)
# 
# addWorksheet(wb, "فیزیک")
# writeData(wb, sheet = "فیزیک", x = DP2P,colNames = TRUE,rowNames = TRUE)
# 
# addWorksheet(wb, "شیمی")
# writeData(wb, sheet = "شیمی", x = DP2C,colNames = TRUE,rowNames = TRUE)
# 
# 
# saveWorkbook(wb, file = "/Users/ardalanmirshani/Dropbox/OCC/Shiny Templates/Shiny apps/Data_P2.xlsx", overwrite = TRUE)
# openXL(wb)