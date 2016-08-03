dd <- read.csv("amber_merged.csv", na.strings = c("", " "))

df1 <- dd[dd$VISITNUM == "1", ]

df <- df1[, c(
  "PTID", "VISITYR", "VISITNUM", "BIRTHYR", "SEX", "RACE", "HISPANIC", "EDUC", "primary_Dx", "CogImpairmt_subCateg", "RESIDENC", "LIVSIT", "INBIRYR", "INSEX", "INEDUC", "INRELTO", "INRELTOX", "INLIVWTH", "INVISITS", "INCALLS", "MARISTAT", "MARISTAX", "JUDGMENT", "COMMUN", "HOMEHOBB", "PERSCARE", "CDRSUM", "CDRGLOB", "COMPORT", "CDRLANG",  
   "BILLS", "TAXES", "SHOPPING", "GAMES", "STOVE", "MEALPREP", "EVENTS", "PAYATTN", "REMDATES", "TRAVEL", "NPIQINF", "NPIQINFX", "DEL", "DELSEV", "HALL", "HALLSEV", "AGIT", "AGITSEV", "DEPD", "DEPDSEV", "ANX", "ANXSEV", "ELAT", "ELATSEV", "APA", "APASEV", "DISN", "DISNSEV", "IRR", "IRRSEV", "MOT", "MOTSEV", "NITE", "NITESEV", "APP", "APPSEV"
)]

library(plyr)

df$INRELTO <- mapvalues(df$INRELTO, paste0(seq(1:7)), c("Spouse/partner", "Child", "Sibling", "Other relative", "Friend/Neighbor", "Paid caregiver", "Other"))

df$RESIDENC <- mapvalues(df$RESIDENC, paste0(seq(1:5)), c("Single family residence", "Retirement community", "Assisted living", "Skilled Nursing", "Other"))

df$MARISTAT <- mapvalues(df$MARISTAT, c("1", "2", "3", "4", "5", "6", "9"), c("Married", "Widowed", "Divorced", "Separated", "Never Married", "Living as married", "Unknown"))

library(car)

df$INVISITS1 <- recode(df$INVISITS, "NA = 1")

df.1 <- subset(df, BILLS != "8" & TAXES != "8" & SHOPPING != "8" & GAMES != "8" & STOVE != "8" & MEALPREP != "8" & EVENTS != "8" & PAYATTN != "8" & REMDATES != "8" & TRAVEL != "8" ) 

sapply(df.1, function(x) sum(is.na(x)))

which(colnames(df.1)=="INVISITS")

lapply(df.1[, 31:40], function(x) table(df.1[, 28], x)) 
lapply(df.1[, 31:40], function(x) table(df.1[, 16], x)) 
lapply(df.1[, 31:40], function(x) table(df.1[, 18], x)) 

table(df$INLIVWTH, df$INVISITS)

##INLIVWITH = NA when val = 1, recode INVISITS to 1 to reflect daily exposure.

library(MplusAutomation)

