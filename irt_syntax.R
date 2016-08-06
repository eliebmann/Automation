setwd('/Users/eliebmann/Dropbox/IRT_2016')
source('/Users/eliebmann/Dropbox/IRT_2016/mplus.R')
dd <- read.csv("amber_merged.csv", na.strings = c("", " "))

df1 <- dd[dd$VISITNUM == "1", ]

df <- df1[, c(
  "PTID", "VISITYR", "VISITNUM", "BIRTHYR", "SEX", "RACE", "HISPANIC", "EDUC", "primary_Dx", "CogImpairmt_subCateg", "RESIDENC", "LIVSIT", "INBIRYR", "INSEX", "INEDUC", "INRELTO", "INRELTOX", "INLIVWTH", "INVISITS", "INCALLS", "MARISTAT", "MARISTAX", "JUDGMENT", "COMMUN", "HOMEHOBB", "PERSCARE", "CDRSUM", "CDRGLOB", "COMPORT", "CDRLANG",  
   "BILLS", "TAXES", "SHOPPING", "GAMES", "STOVE", "MEALPREP", "EVENTS", "PAYATTN", "REMDATES", "TRAVEL", "NPIQINF", "NPIQINFX", "DEL", "DELSEV", "HALL", "HALLSEV", "AGIT", "AGITSEV", "DEPD", "DEPDSEV", "ANX", "ANXSEV", "ELAT", "ELATSEV", "APA", "APASEV", "DISN", "DISNSEV", "IRR", "IRRSEV", "MOT", "MOTSEV", "NITE", "NITESEV", "APP", "APPSEV", "MMSE"
)]

library(plyr)
library(car)

df$INRELTO <- mapvalues(df$INRELTO, paste0(seq(1:7)), c("Spouse/partner", "Child", "Sibling", "Other relative", "Friend/Neighbor", "Paid caregiver", "Other"))
df$REL1 <- recode(df$INRELTO, "1 = '1'; else = '2'" )

df$RESIDENC <- mapvalues(df$RESIDENC, paste0(seq(1:5)), c("Single family residence", "Retirement community", "Assisted living", "Skilled Nursing", "Other"))

df$MARISTAT <- mapvalues(df$MARISTAT, c("1", "2", "3", "4", "5", "6", "9"), c("Married", "Widowed", "Divorced", "Separated", "Never Married", "Living as married", "Unknown"))

df$INVISITS <- recode(df$INVISITS, "NA = 1")
df$INCALLS <- recode(df$INCALLS, "NA = 1")

df$INEDU <- ifelse(df$INEDUC < 16, 0, 1)# less than college vs. college or more
df$INREL <- ifelse(df$INRELTO == "Spouse/partner", 0, 1)# s/p vs. other
df$MARIG <- ifelse(df$MARISTAT == "Married", 0, 1)# Married vs. unmarried
df$CALLS <- ifelse(df$INCALLS < 4, 0, 1)#Daily/weekly vs. monthly
df$VISITS <- ifelse(df$INVISITS < 4, 0, 1) #Daily/weekly vs. monthly
df$LIV <- ifelse(df$LIVSIT < 2, 0, 1)#lives by self vs. lives w/ someone
df$INAGE <- df$VISITYR - df$INBIRYR


which(colnames(df)=="INAGE")

###THIS Df recodes 8 (didn't do) as NA. To be included in ML estimation
###FOR USE WITH FIML#################################
FAQ_full <- df[, c(1, 31:40, 68:74, 14, 18, 67, 75)]
FAQ_full[, 2:11] <- lapply(FAQ_full[, 2:11], function(x) recode(x, "8 = NA")) 
FAQ_full$INAGE <- recode(FAQ_full$INAGE, "-7987 = NA")

#df.1 <- subset(df, BILLS != "8" & TAXES != "8" & SHOPPING != "8" & GAMES != "8" & STOVE != "8" & MEALPREP != "8" & EVENTS != "8" & PAYATTN != "8" & REMDATES != "8" & TRAVEL != "8" ) 

sapply(df.1, function(x) sum(is.na(x)))

lapply(df.1[, 31:40], function(x) table(df.1[, 28], x)) 
lapply(df.1[, 31:40], function(x) table(df.1[, 16], x)) 
lapply(df.1[, 31:40], function(x) table(df.1[, 18], x)) 

table(df$INLIVWTH, df$INVISITS)

#save(df.1, file = "IRT_ADC.Rdata")
##INLIVWITH = NA when val = 1, recode INVISITS to 1 to reflect daily exposure.
########FAQ ANALYSIS##############
########FAQ ANALYSIS#############
#FAQ <- df.1[, c(1, 31:40)]

library(MplusAutomation)
library(rhdf5)
library(ggplot2)
library(reshape2)

#prepareMplusData(FAQ, "faq.dat")
######ASSESS LOGICAL DEPENDENCE
###### FOR CG VARS##############
cgvars <- FAQ_full[, 12:20]
prepareMplusData(cgvars, "cg.dat")

cg1 <- mplusObject(
  TITLE = "CG - LOGI. DEPENDENCE;",
  VARIABLE ="
  USEVARIABLES ARE INEDU INREL MARIG CALLS VISITS 
  LIV;
  CATEGORICAL ARE INEDU INREL MARIG CALLS VISITS 
  LIV;",
  ANALYSIS = 
  "TYPE = BASIC;",
  rdata = cgvars
  )
mcg <- createSyntax(cg1, "cg_m1", check = TRUE)
rescg <- mplusModeler(cg1, run = 1L, dataout = "cg.dat", modelout = "cg.inp")
####PROBIT MODEL################
m1 <- mplusObject(
  TITLE = "IRT - FAQ;",
  VARIABLE = "
  USEVARIABLES ARE BILLS TAXES SHOPPING GAMES STOVE
  MEALPREP EVENTS PAYATTN REMDATES TRAVEL PTID;
  CATEGORICAL ARE BILLS TAXES SHOPPING GAMES STOVE
  MEALPREP EVENTS PAYATTN REMDATES TRAVEL;
  IDVARIABLE is PTID;",
  MODEL =
  " F1 BY *BILLS 
  TAXES 
  SHOPPING 
  GAMES 
  STOVE
  MEALPREP 
  EVENTS 
  PAYATTN 
  REMDATES 
  TRAVEL;
  F1@1;",
  OUTPUT = 
  "STDYX;
  RESIDUAL TECH10;",
  SAVEDATA = 
  "SAVE = FSCORES;
  DIFFTEST IS DIFF_PROB.dat;
  FILE IS FAQ_THETAS.dat;",
  PLOT = 
  "TYPE IS PLOT1;
  TYPE IS PLOT2;
  TYPE IS PLOT3;",
  rdata = FAQ_full
  )
  
m1syn <- createSyntax(m1, "faq_m1", check = TRUE)
res <- mplusModeler(m1, run = 1L, dataout = "m1d.dat", modelout = "m1.inp")
#######################################################
m2 <- mplusObject(
  TITLE = "IRT - FAQ;",
  VARIABLE = "
  USEVARIABLES ARE BILLS TAXES SHOPPING GAMES STOVE
  MEALPREP EVENTS PAYATTN REMDATES TRAVEL PTID;
  CATEGORICAL ARE BILLS TAXES SHOPPING GAMES STOVE
  MEALPREP EVENTS PAYATTN REMDATES TRAVEL;
  IDVARIABLE is PTID;",
  MODEL =
  " F1 BY *BILLS (L) 
  TAXES (L) 
  SHOPPING (L) 
  GAMES (L) 
  STOVE (L)
  MEALPREP (L) 
  EVENTS (L) 
  PAYATTN (L) 
  REMDATES (L) 
  TRAVEL (L);
  F1@1;",
  ANALYSIS = 
  "DIFFTEST = DIFF_PROB.dat;",
  OUTPUT = 
  "STDYX;
  RESIDUAL TECH10;",
  SAVEDATA = 
  "SAVE = FSCORES;
  FILE IS FAQ_THETAS.dat;",
  PLOT = 
  "TYPE IS PLOT1;
  TYPE IS PLOT2;
  TYPE IS PLOT3;",
  rdata = FAQ_full
  )
  
m2syn <- createSyntax(m2, "faq_m1", check = TRUE)
res2 <- mplusModeler(m2, run = 1L, dataout = "m1d.dat", modelout = "m2.inp")
#####################ADD COVS###########
m3 <- mplusObject(
  TITLE = "IRT - FAQ;",
  VARIABLE = "
  USEVARIABLES ARE BILLS TAXES SHOPPING GAMES STOVE
  MEALPREP EVENTS PAYATTN REMDATES TRAVEL PTID INEDU
  INSEX INLIVWTH MMSE;
  CATEGORICAL ARE BILLS TAXES SHOPPING GAMES STOVE
  MEALPREP EVENTS PAYATTN REMDATES TRAVEL;
  IDVARIABLE is PTID;",
  DEFINE = 
  "CENTER MMSE (GRANDMEAN);",
  MODEL =
  " F1 BY *BILLS 
  TAXES 
  SHOPPING 
  GAMES 
  STOVE
  MEALPREP 
  EVENTS 
  PAYATTN 
  REMDATES 
  TRAVEL;
  F1 ON INEDU INLIVWTH INSEX MMSE;
  F1@1;",
  OUTPUT = 
  "STDYX;
  RESIDUAL TECH10;",
  SAVEDATA = 
  "SAVE = FSCORES;
  DIFFTEST IS DIFF_PROB.dat;
  FILE IS FAQ_THETAS.dat;",
  PLOT = 
  "TYPE IS PLOT1;
  TYPE IS PLOT2;
  TYPE IS PLOT3;",
  rdata = FAQ_full
  )
  
m3syn <- createSyntax(m3, "faq_m1", check = TRUE)
res3 <- mplusModeler(m3, run = 1L, dataout = "m1d.dat", modelout = "m3.inp")
#############MULTI GROUP#############
mg_1 <- mplusObject(
  TITLE = "IRT - FAQ;",
  VARIABLE = "
  USEVARIABLES ARE BILLS TAXES SHOPPING GAMES STOVE
  MEALPREP EVENTS PAYATTN REMDATES TRAVEL INEDU;
  CATEGORICAL ARE BILLS TAXES SHOPPING GAMES STOVE
  MEALPREP EVENTS PAYATTN REMDATES TRAVEL;
  IDVARIABLE is PTID;
  GROUPING = INEDU (0 = LO 1 = HI);",
  MODEL =
  " F1 BY *BILLS 
  TAXES 
  SHOPPING 
  GAMES 
  STOVE
  MEALPREP 
  EVENTS 
  PAYATTN 
  REMDATES 
  TRAVEL;
  F1@1;
  MODEL HI: 
  {BILLS-TRAVEL @1}",
  ANALYSIS =
  "DIFFTEST IS DIFF_PROB.DAT;",
  OUTPUT = 
  "STDYX;
  RESIDUAL TECH10;
  MODINDICES (0);",
  SAVEDATA = 
  "SAVE = FSCORES;
  FILE IS FAQ_THETAS.dat;",
  PLOT = 
  "TYPE IS PLOT1;
  TYPE IS PLOT2;
  TYPE IS PLOT3;",
  rdata = FAQ_full
  )
  
mg_1syn <- createSyntax(mg_1, "faq_m1", check = TRUE)
res_mg1 <- mplusModeler(mg_1, run = 1L, dataout = "m1d.dat", modelout = "mg_1.inp")
#####FREED PAYTTN#####################
mg_2 <- mplusObject(
  TITLE = "IRT - FAQ;",
  VARIABLE = "
  USEVARIABLES ARE BILLS TAXES SHOPPING GAMES STOVE
  MEALPREP EVENTS PAYATTN REMDATES TRAVEL INEDU;
  CATEGORICAL ARE BILLS TAXES SHOPPING GAMES STOVE
  MEALPREP EVENTS PAYATTN REMDATES TRAVEL;
  IDVARIABLE is PTID;
  GROUPING = INEDU (0 = LO 1 = HI);",
  MODEL =
  " F1 BY *BILLS 
  TAXES 
  SHOPPING 
  GAMES 
  STOVE
  MEALPREP 
  EVENTS 
  PAYATTN 
  REMDATES 
  TRAVEL;
  F1@1;
  MODEL LO: F1 BY PAYATTN*;
  MODEL HI: 
  F1 BY PAYATTN*;
  {BILLS-TRAVEL @1}",
  OUTPUT = 
  "STDYX;
  RESIDUAL TECH10;
  MODINDICES (0);",
  SAVEDATA = 
  "DIFFTEST IS DIFF_PROB.dat;
  SAVE = FSCORES;
  FILE = FSCORES.dat",
  PLOT = 
  "TYPE IS PLOT1;
  TYPE IS PLOT2;
  TYPE IS PLOT3;",
  rdata = FAQ_full
  )
mg_2syn <- createSyntax(mg_2, "faq_m1", check = TRUE)
res_mg2 <- mplusModeler(mg_2, run = 1L, dataout = "m1d.dat", modelout = "mg_2.inp")
#######THETAS##############################
funct <- read.table("FAQ_THETAS.dat")
theta <- funct[, 12]
df.1$theta <- theta

param <- extractModelParameters(target = getwd())
faq_unstd <- param[2][[1]]$unstandardized
faq_disc <- faq_unstd[1:10, 3]

disc_num <- rep(1:nrow(faq_unstd[faq_unstd$paramHeader=="F1.BY", ]), each = 3)
diff_num <- c((nrow(faq_unstd[faq_unstd$paramHeader=="F1.BY", ])+1) : (nrow(faq_unstd)-1))

diffs <- c()
for (b in disc_num){
  for(n in diff_num){
    diffs[n] <- faq_unstd[n, 3]/faq_unstd[b, 3]
  }
}

##THIS IS A CBIND FILL FUNCTION#####
cbind.fill <- function(...){
    nm <- list(...) 
    nm <- lapply(nm, as.matrix)
    n <- max(sapply(nm, nrow)) 
    do.call(cbind, lapply(nm, function (x) 
        rbind(x, matrix(, n-nrow(x), ncol(x))))) 
}
##THIS IS A CBIND FILL FUNCTION#####

FAQpars <- data.frame(unlist(cbind.fill(faq_unstd, diffs)))
FAQpars_1 <- FAQpars[11:40, ]
FAQpars_1[, 3:7] <- lapply(FAQpars_1[, 3:7], as.character)
FAQpars_1[, 3:7] <- lapply(FAQpars_1[, 3:7], as.numeric)
FAQpars_1$param <- as.factor(as.character(FAQpars_1$param ))

FAQpars_1$item<- rep(c( "BILLS", "TAXES", "SHOPPING", "GAMES", "STOVE", "MEALPREP", "EVENTS", "PAYATTN", "REMDATES", "TRAVEL"), each = 3)

ggplot() + geom_line(aes(y = param, x = V7, group = item, color = item), data = FAQpars_1) + theme(text = element_text(size=10), axis.text.x = element_text(angle=90, vjust=1)) +xlim(c(0, 3)) 

xlim <- c(0, 3);
ylim <- c(0, 3);
px <- FAQpars_1$V7;
lx <- FAQpars_1$V7;
py <- rep(0, length(px));
ty <- rep(0, length(theta));
lx.buf <- 1;
ly <- .5;

par(xaxs = 'i', yaxs = 'i', mar = c(5, 1, 1, 1));
plot(NA, xlim=xlim, ylim=ylim, axes = FALSE, ann = FALSE);
axis(1);
segments(px, py, lx, ly);
#points(px, py, pch = 16, xpd = NA);
points(theta, ty, pch = 15, col = "red", cex = 0.75)
text(lx, ly, paste0(FAQpars_1$param), pos = 3, cex = 0.5, srt = 90);
#########LOGIT MODEL###############################
#########LOGIT - NOTE CHANGED DF FOR FIML EXPERIMENT
m1_logit <- mplusObject(
  TITLE = "IRT - FAQ;",
  VARIABLE = "
  CATEGORICAL ARE BILLS TAXES SHOPPING GAMES STOVE
  MEALPREP EVENTS PAYATTN REMDATES TRAVEL;
  IDVARIABLE is PTID;",
  ANALYSIS ="
  ESTIMATOR IS ML;
  LINK IS LOGIT;",
  MODEL =
  "F1 BY BILLS* TAXES SHOPPING GAMES STOVE
  MEALPREP EVENTS PAYATTN REMDATES TRAVEL;
  F1@1; [F1@0];",
  OUTPUT = 
  "STDYX;
  RESIDUAL;
  TECH10;",
  SAVEDATA = 
  "SAVE = FSCORES;
  FILE IS FAQlogit_THETAS.dat;",
  PLOT = 
  "TYPE IS PLOT1;
  TYPE IS PLOT2;
  TYPE IS PLOT3;",
  rdata = FAQ_full
  )

  m1syn_l <- createSyntax(m1_logit, "faqlogit_m1", check = TRUE)
  res_l <- mplusModeler(m1_logit, run = 1L, dataout = "m1dlogit.dat", modelout = "m1_logit.inp")
