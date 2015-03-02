
#Initialize data environment
DIRS_MAC_DIR <- "~/Documents/phd/thesis/"
DIRS_WIN_DIR <- "C:/programs/git/phd/phd/thesis/"
DIRS_DATA_UTIL <-"data_util.R"
DIRS_FILE_UTIL <- "file_util.R"
DIRS_TEST_MAC <- list.dirs(DIRS_MAC_DIR)
DIRS_RDATA_WIN <- "D:/CSMAR/rdata"
DIRS_RDATA_MAC <-"~/CSMAR/rdata/"
DIRS_RDATA <-ifelse(length(DIRS_TEST_MAC)>1,DIRS_RDATA_MAC, DIRS_RDATA_WIN)
DIRS_FP_DATA <- ifelse(length(DIRS_TEST_MAC)>1,paste(DIRS_MAC_DIR,DIRS_DATA_UTIL,sep=""),
                       paste(DIRS_WIN_DIR,DIRS_DATA_UTIL,sep="") )

DIRS_FP_FILE <- ifelse(length(DIRS_TEST_MAC)>1,paste(DIRS_MAC_DIR,DIRS_FILE_UTIL,sep=""),
                       paste(DIRS_WIN_DIR,DIRS_FILE_UTIL,sep="") )
setwd(DIRS_RDATA)
load("csmar_cleaned.RData")
source(DIRS_FP_DATA,encoding="UTF-8")

rm(list=ls(pattern = "DIRS_"))

#data preparation for OVER_INV

DD <- arrange(dr,Stkcd,year) %>%
  group_by(Stkcd) %>%
  mutate(B_M=1/M_B,
         COREREV=OP_INCOME+OP_TAX-FAIR_CH,
         EARN_MKT=(CORE_PROFIT+OP_TAX-FAIR_CH)/lag(MKT_CAP),
         LLEV=lag(TT_LIAB/lag(MKT_CAP)),
         LSIZE=lag(log(TT_ASSET)),
         E_P=COREREV/lag(MKT_CAP),
         D=as.integer(YRET>=0),
         LROA=lag(ROA),
         LCASH=lag(CASH),
         LYRET=lag(YRET),
         FY=factor(year),
         LINVEST=lag(INVEST) ) %>%
  winsor_df(p=0.05) %>%
  ungroup() %>%
  p_balance(id="Stkcd",from=2007,to=2013)



lm_oi <- INVEST~ LINVEST+ B_M + LCASH + LSIZE + LLEV + LYRET + FY + INDC
  
DD_OI <- lm(lm_oi,data=DD,na.action = na.exclude)

DD$OVER_INV <- resid(DD_OI)

OV<-select(DD,Stkcd,year,OVER_INV) %>%
  filter(year>=2010)

save(OV,file="D:/CSMAR/rdata/Over_Ivest.RData")
