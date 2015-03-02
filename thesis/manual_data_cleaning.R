
DIRS_MAC_DIR <- "~/Documents/phd/thesis/"
DIRS_WIN_DIR <- "C:/programs/git/phd/phd/thesis/"
DIRS_DATA_UTIL <-"data_util.R"
DIRS_FILE_UTIL <- "file_util.R"
DIRS_TEST_MAC <- list.dirs(DIRS_MAC_DIR)

DIRS_FP_DATA <- ifelse(length(DIRS_TEST_MAC)>1,paste(DIRS_MAC_DIR,DIRS_DATA_UTIL,sep=""),
                       paste(DIRS_WIN_DIR,DIRS_DATA_UTIL,sep="") )

DIRS_FP_FILE <- ifelse(length(DIRS_TEST_MAC)>1,paste(DIRS_MAC_DIR,DIRS_FILE_UTIL,sep=""),
                       paste(DIRS_WIN_DIR,DIRS_FILE_UTIL,sep="") )

source(DIRS_FP_DATA)

rm(list=ls(pattern = "DIRS_"))




d_man<-read.csv("~/CSMAR/rdata/mcs_idx.csv")
nm<-c("IC","CSR","FINCORP","SUBS","SUBMGMT","BUDGET","ERP","MGMTMASTER",
      "MGMTNUM","MGMTAVGAGE","STAFFEDUNUM","SUBNAME","MGMTSEA","MIS","MGMTCH",
      "STAFFNUM","WEB","WEIBO","WEIXIN","HORNOR","HR","PUB","STRATEGY",
      "CULTURE","BRAND","LOGO","Stkcd","year")
names(d_man)<-nm

d_man[is.na(d_man)]<-0


replace_na<-function(x){
  return(ifelse(x==-1,0,x))
}


d_man$MDA<-NULL
d_man$IC<-NULL
d_man$CSR<-NULL
d_man$SUBS<-NULL
d_man$SUBMGMT<-NULL
d_man$STAFFEDUNUM<-NULL
d_man$STAFFNUM<-NULL
d_man$FINCORP[d_man$FINCORP>1]<-1
d_man$MGMTMASTER<-replace_na(d_man$MGMTMASTER)
d_man$MGMTNUM<-replace_na(d_man$MGMTNUM)
d_man$MGMTAVGAGE<-replace_na(d_man$MGMTAVGAGE)
d_man$MGMTSEA<-replace_na(d_man$MGMTSEA)

load("~/CSMAR/rdata/COMBINED_TXT_ANAYSIS.RData")
d_txt$Stkcd<-as.integer(d_txt$corp)
d_txt$year<-as.integer(d_txt$year)

d_all<-left_join(d_man,d_txt)
d_all$corp<-NULL
d_all[is.na(d_all)]<-0

dm<-p_balance(d_all,"Stkcd","year")

dm<-arrange(dm,Stkcd,desc(year))

save(dm,file="~/CSMAR/rdata/ALL_MANUAL_DATA.RData")
