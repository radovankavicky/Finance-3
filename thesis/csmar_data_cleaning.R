
#Initialize data environment
DIRS_MAC_DIR <- "~/Documents/phd/thesis/"
DIRS_WIN_DIR <- "C:/programs/git/phd/phd/thesis/"
DIRS_DATA_UTIL <-"data_util.R"
DIRS_FILE_UTIL <- "file_util.R"
DIRS_TEST_MAC <- list.dirs(DIRS_MAC_DIR)
DIRS_RDATA_WIN <- "D:/CSMAR/"
DIRS_RDATA_MAC <-"~/CSMAR/"
DIRS_RDATA <-ifelse(length(DIRS_TEST_MAC)>1,DIRS_RDATA_MAC, DIRS_RDATA_WIN)
DIRS_FP_DATA <- ifelse(length(DIRS_TEST_MAC)>1,paste(DIRS_MAC_DIR,DIRS_DATA_UTIL,sep=""),
                       paste(DIRS_WIN_DIR,DIRS_DATA_UTIL,sep="") )

DIRS_FP_FILE <- ifelse(length(DIRS_TEST_MAC)>1,paste(DIRS_MAC_DIR,DIRS_FILE_UTIL,sep=""),
                       paste(DIRS_WIN_DIR,DIRS_FILE_UTIL,sep="") )
setwd(DIRS_RDATA)


source(DIRS_FP_DATA,encoding="UTF-8")
source(DIRS_FP_FILE,encoding="UTF-8")

rm(list=ls(pattern = "DIRS_"))



#read in data files----------------------------


f_readtable_list(f_list_file())

MN_map<-read.csv("~/CSMAR/util/Accruals_Name_MAP.csv",header = F,stringsAsFactors= F)



#Combining data------------------------------

names(MNMAPR_Accruals)<-MN_map$V3
j_ac<-arrange(MNMAPR_Accruals,Stkcd,Accper) %>%
  mutate(year=extractyear(Accper)) %>%
  select(-NO_USE1,-NO_USE2,-NO_USE3,-NO_USE4,-Accper)

j_trd<-arrange(TRD_Year,Stkcd,Trdynt) %>%
  mutate(year=Trdynt) %>%
  filter(Yarkettype %in% c(1,4,16)) %>%
  select(Stkcd,year,Yretwd,Ysmvttl)

j_hd <- arrange(HLD_Contrshr,Reptdt) %>%
  distinct(Stkcd)

j_fr <- left_join(FS_Combas,FS_Comins) %>%
  left_join(FS_Comscfd) %>%
  left_join(FS_Comscfi) %>%
  arrange(Stkcd,Accper,Typrep) %>%
  filter(substr(Accper,6,10)=="12-31",Typrep=="A") %>%
  mutate(year=extractyear(Accper)) %>%
  select(-Accper)

j_fi <-left_join(FI_T1,FI_T3) %>%
  left_join(FI_T4) %>%
  left_join(FI_T5) %>%
  left_join(FI_T6) %>%
  left_join(FI_T7) %>%
  left_join(FI_T8) %>%
  left_join(FI_T9) %>%
  left_join(FI_T2) %>%
  left_join(FI_T10) %>%
  arrange(Stkcd,Accper,Indcd,Typrep) %>%
  filter(substr(Accper,6,10)=="12-31",Typrep=="A") %>%
  mutate(year=extractyear(Accper)) %>%
  select(-Accper)

j_fa <- arrange(FIN_Audit,Stkcd,Accper) %>%
  filter(substr(Accper,6,10)=="12-31") %>%
  mutate(year=extractyear(Accper)) %>%
  select(-Accper)

df <- left_join(j_ac,j_fr) %>%
  left_join(j_fa) %>%
  left_join(j_trd) %>%
  left_join(j_fi) %>%
  left_join(j_hd) %>%
  left_join(HLD_Copro)
  


#Varibale screening------------------------------
#删掉0和NA过多的列
xdf<-rm_nacol(df,0.98) %>%
  arrange(Stkcd,desc(year)) %>%
  distinct(Stkcd,year) %>%
  #生成行业代码C仍然用2级，别的行业用1级
  mutate(INDC=as.factor(ifelse(substr(Indcd,1,1)=="C",substr(Indcd,1,2),substr(Indcd,1,1)))) %>%
  #生成一级行业代码
  mutate(INDCD=as.factor( substr(Indcd,1,1) ) ) %>%
  #找出国企
  mutate(SOE=substr(S0702b,1,2) %in% c("11","21","22","23")) %>%
  #找出交叉上市公司
  mutate(CROSSLIST=nchar(Crcd)>1) %>%
  #找出非标审计意见
  mutate(AUDIT=Audittyp=="标准无保留意见") %>%
  #只选A股
  filter(Stktype=="A") %>%
  #去掉部分没用的数据
  select(-S0702b,-Crcd,-Audittyp,-Indcd,-Stktype)
xdf[is.na(xdf)]<-0

#remove used data.frame objects--------------

rm(list=ls(pattern="FI_"))
rm(list=ls(pattern="FS_"))
rm(list=ls(pattern="HLD_"))
rm(list=ls(pattern="TRD_"))
rm(list=ls(pattern="FIN_"))
rm(list=ls(pattern="d_"))
rm(list=ls(pattern="j_"))
rm(list=ls(pattern="MN"))

#Varibale cleaning------------------------------
p_summary(xdf,id="Stkcd",t="year")

#contruct readable name data.frame 
dr<-select(xdf,Stkcd,year,TT_ASSET:TT_ACCRUAL,INDC:AUDIT)




#应收票据
dr$NOTERCV<-xdf$A001110000
#其他应收款
dr$ORCV<-xdf$A001121000
#长期待摊费用
dr$LTM_EXP<-xdf$A001221000
#存货
dr$INVENTORY<-xdf$A001123000
#应付账款
dr$ACC_PAYA<-xdf$A002108000
#应付税费
dr$TAX_PAYA<-xdf$A002113000
#其他流动资产
dr$OTH_CURASST<-xdf$A001125000
#营业税金
dr$OP_TAX<-xdf$B001207000
#销售费用
dr$SALES_EXP<-xdf$B001209000
#管理费用
dr$MGMT_EXP<-xdf$B001210000
#公允价值变动
dr$FAIR_CH<-xdf$B001301000
#年度股票回报率
dr$YRET<-xdf$Yretwd
#经营活动现金净流量
dr$OP_CFLOW<-xdf$C001000000
#营业收入
dr$OP_INCOME<-xdf$B001100000
#营业总成本
dr$OP_COST<-xdf$B001200000
#减值损失
dr$IMP_LOSS<-xdf$B001212000
#市净率
dr$M_B<-xdf$F100401A
#每股收益EPS
dr$EPS<-xdf$F020108
#总市值
dr$MKT_CAP<-xdf$Ysmvttl*1000
dr$INVEST<-xdf$C002006000


save.image(file="~/CSMAR/rdata/csmar_cleaned.RData")


#-----Possible Future Usage---------
# #总流动资产
# dr$TCA<-xdf$A001100000
# #现金
# dr$CASH<-xdf$A001101000
# #总流动负债
# dr$TCLIAB<-xdf$A002100000
# #折旧
# dr$DEPRE<-xdf$D000103000
# #总资产
# dr$TA<-xdf$A001000000
# #总收入
# dr$INCOME<-xdf$B001100000
# #固定资产
# dr$PPE<-xdf$A001212000
# #无形资产
# dr$INTANG<-xdf$A001218000
# #应收款
# dr$RCV<-xdf$A001111000
# #一年内到期的长期负债
# dr$LIAB1Y<-xdf$A002125000

# #净利润
# dr$PROFIT<-xdf$B002000000
# #财务费用
# dr$INTEREST<-xdf$B001211000
