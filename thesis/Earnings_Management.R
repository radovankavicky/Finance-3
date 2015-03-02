
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
load("MCS_INDEX.RData")
load("csmar_cleaned.RData")
source(DIRS_FP_DATA,encoding="UTF-8")

rm(list=ls(pattern = "DIRS_"))



#Earnings Management Data preparation--------------

DD0<-p_balance(dr,"Stkcd","year",2007,2013) %>%
  arrange(Stkcd,year) %>%
  group_by(Stkcd) %>%
  mutate(TACC_TA=TT_ACCRUAL/lag(TT_ASSET),
         LTA=lag(TT_ASSET),
         ONE_TA=1/LTA,
         DREV_TA=(CORE_INCOME-lag(CORE_INCOME))/lag(TT_ASSET),
         PPE_TA=lag(PPE_ORIG)/lag(TT_ASSET),
         REC=RECV+NOTERCV+ORCV,
         REC_TA=(REC-lag(REC))/lag(TT_ASSET),
         IAL_TA=INTANG/lag(TT_ASSET),
         DIF_TA=DREV_TA-REC_TA,
         DIF_WC=RECV-lag(RECV)+INVENTORY-lag(INVENTORY) -
           (ACC_PAYA-lag(ACC_PAYA))-(TAX_PAYA-lag(TAX_PAYA)) +
           OTH_CURASST-lag(OTH_CURASST),
         CFO_TA=OP_CFLOW/lag(TT_ASSET),
         REV_TA=(CORE_INCOME)/lag(TT_ASSET),
         COGS_TA=OP_COST/lag(TT_ASSET),
         PROD_TA=(OP_COST+INVENTORY-lag(INVENTORY))/lag(TT_ASSET),
         DDREV_TA=(lag(CORE_INCOME)-lag(lag(CORE_INCOME) )  )/lag(TT_ASSET),
         DISEXP_TA=(SALES_EXP+MGMT_EXP)/lag(TT_ASSET),
         LREV_TA=lag(CORE_INCOME)/lag(TT_ASSET)
         ) %>%
  winsor_df
  



#Time Series Modified Jones Model(Dechow,1995)--------------------
fm_ts <-TACC_TA ~ ONE_TA + DREV_TA + PPE_TA + 0

js2011 <-group_lm(filter(DD0,year<2011),by="Stkcd",fm_ts)
js2012 <-group_lm(filter(DD0,year<2012),by="Stkcd",fm_ts)
js2013 <-group_lm(filter(DD0,year<2013),by="Stkcd",fm_ts)
js11df <-ldply(js2011$mod,coef)
js12df <-ldply(js2012$mod,coef)
js13df <-ldply(js2013$mod,coef)

js11df<-cbind(js2011$Stkcd,js11df)
js11df$year<-2011
names(js11df) <-c("Stkcd","b_ONE_TA","b_DREV_TA","b_PPE_TA","year")
js12df<-cbind(js2012$Stkcd,js12df)
js12df$year<-2012
names(js12df) <-c("Stkcd","b_ONE_TA","b_DREV_TA","b_PPE_TA","year")
js13df<-cbind(js2013$Stkcd,js13df)
js13df$year<-2013
names(js13df) <-c("Stkcd","b_ONE_TA","b_DREV_TA","b_PPE_TA","year")

jsts <-rbind(js11df,js12df)
jsts <-rbind(jsts,js13df)
jsts <- na2zero_df(jsts)
jsts <- arrange(jsts,Stkcd,year)

EM <- filter(DD0,year>=2011) %>%
  left_join(jsts) %>%
  arrange(Stkcd,year) %>%
  group_by(Stkcd) %>%
  mutate(MODI_NDA=b_ONE_TA*ONE_TA+b_DREV_TA*(DREV_TA-REC_TA)+b_PPE_TA*PPE_TA,
               MODI_DA=winsor(TACC_TA-MODI_NDA)
               )

summary(EM$MODI_DA)



#Cross Sectional Jones Model(Lu,1997)-------


fm_cs <-TACC_TA ~ ONE_TA + DIF_TA + PPE_TA + IAL_TA  + 0
cs <-group_lm(filter(DD0,year>=2011),by="year",fm_cs)
cs2011<-resid(cs$mod[[1]])
cs2012<-resid(cs$mod[[2]])
cs2013<-resid(cs$mod[[3]])

cs11df<-select(DD0,Stkcd,year) %>%
  ungroup() %>%
  filter(year==2011) %>%
  mutate(LU_DA=winsor(cs2011))

cs12df<-select(DD0,Stkcd,year) %>%
  ungroup() %>%
  filter(year==2012) %>%
  mutate(LU_DA=winsor(cs2012))

cs13df<-select(DD0,Stkcd,year) %>%
  ungroup() %>%
  filter(year==2013) %>%
  mutate(LU_DA=winsor(cs2013))

jscs <-bind_rows(cs11df,cs12df) %>%
  bind_rows(cs13df) %>%
  arrange(Stkcd,desc(year)) %>%
  right_join(EM) %>%
  group_by(Stkcd)


EM<-jscs

rm(list=ls(pattern="js"))
rm(list=ls(pattern="cs"))
summary(EM$LU_DA)

o_descriptive(as.data.frame(select(EM,TACC_TA,ONE_TA,DIF_TA,PPE_TA,IAL_TA,LU_DA) ),
              head="EM",file="LU_DA.html"
)

#Dichev and Dechow Model----------------

DD<-select(DD0,Stkcd,year,DIF_WC,OP_CFLOW,TT_ASSET) %>%
  arrange(Stkcd,year) %>%
  group_by(Stkcd) %>%
  mutate(F_CFLOW=lead(OP_CFLOW)/TT_ASSET,
         L_CFLOW=lag(OP_CFLOW)/lag(TT_ASSET) ) %>%
  winsor_df

fm_roll <- DIF_WC ~ F_CFLOW + OP_CFLOW + L_CFLOW + 0

DD_tmp<-group_roll_lm(DD,fm_roll,by="Stkcd",windows = 3,align = "center")

DD<-group_roll_beta2df(DD,DD_tmp,windows = 3,id = "Stkcd",t="year")

DD1<- mutate(DD,
            DD_DA=winsor(DIF_WC - B_F_CFLOW*F_CFLOW + B_OP_CFLOW*OP_CFLOW +
                           B_L_CFLOW*L_CFLOW) )%>%
  select(Stkcd,year,DD_DA) %>%
  arrange(Stkcd,year) %>%
  filter(year>=2011) 
  


EM<-left_join(EM,DD1) %>%
  select(Stkcd,year,DD_DA,MODI_DA,LU_DA) %>%
  arrange(Stkcd,year)

summary(select(EM,DD_DA,MODI_DA,LU_DA))




#Roychowdhury（2006）model-------------------------

fm_cfo <- CFO_TA ~ ONE_TA + REV_TA + DREV_TA +0
fm_prod <- PROD_TA ~ ONE_TA + REV_TA + DREV_TA + DDREV_TA +0
fm_exp <- DISEXP_TA ~ ONE_TA + REV_TA + 0

R_CFO  <- group_lm(DD0,by="Stkcd",fm_cfo)

R_PROD <- group_lm(DD0,by="Stkcd",fm_prod)

R_EXP <- group_lm(DD0,by="Stkcd",fm_exp)

DD1<-group_lm_beta2df(DD0,R_CFO,by="Stkcd") %>%
  mutate(op_CFO=CFO_TA-B_ONE_TA*ONE_TA-B_REV_TA*REV_TA-B_DREV_TA*DREV_TA) %>%
  select(Stkcd,year,op_CFO)

DD2<-group_lm_beta2df(DD0,R_PROD,by="Stkcd") %>%
  mutate(op_PROD=PROD_TA-B_ONE_TA*ONE_TA-B_REV_TA*REV_TA
         -B_DREV_TA*DREV_TA) %>%
  select(Stkcd,year,op_PROD)

DD3<-group_lm_beta2df(DD0,R_EXP,by="Stkcd") %>%
  mutate(op_EXP=DISEXP_TA-B_ONE_TA*ONE_TA-B_REV_TA*REV_TA) %>%
  select(Stkcd,year,op_EXP)

EM <- left_join(EM,DD1) %>%
  left_join(DD2) %>%
  left_join(DD3) %>%
  mutate(OP_DA=op_PROD+op_EXP+op_CFO)


#Clean up--------------------
rm(list=ls(pattern="DD"))
rm(list=ls(pattern="fm_"))

save(EM,file="~/CSMAR/rdata/Earnings_Mangement.RData")
