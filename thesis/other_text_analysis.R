source("~/Documents/phd/thesis/file_util.R")
require(dplyr)
require(stringr)
require(tm)
require(jiebaR)
require(wordcloud)
require(RTextTools)

setwd("~/CSMAR/fs")

#Analysis of CSR reports --------
fl_csr<-f_list_csr()
d_csr<-f_reader(fl_csr)
kw<-c("员工","环境","供应商","客户","公益","理念","制度")
kw_score<-f_hasword(kw,d_csr$txt)
colnames(kw_score)<-kw
d_csr<-cbind(d_csr,kw_score)
d_csr$txt<-NULL
d_csr$tlen<-NULL
d_csr$csrscore<-rowSums(d_csr[,3:9])

#Analysis of IC reports---------
fl_ic<-f_list_ic()
d_ic<-f_reader(fl_ic)
d_ic$icscore<-percent_rank(d_ic$tlen)
d_ic$txt<-NULL

#Analysis of  web strategy---------

fl_strategy<-f_list_strategy()
d_strategy<-f_reader(fl_strategy)
d_strategy$year<-NULL
d_strategy$txt<-NULL
d_strategy$tlen<-NULL

d_strategy$txtstrategy<-d_strategy$tlen>20


#Analysis of web culter---------

fl_culture<-f_list_culture()
d_culture<-f_reader(fl_culture)
d_culture$year<-NULL
d_culture$txt<-NULL
d_culture$tlen<-NULL
d_culture$txtculture<-d_culture$tlen>5

load("~/CSMAR/rdata/D_MGMT.RData")
d_txt<-left_join(d_mgmt,d_csr) %>%
    left_join(d_ic) %>%
    left_join(d_strategy) %>%
    left_join(d_culture) %>%
    select(corp,year,sentiment,csrscore,icscore,txtstrategy,txtculture)

d_txt[is.na(d_txt)]<-0

save(d_txt,file="~/CSMAR/rdata/COMBINED_TXT_ANAYSIS.RData")
