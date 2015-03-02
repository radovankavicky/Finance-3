source("~/Documents/phd/thesis/file_util.R")
require(dplyr)
require(stringr)
require(tm)
require(jiebaR)
require(wordcloud)
require(RTextTools)

setwd("~/CSMAR/fs/")

# Wordclouds function for sample data -------------------------------------

fm_wc2png<-function(firmid,year,strVector){
  fname<-f_id2str(firmid)
  yname<-as.character(year)
  flname<-paste(fname,yname,".png",sep = "")
  
  png(file = flname, bg = "white",width = 500,height = 600,units = "px")
  par(cex=2,mar=c(0.01,0.01,0.01,0.01),oma=c(0.01,0.01,0.01,0.01))
  fm_wc(fm_tdm(strVector))
  dev.off()
}

#---------MD&A data preparation for manual classification---------------
#read in all MD&A texts
all_mgmt<-f_reader(f_list_mgmt())
#word split
all_mgmt$txt<-f_split(all_mgmt$txt)


#choose random sample from mgmt texts

#----Run Once Block-------
#run only first time to ensure the sample are the same 
#And then manually rate sentiment ofthe sample data in CSV

#sample_mgmt<-sample_n(all_mgmt,150)
#save(sample_mgmt,file="~/Documents/phd/thesis/mgmtsample.RData")
#write.csv(sample_mgmt,file="~/Documents/phd/thesis/mgmtsample.csv")
#RData file can be load back using load()
#setwd("~/Documents/phd/")


#generate all sample wordclouds

# for (i in 1:length(sample_mgmt$corp)){
#   fm_wc2png(sample_mgmt$corp[i],sample_mgmt$year[i],sample_mgmt$txt[i])
#   
# }

#---------------join back rated file----
f_rated<-read.csv("~/CSMAR/rdata/mgmt_sample.csv",stringsAsFactors=F)

#change type to char for matching purpose
f_rated$corp<-f_id2str(as.integer(f_rated$corp))
f_rated$year<-as.character(f_rated$year)

#the raw data with manually coded sentiments in 1:150
tt_mgmt<-select(all_mgmt,corp,year,txt) %>%
  left_join(f_rated,by = c("corp","year")) %>%
  arrange(senti,corp,year) 

#total observations of data
nobs<-length(tt_mgmt$corp)



#---------training model-------

#create a document term matrix
tmatrix<-create_matrix(tt_mgmt$txt)

#training container virgin=F stands for training set
t_container <- create_container(tmatrix,tt_mgmt$senti,trainSize=1:100, testSize=101:150,virgin=F)

#automatic rating container using supervised machine learning algorithm
#virgin=T stands for real classification set
r_container <- create_container(tmatrix,tt_mgmt$senti,trainSize=1:150, testSize=151:nobs,virgin=T)

#using t_container to train a set of machine learning algorithms 
ml_models <- train_models(t_container, algorithms=c("RF","SVM","GLMNET","MAXENT","BOOSTING"))

#classification using trained ml_models
ml_results <- classify_models(r_container,ml_models)



r_mgmt<-select(tt_mgmt,corp,year,senti)
r_mgmt<-bind_cols(r_mgmt[151:nobs,],ml_results)
r_mgmt<-bind_rows(r_mgmt,tt_mgmt[1:150,])

r_mgmt$txt<-NULL
factor2int<-function(x){
  tmp<-as.integer(x)-1
  return(tmp)
}

tmp<-r_mgmt[,c("FORESTS_LABEL","SVM_LABEL","GLMNET_LABEL","MAXENTROPY_LABEL","LOGITBOOST_LABEL" )]
tmp<-sapply(tmp,factor2int)
sentiment<-rowSums(tmp)>0
r_mgmt<-cbind(r_mgmt,sentiment)
r_mgmt[!is.na(r_mgmt$senti),"sentiment"]<-r_mgmt[!is.na(r_mgmt$senti),"senti"]

d_mgmt<-select(r_mgmt,corp,year,sentiment)
save(d_mgmt,file="~/CSMAR/rdata/D_MGMT.RData")
save(r_mgmt,file="~/CSMAR/rdata/MDA_ALL.RData")
write.csv(r_mgmt,file="~/CSMAR/rdata/MDA_ALL.csv")
