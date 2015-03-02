#forewords: all following codes are tested with Mac version of R 3.12
#Due to the problematic unicode character encoding in Windows
#All Chinese text analyses tasks are suggested to perform under Mac or Linux
#Mac is an ideal platform cause the packages are readily available 
#Under Linux many r packages need to be compiled by yourself


#set RStudio encoding to UTF-8
#my Sys.getlocale() results:"en_US.UTF-8/...." no need to set to zh_CN.UTF-8
#if the raw text is encoded other than UTF-8
#Access Mac Appstore and download a free program called "汉字编码转换"
#This app can do encoding convertion in batch mode
require(plyr)
require(stringr)
require(tm)
require(jiebaR)
require(wordcloud)
require(dplyr)


#universal jiebaR cutter for all text splitting purpose
jcutter<-worker(user ="/Users//Luis/CSMAR//util//finance.txt")

#----------------helper----------------------
#function to extract firm year info
f_firm_year<-function(str){
  tmp_firm<-as.integer(substr(str,1,6))
  tmp_year<-as.integer(substr(str,8,11))
  tmp<-cbind(tmp_firm,tmp_year)
  return(tmp)
}

#funtion to align integer firm-id to string
f_id2str<-function(firmid){
  tmp<-formatC(firmid,width = 6,flag = "0")
  return(tmp)
}

#fucntion to find if there is any word match, keywords is a char vector
f_hasword<-function(keywords,strVector) {
  nk<-length(keywords)
  ns<-length(strVector)
  ncount<-rep(0,nk*ns)
  ncount<-matrix(ncount,nrow = ns,ncol = nk)
  colnames(ncount)<-keywords
  for(i in 1:nk) {
    tmp<-grep(keywords[i],strVector)
    ncount[tmp,i]<-1
  } 
  return(ncount)
}

#word cutter wrapper
jcut <- function(str="",cutter) {
  
  ftmp <- cutter<=str
  ftmp<-paste(ftmp,collapse = " ")
  return(ftmp)
}

#fucntion to do word split
f_split<-function(str) {
  
  chars <-sapply(str,jcut,cutter=jcutter,USE.NAMES = F)
  return(chars)

}


#---------------------enum file names-----------------------
#funtion wrapper for enumerate file names with specific extension 
f_list_file<-function(f_path = ".",ext="txt"){
  p<-paste("*.",ext,"$",sep="")
  tmp<-list.files(path=f_path,recursive = F,pattern = p)
  return(tmp)
}

#funtion wrapper for enumerate MD&A file names
f_list_mgmt<-function(f_path = "."){
  tmp<-list.files(path=f_path,recursive = T,pattern = "*A\\d{4}\\.txt$")
  return(tmp)
}

#funtion wrapper for enumerate internal control file names
f_list_ic<-function(f_path= "."){
  tmp<-list.files(path=f_path,recursive = T,pattern = "*B\\d{4}\\.txt$")
  return(tmp)
}

#funtion wrapper for enumerate CSR file names
f_list_csr<-function(f_path= "."){
  tmp<-list.files(path=f_path,recursive = T,pattern = "*C\\d{4}\\.txt$")
  return(tmp)
}

#funtion wrapper for enumerate strategy file names
f_list_strategy<-function(f_path= "."){
  tmp1 <-list.files(path=f_path,recursive = T,pattern = "*D\\d{0}\\.txt$")
  tmp2 <-list.files(path=f_path,recursive = T,pattern = "*D\\d{4}\\.txt$")
  f_m<-t(sapply(tmp2,f_firm_year,simplify = T))
  t_tb<-tbl_df(as.data.frame(f_m))
    colnames(t_tb)<-c("firm","year")

  t_uniq<-t_tb[!duplicated(t_tb[c("firm")]),]
  t_uniq<-t_uniq[(!is.na(t_uniq$firm)),]
  t_uniq<-t_uniq[(!is.na(t_uniq$year)),]
  names<-paste(f_id2str(t_uniq$firm),"D",as.character(t_uniq$year),".txt",sep="")
  names<-c(tmp1,names)
  return(names) 
}

#funtion wrapper for enumerate culture file names
f_list_culture<-function(f_path= "."){
  tmp1 <-list.files(path=f_path,recursive = T,pattern = "*E\\d{0}\\.txt$")
  tmp2 <-list.files(path=f_path,recursive = T,pattern = "*E\\d{4}\\.txt$")
  f_m<-t(sapply(tmp2,f_firm_year,simplify = T))
  t_tb<-tbl_df(as.data.frame(f_m))
  colnames(t_tb)<-c("firm","year")
  
  t_uniq<-t_tb[!duplicated(t_tb[c("firm")]),]
  t_uniq<-t_uniq[(!is.na(t_uniq$firm)),]
  t_uniq<-t_uniq[(!is.na(t_uniq$year)),]
  names<-paste(f_id2str(t_uniq$firm),"E",as.character(t_uniq$year),".txt",sep="")
  names<-c(tmp1,names)
  return(names) 
}

#------------read in CSMAR tab delim csv or txt file-----
#batch version
f_readtable_list<-function(files){
  #初始化一个与文本文件总数量等长的空list
  n_fl<-length(files)
  lstname<-list()
  length(lstname)<-n_fl
  lstname<-strsplit(files,split = "[.]")
  for(i in 1:n_fl)
  {
    oname <- lstname[[i]][1]
    print(oname)
    assign(oname, f_readtable(files[i]),envir=globalenv())
  }
}

#single_version
f_readtable<-function(file){

  df<-read.table(file,header = T,sep="\t",stringsAsFactors = F, na.strings = "NULL") %>%
    tbl_df()
 
  return(df)
}


#-------------scan in text files------------------
f_substr_test<-function(x,f,l){
  tmp<-strsplit(x,"[.]")[[1]][1]
  if(nchar(tmp)<11) return("9999")
  substr(x,f,l)
}

f_reader<-function(files) {
  #初始化一个与文本文件总数量等长的空list
  n_fl<-length(files)
  lst<-list()
  length(lst)<-n_fl 
  #用一个df容纳所有的公司和年度信息
  corp<-sapply(files,substr,1,6,USE.NAMES = F)
  
  year<-sapply(files,f_substr_test,8,11,USE.NAMES = F)
  df_fs<-data_frame(corp,year)      
  #批量读入文本
  for(i in 1:n_fl){
    tmp<-scan(file=files[i],what=character(),encoding = "UTF-8",skipNul=T) %>%
      paste(collapse = " ") 
    lst[i]<-tmp
  }
  
  df_fs$txt<-unlist(lst)
  df_fs<- mutate(df_fs,tlen=nchar(txt))
  
  return(tbl_df(df_fs))
}

#------------text mining--------------------
#define a list or vector as VectorSource corpus
fm_tdm<-function(list_chr){
  #Chinese stopwords for all text splitting purpose
  stopwordCN<-readLines("~/CSMAR/util/stopwordcn.txt",encoding = "UTF-8",skipNul = T,warn = F)
  
  if(class(list_chr)=="list") list_chr<-unlist(list_chr)
  f1<-VectorSource(list_chr) %>%
    Corpus %>%
    tm_map(removeWords,stopwordCN)
  
  #generate term document matrix
  tdm <-TermDocumentMatrix(f1)
  tdm<-removeSparseTerms(tdm,0.9999)
  return(tdm)
}  

#Kmeans cluster analysis
fm_cluster<-function(tdm,ngroup=5){
  tdm_matrix <- t(as.matrix(tdm))
  km <- kmeans(tdm_matrix , ngroup)
  return(km)
}

#function wrapper for wordcloud generating
fm_wc<-function(tdm){
  #use Heiti fonts to let mac system work for Chinese characters
  par(family = "STHeiti")
  
  #generate tdm and sort
  m1<-as.matrix(tdm)
  m1 <- sort(rowSums(m1),decreasing=TRUE)
  m1<-data.frame(word = names(m1),freq=m1)
  
  wordcloud(m1$word,m1$freq, max.words=80, scale=c(1.5,0.5),
            random.order=FALSE,rot.per=0.5, use.r.layout=T,colors=brewer.pal(8,"Dark2")) 
}
