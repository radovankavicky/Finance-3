require(plyr)
require(zoo)
require(AER)
require(stargazer)
require(dplyr)
require(lazyeval)


#dplyr包里的主要函数都有加_的版本，可以接受字符串参数
#比如下面用的filter_, group_by_
#这样就可以接受lazyeval变量，在执行时拼出来参数
#主要可以处理不同面板data.frame里面个体变量id和时间变量t命名不同的问题
#直接将字符串传递到函数里

p_balance<-function(df,id="Stkcd",t="year",from=2011,to=2013) {
  #计算时间变量长度
  ncount<-to-from+1
  #如果时间变量是字符的，改成整数
  df[[t]]<-as.integer(df[[t]])
  #传递给dplyr::filter_的参数，方便执行期动态转换为df实际的变量名
  xt<-list(pt=as.symbol(t),pfrom=from,pto=to)
  #真正用dplyr执行筛选平衡面板的部分
  #先排序，然后根据id分组，选出程序参数里定好的时间范围
  #选出来平衡的数据，然后去掉分组，返回处理好的df，时间变为integer了
  df<-arrange_(df,.dots=list(id,t)) %>%
    group_by_(.dots=list(id)) %>%
    filter_(interp("pt>=pfrom & pt<=pto",.values = xt)) %>%
    filter(n()==ncount) %>%
    ungroup()
  return(df)
}

#描述面板数据的类型
p_summary<-function(df,id="Stkcd",t="year"){
  df[[t]]<-as.integer(df[[t]])
  #输出是否有id和t重复的观测
  n_dup<-duplicated(df[,c(id,t)])
  if(sum(n_dup,na.rm=T)>0) {
    print("There is duplicate record,check id and t variable!")
    print(df[n_dup,])
  }  
  #用lazyeval来让运行时决定id和t的实际名
  p_max<-interp(quote(max(var)), var = as.name(t))
  p_min<-interp(quote(min(var)), var = as.name(t))
  p_n<-interp(quote(n()))
  #参数列表
  dots<-list(p_min,p_max,p_n)
  #先排序，按照id分组，分别统计最小、最大时间值和每个id观测数量
  #然后再生成一个组合的字符变量pattern 小-大-长度
  #再次根据pattern分组，然后统计计算总数
  p<-arrange_(df,.dots = list(id,t)) %>%
    group_by_(.dots = list(id)) %>%
    dplyr::summarise_(.dots = setNames(dots, c("min_t","max_t","obs_t"))) %>%
    mutate(pattern = paste(as.character(min_t),as.character(max_t),as.character(obs_t),sep="-")) %>%
    group_by(pattern) %>%
    dplyr::summarise(style = n())
    
  #输出时间分布类型
  print(p)
  barplot(p$style,names.arg=p$pattern)
  #return(p)
}

#从年报字符串提取年信息，变为integer
extractyear<-function(x){
  year<-substr(x,1,4)
  year<-as.integer(year)
  return(year)
}

#相关系数矩阵，下Pearson上Spearman
corr<-function(x){
  p_cor<-cor(x,use = "complete");
  s_cor<-cor(x,use = "complete",method="spearman");
  p_cor[upper.tri(p_cor)==TRUE]<-s_cor[upper.tri(s_cor)==TRUE];
  return(p_cor);
}

#描述性统计输出
o_descriptive<-function(...,head="Descriptive Statistics",file="./desc.htm"){
  message("Try to use as.data.frame for tbl_df objects")
  stargazer(...,
            type="html",title=head,
            out.header=TRUE,summary.logical=T,
            digits=2,median=TRUE, summary=T,
            column.sep.width ="10pt",
            out=file
  )
}

#相关系数矩阵输出
o_corr<-function(...,head="Correlations",file="./cor.htm"){
  stargazer(...,
            type="html",title=head,
            out.header=TRUE,summary.logical=FALSE,
            digits=2,median=TRUE,
            out=file,
            notes="Left:Pearson,Right:Spearman"
  )
}
#回归结果输出
o_reg<-function(...,head="Regression Result",ylab,ctrl,ctlab,file="./reg.htm"){
  stargazer(...,type="html",title=head,
            report="vc*p",digits=3,
            dep.var.labels=ylab,
            model.names=FALSE,model.numbers=TRUE,
            header=FALSE,
            omit=ctrl,omit.labels=ctlab,
            flip=TRUE,out.header=TRUE,out=file)
}
 
#Winsorize函数，缩尾用
winsor<-function(x,p=0.01) {
  if(length(p) != 1 || p < 0 || p > 0.5) {
    stop("bad p-value for winsorization")
  }
  lim <- quantile(x, probs=c(p, 1-p),na.rm = T)
  x[ x < lim[1] ] <- lim[1]
  x[ x > lim[2] ] <- lim[2]
  return(x)
}

winsor_df<-function(x,p=0.01){
  for(i in names(x)){
    
    if(class(x[[i]])=="numeric") {
       x[[i]] <- winsor(x[[i]],p)
    }
    
  }
  return(x)
  
}

#分组OLS回归，可以分年或者分公司，分行业.
group_lm<-function(df,by="year",formula){ 
  mds <- group_by_(df,.dots = list(by)) %>%
    do(mod=lm(formula, data = .,na.action = na.exclude))
  return(mds)
}

#将回归结果中的变量名符号变成_然后去掉空格
norm_beta_name <- function(x) {
  tmp<-gsub("[[:punct:]]", "_", x)
  tmp<-gsub("[[:space:]]", "", tmp)
  return(tmp)
}

#提取group_roll_lm的结果，合并回data frame，合并时会去掉windows-1行
group_lm_beta2df <- function(df,grouplm, by="year"){
  message("May introduce NA ")
  lmdf <-ldply(grouplm[[2]],coef)
  tn<-names(lmdf)
  tn<-norm_beta_name(tn)
  tn<-paste("B_",tn,sep="")
  names(lmdf)<-tn
  lmdf<-cbind(grouplm[by],lmdf)
  tdf<-left_join(df,lmdf,by=by)
  return(tdf)
}

#根据by来进行滚动回归，windows是滚动期数,align是对齐方式,返回回归系数
group_roll_lm <- function(df,by="Stkcd",formula,windows=3,align="center"){ 
  message("Needs continous panel data,better p_balance first")
  mds <- group_by_(df,.dots = list(by)) %>%
    do(mod=rollapply(data=., width=windows, by.column=FALSE, align=align,
                     FUN= function(x){
                       tmp<-lm(formula,data=as.data.frame(x),na.action = na.exclude)
                       return(coef(tmp))}
                     )
    )
  return(mds)
}




#提取group_roll_lm的结果，合并回data frame，合并时会去掉windows-1行
group_roll_beta2df <- function(df,grouplm, windows =3,id="Stkcd",t="year"){
  message("note that windows-1 rows will be discarded ")
  minytr <- min(df[[t]],na.rm = T)
  #增加了对变量的2次转置，确保如果是单行数据的话变成列形式
  lmdf<-ldply(grouplm[[2]],function(x,...){
    x<-t(t(x))
    rbind(x,...)
  })
  tn<-names(lmdf)
  tn<-norm_beta_name(tn)
  tn<-paste("B_",tn,sep="")
  names(lmdf)<-tn
  
  xt<-list(pt=as.symbol(t),pfrom=minytr+windows-1)
  
  tdf<-arrange_(df,.dots=list(id,t)) %>%
    filter_(interp("pt>=pfrom",.values = xt))
  
  tdf<-cbind(tdf,lmdf)
  return(tdf)
}


group_roll_func <- function(df,by="Stkcd",windows=3,align="center",func=sd,...){ 
  message("Needs continous panel data,better p_balance first")
  mds <- group_by_(df,.dots = list(by)) %>%
    do(mod=rollapply(data=., width=windows, by.column=FALSE, align=align,
                     FUN= func,...)
    )
  return(mds)
}


#去除NA或0比例高于percent的变量列
rm_nacol <- function(x,percent){
  nx<-length(x[[1]])
  for(i in names(x)){
    
    if(class(x[[i]])=="numeric") {
      j <- sum(as.integer(is.na(x[[i]]) | x[[i]]==0))/nx
      if(j>percent) x[[i]] <- NULL
    }
    
  }
  return(x)
}

#将data.frame中NA换成0
na2zero_df <- function(x){
  
  for(i in names(x)){
    
    if(class(x[[i]])=="numeric") {
      x[is.na(x[[i]]),i]<-0     
    }  
  }
  return(x)
}

o_png<-function(file="png1.png",w=600,h=600){
  png(filename = file,
      width = w, height = h, units = "px", pointsize = 12,
      bg = "white", family = "SimHei", restoreConsole = TRUE,
      type = "windows", antialias="cleartype")
  
  
}

