#7500qPCR结果计算
 
library("readxl")
library("plyr")
library("xlsx")
library("reshape2")
library("ggplot2")
 
calculate<-function(path,nc_name,test_name,nc_taget,test_taget){
  
   data_t<-read_excel(path,sheet="Results",skip=6)
   for(i in seq(1,length(data_t$Well))){
     if(is.na(data_t$Well[[i]])) break
   }
   data_t<-data_t[1:i-1,]
   
   a<-data_t[data_t$`Sample Name`%in%c(nc_name)&data_t$`Target Name`%in%c(nc_taget),"Cт"]
   b<-data_t[data_t$`Sample Name`%in%c(nc_name)&data_t$`Target Name`%in%c(test_taget),"Cт"]
   c<-data_t[data_t$`Sample Name`%in%c(test_name)&data_t$`Target Name`%in%c(nc_taget),"Cт"]
   d<-data_t[data_t$`Sample Name`%in%c(test_name)&data_t$`Target Name`%in%c(test_taget),"Cт"]
   
   nc_jd<-2^(a-b)
   test_jd<-2^(c-d)
   nc_m<-mean(nc_jd[[1]])
   
   #相对表达量
   nc_xd<-nc_jd/nc_m
   test_xd<-test_jd/nc_m
   
   output<-data.frame("对照组表达"=nc_xd[[1]],"实验组表达"=test_xd[[1]], stringsAsFactors=F)
   attr(output,"powered_by")<-"Tuizhi"
   
   output
 }
polt1<-function(o){
   a<-mean(o$对照组表达)
   b<-mean(o$实验组表达)
   c<-sd(o$对照组表达)
   d<-sd(o$实验组表达)
   o2<-data.frame(
      value = c(a,b),
      lable = c("对照组表达","实验组表达"),
      sd=c(c,d)
   )
   p<-ggplot(o2,aes(x=lable,y=value))+
         geom_bar(stat="identity",colour="black")+
         geom_errorbar(aes(ymin=value-sd,ymax=value+sd),size=.3,width=.2)+
         xlab(test_taget) +
         ylab("expression") +
         ggtitle(sprintf("The Expression of %s",test_taget))
   p
}



#输入文件目录
path<-"C:\\Users\\Tuizhi\\Desktop\\original_result\\Q_PCR_result\\20190612\\20190612_HSC_NOTCH_data.xls"
#eg："C:\\Users\\Tuizhi\\Desktop\\original_result\\Q_PCR_result\\20190612\\20190612_HSC_NOTCH_data.xls"

#在引号内输入对照组名称，实验组名称，内参名称，目的基因名称
nc_name<-"NC"
test_name<-"OE"
nc_taget<-"ACTB"
test_taget<-"NOTCH1"


o<-calculate(path,nc_name,test_name,nc_taget,test_taget)
h0<-t.test(o$对照组表达,o$实验组表达)
polt1(o)

cat("对照组",test_taget,"相对表达量：",o$对照组表达,
    "\n实验组",test_taget,"相对表达量：",o$实验组表达,"\n");apply(o,2,summary);print(h0)
















