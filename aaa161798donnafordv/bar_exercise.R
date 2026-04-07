library(ggplot2)
head(data)
head(infert)
d<-data.frame(table(infert$induced,infert$age))
names(d)<-c('induced','age','freq')
ggplot(d,aes(x=age,y=freq,fill=induced))+
  geom_bar(stat='identity',position='dodge')

sale<-read.table('clipboard',header=T,sep='\t')
data<-read.table('clipboard',header=T,sep='\t')
icut<-unique(data$pcut)
ggplot(data,aes(x=pcut,y=prop,fill=pcol))+
  geom_bar(stat='identity')+
  geom_text(aes(x=x,y=y,label=mylab),data=sale,inherit.aes=F,vjust=0.8)+
  scale_x_discrete(limits=icut)
