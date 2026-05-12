library(ggplot2)
library(MASS)

# Q1
UScereal
d<-data.frame(table(UScereal$mfr,UScereal$shelf))
d
ggplot(d,aes(x=Var1,y=Var2,size=Freq))+
  geom_point(shape=21,fill='lightyellow',color='black')+
  scale_size_area()

# Q2
cats
y1<-cats$HWT[cats$Sex=='F']
x1<-cats$BWT[cats$Sex=='F']
y2<-cats$HWT[cats$Sex=='M']
x2<-cats$BWT[cats$Sex=='M']
t1<-'R[1]^2==0.2831'
t2<-'R[1]^2==0.6289'
ggplot(cats,aes(x=Bwt,y=Hwt,fill=Sex))+
  geom_point(shape=24)+
  geom_rug(aes(color=Sex))+
  geom_smooth(method=lm,linewidth=1)+
  annotate('text',label=t1,x=3.5,y=8,parse=TRUE)+
  annotate('text',label=t2,x=3.5,y=7,parse=TRUE)

# Q3
ggplot(diamonds,aes(x=cut,y=price))+
  geom_violin(fill='grey',color=NA)+
  geom_boxplot(aes(fill=color),
               position=position_dodge(0.5),width=0.3,outlier.alpha=0.1,alpha=0.3)


# axes
ggplot(birthwt,aes(x=factor(race),y=bwt))+
  geom_boxplot()

# flip axes
ggplot(birthwt,aes(x=factor(race),y=bwt))+
  geom_boxplot()+coord_flip()

# limites
ggplot(birthwt,aes(x=factor(race),y=bwt))+
  geom_boxplot()+ylim(0,7000)
ggplot(birthwt,aes(x=factor(race),y=bwt))+
  geom_boxplot()+scale_y_continuous(limits = c(0,7000))
ggplot(birthwt,aes(x=factor(race),y=bwt))+
  geom_boxplot()+expand_limits(y=c(0,7000))

ggplot(trees,aes(x=Girth,y=Height))+
  geom_line()+xlim(0,30)
ggplot(trees,aes(x=Girth,y=Height))+
  geom_line()+scale_x_continuous(limits = c(0,30))
ggplot(trees,aes(x=Girth,y=Height))+
  geom_line()+expand_limits(x=c(0,30))

ggplot(PlantGrowth,aes(x=group,y=weight))+
  geom_boxplot()
ggplot(PlantGrowth,aes(x=group,y=weight))+
  geom_boxplot()+scale_y_continuous(limits=c(4.5,5.5))# select some data
ggplot(PlantGrowth,aes(x=group,y=weight))+
  geom_boxplot()+coord_cartesian(ylim=c(4.5,5.5))# zoom in

# reverse axes
ggplot(birthwt,aes(x=factor(race),y=bwt))+
  geom_boxplot()+scale_y_reverse()
ggplot(trees,aes(x=Girth,y=Height))+
  geom_line()+scale_x_reverse()

# reorder
ggplot(birthwt,aes(x=factor(race),y=bwt))+
  geom_boxplot()+
  scale_x_discrete(limits=as.character(c(1,3,2)))
ggplot(birthwt,aes(x=factor(race),y=bwt))+
  geom_boxplot()+
  scale_x_discrete(limits=as.character(c(1,3)))
ggplot(birthwt,aes(x=factor(race),y=bwt))+
  geom_boxplot()+
  scale_x_discrete(limits=rev(levels(as.factor(birthwt$race))))

# tick marks
ggplot(birthwt,aes(x=factor(race),y=bwt))+
  geom_boxplot()+
  scale_y_continuous(breaks = c(1000,2500,3700,5000),
                     labels=c('minimum','low','high','maximun'))

ggplot(birthwt,aes(x=factor(race),y=bwt))+
  geom_boxplot()+theme(axis.text.y=element_blank())
ggplot(birthwt,aes(x=factor(race),y=bwt))+
  geom_boxplot()+theme(axis.ticks.y=element_blank())
ggplot(birthwt,aes(x=factor(race),y=bwt))+
  geom_boxplot()+theme(axis.ticks.y=element_blank(),axis.text.y=element_blank())

ggplot(birthwt,aes(x=factor(race),y=bwt))+
  geom_boxplot()+scale_y_continuous(breaks = NULL)

ggplot(birthwt,aes(x=factor(race),y=bwt))+
  geom_boxplot()+theme(axis.text.x=element_text(angle = 90,hjust=0.5,vjust = 0.5))
ggplot(birthwt,aes(x=factor(race),y=bwt))+
  geom_boxplot()+scale_x_discrete(labels=c('one','two','three'))+
  theme(axis.ticks.y=element_blank(),axis.text.y=element_text(color='violetred',face='bold',size=10))

ggplot(economics,aes(x=date,y=psavert))+geom_line()
esub<-subset(economics,date>=as.Date('2010-01-01'))
db<-seq(as.Date('2010-01-01'),as.Date('2015-04-01'),by='6 months')

ggplot(esub,aes(x=date,y=psavert))+geom_line()+
  scale_x_date(breaks=db)+
  theme(axis.ticks.y=element_blank(),axis.text.y=element_text(color='violetred',face='bold',size=10))

# labels
ggplot(birthwt,aes(x=factor(race),y=bwt))+
  geom_boxplot()+xlab('race')+ylab('birth weight')
ggplot(birthwt,aes(x=factor(race),y=bwt))+
  geom_boxplot()+scale_x_discrete(name='race')+
  scale_y_continuous(name='birth weight')

ggplot(birthwt,aes(x=factor(race),y=bwt))+
  geom_boxplot()+theme(axis.title = element_blank())
ggplot(birthwt,aes(x=factor(race),y=bwt))+
  geom_boxplot()+theme(axis.title.y = element_blank())

ggplot(birthwt,aes(x=factor(race),y=bwt))+
  geom_boxplot()+xlab('race')+ylab('birth weight')+
  theme(axis.title.y = element_text(face='italic',angle=0,size=9,color = 'blue'))

# title
ggplot(esub,aes(x=date,y=psavert))+geom_line()+
  ggtitle('title for plot')+
  theme(plot.title = element_text(face='bold.italic',color='darkred',hjust=0.5))

# circular graph
ggplot(mtcars,aes(x='',fill=factor(cyl)))+geom_bar()
ggplot(mtcars,aes(x='',fill=factor(cyl)))+geom_bar()+coord_polar(theta = 'y')
ggplot(mtcars,aes(x='',fill=factor(cyl)))+geom_bar(width=1)+coord_polar()

m<-matrix(mdeaths,ncol=12,byrow=T)
mave<-colMeans(m)
md<-data.frame(1:12,mave)
names(md)<-c('month','deaths')
ggplot(md,aes(x=month,y=deaths))+geom_line()+
  scale_x_continuous(breaks = 1:12)+coord_polar()
