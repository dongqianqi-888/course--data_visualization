library(ggplot2)
g<-ggplot(trees,aes(x=Girth,y=Height))
g+geom_line()

# expand limits
g+geom_line()+expand_limits(x=5,y=50)

# add pomits
g+geom_line()+geom_point()

# log-scale limit
g+geom_line()+scale_y_log10()
g+geom_line()+scale_x_log10()

ggplot(sleep,aes(x=as.numeric(ID),y=extra))+
  geom_line()

# multiple lines
ggplot(mpg,aes(x=displ,y=cty,colour=drv))+geom_line()
ggplot(mpg,aes(x=displ,y=cty,linetype=drv))+geom_line()
ggplot(mpg,aes(x=displ,y=cty,group=drv))+geom_line()
ggplot(mpg,aes(x=displ,y=cty,shape=drv))+geom_line()+geom_point()

# change line or point features
g+geom_line(color='red',linetype='dashed',size=2)
g+geom_line(color='blue',linetype='dotted',size=0.5)
g+geom_line(color='red',linetype='dashed',size=2)+geom_point(shape=19,size=3,color='blue')
g+geom_line(color='red',linetype='dashed',size=2)+geom_point(shape=21,size=3,color='blue',fill='hotpink')

# area chart
ggplot(longley,aes(x=Year,y=Armed.Forces))+geom_area()
ggplot(longley,aes(x=Year,y=Armed.Forces))+geom_area(fill='lightpink',color='blue')
ggplot(longley,aes(x=Year,y=Armed.Forces))+geom_area(fill='lightpink',color='blue',alpha=0.4)

# stack area chart
library(babynames)
baby<-subset(babynames,subset=(sex=='F'& names %in% c('Alice','Emma','Mary')))
ggplot(baby,aes(x=year,y=n,fill=name))+geom_area()

# insert confidence intervals
std<-sd(longley$Armed.Forces)
ggplot(longley,aes(x=Year,y=Armed.Forces))+
  geom_ribbon(aes(ymin=Armed.Forces-std,ymax=Armed.Forces+std))
ggplot(longley,aes(x=Year,y=Armed.Forces))+
  geom_ribbon(aes(ymin=Armed.Forces-std,ymax=Armed.Forces+std),fill='lightblue')+
  geom_line()

# or use geom_line
ggplot(longley,aes(x=Year,y=Armed.Forces))+
  geom_line(aes(y=Armed.Forces-std),color='lightgrey',linetype='dashed')+
  geom_line(aes(y=Armed.Forces+std),color='lightgrey',linetype = 'dashed')+
  geom_line()

# dual-axes chart
x<-1:100
var1<-cumsum(rnorm(100))
var2<-var1^2
d<-data.frame(x,var1,var2)
ggplot(d,aes(x=x))+
  geom_line(aes(y=var1),color='red')+
  geom_line(aes(y=var2/10),color='blue')+
  scale_y_continuous(sec.axis = sec_axis(~.*10,name='var2'))
