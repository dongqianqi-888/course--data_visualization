library(ggplot2)
diamonds
data<-subset(diamonds,subset=(carat<1 & price<=5000 & clarity=='IF'))
ggplot(data,aes(x=carat,y=price,color=color))+geom_line()

ggplot(data,aes(x=carat,y=price,color=color))+
  geom_area(aes(fill=cut),alpha=0.5)

################################
trees
# scatter plot
ggplot(trees,aes(x=Girth,y=Height))+geom_point()

# default shape is 16, default size is 2
ggplot(trees,aes(x=Girth,y=Height))+geom_point(shape=21,size=5)

# group based on 3 variable
ggplot(mpg,aes(x=cty,y=hwy,color=class))+geom_point(shape=19,size=5)
ggplot(mpg,aes(x=cty,y=hwy,color=class,shape=class))+geom_point(size=5)
ggplot(mpg,aes(x=cty,y=hwy,color=class,shape=class))+geom_point(size=5)+scale_shape_manual(values=1:7)
ggplot(mtcars,aes(x=drat,y=wt,shape=factor(am),color=factor(am)))+
  geom_point(size=5)+
  scale_shape_manual(values=c(2,5))+
  scale_color_manual(values=c('red','black'))
ggplot(mtcars,aes(x=drat,y=wt,shape=factor(am),color=factor(am),fill=factor(am)))+
  geom_point(size=5)+
  scale_shape_manual(values=c(2,5))+
  scale_color_manual(values=c('red','black'))+
  scale_fill_manual(values=c('black','red'))

# if 3 variable is continuous(use=fill)
ggplot(mpg,aes(x=cty,y=hwy,color=displ))+geom_point()
ggplot(mpg,aes(x=cty,y=hwy,size=displ))+geom_point()

ggplot(mtcars,aes(x=drat,y=wt,fill=factor(qsec)))+
  geom_point(shape=21,color='blue',size=4,alpha=0.5)

ggplot(mtcars,aes(x=drat,y=wt,fill=qsec))+
  geom_point(shape=21,fill='blue',color='blue',alpha=0.5)

ggplot(mtcars,aes(x=drat,y=wt,fill=qsec))+
  geom_point(shape=21,color='blue',size=4,alpha=0.5)

# overplotting
ggplot(diamonds,aes(x=carat,y=price))+geom_point()
ggplot(diamonds,aes(x=carat,y=price))+geom_point(alpha=0.1)

ggplot(Loblolly,aes(x=age,y=height))+geom_point()
ggplot(Loblolly,aes(x=age,y=height))+geom_point(position=position_jitter(width = 0.3,height = 0.2))
## when the data in a same point, position_jitter can resolute the data.

# adding fitted lines
ggplot(trees,aes(x=Girth,y=Height))+geom_point()+
  geom_smooth(method=lm)# 95%
ggplot(trees,aes(x=Girth,y=Height))+geom_point()+
  geom_smooth(method=lm,level=0.99)
ggplot(trees,aes(x=Girth,y=Height))+geom_point()+
  geom_smooth(method=lm,se=FALSE)

# y=62.013+1.054x
txt="italic(y)==62.013+1.054*italic(x)"
ggplot(trees,aes(x=Girth,y=Height))+geom_point()+
  geom_smooth(method=lm)+
  annotate('text',label=txt,x=18,y=65,parse=TRUE)

ggplot(trees,aes(x=Girth,y=Height))+geom_point()+
  geom_smooth()# locally weighted polynomial curve

# adding marginal rugs
ggplot(faithful,aes(x=eruptions,y=waiting))+geom_point()
ggplot(faithful,aes(x=eruptions,y=waiting))+geom_point()+geom_rug()
ggplot(faithful,aes(x=eruptions,y=waiting))+geom_point()+geom_rug(color='blue',size=2)

# adding labels
car<-rownames(mtcars)
new<-data.frame(car,mtcars)
ggplot(new,aes(x=drat,y=wt))+geom_point()+
  geom_text(aes(label=car),size=3,vjust=1,hjust=0.5)

