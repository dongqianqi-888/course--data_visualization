library(ggplot2)
library(MASS)
# boxplot
ggplot(birthwt,aes(x=factor(race),y=bwt))+geom_boxplot()

#default outlier=1.5*iqr
ggplot(birthwt,aes(x=factor(race),y=bwt))+geom_boxplot(coef=2)

ggplot(birthwt,aes(x=factor(race),y=bwt))+geom_boxplot(outlier.size=3,outlier.shape = 21,
                                                       outlier.colour = 'red',outlier.fill='pink')

# single variable
ggplot(birthwt,aes(x=1,y=bwt))+geom_boxplot()

# notched boxplot
ggplot(birthwt,aes(x=factor(race),y=bwt))+geom_boxplot(notch = T)

# add markers
ggplot(birthwt,aes(x=factor(race),y=bwt))+geom_boxplot()+
  stat_summary(fun='mean',geom='point',
               shape=23,fill='skyblue',size=4)

# group boxplot
ggplot(birthwt,aes(x=factor(race),y=bwt,fill=factor(smoke)))+geom_boxplot()
ggplot(birthwt,aes(x=factor(race),y=bwt,fill=factor(smoke)))+geom_boxplot(width=0.5)
ggplot(birthwt,aes(x=factor(race),y=bwt,fill=factor(smoke)))+geom_boxplot(width=0.5,position=position_dodge(0.7))

# violin plot
ggplot(birthwt,aes(x=factor(race),y=bwt))+geom_violin(fill='pink')


       