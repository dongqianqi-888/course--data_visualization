# exercise
library(ggplot2)
tr<-read.csv('traits.csv')
ggplot(tr,aes(x=factor(traits),y=program,fill=score))+
  geom_tile()+
  scale_fill_gradient2(midpoint=5,low='darkgreen',high = 'darkorange',mid='yellow')+
  theme(axis.text.x=element_text(angle=90,hjust=0.5,vjust=0.3))

ra<-read.csv('rain.csv')
map<-map_data('world',region = 'Malaysia')
head(ra)
qs<-quantile(ra$ave,c(0.25,0.5,0.75))
category<-ra$ave
category[category<qs[1]]<-1
category[category<qs[2] & category>qs[1]]<-2
category[category<qs[3] & category>qs[2]]<-3
category[category>qs[3]]<-4
nra<-data.frame(ra,category)
ggplot(map,aes(x=long,y=lat,group=group))+
  coord_cartesian(xlim=c(99,105))+
  geom_polygon(fill='skyblue',color='darkblue')+
  geom_point(aes(x=longitude,y=latitude,fill=factor(category),size=ave),data=nra,inherit.aes = F,
             shape=21)+labs(fill='category',size='amount,mm')+
  scale_fill_discrete(labels=c('less than 25%','25%~50%','50%~75%','more than 75%'))




library(plotly)#----------------------------------------------------------------------------------
library(leaflet)
plot_ly(data=iris,x=~Sepal.Length,y=~Petal.Length)
# point
plot_ly(data=iris,x=~Sepal.Length,y=~Petal.Length)%>%
  add_markers()
plot_ly(data=iris,x=~Sepal.Length,y=~Petal.Length)%>%
  add_markers(symbol=I(2))
plot_ly(data=iris,x=~Sepal.Length,y=~Petal.Length,
        symbol=~Species,symbols=I(1:3))
plot_ly(data=iris,x=~Sepal.Length,y=~Petal.Length,
        symbol=~Species,colors=c('blue','red','yellow'))
plot_ly(data=iris,x=~Sepal.Length,y=~Petal.Length)%>%
  add_markers(symbol=~Species,colors=c('blue','red','yellow'),symbols=I(1:3))
plot_ly(data=iris,x=~Sepal.Length,y=~Petal.Length,z=~Sepal.Width,
        symbol=~Species,colors=c('blue','red','yellow'))     

# line
plot_ly(data=iris,x=~Sepal.Length,y=~Petal.Length)%>%
  add_lines()
plot_ly(data=iris,x=~Sepal.Length,y=~Petal.Length)%>%
  add_paths()
plot_ly(data=iris,x=~Sepal.Length,y=~Petal.Length,linetype = ~Species)%>%
  add_lines()

longley
m<-lm(Employed~Year,data=longley)
new<-data.frame(longley$Year,longley$Employed,m$fitted.values)
names(new)<-c('year','employed','fitted')
plot_ly(data=new,x=~year)%>%
  add_markers(y=~employed,color=I('skyblue'))%>%
  add_lines(y=~fitted,color=I('darkblue'))
std<-sd(longley$Employed)
plot_ly(data=new,x=~year)%>%
  add_markers(y=~employed,color=I('skyblue'))%>%
  add_lines(y=~fitted,color=I('darkblue'))%>%
  add_ribbons(ymin=~fitted-std,ymax=~fitted+std,color=I('grey'))

# bars and histograms
plot_ly(diamonds,x=~price)%>%add_histogram()

x<-1:5
y<-c(7,8,4,3,5)
z<-data.frame(x,y)
plot_ly(z,x=~x,y=~y)%>%add_bars()

plot_ly(diamonds,x=~color,color=~cut)%>%add_histogram()
plot_ly(diamonds,x=~color,color=~cut)%>%add_histogram()%>%layout(barmode='stack')

subplot(
  plot_ly(diamonds,x=~color,color=~cut)%>%add_histogram(name='cut'),
  plot_ly(diamonds,x=~color,color=~clarity)%>%add_histogram(name='clarity')%>%layout(barmode='stack')
)

# boxplot
plot_ly(diamonds,x=~cut,y=~price)%>%add_boxplot()
plot_ly(diamonds,x=~interaction(color,cut),y=~price)%>%add_boxplot(color=~color)

# heatmap
austin<-subset(txhousing,city="Austin")
plot_ly(austin,x=~year,y=~factor(month),z=~sales)%>%add_heatmap()

# map
library(maps)
world<-map_data('world')
library(tmap)
library(sf)
selangor<-st_read('Selangor/Selangor.shp')
ms<-tm_shape(selangor)+tm_polygons()
view('ms')

ms+tm_basemap(sever='OpenTopoMap')

library(leaflet)
leaflet()%>%addProviderTiles(providers$CartoDB.Positron)
leaflet()%>%addProviderTiles(providers$Esri.WorldImagery)
leaflet()%>%addProviderTiles(providers$Esri.WorldStreetMap)

leaflet()%>%addProviderTiles(providers$Esri.WorldStreetMap)%>%
  addMarkers(lng=102,lat=3,popup='hello malaysia')
leaflet()%>%addProviderTiles(providers$Esri.WorldStreetMap)%>%
  setView(lng=102,lat=3,zoom=5)

name<-c('k','s','j')
lat<-c(6.1,3.1,1.4)
long<-c(100.3,101.7,103.7)
my<-data.frame(name,lat,long)
leaflet(my)%>%addProviderTiles(providers$Esri.WorldStreetMap)%>%
  addMarkers(~long,~lat,popup=~name)
leaflet(my)%>%addProviderTiles(providers$Esri.WorldStreetMap)%>%
  addCircles(lng=102,lat=3,radius = 500,popup='circle')
leaflet(my)%>%addProviderTiles(providers$Esri.WorldStreetMap)%>%
  addPolygons(lng=c(100.3,101.7,103.7),lat=c(6.1,3.1,1.4),popup='polygons')
leaflet(selangor)%>%addProviderTiles(providers$Esri.WorldStreetMap)%>%
  addPolygons(popup=~NAME_1)

# finishing
f<-list(family='courier new',size=18,color='navy')
x1<-list(title='year',titlefont=f)
y1<-list(title='month',titlefont=f)
plot_ly(austin,x=~year,y=~factor(month),z=~sales)%>%add_heatmap()%>%
  layout(title='heating',xaxis=x1,yaxis=y1)

# legend
f<-list(family='sans-serif',size=12,coor='plum')
l<-list(font=f,bgcolor='ivory',bordercolor='purple',borderwidth=2,orientation='h')
plot_ly(iris,x=~Sepal.Length,y=~Petal.Length)%>%
  add_markers(color=~Species,symbol=~Species,colors=c('red','blue','green'),symbols=I(3:5))%>%
  layout(legend=l)
plot_ly(iris,x=~Sepal.Length,y=~Petal.Length)%>%
  add_markers(color=~Species,symbol=~Species,colors=c('red','blue','green'),symbols=I(3:5))%>%
  layout(legend=l,hoverlabel=list(bgcolor='khaki',bordercolor='chocolate',font=list(family='Arial',color='chocolate')))
plot_ly(iris,x=~Sepal.Length,y=~Petal.Length,
        hoverinfo='text',text=~paste('species:',Species))%>%
  add_markers(color=~Species,symbol=~Species,colors=c('red','blue','green'),symbols=I(3:5))%>%
  layout(legend=l,hoverlabel=list(bgcolor='khaki',bordercolor='chocolate',font=list(family='Arial',color='chocolate')))

plot_ly(iris,x=~Sepal.Length,y=~Petal.Length)%>%
  add_markers(color=~Species,symbol=~Species,colors=c('red','blue','green'),symbols=I(3:5))%>%
  layout(dragmode='select')
plot_ly(iris,x=~Sepal.Length,y=~Petal.Length)%>%
  add_markers(color=~Species,symbol=~Species,colors=c('red','blue','green'),symbols=I(3:5))%>%
  layout(dragmode='pan')
plot_ly(iris,x=~Sepal.Length,y=~Petal.Length)%>%
  add_markers(color=~Species,symbol=~Species,colors=c('red','blue','green'),symbols=I(3:5))%>%
  layout(dragmode='lasso')

gf<-ggplot(mpg,aes(x=displ,y=hwy))+geom_point()+facet_wrap(~class)
ggplotly(gf)


library(gganimate)
library(gifski)
library(gapminder)
head(gapminder)
ggplot(gapminder,aes(x=gdpPercap,y=lifeExp,size=pop,color=country))+
  geom_point(alpha=0.7)+
  scale_x_log10()+
  guides(color=F,size=F)+
  labs(x='GDP',y='life cost')
ggplot(gapminder,aes(x=gdpPercap,y=lifeExp,size=pop,color=country))+
  geom_point(alpha=0.7)+
  scale_x_log10()+
  guides(color=F,size=F)+
  labs(x='GDP',y='life cost')+
  transition_time(year)
ggplot(gapminder,aes(x=gdpPercap,y=lifeExp,size=pop,color=country))+
  geom_point(alpha=0.7)+
  scale_x_log10()+
  guides(color=F,size=F)+
  labs(x='GDP',y='life cost')+
  facet_wrap(~continent)+
  transition_time(year)
ggplot(gapminder,aes(x=gdpPercap,y=lifeExp,size=pop,color=country))+
  geom_point(alpha=0.7)+
  scale_x_log10()+
  guides(color=F,size=F)+
  labs(x='GDP',y='life cost',
       title='year:{frame_time}')+
  facet_wrap(~continent)+
  transition_time(year)
ggplot(gapminder,aes(x=gdpPercap,y=lifeExp,size=pop,color=country))+
  geom_point(alpha=0.7)+
  scale_x_log10()+
  guides(color=F,size=F)+
  labs(x='GDP',y='life cost',
       title='year:{frame_time}')+
  facet_wrap(~continent)+
  transition_time(year)+view_follow()

ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, size = pop, color = country)) +
  geom_point(alpha = 0.7) +
  scale_x_log10() +
  guides(color = "none", size = "none") +
  labs(x = "GDP per capita", y = "Life expectancy",
       title = "Year: {frame_time}") +
  facet_wrap(~continent) +
  transition_time(year) +view_follow(fixed_y=T)

ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, size = pop, color = country)) +
  geom_point(alpha = 0.7) +
  scale_x_log10() +
  guides(color = "none", size = "none") +
  labs(x = "GDP per capita", y = "Life expectancy",
       title = "Year: {frame_time}") +
  facet_wrap(~continent) +
  transition_time(year) +
  shadow_wake(wake_length = 0.1)

ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, size = pop, color = country)) +
  geom_point(alpha = 0.7) +
  scale_x_log10() +
  guides(color = "none", size = "none") +
  labs(x = "GDP per capita", y = "Life expectancy",
       title = "Year: {frame_time}") +
  facet_wrap(~continent) +
  transition_time(year) +
  shadow_mark(alpha=0.3,size=0.5)

# static plot
ggplot(airquality,aes(x=Day,y=Temp,group=Month,
                      color=factor(Month)))+geom_line()+
  labs(x='day of month',y='temperature')+
  theme(legend.position = 'top')

ggplot(airquality,aes(x=Day,y=Temp,group=Month,
                      color=factor(Month)))+geom_line()+geom_point()+
  labs(x='day of month',y='temperature')+
  theme(legend.position = 'top')+
  transition_reveal(Day)

ggplot(airquality,aes(x=Day,y=Temp,group=Month,
                      color=factor(Month)))+geom_line()+
  geom_point(aes(group=seq_along(Day)))+
  labs(x='day of month',y='temperature')+
  theme(legend.position = 'top')+
  transition_reveal(Day)

# boxplot
mon<-5:9
tem<-c(53,35,64,34,45)
mt<-data.frame(mon,tem)
ggplot(mt,aes(x=mon,y=tem,fill=tem))+
  geom_bar(stat='identity')+
  transition_states(mon)# more smoothing

ggplot(mt,aes(x=mon,y=tem,fill=tem))+
  geom_bar(stat='identity')+
  transition_reveal(mon)# one by one

ggplot(mt,aes(x=mon,y=tem,fill=tem))+
  geom_bar(stat='identity')+
  transition_states(mon)+shadow_mark()

ggplot(mpg)+
  geom_bar(aes(x=class),fill='hotpink',color='darkred')+
  transition_states(class)+shadow_mark()+
  labs(title='class:{closet_state}')
