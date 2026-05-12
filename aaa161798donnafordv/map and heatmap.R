library(ggplot2)
library(RColorBrewer)
library(maps)
library(sf)
library(tmap)
library(ggmap)
library(osmdata)

# heatmaps
austin<-subset(txhousing,city=='Austin')

# tile
ggplot(austin,aes(x=year,y=factor(month),fill=sales))+geom_tile()
ggplot(austin,aes(x=year,y=factor(month),fill=sales))+geom_tile()+
  scale_fill_gradient2(midpoint = 2000, mid='plum',limits=c(0,3500))
ggplot(austin,aes(x=year,y=factor(month),fill=sales))+geom_tile(colour='white',lwd=1.5,linetype = 1)+
  scale_fill_gradient2(midpoint = 2000, mid='plum',low='hotpink',high = 'darkblue')
ggplot(austin,aes(x=year,y=factor(month),fill=sales))+geom_tile(colour='white',lwd=1.5,linetype = 1)+
  scale_fill_gradient2(midpoint = 2000, mid='plum',low='hotpink',high = 'darkblue')+
  coord_fixed()
ggplot(austin,aes(x=year,y=factor(month),fill=sales))+geom_tile(colour='white',lwd=1.5,linetype = 1)+
  scale_fill_gradient2(midpoint = 2000, mid='plum',low='hotpink',high = 'darkblue')+
  coord_fixed()+
  geom_text(aes(label=sales),color='white',size=2)

# raster
ggplot(austin,aes(x=year,y=factor(month),fill=sales))+geom_raster()
ggplot(austin,aes(x=year,y=factor(month),fill=sales))+geom_raster()+
  scale_fill_gradient2(midpoint = 2000, mid='plum',low='hotpink',high = 'darkblue')


#-----------------------------------------------------------------------------------
# maps
wm<-map_data('world')

ggplot(wm,aes(x=long,y=lat,group=group))+
  geom_polygon(fill='skyblue',color='darkblue')
ggplot(wm,aes(x=long,y=lat,group=group))+
  geom_path(fill='skyblue',color='darkblue')# no fill

unique(wm$region)
im<-map_data('italy')
ggplot(im,aes(x=long,y=lat,group=group))+
  geom_polygon(fill='bisque',color='chocolate')

mm<-map_data('world',region='Malaysia')
ggplot(mm,aes(x=long,y=lat,group=group,fill=subregion))+
  geom_polygon(color='chocolate')

s<-st_read(file.choose())
ggplot(s)+geom_sf(fill='wheat',color='tan4')

# add points
map<-read.csv('rain_selangor.csv',header=T)

ggplot(s)+geom_sf(fill='wheat',color='tan4')+
  geom_point(data=map,aes(x=longitude,y=latitude,
                          size=ave),inherit.aes = F,alpha=0.5)

# cloropleth
ggplot(s)+geom_sf(aes(fill=NAME_1),color='black')

mypal<-colorRampPalette(brewer.pal(3,'Dark2'))(3)
ggplot(s)+geom_sf(aes(fill=NAME_1),color='black')+
  scale_fill_manual(values = mypal)

#-------------------------------------------------------------------------
library(tmap)
tm_shape(s)+tm_fill()
tm_shape(s)+tm_borders()
tm_shape(s)+tm_fill()+tm_borders()
tm_shape(s)+tm_polygons()
tm_shape(s)+tm_fill(col='red',alpha=0.5)+
  tm_borders(col='blue',lwd=3,lty=2)
tm_shape(s)+tm_polygons('NAME_1',alpha=0.7)+
  tm_borders()
legend_title<-expression('Area (km'^2*')')
tm_shape(s)+tm_polygons('NAME_1',alpha=0.7,title=legend_title)+
  tm_borders()

sm<-tm_shape(s)+tm_polygons()
sm+tm_layout(title='selangor')
sm+tm_layout(scale=5)
sm+tm_layout(bg.color = 'floralwhite')
sm+tm_layout(frame=FALSE)

sm+tm_style('bw')
sm+tm_style('classic')
sm+tm_style('cobalt')

tm_shape(s)+tm_polygons('NAME_1')+
  tm_compass(position=c('left','top'))+
  tm_scalebar(position=c('right','bottom'))+
  tm_layout(legend.position = c('left','bottom'))

map_sf<-st_as_sf(map,coords = c('longitude','latitude'),
                 crs=4326)

tm_shape(s)+tm_polygons()+
  tm_shape(map_sf)+
  tm_dots(size=0.5,alpha=0.5,col='red')
tm_shape(s)+tm_polygons('NAME_1')+
  tm_text('NAME_1',size=0.6)
tm_shape(s[s$NAME_1=='Kuala Lumpur',])+
  tm_polygons()+tm_fill(col='blue',alpha=0.4)

#--------------------------------------------------------
library(osmdata)
head(available_features())

register_stadiamaps('0cfbdfcc-4b04-4b49-8654-a88643b284f8')     

map<-get_stadiamap(getbb('Madrid'),
                   maptype = 'stamen_toner')
ggmap(map)

q<-add_osm_feature(opq(getbb('Madrid')),'amenity','cinema')
str(q)
cinima<-osmdata_sf(q)
ggmap(map)+
  geom_sf(data = cinima$osm_points,inherit.aes = F,
          color='seagreen',fill='darkgreen',
          alpha=0.5,size=4,shape=21)+labs(x='x',y='y')

m<-c(-10,30,5,46)
q<-add_osm_feature(add_osm_feature(opq(m,timeout = 25*10),'name','Mercadona'),'shop','supermarket')
str(q)
mercadona<-osmdata_sf(q)
ggplot(mercadona$osm_points)+
  geom_sf(color='seagreen',fill='darkgreen',
          alpha=0.5,size=4,shape=21)+labs(x='x',y='y')+theme_void()

m<-c(99,1,120,6)
q<-add_osm_feature(opq(m,timeout = 25*10),'shop','coffee')
str(q)
coffee<-osmdata_sf(q)
msia<-get_stadiamap(m,maptype = 'stamen_toner_lite')
msia2<-map_data('world',region = 'Malaysia')
ggmap(msia)+
  geom_sf(data = coffee$osm_points,inherit.aes = F,
          color='seagreen',fill='darkgreen',
          alpha=0.5,size=4,shape=21)+labs(x='x',y='y')
ggplot(msia2,aes(x=long,y=lat,group=group))+geom_polygon()+
  geom_sf(data = coffee$osm_points,inherit.aes = F,
          color='seagreen',fill='darkgreen',
          alpha=0.5,size=4,shape=21)+labs(x='x',y='y')
