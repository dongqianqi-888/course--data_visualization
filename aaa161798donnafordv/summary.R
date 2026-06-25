library(ggplot2)
library(RColorBrewer)
library(maps)
library(sf)
library(tmap)
library(ggmap)
library(osmdata)
library(gganimate)
library(plotly)

x<-rep(1:3,2)
y<-rep(1:2,3)
z<-1:6
data<-data.frame(x,y,z)
r<-rbind(x,y)
c<-cbind(x,y)
data<-subset(data,subset = (x==1 & y %in% c(1,2)))
db<-seq(as.Date("2010-01-01"),as.Date("2015-04-01"),by="6month")

g<-ggplot(data,aes(x=factor(x,labels = c('v1','v2','v3')), y=y, fill=z))
### factor表示离散化，可以赋值进去。fill用颜色分类

#---------------------------------------bar-------------------------------------------
### 综合*综合
g+geom_bar(aes(x=reorder(a,b),y=y),
               ### a类按照b的数值排序
               stat='identity',fill='lightblue',color='black',
               ### identity非频数，count频数
               position = position_dodge(0.7),
               ### dodge并列，stack堆叠，fill堆叠填满,这里并列柱子间距0.7
               width=0.5)+
               ### 柱子宽度
  
  geom_text(aes(label=y),
            colour = 'white',hjust=1.5,vjust=1.5)+
  ### 加文字在bar上，hjust调横向，vjust调纵向
  
  scale_x_discrete(limits=c('v1','v2','v3'))
  ### 强制按照limits排序

#----------------------------------------histogram------------------------------------------
### 单个连续
g+geom_histogram(aes(x=x),
                 bins = 50,binwidth = 0.1)
                 ### bins固定箱子数量，binwidth固定箱子宽度，二者选其一

#----------------------------------------box------------------------------------------
### 离散*连续
g+geom_box(coef=2,
           ### 控制离群值IQR*coef的范围
           notch=TRUE+
           ### 是否收腰
           width=0.5,position=position_dodge(0.7)
           ### 调整箱宽和并列箱之间的距离
           )+
  
  stat_summary(fun.y="mean",geom="point",shape=23,size=3,fill="skyblue")
  ### 加一个均值点

#---------------------------------------violin-------------------------------------------
### 离散*连续
g+geom_violin()

#--------------------------------------line--------------------------------------------
### 连续有序*连续
g+geom_line(aes(colour=z,linetype=z,group=z),
            linetype='dashed',color='red',size=2)+
  
  expand_limits(x=c(5,10))+
  ### 横轴范围调整
  
  scale_x_log10()+
  scale_y_log10()+
  ### x、y轴取对数
  
  geom_point(aes(shape=z),
             shape=19,size=2,color='blue')+
  ### 加一些点
  
  geom_area(aes(fill=z),
            fill='lightblue',color='black',alpha=0.2)+
  ### 加一个面积
  
  geom_ribbon(aes(ymin=x-sd(X),ymax=x+sd(x)),
              alpha=0.3,fill='yellow')+
  ### 加一条面积（置信区间）
  
  geom_line(aes(y=y+sd(y)),
            linetype='dashed',color='grey')
  ### 再加一条线

#----------------------------------------point------------------------------------------
### 连续*连续
g+geom_point(shape=21,size =2,color='blue',fill='navy',
             ### 美化
             aes(color=z,shape=as.factor(z),fill=z,size=z),
             ### 分组
             alpha=0.01,
             ### 虚化的点
             position=position_jitter()
             ### 随机偏移，避免重叠
             )+
  scale_shape_manual(values=1:7)+
  ### 建一个形状组
  scale_color_manual(values=c('red','blue'))+
  ### 建一个颜色组
  scale_fill_manual(values=c('blue','darkred'))+
  ### 建一个填充组
  scale_size_area()
  ### 使数值和点的面积成正比

#--------------------------------------heatmap--------------------------------------------
### 离散有序*离散有序
g+geom_tile(color='plum',lwd=1.5,linetype = 1)+
  geom_raster()+
  ### 类似的功能
  
  scale_fill_gradient2(midpoint = 10,low='hotpink',mid='steelblue',high = 'seagreen',limits=c(0,20))+
  ### 设置图例条的颜色、布局
  
  coord_fixed()+
  ### 锁定横纵轴比例
  
  geom_text(aes(labels=x),color='white',size=2)
  ### 添加标签

#-------------------------------------map---------------------------------------------
### 连续数据和region
mapw<-map_data('world')
ggplot(mapw,aes(x=long,y=lat,group=group))+
  geom_polygon(fill='lightblue',color='navy')+
  ### 填充色块
  
  geom_path(color='seagreen')
  ### 填充轮廓

## sf包***********************************
map<-st_read('map.shp')
ggplot(map)+
  geom_sf(aes(fill=x),
          fill='wheat',color='tan4')+
  ### 填充色块给shp文件
  
  geom_point(value,aes(x=long,y=lat,size=z),
             alpha=0.2,shape=3,inherit.aes = F)+
             ### inherit.aes=F相当于拒绝了旧数据的映射
  
  scale_fill_manual(values=colorRampPalette(brewer.pal(3,'Dark2'))(3))
  ### 一个自动的颜色组

## tmap包（1）*****************************
tm_shape(map)+
  tm_fill(col='red',alpha=0.3,
          col='x',title=expression("Area (km"^2*")")
          ### 分组且加legend标题
          )+
  
  tm_borders(col='blue',lwd=3,lty=2)+
  tm_polygons('x')+
  tm_layout(title='malaysia',
            ### 加标题
            scale=5,
            ### 边界线变粗
            bg.color='floralwhite',
            ### 设置背景颜色
            frame=FALSE,
            ### 无框
            legend.position=c('left','bottom'))+
            ### 调整legend位置
  tm_style('bw')+
  ### 'classic','cobalt'可选
  tm_compass(position = c("left", "top")) +
  ### 方向指针
  tm_scale_bar(position = c("right", "bottom")) 
  ### 距离条

## tmap包（2）*****************************************
mymap<-read.csv(file="rain_selangor.csv",header=T)
mymap_sf <- st_as_sf(mymap, coords = c("longitude", "latitude"), crs = 4326)
### 转化为地图数据
tm_shape(selangor) + tm_polygons() +
  tm_shape(mymap_sf) +
  ### 可叠加
  tm_dots(size = 0.5,alpha=0.5,col="red")+
  tm_text('x',size=2)
  ### 加注释

## osm包************************************************
register_stadiamaps('0cfbdfcc-4b04-4b49-8654-a88643b284f8') 
### 登录接口密钥

q <- add_osm_feature(opq(getbb("city")), "amenity", "cinema")
cinema <- osmdata_sf(q)
### 查找电影院

mad_map <- get_stadiamap(getbb("city"), maptype = "stamen_toner")

ggmap(mad_map) +
  geom_sf(data = cinema$osm_points,
          inherit.aes = FALSE,
          colour = "seagreen", fill = "darkgreen",
          alpha = 0.5, size = 4, shape = 21) +
  labs(
    title = "主标题",
    subtitle = "副标题",
    caption = "数据来源说明",
    x = "横轴名称",
    y = "纵轴名称",
    colour = "图例标题"
  )

#---------------------------------------feature-------------------------------------------
+coord_flip()# 翻转坐标
+coord_cartesian(ylim=c(4.5,5.5))# 聚焦局部
+coord_polar(theta = "y")# 坐标围城圈（画pie图）
+coord_polar()# 画圈图

+facet_grid(z~.)# 分类子图（横向）
+facet_grid(.~z)# 分类子图（纵向）
+facet_grid(a~b,# 方格子图
            scales="free_y",# 布局随意
            scales="free_y")# 全局布局随意
+facet_wrap(~class,ncol=2)# 两列子图

+scale_y_continuous(limits=c(0,10),# 限制连续y坐标
                    breaks=c(1000,2500,3000,3500,4000,5000),# 规定坐标值
                    labels=c("Minimum","Lower Bound","Upper Bound","Maximum"),# 文字坐标值
                    name = 'y')# 设置名字
+scale_y_continuous(breaks=NULL)# 隐藏y轴标签
+scale_x_discrete(limits=as.character(c(1,3,2)))# x轴排序
+scale_x_discrete(limits=rev(levels(factor(birthwt$race))),# 离散变量倒序
                  labels=c("ONE","TWO","THREE"))# x轴标签值设置值
+scale_y_reverse()# 反转y轴（从大到小）
+scale_x_date(breaks=date)# 设置时间轴的值
+scale_fill_discrete(name="RACE",# 图例标题
                     guide=FALSE,# 删掉图例
                     limits=as.character(c(1,3,2)),# 图例文字排序 
                     labels=c("ONE","TWO","THREE"))# 图例文字值
+scale_fill_hue(guide=guide_legend(reverse=TRUE))# 图例文字排序
+scale_fill_discrete()# 值的颜色组（彩色）
+scale_fill_grey()# 值的颜色组（灰）
+scale_fill_brewer()# 值的颜色组（蓝）
+scale_fill_brewer(palette="BrBG")# 值的颜色组（黄）
+scale_colour_hue(l=30)# 值的颜色组
                     
+ggtitle('title')# 设置全局标题
+xlab('x')# 设置x轴标题
+labs(fill="RACE",shape="am",colour="vs")# 图例标题
+guides(fill=FALSE)# 删掉图例
+guides(fill=guide_legend(title=NULL))# 删掉图例标题
+guides(fill=guide_legend(reverse=TRUE))# 图例文字倒序
+guides(fill=guide_legend(title.theme=element_text(face="bold",angle=30)))# 美化图例标题
+guides(fill=guide_legend(label.theme=element_text(face="bold",angle=30)))# 美化图例文字

+theme(axis.title=element_blank(),# 隐藏标题
       axis.title.y=element_blank(),# 隐藏y轴标题
       axis.title.y=element_text(face="italic",angle=0,size=9,colour="skyblue"),
       
       axis.text=element_blank(),# 隐藏全局标签
       axis.text.y=element_blank(),# 隐藏y轴标签
       axis.text.x=element_text(angle=90,hjust=0.5,vjust=0.5,# 设置x轴标签的角度和位置
                                colour="violetred",face="bold",size=10),# 设置颜色字体和大小
       
       axis.ticks=element_blank(),# 隐藏全局刻度
       
       legend.title=element_text(face="bold",colour="seagreen",size=10),# 美化图例标题
       legend.key=element_rect(fill="skyblue",colour="darkblue"),# 美化图例符号背景
       legend.text=element_text(face="bold",colour="seagreen",size=10),# 美化图例文字
       legend.position="top",# 图例位置
       legend.position=c(1,1),legend.justification=c(1,1),# 右上角与右上角对齐
       legend.background=element_blank(),# 移除图例背景
       
       strip.text=element_text(face="bold",colour="darkblue",size=10),# 子图标签
       strip.background=element_rect(fill="skyblue",colour="darkblue"),# 子图背景
       
       panel.grid.major=element_line(colour="blue"),# 主网格线颜色
       panel.grid.minor=element_line(colour="skyblue",linetype="dashed"),# 次网格线类型
       panel.background=element_rect(fill="lightblue"),# 图的背景
       panel.border=element_rect(colour="darkblue",fill=NA)# 图的边框
       )
+theme_bw()# 明亮色主题
+theme_grey(base_size=15)# 灰色主题

#--------------------------------------animation--------------------------------------------
## point plot===========
+transition_time(year)
### 图片随连续时间逐帧变化

## line plot============
+transition_reveal(Day)
### 图片随连续轨迹变化

## bar plot=============
+transition_states(Month)
### 图片随离散时间变化

## 特效================
+view_follow(fixed_y = TRUE)
### 坐标随数据变化，y轴固定
+shadow_wake(wake_length = 0.1)
### 数据变化有拖尾
+shadow_mark(alpha = 0.3, size = 0.5)
### 数据变化轨迹保留


#-----------------------------------------interactive-----------------------------------------
## ====================================================scatter
plot_ly(data,x=~x,y=~y,z=~z)%>%
  add_markers(alpha=0.3,
              symbol=~z,
              color=~z,
              colors=c('plum','darkblue','navy'),
              symbols=I(1:3))

## =========================================================line
plot_ly(data,x=~x,y=~y)%>%
  add_lines(y=~lm(y~x,data)$fitted,
            linetype=~z,
            color=I('hotpink'))%>%
  add_ribbons(ymin=~fitted-sd(y),ymax=~fitted_sd(y),
              color=I('grey'))%>%
  add_paths()

## =======================================================bar&histogram
subplot(
plot_ly(data,x=~x)%>%
  add_histogram(color=~z)%>%
  layout(barmode='stack'),
plot_ly(data,x=~x,y=~y)%>%
  add_bars()
)

## =================================================box
plot_ly(data,x=~interaction(a,b),y=~y)%>%
  add_boxplot(color=~z)

## ================================================heatmap&map
plot_ly(data,x=~x,y=~factor(y),z=~z)%>%
  add_heatmap()

map<-map_data('world')
map%>%group_by(group)%>%plot_ly(x=~lont,y=~lat)

city<-st_read('map.shp')
map<-tm_shape(city)+tm_polygon()
tmap_mode('view')
map
map+tm_basemap(server='OpenTopoMap')

leaflet()%>%addProviderTiles(providers$CartoDB.Positron)
leaflet()%>%addProviderTiles(providers$Esri.WorldImagery)
leaflet()%>%addProviderTiles (providers$Esri.WorldStreetMap)
leaflet()%>%addProviderTiles(providers$Esri.WorldStreetMap)%>%
  setView(lng= 102,lat = 3, zoom = 5)%>%
  ### 聚焦某个画幅
  addMarkers(lng= 102,lat = 3, 
             popup = "Hello from Malaysia!",
             popup = ~name)%>%
  ### 加一个标记
  addcircles(lng = 102,
             lat = 3, radius = 500, popup = "Circle area")%>%
  ### 加一个圆圈
  addPolygons(lat=c(6.1, 3.1, 1.4), lng = c(100.3, 101.7, 103.7),popup = "Polygon")%>%
  ### 加一个三角
  addPolygons(popup=~region)
  ### 加一个区域多边形

## ==============================================feature
f<-list(family='Courier',size=18,color='navy')
l<-list(font = list(family = "sans-serif",
                    size = 12,
                    color = "plum"),
        bgcolor = "ivory",
        bordercolor = "purple",
        borderwidth = 2,
        orientation="h")

%>%layout(title='dfdsa',
          ### 标题
          
          xaxis=list(title='fdsd',titlefont=f),
          yaxis=list(title='dfax',titlefont=f),
          ### x、y轴
          
          legend=l,
          ### 图例
          
          hoverlabel=list(bgcolor="khaki",bordercolor="chocolate",
                          font=list(family="Arial",color="chocolate")),
          ### 鼠标悬停框
          
          dragmode='lasso')
          ### 鼠标拖动选项，zoom（缩放）、pan（平移）、select（框选）、lasso（套索）

## 和ggplot一起用生成交互图
gf<-ggplot(mpg,aes (x=displ,y=hwy))+geom_point()+facet_wrap(~class)
ggplotly(gf)
  
