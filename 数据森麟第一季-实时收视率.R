
##########PART1：读取必要package########## 
## 字符处理
library(stringr)
## 数据爬取
library(RCurl)
library(XML)
library(RSelenium)
## 数据处理
library(sqldf)
library(data.table)
library(dplyr)
## 地图文件处理
library(maptools)  
library(mapproj)
## 数据可视化
library(ggplot2)    
library(ggthemes)



##########PART2：开始爬取过程########## 
setwd('D:/爬虫/收视率')

## 连接Server
remDr <- remoteDriver(remoteServerAddr = "127.0.0.1" 
                      , port = 4444
                      , browserName = "chrome")

## 打开浏览器
remDr$open()
remDr$navigate('http://www.csm-huan.com/index_weishi.html')
tv_rate <- data.frame(num=1:33)

## 循环爬取33个电视台的收视率
tv_rate$station <- 0
tv_rate$program <- 0
tv_rate$rate <- 0
tbody = '//*[@id="tbody"]'
content  <- getNodeSet(htmlParse(remDr$findElement(using = "xpath",
          tbody)$getElementAttribute("outerHTML")[[1]],encoding='utf-8'),
                     '//td')
content <- sapply(content,xmlValue)
for(i in 1:33){
  tv_rate$station[i]=gsub('\\d','',content[i*3-2])
  tv_rate$program[i]=content[i*3-1]
  tv_rate$rate[i]=as.numeric(gsub('\\%','',content[i*3]))
}


##########PART3：实时收视率写入本地########## 
loc <- sprintf('实时收视率%s.csv',format(Sys.time(),format="%m%d%H%M"))
write.csv(tv_rate,loc)

##########PART4：地图文件绘制########## 
province <- fread('卫视对照.csv',header=TRUE)
## 省份/电视台名称连接
province_rate <- sqldf('select b.*,a.*
                       from tv_rate a 
                       inner join province b on a.station = b.station')
province_rate$rate <- as.numeric(substr(as.character(province_rate$rate),1,6))

## 节目名称/排名标准化
for(i in 1:nrow(province_rate)){
  this_program <- strsplit(province_rate$program,split=':')[i]
  len <- length(this_program[[1]])
  this_program <- this_program[[1]][len]
  this_program <- strsplit(this_program,split='\\(')
  province_rate$program_name[i] <- this_program[[1]][1]
}
province_rate$com <- paste(province_rate$station,':',
                           as.character(province_rate$rate),'%',sep = '')
province_rate$rank <-1:31

## 地图文件融合
china_map <- readShapePoly("bou2_4p.shp")
china_map1 <- china_map@data
china_map1$id <- 0:(nrow(china_map1)-1)
china_map1$id <- as.character(china_map1$id)
china_map2 <- fortify(china_map)
china_map3 <- left_join(china_map2, china_map1,by='id')
colnames(province_rate)[2] <- 'NAME'
china_map3$NAME <- as.character(china_map3$NAME)
china_map4 <- left_join(china_map3,province_rate,by = 'NAME')

## 全国整体情况绘制
ggplot() +
  geom_polygon(data=china_map4,aes(x=long,y=lat,group=group,fill=rate),col='pink')+coord_map()+
  scale_fill_gradient2(low='white',high='#DC143C',mid='orange',
                       midpoint=max(province_rate$rate,na.rm = TRUE)/2)+
  xlim(73,137)+ylim(17,55)+scale_color_wsj()+
  geom_text(data=province_rate,aes(x=longitude,y=latitude,label=province_name,size=rate),size=2.8,alpha=0.7)+
  ggtitle(label = sprintf('实时收视率%s',format(Sys.time(),format="%Y/%m/%d %H:%M")))+
  theme_map()+theme(panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    axis.ticks = element_blank(),
                    axis.line = element_blank(),
                    axis.text = element_blank(),
                    axis.title = element_blank(),
                    legend.position = 'NONE',
                    plot.title = element_text(hjust=0.5,size=25)
  )


## 分省情况绘制
china_map5 <- china_map4
ggplot() +
  geom_polygon(data=subset(china_map5,!NAME %in%c('台湾省','香港特别行政区')&rank!=31),
               aes(x=long,y=lat,group=group,fill=rate),col='#FF6EB4')+
  scale_fill_gradient2(low='white',high='#DC143C',mid='orange',
                       midpoint=max(province_rate$rate,na.rm = TRUE)/2)+
  theme_wsj()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust=0.5),
        legend.position='none')+
  labs(title = sprintf('实时收视率分省对比%s',format(Sys.time(),
                                            format="%Y/%m/%d %H:%M")))+
  facet_wrap(~com,scales = 'free',shrink = TRUE)+
  geom_text(data=subset(province_rate,rank!=31),aes(x=longitude,
            y=latitude,label=program_name),size=2.8,alpha=0.8)





















