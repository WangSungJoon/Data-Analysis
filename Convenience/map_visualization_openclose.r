# 필요 패키지 로드 
library(dplyr)
library(ggplot2)
library(rgeos)
library(maptools)
library(rgdal)
library(raster)
library(showtext)
library(curl)

# 파일불러오기
openclose<-read.csv("c:/data/convenience/origin/openclose.csv")
code<-read.csv("c:/data/convenience/origin/code.csv")
gu_id<-read.csv("c:/data/convenience/origin/gu_id.csv")
gu_location <- read.csv("c:/data/convenience/origin/gu_location.csv", header = TRUE)
# View(openclose)

#개폐업 데이터에 구 이름 join
openclose <- left_join(openclose, code, by = "상권코드")
names(openclose)

#구별 개업 점포수 합산
open <- openclose %>%
    group_by(시군구명) %>%
    summarise(open_count=sum(개업점포수))
# View(open)

#구별 폐업 점포수 합산
close <- openclose %>%
    group_by(시군구명) %>%
    summarise(close_count=sum(폐업점포수))
# View(close)

rate <- data.frame('gu' = open$시군구명,
    close_rate = open$open_count / close$close_count)
# View(survive)

names(open)<-c('gu','open_count')
names(close)<-c('gu','close_count')
names(gu_id)<-c('gu','id')


# gu_id 조인 
open <- left_join(open, gu_id, by = "gu")
close <- left_join(close, gu_id, by = "gu")
rate <- left_join(rate, gu_id, by = "gu")
# View(open)

# 우리나라 지도 GIS포맷
map_shape <- shapefile("c:/data/convenience/SVG/SIG.shp")
map <- fortify(map_shape, region = "SIG_CD")
# View(map)
# str(map)

# 행정코드를 이용해 서울만 추출
map$id <- as.numeric(map$id)
seoul_map <- map[map$id <= 11740,]

# 각 데이터에 map 데이터 join
open_data <- merge(seoul_map, open, by = "id")
close_data <- merge(seoul_map, close, by = "id")
rate_data <- merge(seoul_map, rate, by = "id")
# View(open_data)
# View(close_data)

#글꼴
font_add_google('Do Hyeon', 'pen')

#자동실행
showtext_auto()

#사이즈 기본값 지정
update_geom_defaults("text", list(size = 4))

#개업 점포수 시각화
ggplot() + 
    geom_polygon(data = open_data, 
                aes(x = long, 
                    y = lat, 
                    group = group, 
                    fill = open_count),
                color = "white") +
    scale_fill_gradient(low = "white",
                        high = "#FBCF61",
                        space = "Lab",
                        guide = "colourbar") +
    labs(fill = "서울 구별 개업 점포수") +
    theme_void() +
    theme(legend.position = c(.15, .85)) +
    theme(text=element_text(size=16, family='pen'))+
    geom_text(data = gu_location,
                aes(x = long,
                    y = lat,
                    label = paste(seoulgu, mean_seoul_gu, sep = "\n"),family='pen')) 

#폐업 점포수 시각화
ggplot() + 
    geom_polygon(data = close_data, 
                aes(x = long, 
                    y = lat, 
                    group = group, 
                    fill = close_count),
                color = "white") +
    scale_fill_gradient(low = "white",
                        high = "#D291BC",
                        space = "Lab",
                        guide = "colourbar") +
    labs(fill = "서울 구별 폐업 점포수") +
    theme_void() +
    theme(legend.position = c(.15, .85)) +
    theme(text=element_text(size=16, family='pen'))+
    geom_text(data = gu_location,
                aes(x = long,
                    y = lat,
                    label = paste(seoulgu, mean_seoul_gu, sep = "\n"),family='pen')) 

#개업 대비 폐업 점포수 시각화
ggplot() + 
    geom_polygon(data = rate_data, 
                aes(x = long, 
                    y = lat, 
                    group = group, 
                    fill = close_rate),
                color = "white") +
    scale_fill_gradient(low = "white",
                        high = "red",
                        space = "Lab",
                        guide = "colourbar") +
    labs(fill = "개업 대비 폐업 비율") +
    theme_void() +
    theme(legend.position = c(.15, .85)) +
    theme(text=element_text(size=16, family='pen'))+
    geom_text(data = gu_location,
                aes(x = long,
                    y = lat,
                    label = paste(seoulgu, mean_seoul_gu, sep = "\n"),family='pen')) 
