# 필요 패키지 설치
# install.packages("ggplot2") 
# install.packages("raster")
# install.packages("rgeos")
# install.packages("dplyr")
# install.packages('raster')
# install.packages("rgeos")
# install.packages("maptools")
# install.packages("rgdal")
# install.packages('showtext')
# install.packages('curl')

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
rent<-read.csv("c:/data/convenience/origin/rent.csv")
gu_id<-read.csv("c:/data/convenience/origin/gu_id.csv")
gu_location <- read.csv("c:/data/convenience/origin/gu_location.csv", header = TRUE)

# 파일확인
# View() : 테이블로 보기
# dim () : 차원수 확인
# str () : 데이터 구조 변수 개수 변수 명 관치 개수 관찰치 미리보기 
# View(rent)
# View(gu_id)
# View(gu_location)
# dim(rent)
# dim(gu_id)
# dim(gu_location)

# 변수명이 깨지는 걸 방지하여 변수명 변경
names(rent)<-c('gu','dong','avg','first','etc')
names(gu_id)<-c('gu','id')

# str(df)
# str(gu_id)

# 구별 임대가격 평균 계산
gu_rent<-rent %>%
    group_by(gu) %>%
    summarise(rent_mean=round(mean(avg),2))

# View(gu_rent)
# gu_rent<-as.numeric(gu_rent$rent_mean)

# View(gu_id)
# str(gu_rent)

# gu_id 조인 
gu_rent <- left_join(gu_rent, gu_id, by = "gu")
# View(gu_rent)

# 우리나라 지도 GIS포맷
map_shape <- shapefile("c:/data/convenience/SVG/SIG.shp")
map <- fortify(map_shape, region = "SIG_CD")
# View(map)
# str(map)

# 행정코드를 이용해 서울만 추출
map$id <- as.numeric(map$id)
seoul_map <- map[map$id <= 11740,]

# 기존데이터에 map join
data <- merge(seoul_map, gu_rent, by = "id")
View(data)

#글꼴
font_add_google('Do Hyeon','pen')

#자동실행
showtext_auto()

#사이즈 기본값 지정
update_geom_defaults("text", list(size = 4))

ggplot() + 
    geom_polygon(data = data, 
                aes(x = long, 
                    y = lat, 
                    group = group, 
                    fill = rent_mean),
                color = "white") +
    scale_fill_gradient(low = "white",
                        high = "#00CC99",
                        space = "Lab",
                        guide = "colourbar") +
    labs(fill = "서울 구별 평균 임대료") +
    theme_void() +
    theme(legend.position = c(.15, .85)) +
    theme(text=element_text(size=16, family='pen'))+
    geom_text(data = gu_location,
                aes(x = long,
                    y = lat,
                    label = paste(seoulgu, mean_seoul_gu, sep = "\n"),family='pen')) 

