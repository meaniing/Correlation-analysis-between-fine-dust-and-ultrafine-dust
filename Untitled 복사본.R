#----
library(ggmap)
library(ggplot2)
library(rgeos)
library(maptools)
library(rgdal)
library(sf)
library(dplyr)
#-----
getwd()
setwd("/Users/kimtaemin/Desktop/R programing file") #1년 데이터 불러오기 
getwd()
#------
csv_weather <- read.csv('2021~2022 weather data.csv')
class(csv_weather) #데이터 객체 구성요소의 속성 확인 

colnames(csv_weather) <- c('날짜', '측정소명', '미세먼지', '초미세먼지',  '오존', '이산화질소', '일산화탄소', '아황산가스')
csv_weather#데이터 불러오기 

SeoulAir <- csv_weather #이름을 SeoulAir로 변환 

#분석변수 추출 & 변수이름 변경 

library('dplyr')#dplyr 라이브러리 불러오기 
str(SeoulAir)# 객체 확인 
SeoulAir <- SeoulAir %>% rename(data = '날짜', district = '측정소명', pm10 = '미세먼지',pm2.5 = '초미세먼지') %>% select(data, district, pm10, pm2.5)
#파이프 연산자 활용
str(SeoulAir)#잘 되었는지 확인 2

#변수의 결측치, 이상치 확인 후 처리

table(SeoulAir$data)
table(SeoulAir$district)
SeoulAir <-  SeoulAir %>% filter(district != '평균') #평균 중복 계산으로 삭제

summary(SeoulAir$pm10) #미세먼지 결측치 확인 
summary(SeoulAir$pm2.5) #초미세먼지 결측치 확인 

#파생 변수 만들기 

str(SeoulAir)
SeoulAir$month <- substr(SeoulAir$data, 6, 7)
SeoulAir$day <- substr(SeoulAir$data, 9, 10)
str(SeoulAir) # 변수가 chr형이라 num으로 변경 

SeoulAir$month <- as.numeric(SeoulAir$month)
SeoulAir$day <- as.numeric(SeoulAir$day)
str(SeoulAir)

SeoulAir$season <- ifelse(SeoulAir$month %in% c('3', '4', '5'), 'spring', ifelse(SeoulAir$month %in% c('6', '7', '8'), 'summer', ifelse(SeoulAir$month %in% c('9', '10', '11'), 'autume', 'winter')))
#계절 추가 
str(SeoulAir)

#미세먼지 분석 

SeoulAir %>% filter(!is.na(pm10)) %>% 
  filter(pm10 == max(pm10)) %>%
  select(data, district, pm10)

#1) 평균이 가장 낮은 5개의 구 (오름차순)

SeoulAir %>%
  filter(!is.na(pm10)) %>%
  group_by(district) %>%
  summarise(m = mean(pm10)) %>%
  arrange(m) %>%
  head(5)

#2) 평균이 가장 높은 5개 구 (내림차순)

SeoulAir %>%
  filter(!is.na(pm10)) %>%
  group_by(district) %>%
  summarise(m = mean(pm10)) %>%
  arrange(desc(m)) %>%
  head(5)

#3) 계절별 분석

SeoulAir %>%
  filter(!is.na(pm10)&!is.na(pm2.5)) %>%
  group_by(season) %>%
  summarise(pm10_m = mean(pm10),
            pm2.5_m = mean(pm2.5)) %>%
  arrange(pm10_m)

#4) 미세먼지 등급 분석 

SeoulAir %>%
  filter(!is.na(pm10)) %>%
  mutate(pm_grade = ifelse(pm10 <= 15, 'good',
                           ifelse(pm10 <= 35, 'nomal',
                                  ifelse(pm10 <= 75, 'bed',
                                         ifelse(pm10 <= 500, 'very bad'))))) %>%
group_by(pm_grade) %>%
  summarise(cnt = n()) %>%
  mutate(total = sum(cnt),
         pct = round(cnt/total * 100, 1)) %>%
  select(pm_grade, cnt, pct) %>%
  arrange(desc(cnt))

#5) 구별 미세먼지 등급 비교 
SeoulAir %>%
  filter(!is.na(pm10)) %>%
  mutate(pm_grade = ifelse(pm10 <= 15, 'good',
                           ifelse(pm10 <= 35, 'nomal',
                                  ifelse(pm10 <= 75, 'bed',
                                         ifelse(pm10 <= 500, 'very bad'))))) %>%
  group_by(district, pm_grade) %>%
  summarise(cnt = n()) %>%
  mutate(total = sum(cnt),
         pct = round(cnt/total * 100, 1)) %>%
  filter(pm_grade == 'good') %>%
  select(pm_grade, cnt, pct) %>%
  arrange(desc(cnt))

##미세먼지 추이 그래프 그리기 

library(ggplot2)
ggplot(data = SeoulAir, aes(x = data, y = pm10)) +
  geom_line()

SeoulAir$data <- as.Date(SeoulAir$data)
ggplot(data = SeoulAir, aes(x = data, y = pm10)) +
  geom_line()

#계절별 미세먼지 등급 비율 그래프 그리기 

#1) 일반 막대 그래프

season_grade <- SeoulAir %>%
  filter(!is.na(pm10)) %>%
  mutate(pm_grade = ifelse(pm10 <= 15, 'good',
                           ifelse(pm10 <= 35, 'nomal',
                                  ifelse(pm10 <= 75, 'bed',
                                         ifelse(pm10 <= 500, 'very bad'))))) %>%
group_by(season, pm_grade) %>%
summarise(cnt = n()) %>%
mutate(total = sum (cnt),
       pct = round(cnt / total * 100, 1)) %>%
select(season, pm_grade, pct)

ggplot(data = season_grade, aes(x = season, y = pct, fill = pm_grade)) +
  geom_col(position = 'dodge') + 
  scale_x_discrete(limits = c('spring', 'summer', 'autume', 'winter')) +
  ggtitle('2021 ~ 2022') + 
  xlab('season') +
  ylab('비율')

#누적 막대 그래프

season_grade <- SeoulAir %>%
  filter(!is.na(pm10)) %>%
  mutate(pm_grade = ifelse(pm10 <= 15, 'good',
                           ifelse(pm10 <= 35, 'nomal',
                                  ifelse(pm10 <= 75, 'bed',
                                         ifelse(pm10 <= 500, 'very bad'))))) %>%
  group_by(season, pm_grade) %>%
  summarise(cnt = n()) %>%
  mutate(total = sum (cnt),
         pct = round(cnt / total * 100, 1)) %>%
  select(season, pm_grade, pct)

ggplot(data = season_grade, aes(x = season, y = pct, fill = pm_grade)) +
  geom_col(position = 'stack') + 
  scale_x_discrete(limits = c('spring', 'summer', 'autume', 'winter')) +
  ggtitle('2021 ~ 2022') + 
  xlab('season') +
  ylab('비율')

#2) 25개구 연간 초 미세먼지 평균 구하고, 평균의 내림차순으로 막대 그래프 정렬 

district_pm2.5 <- SeoulAir %>%
  filter(!is.na(pm2.5)) %>%
  group_by(district) %>%
  summarise(m = mean(pm2.5))

ggplot(data = district_pm2.5, aes(x = reorder(m, district), y = district)) +
  geom_bar(stat = 'identity')

ggplot(data = district_pm2.5, aes(x = reorder(district, m), y = m)) +
  geom_bar(stat = 'identity')

ggplot(data = district_pm2.5, aes(x = reorder(district, m), y = m)) +
  geom_bar(stat = 'identity')+
  coord_flip()

#3) 연간 초 미세먼지 평균이 가장 높은 구와 가장 낮은 구의 평균이 통계적으로 차이가 있는가 ?

SeoulAir %>%
  filter(!is.na(pm2.5)) %>% 
  group_by(district) %>%
  summarise(m = mean (pm2.5)) %>% # step1
  filter(m == max(m)|
         m == min (m)) # step2

#활당값 

district_2 <-  SeoulAir %>%
  filter(district %in% c('노원구', '성동구'))
district_2

#t.test(data = 데이터 세트, 종속변수(비교값)~독립변수(비교대상))

#유의수준 분석
t.test(data = district_2, pm2.5~district)

#상관관계 분석
cor.test(SeoulAir$pm10, SeoulAir$pm2.5) #상관계수는 0.8773이다. 
#즉 미세먼지와 초 미세먼지는 정적인 상관관계가 있다고 할 수 있다. 

#---------------------
#단순회귀분석

#H0: 미세먼지는 초미세먼지에 영향을 주지 않는다.
#H1: 미세먼지는 초 미세먼지에 영향을 준다.
lm(data = SeoulAir, pm2.5~pm10) 
#위에 코드 해석 : pm10계수는 0.6068이며, 절편은 -1.1997이다. 
#pm2.5 = -1.1997 + 0.0668 * pm10
#즉, 미세먼지가 1단위 올라갈때마다 초 미세먼지는 0.0668만큼 증가한다.(독립변수가 종속변수에 미치는 영향력)


RA <- lm(data = SeoulAir,pm2.5~pm10)
summary(RA)
#분석 해석 : 미세먼지가 초미세먼지에 미치는 영향력은 유의수준 p<.001에서 0.606820이다.
#회귀모형은 적합하여 회귀계수는 통계적으로 유의미하다 
#수정된 결정계수는 0.7698로 회귀식의 설명력에 신뢰성을 가지고 있다. 
#---------------------

#H0: 초미세먼지는 미세먼지에 영향을 주지 않는다.
#H1: 초미세먼지는 미세먼지에 영향을 준다.
lm(data = SeoulAir, pm10~pm2.5)
#pm2.5의 계수는 1.269이며, 절편은 9.284이다.
#pm10 = 9.284 + 1.269 * pm2.5 
#즉, 초미세먼지 1단위가 올라갈때 마다 미세먼지는 1.269만큼 증가 

RA2 <- lm(data = SeoulAir, pm10~pm2.5)
summary(RA2)
#분석 해석 : p<.001에서 1.268630이다. 
#회귀모형은 적합하여 회귀계수는 통계적으로 유의미하다. 
#초미세먼지는 미세먼지에 정적인 영향을 준다.
#수정된 결정계수는 0.7698이므로 회귀식의 설명력에 신뢰성을 가지고 있다고 할 수 있다.

#-----
library(ggmap)
library(ggplot2)
library(rgeos)
library(maptools)
library(rgdal)
library(sf)

SeoulAir %>%
  filter(!is.na(pm10)) %>%
  mutate(pm_grade = ifelse(pm10 <= 15, 'good',
                           ifelse(pm10 <= 35, 'nomal',
                                  ifelse(pm10 <= 75, 'bed',
                                         ifelse(pm10 <= 500, 'very bad'))))) %>%
  group_by(district, pm_grade) %>% # district별로 pm_grade 등급 분류
  summarise(cnt = n()) %>%  # district 별로 빈도 총계 구하기
  mutate(total = sum(cnt), 
         pct = round(cnt/total*100,1)) %>% # district별로 pm_grade등급별 백분율
  filter(pm_grade == 'good') %>% 
  select(district, cnt, pct) %>% 
  arrange(desc(pct)) %>% 
  head(5)

p2 <- read.csv('/Users/kimtaemin/Desktop/R programing file/sample.csv')
p <- read.csv('/Users/kimtaemin/Desktop/R programing file/sample.csv')

p2 <- SeoulAir %>%
  filter(!is.na(pm10)) %>%
  mutate(pm_grade = ifelse(pm10 <= 15, 'good',
                           ifelse(pm10 <= 35, 'nomal',
                                  ifelse(pm10 <= 75, 'bed',
                                         ifelse(pm10 <= 500, 'very bad'))))) %>%
  group_by(district, pm_grade) %>% 
  summarise(cnt = n()) %>%  
  mutate(total = sum(cnt), 
         pct = round(cnt/total*100,1)) %>% 
  filter(pm_grade == 'good') %>% 
  select(district, cnt, pct) %>% 
  arrange(desc(pct)) 

p2$id <- as.numeric(p$id)
p2<- p2 %>% relocate(district,id,cnt,pct)
str(p2)


map <- readShapePoly('/Users/kimtaemin/Desktop/SIG_201703/TL_SCCO_SIG.shp')

class(map)
slotNames(map)
map_info <- map@data
head(map_info)

map@proj4string

map <- spTransform(map, CRSobj = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
map@polygons[[1]]@Polygons[[1]]@coords %>% head(n = 10L)


new_map <- fortify(map, region = 'SIG_CD') # map의 SIG_ID를 ID변수로 지정해준다

new_map$id <- as.numeric(new_map$id)
seoul_map <- new_map[new_map$id <= 11740,]

p2_merge <- merge(seoul_map, p2, by = 'id')

map_graph2 <- ggplot() + geom_polygon(data = p2_merge, aes(x = long, y =yat, group = group, fill = cnt))

map_graph2 +
  scale_fill_gradient(low = "#C3D7A4", high = "#52854C", space = "Lab", guide = "colourbar") +
  theme_bw() +
  labs(title = "서울시 미세먼지 '좋음'수준") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), plot.title = element_text(face = "bold", size = 18, hjust = 0.5)) +
  geom_text(data = seoul_district2, aes(x = long, y = lat, label= paste(district)))