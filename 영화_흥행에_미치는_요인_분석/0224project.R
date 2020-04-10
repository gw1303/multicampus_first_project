#########################################################################################
'프로젝트'
#########################################################################################
## 독립변수 : 영화의 매출액, 관객수
## 종속변수 : 감독, 출연배우, 리뷰에 가장 많이 등장한 단어 10개

## 한국영화진흥원 상업영화 일간 박스오피스 xml로 불러오기
install.packages('stringr')
library(stringr)

api_key <- '46f213e3bda7b61b3bc000dc34b13197'

'http://www.kobis.or.kr/kobisopenapi/webservice/rest/boxoffice/searchDailyBoxOfficeList.xml?key=430156241533f1d058c603178cc3ca0e&multiMovieYn=N&targetDt=20120101'
# 영화 흥행정보 csv로드 
df <- read.csv('boxoffice.csv',
               stringsAsFactors = F,
               na.strings = c('','0'))

head(df)
str(df)
summary(df)

# 필요없는 열 날리기
# 순위, 국적, X1
df <- df[-c(1,6,9)]
df

# 영화명에 공백 날리기
df$영화명_공백X <- str_replace_all(df$영화명,' ','')


# 숫자 데이터를 numeric 타입으로 변환하기 위해 특수기호 제거
df$개봉일 <- str_replace_all(df$개봉일,'-','')
df$매출액 <- str_replace_all(df$매출액,',','')
df$관객수 <- str_replace_all(df$관객수,',','')
df$스크린수 <- str_replace_all(df$스크린수,',','')


# 숫자 데이터를 numeric 타입으로 변환
df[c(3,4,5)] <- sapply(df[c(3,4,5)], as.numeric)
str(df)


# 영화 이름으로 영화 코드를 추출해서 저장
df$영화코드 <- rep(NA,dim(df)[1])

install.packages('XML')
library(XML)
for (i in 406:500){
  url <- paste('http://www.kobis.or.kr/kobisopenapi/webservice/rest/movie/searchMovieList.xml?key=552f23b339058f4301ede3fcda933284&movieNm=',df$영화명_공백X[i], sep='')
  
  a <- read_xml(url) %>% xml_nodes('movieNm') %>% xml_text() == df$영화명[i]
  b <-read_xml(url) %>% xml_nodes('repNationNm') %>% xml_text() == df$대표국적[i]
  c <- read_xml(url) %>% xml_nodes('movieCd') %>% xml_text()
  print(i)
  
  df$영화코드[i] <- ifelse(length(a[a==T])>0,c[a&b],NA)
  
  print('성공')
}

# 영화코드 NA값 찾아서 채우기 
df$영화코드[is.na(df$영화코드)] <- 
  c('20060151',  # 괴물
    '20148851',  # 암살
    '20161725',  # 마스터
    '20153444',  # 럭키
    '20072152',  # 좋놈
    '20124039',  # 타워
    '20144444',  # 아가씨
    '20165748',  # 꾼
    '20176103',  # 돈
    '20154661',  # 형
    '20137044',  # 친구2
    '20134582',  # 소원
    '20010040',  # 친구
    '20070441',  # 사랑
    '20040554',  # 가족
    '20124936',  # 나의 ps파트너
    '20090023',  # 작전
    '20070433',  # 행복
    '20166384',  # 하루
    '20040571',  # 령
    '20070639',  # 내 사랑
    '20072151'  # 바보
  )

# df중간 저장
save_df <- df

# 매출액 NA값 채우기
mean(df$매출액 / df$관객수, na.rm=T)  # 7551

df$매출액[is.na(df$매출액)] <- df$관객수 * 7551

summary(df)

write.csv(df, file = 'movie.csv')

df <- read.csv('movie.csv')

## 영화 배우 불러오기
movie_cast <- data.frame('영화명'=df$영화명, stringsAsFactors = F)
str(movie_cast)

# 영화코드 꼬인 것 수정 
df$영화코드[df$영화명==movie_cast[82,1]]  <- '20124401'
df$영화코드[df$영화명==movie_cast[85,1]]  <- '20144641'
df$영화코드[df$영화명==movie_cast[209,1]] <- '20030152'
df$영화코드[df$영화명==movie_cast[231,1]] <- '20060100'
df$영화코드[df$영화명==movie_cast[232,1]] <- '20122784'
df$영화코드[df$영화명==movie_cast[244,1]] <- '20137043'
df$영화코드[df$영화명==movie_cast[256,1]] <- '20124045'
df$영화코드[df$영화명==movie_cast[263,1]] <- '20134123'
df$영화코드[df$영화명==movie_cast[315,1]] <- '20179230'
df$영화코드[df$영화명==movie_cast[319,1]] <- '20060353'
df$영화코드[df$영화명==movie_cast[340,1]] <- '20126671'
df$영화코드[df$영화명==movie_cast[342,1]] <- '20070205'
df$영화코드[df$영화명==movie_cast[358,1]] <- '20050346'
df$영화코드[df$영화명==movie_cast[384,1]] <- '20136061'
df$영화코드[df$영화명==movie_cast[438,1]] <- '20090311'
df$영화코드[df$영화명==movie_cast[500,1]] <- '19820019'

'82  # 신세계
85   # 판도라
209  # 위대한 유산
231  # 사생결단
232  # 몽타주
244  # 남자가 사랑할 때
256  # 코리아
263  # 집으로 가는 길
315  # 도어락
319  # 해바라기
340  # 런닝맨
342  # 검은 집
358  # 싸움의 기술
384  # 결혼전야
438  # 시크릿
500  # 만추'


# 포스터에 이름이 언급되는 7명정도의 배우 이름 추출 
for (i in  c(82,85,209,231,232,244,256,263,315,319,340,342,358,384,438,500)) {
  url <-paste('http://www.kobis.or.kr/kobisopenapi/webservice/rest/movie/searchMovieInfo.xml?key=430156241533f1d058c603178cc3ca0e&movieCd=',
              df$영화코드[i], sep='') 
  
  x <- read_xml(url) %>% xml_nodes('actor peopleNm') %>% xml_text()
  
  print(i)
  movie_cast$배우[i] <- paste(x[1:7], collapse = ' ')
  print('성공')
}

save_cast <- cast


## 집에서 작업하느라 중간저장 파일 로드 
df <- read.csv('movie.csv')
cast <- read.csv('movie_cast.csv')

# 중복 배우 삭제를 위해 모든 배우값을 하나의 벡터로 만듬
actor <- paste(cast$배우, collapse = ' ')
actor <- unlist(str_split(actor, ' '))

# unique 함수를 이용해 중복 배우 제거
actor_list <- unique(actor)

# 2,3글자가 아닌 배우 삭제 
actor_list <- actor_list[-which(nchar(actor_list)>3)]
actor_list <- actor_list[-which(nchar(actor_list)<2)]
actor_list <- actor_list[-which(actor_list=='NA')]


# 배우들의 이름을 열이름으로 갖는 데이터프레임 생성 
actor_col <- data.frame(stringsAsFactors = F)
actor_col <- rbind(actor_col, actor_list)
colnames(actor_col) <- actor_list

# 각 열의 값을 NA로 초기화
actor_col[1,] <- NA

# cast 데이터프레임과 배우이름을 열로 갖는 데이터프레임 결합 
cast_col <-cbind(cast, actor_col)

# 데이터 값을 Factor타입이 아닌 char 타입으로 변경
cast_col <- data.frame(sapply(cast_col, as.character), stringsAsFactors = F)


# 배우 컬럼에 요소로 들어있으면 배우 이름 컬럼값으로 1을 준다.
for (i in 1:dim(cast_col)[1]){
  actor <- str_split(cast_col$배우, ' ')[[i]]
  
  for (j in 1: length(actor)){
    target <- which(colnames(cast_col) == actor[j])
    cast_col[i,target] <- 1
  }
}

# 영화에 출현하지 않은 배우 열의 값을 0으로 
cast_col[is.na(cast_col)] <- 0

cast_col[3:dim(cast_col)[2]]


res <- cbind(df[c(2,4,5)],cast_col[3:dim(cast_col)[2]])

# 데이터를 숫자 타입으로 변경
res[-1] <- data.frame(sapply(res[-1], as.numeric), stringsAsFactors = F)
str(res)
dim(res)

# 출연 횟수가 5회 이하인 배우들 제거 
idx <- c(0)
j <- 1
for (i in 4:dim(res)[2]){
  if (sum(res[i]) < 5){
    idx[j] <- i
    j <- j +1  
  }
}

res_filter <- res[-idx]
dim(res_filter)  # 500 167


write.csv(res, 'res.csv')

paste(colnames(res_filter), collapse = '+')

# 선형회귀분석 모델
model <- lm(formula = 관객수~최민식+류승룡+조진웅+진구+이하늬+진선규+이동휘+하정우+차태현+주지훈+김향기+마동석+황정민+김윤진+오달수+정진영+장영남+라미란+유아인+유해진+송강호+변희봉+박해일+김윤석+김혜수+이정재+전지현+김해숙+박원상+김정태+이경영+최덕문+이병헌+한효주+김인권+심은경+류준열+장동건+원빈+공유+정유미+최우식+설경구+하지원+박중훈+엄정화+이민기+김영애+곽도원+송영창+정원중+안성기+허준호+정재영+강신일+이선균+조여정+강동원+이성민+박성웅+김응수+주진모+조정석+김지영+백윤식+손예진+박철민+나문희+성동일+박보영+신하균+임하룡+현빈+김주혁+조성하+한지민+김무열+김상경+이요원+박희순+이희준+한석규+류승범+엄지원+진경+김종수+조승우+김홍파+배성우+조재윤+이범수+정준호+이현우+손현주+김성균+고창석+김상호+정우성+김광일+김아중+전혜진+김희원+김성오+임수정+염정아+정웅인+박용우+강하늘+신현준+김수미+김효진+문정희+윤제문+김의성+조인성+김뢰하+김유정+박해준+차승원+김하늘+권상우+김혜옥+신정근+조우진+김명민+송지효+김대명+강예원+문소리+엄태웅+이제훈+김혜정+김원진+임창정+최성국+박준규+박정민+조은지+유지태+조재현+최종원+고수+전도연+정만식+한예리+윤여정+유준상+이광수+김래원+김광규+수애+이문식+정경호+김강우+송새벽+오정세+유오성+공효진+이준혁+천호진+성지루+유승호+이성재+김수로,
            data=res)
summary(model)

## R-squard 0.53 / Adjusted R-squared 0.30 /  p-value 0.05 이하
## 다중회귀 분석의 경우 독립변수가 많아지면 설명력이 증가되는 단점이 존재.
## 너무 많은 독립변수를 가지고 회귀분석시 수정된 결졍계수 활용 
## 설명력이 너무 약한 문제 

# 매출액 모델 
model2 <- lm(formula = 매출액~최민식+류승룡+조진웅+진구+이하늬+진선규+이동휘+하정우+차태현+주지훈+김향기+마동석+황정민+김윤진+오달수+정진영+장영남+라미란+유아인+유해진+송강호+변희봉+박해일+김윤석+김혜수+이정재+전지현+김해숙+박원상+김정태+이경영+최덕문+이병헌+한효주+김인권+심은경+류준열+장동건+원빈+공유+정유미+최우식+설경구+하지원+박중훈+엄정화+이민기+김영애+곽도원+송영창+정원중+안성기+허준호+정재영+강신일+이선균+조여정+강동원+이성민+박성웅+김응수+주진모+조정석+김지영+백윤식+손예진+박철민+나문희+성동일+박보영+신하균+임하룡+현빈+김주혁+조성하+한지민+김무열+김상경+이요원+박희순+이희준+한석규+류승범+엄지원+진경+김종수+조승우+김홍파+배성우+조재윤+이범수+정준호+이현우+손현주+김성균+고창석+김상호+정우성+김광일+김아중+전혜진+김희원+김성오+임수정+염정아+정웅인+박용우+강하늘+신현준+김수미+김효진+문정희+윤제문+김의성+조인성+김뢰하+김유정+박해준+차승원+김하늘+권상우+김혜옥+신정근+조우진+김명민+송지효+김대명+강예원+문소리+엄태웅+이제훈+김혜정+김원진+임창정+최성국+박준규+박정민+조은지+유지태+조재현+최종원+고수+전도연+정만식+한예리+윤여정+유준상+이광수+김래원+김광규+수애+이문식+정경호+김강우+송새벽+오정세+유오성+공효진+이준혁+천호진+성지루+유승호+이성재+김수로,
             data=res)
summary(model2)
## R-squard 0.44 / Adjusted R-squared 0.16 /  p-value 0.05 이하


# 모델에서 coefficients를 티켓파워라고 지정
ticket_power <- data.frame(model$coefficients)
summary(ticket_power)
# 최대값 : 3815816 ,평균 : 390744 , 최소값 : -2077585

# 티켓파워가 높은 순으로 정렬
ticket_power$model.coefficients <- sort(ticket_power$model.coefficients, decreasing = T)

ticket_power

colnames(ticket_power) <- 'ticketpower'

## 시각화 
install.packages('ggplot2')
library(ggplot2)

ticket_power$actor <- rownames(ticket_power)

top10 <- head(ticket_power, 11)
top10 <- top10[2:11,]
bottom10 <- tail(ticket_power, 10)


ggplot(data = top10, aes(reorder(actor,ticketpower), ticketpower)) +
  geom_bar(stat="identity",width=0.7, fill = 'coral') + 
  ggtitle('스타파워 TOP 10') +
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 25, color = "darkblue")) +
  labs(x="배우", y="스타파워") +
  scale_y_continuous(labels = scales::comma) +
  coord_flip()+
  ggsave("top10.jpg", dpi = 300)

res[res$진구 == '1']$영화명

ggplot(data = bottom10, aes(reorder(actor,ticketpower), ticketpower)) +
  geom_bar(stat="identity",width=0.7, fill = 'coral') + 
  ggtitle('스타파워 BOTTOM 10') +
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 25, color = "darkblue")) +
  labs(x="배우", y="스타파워") +
  scale_y_continuous(labels = scales::comma) +
  coord_flip() +
  ggsave("bottom10.jpg", dpi = 300)

#+
# ggsave("main_filter.jpg", dpi = 300)

res

## 장르 불러오기
library(rvest)
genre <- c(0)

for (i in  1:500) {
  url <-paste('http://www.kobis.or.kr/kobisopenapi/webservice/rest/movie/searchMovieInfo.xml?key=430156241533f1d058c603178cc3ca0e&movieCd=',
              df$영화코드[i], sep='') 
  
  genre[i] <- read_xml(url) %>% xml_nodes('genre') %>% xml_text() %>% paste(collapse = ' ')
  
  print(i)
  print('성공')
}

df$장르 <- genre
df


## 메인장르 추출
main_filter <- read.csv('project/main_filter.csv')

genre_filter <- read.csv('project/genre_filter.csv')

ggplot(data = genre_filter, aes(reorder(genre,관객수), 관객수)) +
  geom_bar(stat="identity",width=0.7, fill = 'coral') + 
  ggtitle('복합장르별 평균 관객 수') +
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 25, color = "darkblue")) +
  labs(x="장르", y="평균 관객 수") +
  scale_y_continuous(labels = scales::comma) +
  coord_flip() +
  ggsave("genre_filter.jpg", dpi = 300)

ggplot(data = main_filter, aes(reorder(main_genre,관객수), 관객수)) +
  geom_bar(stat="identity",width=0.7, fill = 'coral') + 
  ggtitle('메인장르별 평균 관객 수') +
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 25, color = "darkblue")) +
  labs(x="장르", y="평균 관객 수") +
  scale_y_continuous(labels = scales::comma) +
  coord_flip() +
  ggsave("main_filter.jpg", dpi = 300)


## 배급사 파워 
unique(df$배급사)

distributor_list <- unique(unlist(str_split(df$배급사, ',')))

distributor_list <- str_replace_all(distributor_list, '\\(주\\)','')
distributor_list <- str_replace_all(distributor_list, '\\(.+\\)','')
distributor_list <- str_replace_all(distributor_list, '㈜','')
distributor_list <- str_replace_all(distributor_list, ' ','')

# 배우들의 이름을 열이름으로 갖는 데이터프레임 생성 
distributor_col <- data.frame(stringsAsFactors = F)
distributor_col <- rbind(distributor_col, distributor_list)
colnames(distributor_col) <- distributor_list

df$배급사 <- sapply(df$배급사, as.character)
str(df)

df$배급사[is.na(df$배급사)] <- '-'

distributer <- df[c(3,6,9)]


# cast 데이터프레임과 배우이름을 열로 갖는 데이터프레임 결합 
distributor_col <-cbind(distributer, distributor_col)
distributor_col[4:53] <- NA

str(distributor_col)


# 배우 컬럼에 요소로 들어있으면 배우 이름 컬럼값으로 1을 준다.
for (i in 1:dim(distributor_col)[1]){
  distributor <- str_split(distributor_col$배급사, ',')[[i]]
  
  distributor <- str_replace_all(distributor, '\\(주\\)','')
  distributor <- str_replace_all(distributor, '\\(.+\\)','')
  distributor <- str_replace_all(distributor, '㈜','')
  distributor <- str_replace_all(distributor, ' ','')
  
  for (j in 1: length(distributor)){
    target <- which(colnames(distributor_col) == distributor[j])
    distributor_col[i,target] <- 1
  }
}

# 영화에 출현하지 않은 배우 열의 값을 0으로 
distributor_col[is.na(distributor_col)] <- 0


distributor_col <- distributor_col[-16]

sum(distributor_col[6])

# 출연 횟수가 5회 이하인 배우들 제거 
idx <- c(0)
j <- 1
for (i in 4:dim(distributor_col)[2]){
  if (sum(distributor_col[i]) < 5){
    idx[j] <- i
    j <- j +1  
  }
}

distributor_col_filter <- distributor_col[-idx]
dim(distributor_col_filter)


model3 <- lm(관객수~씨제이이앤엠+롯데쇼핑롯데엔터테인먼트+쇼박스+넥스트엔터테인먼트월드+시네마서비스+롯데컬처웍스롯데엔터테인먼트+씨제이엔터테인먼트+워너브러더스코리아+이십세기폭스코리아+메가박스중앙플러스엠+영화사청어람+CGV아트하우스+쇼이스트+시너지하우스,
                data=distributor_col_filter)

summary(model3)

paste(colnames(distributor_col_filter),collapse = '+')


distributor_power <- data.frame(model3$coefficients)
summary(distributor_power)
# 최대값 : 3815816 ,평균 : 390744 , 최소값 : -2077585

# 티켓파워가 높은 순으로 정렬
distributor_power$model.coefficients <- sort(distributor_power$model.coefficients, decreasing = T)

distributor_power

colnames(distributor_power) <- 'distributor_power'

distributor_power$distributor <- rownames(distributor_power)

distributor_power <- distributor_power[2:15,]

ggplot(data = distributor_power, aes(reorder(distributor,distributor_power), distributor_power)) +
  geom_bar(stat="identity",width=0.7, fill = 'coral') + 
  ggtitle('배급사 흥행파워') +
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 25, color = "darkblue")) +
  labs(x="배급사", y="평균 관객 수") +
  scale_y_continuous(labels = scales::comma) +
  coord_flip() +
  ggsave("distributor_power.jpg", dpi = 300)


df

df$영화명_공백X <- str_replace_all(df$영화명_공백X,'\\(.+\\)','')

for (i in 1:10){
  name <- df$영화명_공백X[i]
  
  url <- paste('https://search.naver.com/search.naver?sm=tab_hty.top&where=nexearch&query=영화',name, sep = '')
  
  grade <- read_html(url) %>% html_nodes('.r_grade em') %>% html_text()
  
  df$네티즌평점[i] <- grade[1]
  df$평론가평점[i] <- grade[3]
  print(i)
}



length(df$네티즌평점[is.na(df$네티즌평점)])


df$영화명_공백X[is.na(df$네티즌평점)]

df$네티즌평점[df$영화명_공백X == '고死두번째이야기:교생실습'] <- 4.72
df$평론가평점[df$영화명_공백X == '고死두번째이야기:교생실습'] <- 1.67



df[c(13,14)] <- data.frame(sapply(df[c(13,14)], as.numeric))
plot(df$네티즌평점,df$관객수)


ggplot(data = df, aes(네티즌평점, 관객수)) +
  geom_point() + 
  ggtitle('네티즌 평점별 관객 수') +
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 25, color = "darkblue")) +
  labs(x="네티즌 평점", y="관객 수") +
  scale_y_continuous(labels = scales::comma)+ stat_smooth(method = 'lm') +
  ggsave("netizen_grade.jpg", dpi = 300)


ggplot(data = df, aes(평론가평점, 관객수)) +
  geom_point() + 
  ggtitle('평론가 평점별 관객 수') +
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 25, color = "darkblue")) +
  labs(x="평론가 평점", y="관객 수") +
  scale_y_continuous(labels = scales::comma)+ stat_smooth(method = 'lm') +
  ggsave("grade.jpg", dpi = 300)

cor(df$네티즌평점,df$관객수)
cor(df$관객수[!is.na(df$평론가평점)],df$평론가평점[!is.na(df$평론가평점)])

    
df$영화명[df$네티즌평점>=9.4]

df$영화명_공백X <- sapply(df$영화명_공백X, as.character)

dim(df)[1]
for (i in 1:10){
  name <- URLencode(df$영화명_공백X[i])
  
  url <- paste('http://www.cine21.com/search//?q=',name, sep = '')
  
  grade <- read_html(url) %>% html_nodes('.num') %>% html_text()
  
  df$씨네21_네티즌평점[i] <- grade[1]
  df$씨네21_평론가평점[i] <- grade[3]
  print(i)
}

name <- URLencode(df$영화명_공백X[2])

url <- paste('http://www.cine21.com/search//?q=',name, sep = '')

read_html(url) %>% html_nodes('.num')%>% html_text()





df$씨네21_네티즌평점[df$영화명_공백X =='지금사랑하는사람과살고있습니까?'] <- df$네티즌평점[df$영화명_공백X =='지금사랑하는사람과살고있습니까?']
df$씨네21_평론가평점[df$영화명_공백X =='지금사랑하는사람과살고있습니까?'] <- df$평론가평점[df$영화명_공백X =='지금사랑하는사람과살고있습니까?']

df[c(15,16)] <- data.frame(sapply(df[c(15,16)], as.numeric))
str(df)


ggplot(data = df, aes(씨네21_네티즌평점, 관객수)) +
  geom_point() + 
  ggtitle('씨네21 네티즌 평점별 관객 수') +
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 25, color = "darkblue")) +
  labs(x="네티즌 평점", y="관객 수") +
  scale_y_continuous(labels = scales::comma)+ stat_smooth(method = 'lm') +
  ggsave("cine21_netizen_grade.jpg", dpi = 300)


ggplot(data = df, aes(씨네21_평론가평점, 관객수)) +
  geom_point() + 
  ggtitle('씨네21 평론가 평점별 관객 수') +
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 25, color = "darkblue")) +
  labs(x="평론가 평점", y="관객 수") +
  scale_y_continuous(labels = scales::comma)+ stat_smooth(method = 'lm') +
  ggsave("cine21_grade.jpg", dpi = 300)

df$영화명[df$씨네21_네티즌평점<=1.0]
cor(df$씨네21_네티즌평점,df$관객수)

df$영화코드[df$영화명=='사도']

name <- URLencode('사도')

url <- paste('http://www.cine21.com/search//?q=',name, sep = '')

read_html(url) %>% html_nodes('.sub_info a') %>% html_text()
tit

df
for (i in  1:5) {
  url <-paste('http://www.kobis.or.kr/kobisopenapi/webservice/rest/movie/searchMovieInfo.xml?key=552f23b339058f4301ede3fcda933284&movieCd=',
              df$영화코드[i], sep='') 
  
  name <- read_xml(url) %>% xml_nodes('peopleNm') %>% xml_text()
  
  df$감독[i] <- name[1]
  
  print(i)
  print('성공')
}

df$감독

url <-paste('http://www.kobis.or.kr/kobisopenapi/webservice/rest/movie/searchMovieInfo.xml?key=552f23b339058f4301ede3fcda933284&movieCd=',
            df$영화코드[83], sep='') 

read_xml(url) %>% xml_nodes('peopleNm') %>% xml_text()


df2 <- df[c(3,6,12,13)]
colnames(df2) <- c('영화명','관객수','복합장르','대표장르')

df3 <- df[c(3,6,14,15,16,17)]
df3[5] <- df3[3]
df3[6] <- df3[4]
df3
df3[c(3,4,5,6)] <- sapply(df3[c(3,4,5,6)], as.numeric)
str(df3)

df3[5] <- df3[5] - 0.25
df3[6] <- df3[6] + 0.25
df3


## 다중공선성 검증
install.packages("car")
library("car")

vif <- vif(model)

vif[vif>10]

par(mfrow=c(2,2))
plot(model)

par(mfrow=c(1,1))
qqPlot(model,id.method="identify", simulate = TRUE, main = "Q-Q plot")


durbinWatsonTest(model)



