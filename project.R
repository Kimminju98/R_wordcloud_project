library(tidyverse)
library(tidytext)
library(glue)
library(stringr)

# 교보문고 폴더에 있는 모든 파일 가져오기
files<-list.files("교보파일")
files


# GetFile 함수
# 각 파일을 읽어드려서, 도서명만 text로 추출하기
GetFile <- function(file){
  fileName <- glue("./교보파일/",file,sep="")
  f<-read.csv(fileName)
  text <- as.character(f$도서명)
  text <- str_replace_all(text,"개정판"," ")
  text <- str_replace_all(text,"시리즈"," ")
  text <- str_replace_all(text,"CD"," ")
  text <- str_replace_all(text,"교과서"," ")
  text <- str_replace_all(text,"리커버"," ")
  return(text)
}

# 종합(2018년) / 기술(각 3년도별) / 자기계발(각 3년도별)의 도서명 데이터 합치기

Every <- data_frame();
Tech_2016 <- data_frame();
Tech_2017 <- data_frame();
Tech_2018 <- data_frame();
Self_2016 <- data_frame();
Self_2017 <- data_frame();
Self_2018 <- data_frame();

for(i in files){
  if(str_match(i,"...._......_(..)")[2] == "주간") 
    Every <- paste(Every,GetFile(i));
  
  if(str_match(i,"...._......_.._....._..._(..)")[2]=="기술" & 
     str_match(i,"...._......_.._(....)")[2]=="2016") 
    Tech_2016 <- paste(Tech_2016,GetFile(i))
  if(str_match(i,"...._......_.._....._..._(..)")[2]=="기술" & 
     str_match(i,"...._......_.._(....)")[2]=="2017")
    Tech_2017 <- paste(Tech_2017,GetFile(i))
  if(str_match(i,"...._......_.._....._..._(..)")[2]=="기술" & 
     str_match(i,"...._......_.._(....)")[2]=="2018")
    Tech_2018 <- paste(Tech_2018,GetFile(i))
  
  if(str_match(i,"...._......_.._....._..._(....)")[2]=="자기계발" & 
     str_match(i,"...._......_.._(....)")[2]=="2016")
    Self_2016 <- paste(Self_2016,GetFile(i))
  if(str_match(i,"...._......_.._....._..._(....)")[2]=="자기계발" & 
     str_match(i,"...._......_.._(....)")[2]=="2017")
    Self_2017<- paste(Self_2017,GetFile(i))
  if(str_match(i,"...._......_.._....._..._(....)")[2]=="자기계발" & 
     str_match(i,"...._......_.._(....)")[2]=="2018")
    Self_2018 <- paste(Self_2018,GetFile(i))
}

# 데이터 가공 함수
# 1. 필요없는 단어 삭제 (특수문자, 숫자)
# 2. 단어별 개수
# 3. 단어의 개수가 150보다 작은 것만 추출

GetData <- function(txt){
  txt <- str_replace_all(txt,"\\W"," ")
  txt <- str_replace_all(txt,"\\d"," ")
  txt <- str_replace_all(txt,"Do"," ")
  txt <- str_replace_all(txt,"it"," ")
  txt <- str_replace_all(txt,"HardCover"," ")
  txt <- str_replace_all(txt,"한정판"," ")
  txt <- str_replace_all(txt,"CC"," ")
  txt <- str_replace_all(txt,"양장"," ")
  
  nouns <- extractNoun(txt)
  nouns <- unlist(nouns)
  nouns <- Filter(function(x){nchar(x)>=2},nouns)
  wordcount <- table(unlist(nouns))
  wordcount
  df_word <- as.data.frame(wordcount,stringsAsFactors=F)
  df_word <- rename(df_word,
                    word=Var1,
                    freq=Freq)
  df_word <- df_word %>% filter(freq <= 200)
  return(df_word)
}

# 알아보는 방법!
top30 <- GetData(Every) %>%
  arrange(desc(freq)) %>%
  head(30)
top30

#  wordclould 생성 함수
library(rJava)
library(memoise)
library(KoNLP)
library(wordcloud)
library(RColorBrewer)

GetWordCloud <- function(df_word){
  pal <- brewer.pal(8,"Dark2")
  wordcloud(words = df_word$word,
            freq = df_word$freq,
            random.order = F,
            rot.per = 0.1,
            scale=c(6,0.2),
            colors = pal)
}


# 단어별 빈도수
ShowFreWord <- function(df_word){
  top20 <- df_word %>%
    arrange(desc(freq)) %>%
    head(20)
  order <- arrange(top20,freq)$word
  ggplot(data=top20, aes(x=factor(word),y=freq)) +
    geom_col(aes(fill=-freq)) +
    coord_flip() +
    scale_x_discrete(limit=order) +
    geom_text(aes(label=freq),hjust=-0.3) +
    th
}

# wordcloud2 생성!
require(devtools)
#install_github("lchiffon/wordcloud2")
library(wordcloud2)
GetWordCloud2 <- function(df_word){
  wordcloud2(data=df_word, color="random-light", shape='circle',
             backgroundColor = 'gray',rotateRatio = 0.75)
}
GetWordCloud2(GetData(Self_2018))


ShowFreWord(GetData(Tech_2016))
ShowFreWord(GetData(Tech_2017))
ShowFreWord(GetData(Tech_2018))

GetWordCloud(GetData(Every))
GetWordCloud(GetData(Tech_2016))
GetWordCloud(GetData(Tech_2017))
GetWordCloud(GetData(Tech_2018))
GetWordCloud(GetData(Self_2016))
GetWordCloud(GetData(Self_2017))
GetWordCloud(GetData(Self_2018))

