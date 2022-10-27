#라이브러리 다운로드
install.packages("tm")
install.packages("wordcloud")
install.packages("RColorBrewer")

#라이브러리 로딩 
library(stringr)
library(RColorBrewer)
library(NLP)
library("tm")
library(wordcloud)


# 파일읽기 
data<-read.csv("./TrainSet.csv" , header= T, encoding = "UTF-8")


#head(data)
#View(data)
#summary(data)

#class의 이름을 보여준다.
colnames(data) 



#뇌졸증 유무에 따라 데이터 스플릿
splitdata <- split(data, data$AcuteInfarction)


#뇌졸증 유무 글자 길이 
nchar(splitdata["0"])
nchar(splitdata["1"])




#문장 자르기(띄어쓰기 기준)



splitData1 <- split(splitdata$`1`, " ")
splitData0 <- split(splitdata$`0`, " ")




splitData0_Findings<- as.data.frame(splitData0$` `$Findings, stringsAsFactors = F)
#row 갯수 
splitData0_Findings%>%nrow


#결측치 
sum(is.na(splitData0$` `$Findings))



splitData0_Findings <- unique(splitData0_Findings)
#row 갯수 
splitData0_Findings%>%nrow()




splitData0_Findings <- unlist(splitData0_Findings)

splitData0_Findings <-str_split(splitData0_Findings," ")
splitData0_Findings <- unlist(splitData0_Findings)

splitData0_Findings <- as.data.frame(splitData0_Findings)




#스플릿 한 테이브 상위 100개로 나눠서 정렬후 출력 

splitData0_Findings_top100 <- head(sort(table(splitData0_Findings), decreasing = T),100)


splitData0_Findings_top100







#문장 빈도별 단어구름 
wordcloud(names(splitData0_Findings_top100), splitData0_Findings_top100 , family="AppleGothic")



  
  