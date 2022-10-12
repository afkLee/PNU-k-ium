#라이브러리 다운로드
install.packages("tm")
#라이브러리 로딩 
library(stringr)
library("tm")

# 파일읽기 
data<-read.csv("./TrainSet.csv" , header= T, encoding = "UTF-8")


#head(data)
#View(data)
#summary(data)

#class의 이름을 보여준다.
colnames(data)


#뇌졸증 유무에 따라 데이터 스플릿
splitdata <- split(data, data[,"AcuteInfarction"])

#띄어쓰기 기준으로 문자열 분해
strsplit(splitdata, " ")


#뇌졸증 유무 글자 길이 
nchar(splitdata["0"])
nchar(splitdata["1"])


#문장 자르기(띄어쓰기 기준)
splitData1 <- str_split(splitdata["1"], " ")
splitData0 <- str_split(splitdata["0"], " ")

splitData0
splitData1


#스플릿한 데이터 에 각 문자 갯수 알려줌 
table(splitData0)
table(splitData1)

nchar(splitData0)

splitData0 <- sub('\n"[ADDENDUM]\\nClinical', "", splitData0)

table(splitData0)
nchar(splitData0)


  