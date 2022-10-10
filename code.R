#라이브러리 다운로드

#라이브러리 로딩 


# 파일읽기 
data<-read.csv("./TrainSet.csv" , header= T, encoding = "UTF-8")


head(data)


View(data)


summary(data)

#class의 이름을 보여준다.
colnames(data)


#class별로 데이터들 저장
Findingsdata <- Corpus(VectorSource(data[,"Findings"]))
Conclusiondata <- Corpus(VectorSource(data[,"Conclusion."]))
AcuteInfarctiondata <- Corpus(VectorSource(data[,"AcuteInfarction"]))




