#라이브러리 다운로드
install.packages("tm")


#라이브러리 로딩 
library(stringr)
library(NLP)
library("tm")
library(plyr)

# 파일읽기 
data<-read.csv("./TrainSet.csv" , header= T, encoding = "UTF-8")






#뇌졸증 유무에 따라 데이터 스플릿
splitdata <- split(data, data$AcuteInfarction)









#문장 자르기(띄어쓰기 기준)



splitData1 <- split(splitdata$`1`, " ")
splitData0 <- split(splitdata$`0`, " ")

splitData0_Findings<- as.data.frame(splitData0$` `$Findings, stringsAsFactors = F)
splitData1_Findings<- as.data.frame(splitData1$` `$Findings, stringsAsFactors = F)
splitData0_Conclustion<- as.data.frame(splitData0$` `$Conclusion., stringsAsFactors = F)
splitData1_Conclustion<- as.data.frame(splitData1$` `$Conclusion., stringsAsFactors = F)





splitData0_Findings <- unique(splitData0_Findings)
splitData1_Findings <- unique(splitData1_Findings)
splitData0_Conclustion <- unique(splitData0_Conclustion)
splitData1_Conclustion <- unique(splitData1_Conclustion)


splitData0_Findings <- unlist(splitData0_Findings)
splitData1_Findings <- unlist(splitData1_Findings)
splitData0_Conclustion <- unlist(splitData0_Conclustion)
splitData1_Conclustion <- unlist(splitData1_Conclustion)


#단어 자르기 
splitData0_Findings <-str_split(splitData0_Findings,",")
splitData1_Findings <-str_split(splitData1_Findings,",")
splitData0_Conclustion <-str_split(splitData0_Conclustion,",")
splitData1_Conclustion <-str_split(splitData1_Conclustion,",")

splitData0_Findings <- unlist(splitData0_Findings)
splitData1_Findings <- unlist(splitData1_Findings)
splitData0_Conclustion <- unlist(splitData0_Conclustion)
splitData1_Conclustion <- unlist(splitData1_Conclustion)







#진단 
sentimental = function(sentences, positive, negative){
  
  scores = laply(sentences, function(sentence, positive, negative) {
    
    sentence <- unlist(sentence) 
    sentence <-   str_split(sentence,",")

    
    words = unlist(sentence)                    # unlist() : list를 vector 객체로 구조변경
    
    pos.matches = match(words, positive)           # words의 단어를 positive에서 matching
    neg.matches = match(words, negative)
    
    pos.matches = !is.na(pos.matches)            # NA 제거, 위치(숫자)만 추출
    neg.matches = !is.na(neg.matches)
    
    score = sum(pos.matches) - sum(neg.matches)  # 긍정 - 부정   
    return(score)
  }, positive, negative)
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

resultFindingsScore = list()
resultBool = list()
resultConclusionScore = list()



for(i in 1:6190){
result=sentimental(data$Findings[i], splitData1_Findings, splitData0_Findings)
  if(result$score >=0){
    resultFindingsScore[i] <- T
    if(data$AcuteInfarction[i] == 0){
      resultBool[i] <- F
    }else{
      resultBool[i] <- T
    }
  }else{
    resultFindingsScore[i] <- F
    if(data$AcuteInfarction[i] == 0){
      resultBool[i] <- F
    }else{
      resultBool[i] <- T
    }   
  }
}

for(i in 1:6190){
  result=sentimental(data$Conclusion.[i], splitData1_Conclustion, splitData0_Conclustion)
  if(result$score >=0){
    resultConclusionScore[i] <- T
  }else{
    resultConclusionScore[i] <- F

  }
}


resultScore = list()
for(i in 1:6190){
  if(resultFindingsScore[i] == T && resultConclusionScore[i] ==T){
    resultScore[i] <- T
  }else{
    resultScore[i] <- F
  }
  
}

resultScore <- unlist(resultScore)
resultBool <- unlist(resultBool)
accuracy = 0

for(i in 1:6190){
  if(resultScore[i] == resultBool[i]){
    accuracy = accuracy+1
  }
  
}


#정확도
print((accuracy/ 6190)*100)

  
  