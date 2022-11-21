#라이브러리 다운로드
install.packages("tm")
install.packages("wordcloud")
install.packages("RColorBrewer")

#라이브러리 로딩 
library(stringr)
library(NLP)
library("tm")
library(plyr)
library(wordcloud)
library(RColorBrewer)


#  Valid파일
validData <-read.csv("./ValidationSet.csv" , header= T, encoding = "UTF-8")

# 텍스트마이닝 파일 
data <-read.csv("./TrainSet.csv" , header= T, encoding = "UTF-8")







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


#단어 자르기 function
splitText = function(Texts){
  
  
    
  Texts <- unlist(Texts) 
  Texts <-   str_split(Texts,"\n")
  Texts <- unlist(Texts) 
  Texts <-   str_split(Texts,",")
    Texts <- unlist(Texts) 
    Texts <-   str_split(Texts,"at")
    Texts <- unlist(Texts) 
    Texts <-   str_split(Texts,"axial T2WI")
    Texts <- unlist(Texts)
    Texts <-   str_split(Texts,"axial FLAIR")
    Texts <- unlist(Texts) 
    Texts <-   str_split(Texts,"sagittal T1WI")
    Texts <- unlist(Texts) 
    Texts <-   str_split(Texts,"Axial T1WI")
    Texts <- unlist(Texts) 
    Texts <-   str_split(Texts,"axial T2* GRE image")
    Texts <- unlist(Texts) 
    Texts <-   str_split(Texts,"Clinical inform")
    Texts <- unlist(Texts) 
    Texts <-   str_split(Texts,"axial DWI")
    Texts <- unlist(Texts) 
    Texts <-   str_split(Texts,"hy.")
    Texts <- unlist(Texts) 
    Texts <-   str_split(Texts,"2. Microangiop")
    Texts <- unlist(Texts) 
    Texts <-   str_split(Texts,"3. Microangiop")
    Texts <- unlist(Texts) 
    Texts <-   str_split(Texts,"rophy.")
    Texts <- unlist(Texts) 
    Texts <-   str_split(Texts,"the bil")
    Texts <- unlist(Texts)  
    Texts <-   str_split(Texts,"bil")
    Texts <- unlist(Texts)  
    
    
    
    words = unlist(Texts)                    # unlist() : list를 vector 객체로 구조변경


  return (words);
}


#단어 자르기 
splitData0_Findings <- splitText(splitData0_Findings)  
splitData1_Findings <-splitText(splitData1_Findings) 
splitData0_Conclustion <-splitText(splitData0_Conclustion) 
splitData1_Conclustion <-splitText(splitData1_Conclustion) 


splitData0_Findings <- unique(splitData0_Findings)
splitData1_Findings <- unique(splitData1_Findings)
splitData0_Conclustion <- unique(splitData0_Conclustion)
splitData1_Conclustion <- unique(splitData1_Conclustion)




Findingspositives1 = list()
Findingsnegatives1 = list()
Findingspositives2 = list()
Findingsnegatives2 = list()
Conclustionpositives1 = list()
Conclustionnegatives1 = list()
Conclustionpositives2 = list()
Conclustionnegatives2 = list()

#진단 
sentimental = function(sentences, positive, negative){
  
  scores = laply(sentences, function(sentence, positive, negative) {
    
    sentence <- splitText(sentence)
   

    
  
    words = unlist(sentence)                    # unlist() : list를 vector 객체로 구조변경
    
    pos.matches = match(words, positive)           # words의 단어를 positive에서 matching
    neg.matches = match(words, negative)
    
    pos.matches = !is.na(pos.matches)            # NA 제거, 위치(숫자)만 추출
    neg.matches = !is.na(neg.matches)
    
    
    # 긍정 - 부정   


    score = sum(pos.matches) - sum(neg.matches)
    
    score.df = data.frame(score=score, positive =  sum(pos.matches),negative = sum(neg.matches))
    
    return (score.df)
  }, positive, negative)
  
  scores.df = data.frame(score=scores$score, text=sentences,positive = scores$positive , negative = scores$negative)
  return(scores.df)
}



resultFindingsScore = list()
resultBool = list()
resultConclusionScore = list()





for(i in 1:2653){
  result=sentimental(validData$Findings[i], splitData1_Findings, splitData0_Findings)
  results=sentimental(validData$Findings[i], splitData1_Conclustion, splitData0_Conclustion)
  
  Findingspositives1[i] <- result$positive
  Findingsnegatives1[i] <- result$negative
  Findingspositives2[i] <- results$positive
  Findingsnegatives2[i] <- results$negative
  
  
  if(result$score > 0 || results$score > 0){
    resultFindingsScore[i] <- T
    if(validData$AcuteInfarction[i] == 0){
      resultBool[i] <- F
    }else{
      resultBool[i] <- T
    }
  }else{
    resultFindingsScore[i] <- F
    if(validData$AcuteInfarction[i] == 0){
      resultBool[i] <- F
    }else{
      resultBool[i] <- T
    }   
  }
}

for(i in 1:2653){
  result=sentimental(validData$Conclusion.[i], splitData1_Conclustion, splitData0_Conclustion)
  results=sentimental(validData$Conclusion.[i], splitData1_Findings, splitData0_Findings)
  
  
  Conclustionpositives1[i] <- result$positive
  Conclustionnegatives1[i] <- result$negative
  Conclustionpositives2[i] <- results$positive
  Conclustionnegatives2[i] <- results$negative
  if(result$score >= 0 || results$score >= 0){
    resultConclusionScore[i] <- T
  }else{
    resultConclusionScore[i] <- F
    
  }
}

allPositives = list()
allNegatives = list()



for(i in 1:2653){
  allPositives[i] <- Findingspositives1[[i]]+Findingspositives2[[i]]+Conclustionpositives1[[i]]+Conclustionpositives2[[i]]
  allNegatives[i] <-Findingsnegatives1[[i]]+Findingsnegatives2[[i]]+Conclustionnegatives1[[i]]+Conclustionnegatives2[[i]]
}




resultScore = list()
for(i in 1:2653){
  A <- allPositives[[i]] + allNegatives[[i]]
  
  cat(allPositives[[i]]/A , " " , allNegatives[[i]]/A, "\n")

  
  
  if (( allPositives[[i]] > allNegatives[[i]]  )){
    resultScore[i] <- T
    
  }

  else{
    resultScore[i] <- F
  }
  
}

resultScore <- unlist(resultScore)
resultBool <- unlist(resultBool)
accuracy = 0




for(i in 1:2653){
  if(resultScore[i] == resultBool[i]){
    accuracy = accuracy+1
  }

  
}



#정확도

print((accuracy/2653))



result<-head(sort(table(resultScore), decreasing = T),2)
result



splitData0_Findings_top100 <-head(sort(table(splitData0_Findings), decreasing = T),10)
splitData1_Findings_top100 <-head(sort(table(splitData1_Findings), decreasing = T),10)
splitData0_Conclustion_top100 <-head(sort(table(splitData0_Conclustion), decreasing = T),10)
splitData1_Conclustion_top100 <-head(sort(table(splitData1_Conclustion), decreasing = T),10)

