#라이브러리 다운로드
install.packages("tm")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("dplyr")
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


#데이터 전처리 전 시각화


splitData0_Findings_top100 <-head(sort(table(splitData0_Findings), decreasing = T),10)
splitData1_Findings_top100 <-head(sort(table(splitData1_Findings), decreasing = T),10)
splitData0_Conclustion_top100 <-head(sort(table(splitData0_Conclustion), decreasing = T),10)
splitData1_Conclustion_top100 <-head(sort(table(splitData1_Conclustion), decreasing = T),10)

splitData0_Findings_top100

splitData1_Findings_top100

splitData0_Conclustion_top100

splitData1_Conclustion_top100








splitData0_Findings <- unlist(splitData0_Findings)
splitData1_Findings <- unlist(splitData1_Findings)
splitData0_Conclustion <- unlist(splitData0_Conclustion)
splitData1_Conclustion <- unlist(splitData1_Conclustion)


#띄어쓰기나 이런거는 처리해야 단어로 나옴. 그래서 의미없음 무시 해도됨 
병xFindings_전처리전데이터 <-   str_split(splitData0_Findings," ")
병xFindings_전처리전데이터 <- unlist(병xFindings_전처리전데이터)  
병xFindings_전처리전데이터 <-   str_split(병xFindings_전처리전데이터,"\n")
병xFindings_전처리전데이터 <- unlist(병xFindings_전처리전데이터)  
병xFindings_전처리전데이터 <- table(병xFindings_전처리전데이터)






#데이터 전처리 전 단어 구름
wordcloud(words=names(병xFindings_전처리전데이터), #단어
          freq=병xFindings_전처리전데이터, #빈도
          min.freq=10, #등장 단어 최소빈도
          max.words=200, #최대 등장 단어 수 
          random.order=FALSE, #순서 임의화 여부
          rot.per=0.1, #90도 회전한 단어 비율
          scale=c(8,0.5), #최대, 최소 단어 스케일
          colors=brewer.pal(8,"Dark2"),     family="AppleGothic") #색 설정



#띄어쓰기나 이런거는 처리해야 단어로 나옴. 그래서 의미없음 무시 해도됨 
병oFindings_전처리전데이터 <-   str_split(splitData1_Findings," ")
병oFindings_전처리전데이터 <- unlist(병oFindings_전처리전데이터)  
병oFindings_전처리전데이터 <-   str_split(병oFindings_전처리전데이터,"\n")
병oFindings_전처리전데이터 <- unlist(병oFindings_전처리전데이터)  
병oFindings_전처리전데이터 <- table(병oFindings_전처리전데이터)



#데이터 전처리 전 단어 구름
wordcloud(words=names(병xFindings_전처리전데이터), #단어
          freq=병xFindings_전처리전데이터, #빈도
          min.freq=10, #등장 단어 최소빈도
          max.words=200, #최대 등장 단어 수 
          random.order=FALSE, #순서 임의화 여부
          rot.per=0.1, #90도 회전한 단어 비율
          scale=c(8,0.5), #최대, 최소 단어 스케일
          colors=brewer.pal(8,"Dark2"),     family="AppleGothic") #색 설정


#띄어쓰기나 이런거는 처리해야 단어로 나옴. 그래서 의미없음 무시 해도됨 
병xConclustion_전처리전데이터 <-   str_split(splitData0_Conclustion," ")
병xConclustion_전처리전데이터 <- unlist(병xConclustion_전처리전데이터)  
병xConclustion_전처리전데이터 <-   str_split(병xConclustion_전처리전데이터,"\n")
병xConclustion_전처리전데이터 <- unlist(병xConclustion_전처리전데이터)  
병xConclustion_전처리전데이터 <- table(병xConclustion_전처리전데이터)

#데이터 전처리 전 단어 구름
wordcloud(words=names(병xFindings_전처리전데이터), #단어
          freq=병xFindings_전처리전데이터, #빈도
          min.freq=10, #등장 단어 최소빈도
          max.words=200, #최대 등장 단어 수 
          random.order=FALSE, #순서 임의화 여부
          rot.per=0.1, #90도 회전한 단어 비율
          scale=c(8,0.5), #최대, 최소 단어 스케일
          colors=brewer.pal(8,"Dark2"),     family="AppleGothic") #색 설정

#띄어쓰기나 이런거는 처리해야 단어로 나옴. 그래서 의미없음 무시 해도됨 
병oConclustion_전처리전데이터 <-   str_split(splitData1_Conclustion," ")
병oConclustion_전처리전데이터 <- unlist(병oConclustion_전처리전데이터)  
병oConclustion_전처리전데이터 <-   str_split(병oConclustion_전처리전데이터,"\n")
병oConclustion_전처리전데이터 <- unlist(병oConclustion_전처리전데이터)  
병oConclustion_전처리전데이터 <- table(병oConclustion_전처리전데이터)

#데이터 전처리 전 단어 구름
wordcloud(words=names(병xFindings_전처리전데이터), #단어
          freq=병xFindings_전처리전데이터, #빈도
          min.freq=10, #등장 단어 최소빈도
          max.words=200, #최대 등장 단어 수 
          random.order=FALSE, #순서 임의화 여부
          rot.per=0.1, #90도 회전한 단어 비율
          scale=c(8,0.5), #최대, 최소 단어 스케일
          colors=brewer.pal(8,"Dark2"),     family="AppleGothic") #색 설정



#단어 자르기 function
splitText = function(Texts){
  

  Texts <- unlist(Texts) 
  Texts <-   str_split(Texts,":")
  Texts <- unlist(Texts) 
  Texts <-   str_split(Texts,"\n")
  Texts <- unlist(Texts) 
  Texts <-   str_split(Texts,",")
    Texts <- unlist(Texts) 
    Texts <-   str_split(Texts,"at")   
    Texts <- unlist(Texts) 
    Texts <-   str_split(Texts,"axial")
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
    Texts <-   str_split(Texts,"T1WI")
    Texts <- unlist(Texts)
    Texts <-   str_split(Texts,"T2WI")
    Texts <- unlist(Texts)
    Texts <-   str_split(Texts,"of")
    Texts <- unlist(Texts)    
    Texts <-   str_split(Texts,"획득하였으며")
    Texts <- unlist(Texts)   
    Texts <-   str_split(Texts,"the")
    Texts <- unlist(Texts)  
    Texts <-   str_split(Texts,"left")
    Texts <- unlist(Texts)  
    Texts <-   str_split(Texts,"right")
    Texts <- unlist(Texts)   
    Texts <-   str_split(Texts,"1.")
    Texts <- unlist(Texts)   
    Texts <-   str_split(Texts,"2.")
    Texts <- unlist(Texts)  
    Texts <-   str_split(Texts,"in")
    Texts <- unlist(Texts)
    Texts <-   str_trim(Texts)
    words = unlist(Texts)                    # unlist() : list를 vector 객체로 구조변경

    

  return (words);
}


#단어 자르기 
splitData0_Findings <- splitText(splitData0_Findings)  
splitData1_Findings <-splitText(splitData1_Findings) 
splitData0_Conclustion <-splitText(splitData0_Conclustion) 
splitData1_Conclustion <-splitText(splitData1_Conclustion) 



#전처리후 단어구름 

#병 없는경우 Findings
wordcloud(words=names(table(splitData0_Findings)), #단어
          freq=table(splitData0_Findings), #빈도
          min.freq=10, #등장 단어 최소빈도
          max.words=200, #최대 등장 단어 수 
          random.order=FALSE, #순서 임의화 여부
          rot.per=0.1, #90도 회전한 단어 비율
          scale=c(30,0.5), #최대, 최소 단어 스케일
          colors=brewer.pal(8,"Dark2"),  
          family="AppleGothic") #색 설정

#병 있을경우 Findings
wordcloud(words=names(table(splitData1_Findings)), #단어
          freq=table(splitData1_Findings), #빈도
          min.freq=3, #등장 단어 최소빈도
          max.words=200, #최대 등장 단어 수 
          random.order=FALSE, #순서 임의화 여부
          rot.per=0.1, #90도 회전한 단어 비율
          scale=c(30,0.5), #최대, 최소 단어 스케일
          colors=brewer.pal(8,"Dark2"),     family="AppleGothic") #색 설정


#병 없을경우 Conclustion
wordcloud(words=names(table(splitData0_Conclustion)), #단어
          freq=table(splitData0_Conclustion), #빈도
          
          min.freq=20, #등장 단어 최소빈도
          max.words=200, #최대 등장 단어 수 
          random.order=FALSE, #순서 임의화 여부
          rot.per=0.1, #90도 회전한 단어 비율
          scale=c(15,0.5), #최대, 최소 단어 스케일
          colors=brewer.pal(8,"Dark2"),     family="AppleGothic") #색 설정



#병 있을경우 Conclustion
wordcloud(words=names(table(splitData1_Conclustion)), #단어
          freq=table(splitData1_Conclustion), #빈도
          min.freq=10, #등장 단어 최소빈도
          max.words=200, #최대 등장 단어 수 
          random.order=FALSE, #순서 임의화 여부
          rot.per=0.1, #90도 회전한 단어 비율
          scale=c(15,0.5), #최대, 최소 단어 스케일
          colors=brewer.pal(8,"Dark2"),     family="AppleGothic") #색 설정


head(sort(table(splitData0_Conclustion), decreasing = T),100)

head(sort(table(splitData1_Conclustion), decreasing = T),100)

head(sort(table(splitData0_Findings), decreasing = T),100)

head(sort(table(splitData1_Findings), decreasing = T),100)



splitData0_Findings <- unique(splitData0_Findings)
splitData1_Findings <- unique(splitData1_Findings)
splitData0_Conclustion <- unique(splitData0_Conclustion)
splitData1_Conclustion <- unique(splitData1_Conclustion)


Findingspositives1 = list()
Findingsnegatives1 = list()
Conclustionpositives1 = list()
Conclustionnegatives1 = list()


#Findings 진단 
sentimental = function(sentences, positive, negative){
  
  scores = laply(sentences, function(sentence, positive, negative) {
    
    sentence <- splitText(sentence)
   

    
  
    words = unlist(sentence)                    # unlist() : list를 vector 객체로 구조변경
    
    
    pos =0
    neg =0
    for(i in words){
     a = str_trim(i)
     if(a== 'No significant change' || a == 'No evidence' || a == 'Normal bra' ){
       pos = 0
       neg = 100000
       break;
     }
if(a == 'Cerebral Infarction' ||a=='cerebral Infarction'  ||  a == '구음장애' || a== 'dysarthria' || a == 'Focal severe stenosis' || a =='tack(TIA)' || a == 'side weakness'|| a == 'MCA territory.' || a == 'Encephalomalacia'){
     
        pos = 100000
        neg = 0
        print(i)
        break;
      }

    }
    
    pos.matches = match(words, positive)           # words의 단어를 positive에서 matching
    neg.matches = match(words, negative)
    
    pos.matches = !is.na(pos.matches)            # NA 제거, 위치(숫자)만 추출
    neg.matches = !is.na(neg.matches)
    
    
    # 긍정 - 부정   

    score = sum(pos.matches) - sum(neg.matches)
    
    score.df = data.frame(score=score, positive =  pos,negative = neg)
    
    return (score.df)
  }, positive, negative)
  
  scores.df = data.frame(score=scores$score, text=sentences,positive = scores$positive , negative = scores$negative)
  return(scores.df)
}

#Conclustion 진단 
Consentimental = function(sentences, positive, negative){
  
  scores = laply(sentences, function(sentence, positive, negative) {
    
    sentence <- splitText(sentence)
    
    
    
    
    words = unlist(sentence)                    # unlist() : list를 vector 객체로 구조변경
    
    
    pos =0
    neg =0
    
    for(i in words){
      a = str_trim(i)
      
      if( a== '시신경로  등에도 이상소견은 보이지 않는다.' || a== '백질-회색질의 구분도 잘 되고 있다.'){
        pos = 0
        neg = 100000
        break;
      }
      
      
      if(a == 'Cerebral Infarction' ||a=='cerebral Infarction'  ||  a == '구음장애' || a== 'dysarthria' || a == 'Focal severe stenosis' || a =='tack(TIA)' || a == 'side weakness'|| a == 'MCA territory.' || a == 'Encephalomalacia'){
        pos = 100000
        neg = 0
        print(a)
        break;
      }
      
      
    }
    

    
    pos.matches = match(words, positive)           # words의 단어를 positive에서 matching
    neg.matches = match(words, negative)
    
    pos.matches = !is.na(pos.matches)            # NA 제거, 위치(숫자)만 추출
    neg.matches = !is.na(neg.matches)
    
    
    # 긍정 - 부정   
    
    score = sum(pos.matches) - sum(neg.matches)
    
    score.df = data.frame(score=score, positive =  pos,negative = neg)
    
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

  Findingspositives1[i] <- result$positive
  Findingsnegatives1[i] <- result$negative

  
  
  if(result$score > 0){
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
  result=Consentimental(validData$Conclusion.[i], splitData1_Conclustion, splitData0_Conclustion)

  
  Conclustionpositives1[i] <- result$positive
  Conclustionnegatives1[i] <- result$negative

  if(result$score >= 1){
    resultConclusionScore[i] <- T
  }else{
    resultConclusionScore[i] <- F
    
  }
}

allPositives = list()
allNegatives = list()



for(i in 1:2653){
  allPositives[i] <- Findingspositives1[[i]]+Conclustionpositives1[[i]]
  allNegatives[i] <-Findingsnegatives1[[i]]+Conclustionnegatives1[[i]]
}




resultScore = list()
for(i in 1:2653){

  

  
  
  if (( allPositives[[i]] > 1   )){
    resultScore[i] <- T
    
  }
  else{
    if(resultConclusionScore[[i]]){
      resultScore[i] <- T
    }else{
    resultScore[i] <- F}
    
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

