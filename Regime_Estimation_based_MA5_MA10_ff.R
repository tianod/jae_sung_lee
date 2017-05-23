##############################################################################################
# Regime_Estimation_based_MA5_&_MA10
#  - 주가자료에 Regime Switching 기법을 적용하여 국면을 판단함 
#  - 종가의 log 값을 사용
#  - kospi200의 데이터 구간은 1년을 기준으로 함
#
# 작성자 : 이재성
# 작성일 : 2017.05.23
##############################################################################################
  
  library(MSwM)
  library(xts)
  library(quantmod)


#regime <- function(){ 
 
# item_cd = readline(prompt="6자리 종목코드를 입력하세요(예시 : 000660) : ")


## 6자리 종목 코드를 입력 ##

item_cd = "000660" 

# 종료일 : 오늘날짜
end.date <- format(Sys.Date(), "%Y-%m-%d")
# 시작일 : 1년전 날짜
start.date <-  format(Sys.Date() - 365, "%Y-%m-%d")

# 종목의 종가 수집
item_paste <- paste0('KRX:',item_cd)
DataG <- getSymbols(item_paste,src="google",auto.assign=FALSE, from = start.date, to = end.date)
  

##### 종가 기준의 국면 추정#######
 
  train.item <- data.frame(I = 1:nrow(DataG), CLOSE_PRICE=DataG[,4])
  colnames(train.item) <- c("I","CLOSE_PRICE")

  # 로그 종가를 활용하여 국면 추정
  train.item$lnYt   <- log(train.item$CLOSE_PRICE)
  mod01 = lm(lnYt ~ 1, data=train.item)
  mod01.result.msm = msmFit(object=mod01, k=2, p=1, sw=c(T,T,T))

  mod01.regimeInfo <- ifelse(mod01.result.msm["Fit"]["filtProb"][,1] >= mod01.result.msm["Fit"]["filtProb"][,2], 1, 0)

  train.item$REGIME <- c(NA,mod01.regimeInfo)
  plot.df <- train.item[-1,]
  plot.df$REGIME2 = 1

  for (i in 2:nrow(plot.df)){
      if (plot.df$REGIME[i]==plot.df$REGIME[i-1]){plot.df$REGIME2[i] = plot.df$REGIME2[i-1]}
      else {plot.df$REGIME2[i] = plot.df$REGIME2[i-1]+1}
  }


  # 추정된 국면 도식화
  
  title_dt <- paste0(item_cd," 종가기준 국면 정보")
  #x11()
  plot(c(plot.df[1,"I"],plot.df[nrow(plot.df),"I"]), c(min(plot.df$CLOSE_PRICE),max(plot.df$CLOSE_PRICE)),type="n", xlab="TIME", 
       ylab="CLOSE_PRICE",main = title_dt)
  m = length(unique(plot.df$REGIME2))

  for(i in 1:m){
    sub <- subset(plot.df, REGIME2 == i)
    if (i %% 2 == 1){polygon(c(first(sub$I)-1,first(sub$I),last(sub$I),last(sub$I)),
                             c(min(plot.df$CLOSE_PRICE),max(plot.df$CLOSE_PRICE),max(plot.df$CLOSE_PRICE),min(plot.df$CLOSE_PRICE)), col="lightgray",border="lightgray")}
    else {polygon(c(first(sub$I)-1,first(sub$I),last(sub$I),last(sub$I)), 
                  c(min(plot.df$CLOSE_PRICE),max(plot.df$CLOSE_PRICE),max(plot.df$CLOSE_PRICE),min(plot.df$CLOSE_PRICE)), col="white",border="white")}
  }
  lines(plot.df$I, plot.df$CLOSE_PRICE,lwd=1.5)
  legend(plot.df[1,"I"], quantile(plot.df$CLOSE_PRICE,0.95),"종가", col = c("black"), text.col = "black", lty = 1, lwd=2, merge = TRUE, bg = "lightyellow")

  
##### MA5(종가) 기준 국면정보 #####

  train.item <- data.frame(I = 1:nrow(DataG), CLOSE_PRICE=DataG[,4])
  colnames(train.item) <- c("I","CLOSE_PRICE")

  # 종가의 5일 이동평균 및 로그변환
  train.item$MA5  <- round(SMA(train.item$CLOSE_PRICE,  5), 2)
  train.item$lnMA5  <- log(train.item$MA5)
  train.item2 <- train.item[-c(1:4),] 
  mod02 = lm(lnMA5 ~ 1, data=train.item2)
  mod02.result.msm = msmFit(object=mod02, k=2, p=1, sw=c(T,T,T))

  mod02.regimeInfo <- ifelse(mod02.result.msm["Fit"]["filtProb"][,1] >= mod02.result.msm["Fit"]["filtProb"][,2], 1, 0)

  train.item2$REGIME <- c(NA,mod02.regimeInfo)

  plot.df <- train.item2[-1,]
  plot.df$I <- 1:nrow(plot.df)
  
  plot.df$REGIME2 = 1
  
  for (i in 2:nrow(plot.df)){
    if (plot.df$REGIME[i]==plot.df$REGIME[i-1]){plot.df$REGIME2[i] = plot.df$REGIME2[i-1]}
    else {plot.df$REGIME2[i] = plot.df$REGIME2[i-1]+1}
  }

  
  title_dt <- paste0(item_cd," MA5(종가)기준 국면 정보")
  #x11()
  plot(c(plot.df[1,"I"],plot.df[nrow(plot.df),"I"]), c(min(plot.df$CLOSE_PRICE),max(plot.df$CLOSE_PRICE)),type="n", xlab="TIME", 
       ylab="CLOSE_PRICE",main = title_dt)
  m = length(unique(plot.df$REGIME2))
  
  for(i in 1:m){
    sub <- subset(plot.df, REGIME2 == i)
    if (i %% 2 == 1){polygon(c(first(sub$I)-1,first(sub$I),last(sub$I),last(sub$I)),
                             c(min(plot.df$CLOSE_PRICE),max(plot.df$CLOSE_PRICE),max(plot.df$CLOSE_PRICE),min(plot.df$CLOSE_PRICE)), col="lightgray",border="lightgray")}
    else {polygon(c(first(sub$I)-1,first(sub$I),last(sub$I),last(sub$I)), 
                  c(min(plot.df$CLOSE_PRICE),max(plot.df$CLOSE_PRICE),max(plot.df$CLOSE_PRICE),min(plot.df$CLOSE_PRICE)), col="white",border="white")}
  }
  lines(plot.df$I, plot.df$CLOSE_PRICE,lwd=1.5)
  lines(plot.df$I, plot.df$MA5, col="red",lwd=1.5)
 
  legend(plot.df[1,"I"], quantile(plot.df$CLOSE_PRICE,0.95),c("종가","MA5"), col = c("black","red"), text.col = "black", lty = 1, lwd=2, merge = TRUE, bg = "lightyellow")
  
#### MA10(종가) 기준 국면정보 #####
  
  train.item <- data.frame(I = 1:nrow(DataG), CLOSE_PRICE=DataG[,4])
  colnames(train.item) <- c("I","CLOSE_PRICE")
  
  # 종가의 10일 이동평균 및 로그변환
  train.item$MA10  <- round(SMA(train.item$CLOSE_PRICE,  10), 2)
  train.item$lnMA10  <- log(train.item$MA10)
  train.item2 <- train.item[-c(1:9),] 
  mod03 = lm(lnMA10 ~ 1, data=train.item2)
  mod03.result.msm = msmFit(object=mod03, k=2, p=1, sw=c(T,T,T))

  mod03.regimeInfo <- ifelse(mod03.result.msm["Fit"]["filtProb"][,1] >= mod03.result.msm["Fit"]["filtProb"][,2], 1, 0)

  train.item2$REGIME <- c(NA,mod03.regimeInfo)

  plot.df <- train.item2[-1,]
  plot.df$I <- 1:nrow(plot.df)

  plot.df$REGIME2 = 1
  
  for (i in 2:nrow(plot.df)){
    if (plot.df$REGIME[i]==plot.df$REGIME[i-1]){plot.df$REGIME2[i] = plot.df$REGIME2[i-1]}
    else {plot.df$REGIME2[i] = plot.df$REGIME2[i-1]+1}
  }
  
  
  
  title_dt <- paste0(item_cd," MA10(종가)기준 국면 정보")
 # x11()
  
  plot(c(plot.df[1,"I"],plot.df[nrow(plot.df),"I"]), c(min(plot.df$CLOSE_PRICE),max(plot.df$CLOSE_PRICE)),type="n", xlab="TIME", 
       ylab="CLOSE_PRICE",main = title_dt)
  m = length(unique(plot.df$REGIME2))
  
  for(i in 1:m){
    sub <- subset(plot.df, REGIME2 == i)
    if (i %% 2 == 1){polygon(c(first(sub$I)-1,first(sub$I),last(sub$I),last(sub$I)),
                             c(min(plot.df$CLOSE_PRICE),max(plot.df$CLOSE_PRICE),max(plot.df$CLOSE_PRICE),min(plot.df$CLOSE_PRICE)), col="lightgray",border="lightgray")}
    else {polygon(c(first(sub$I)-1,first(sub$I),last(sub$I),last(sub$I)), 
                  c(min(plot.df$CLOSE_PRICE),max(plot.df$CLOSE_PRICE),max(plot.df$CLOSE_PRICE),min(plot.df$CLOSE_PRICE)), col="white",border="white")}
  }
  lines(plot.df$I, plot.df$CLOSE_PRICE,lwd=1.5)
  lines(plot.df$I, plot.df$MA10,col="red",lwd=1.5)

  legend(plot.df[1,"I"], quantile(plot.df$CLOSE_PRICE,0.95),c("종가","MA10"), col = c("black","red"), text.col = "black", lty = 1, lwd=2, merge = TRUE, bg = "lightyellow")
  
  #}
  

  

