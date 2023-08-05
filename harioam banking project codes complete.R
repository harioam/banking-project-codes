setwd("C:/Users/harioam datas/banking project")
getwd()
bank_fulltrain=read.csv("bank-full_train.csv",stringsAsFactors = F)
bank_fulltest=read.csv("bank-full_test.csv",stringsAsFactors = F)
View(bank_fulltrain)
View(bank_fulltest)
###so that we have loaded the data and we will move ahead with data cleaning process
 library(dplyr)
glimpse(bank_fulltrain)
 str(bank_fulltrain)
####part 1 quiz solutions
  mean(bank_fulltrain$age)
####
  Summary.data.frame(bank_fulltrain)
Summary.data.frame(bank_fulltrain$balance)  
quantile(bank_fulltrain$balance)
IQR(bank_fulltrain$balance,na.rm = T)
###
quantile(bank_fulltrain$balance p=0.25)-1.5(IQR(bank_fulltrain))
sum(bank_fulltrain$balance<1941)
sum(bank_fulltrain$balance>3427)
var(bank_fulltrain$balance)
########NOW WE WILL REMOVE THE VARIOUS CHARACTERS FOR VARS AS SEMICOLON. COMMAS, HIFEN,WE WILL USE GSUB

CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub("\\/","_",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}
glimpse(bank_fulltrain)
###now we will join boht the data for data cleaning all bank
bank_fulltest$y=NA
View(bank_fulltest)
bank_fulltest$Data="test"
bank_fulltrain$Data="train"
View(bank_fulltrain)
allbank=rbind(bank_fulltest,bank_fulltrain)
View(allbank)
###data cleaning
##creating dummeis for job
sum(is.na(allbank))
apply(allbank,2,function(allbank)sum(is.na(allbank)))
###all the na has been removed,, already moving on to creating dummies for job
sort(table(allbank$job))
allbank=mutate(allbank,
               jobbluecollar=as.numeric(job%in%("blue-collar")),
               jobhm=as.numeric(job%in%("housemaid")),
               jobunse=as.numeric(job%in%c("unknown","self-employed")),
               jobensead=as.numeric(job%in%c("entrepreneur","services","admin.")),
               jobteman=as.numeric(job%in%c("technician","management")),
               jobreun=as.numeric(job%in%c("retired","unemployment")),
)
               
  glimpse(allbank)
allbank$job=NULL 
###now we will move to next variable marital status
table(allbank$marital)
allbank=mutate(allbank,
               msmarry=as.numeric(marital%in%("married")),
               mssingle=as.numeric(marital%in%("single")),
  
) 
glimpse(allbank)
allbank$marital=NULL
glimpse(allbank)
#########for month
sort(table(allbank$month),decreasing = T)
allbank=mutate(allbank,
               month_may=as.numeric(month=="may"),
                                              month_jul_jun=as.numeric(month %in% c("jul","jun")),
                                              month_aug_jan_nov=as.numeric(month %in% c("aug","jan","nov")),
                                              month_feb_apr=as.numeric(month %in% c("feb","apr")),
                                              month_sep_dec_oct=as.numeric(month %in% c("dec","sep","oct"))) 
glimpse(allbank)
allbank$month=NULL
####now we will move to all cat variabales are cat varibales
table(allbank$default)
table(allbank$housing)
table(allbank$loan)
table(allbank$contact)
table(allbank$campaign)
allbank=mutate(allbank,
               defaulty=as.numeric(default=="no"),
               housingy=as.numeric(housing=="yes"),
               loanno=as.numeric(loan=="no"),
               concell=as.numeric(contact=="cellular"),
               contele=as.numeric(contact=="telephone"),
  
)
glimpse(allbank)
allbank$default=NULL
allbank$housing=NULL
allbank$loan=NULL
allbank$contact=NULL
glimpse(allbank)
table(allbank$education)
####creating dummy for education variable
allbank=mutate(allbank,
               edprimary=as.numeric(education=="primary"),
               edsecondary=as.numeric(education=="secondary"),
               edtertiary=as.numeric(education=="tertiary"),
               )
glimpse(allbank)
allbank$education=NULL
table(allbank$poutcome)
#######dummies for outcome
allbank=mutate(allbank,
               outfailure=as.numeric(poutcome=="failure"),
               outother=as.numeric(poutcome=="unknown"),
               outsuccess=as.numeric(poutcome=="success"),
  
)
glimpse(allbank)
 allbank$poutcome=NULL
 
 View(allbank)
allbank$ID=NULL
allbank$y=as.numeric(allbank$y=="yes")
glimpse(allbank)
###now all the data is cleaned so we will  diviide data into train and test,,,
banktrain=filter(allbank,Data=="train")
banktrain$Data=NULL
View(banktrain)
banktest=filter(allbank,Data=="test")
banktest$Data=NULL
View(banktest)
########since we have answers only for train and not train so,,do double check we will deviide train in 2 part train1 and train2
set.seed(2)
s=sample(1:nrow(banktrain),0.8*nrow(banktrain))
banktrain1=banktrain[s,]
banktrain2=banktrain[-s,]
View(banktrain1)
########3now the model and data is ready for the model and prediction,,
#####removing the model multicollinearity using vif
library(car)
fitlm1=lm(y~. ,data=banktrain1)
sort(vif(fitlm1),decreasing = T)
###now doing the iteration and removing the unwanted variable with vif more tha 5
fitlm2=lm(y~. -month_may,data=banktrain1)
sort(vif(fitlm2),decreasing = T)
########month of may was collinear so we will have to remove and after removing ll other collinearlity among variable reduced
fitlm3=lm(y~.-month_may-outother,data = banktrain1)
sort(vif(fitlm3),decreasing=T)
####removing outother and secondary ed and tertiary ed___
fitlm4=lm(y~.-month_may-outother-edsecondary,data = banktrain1)
sort(vif(fitlm4),decreasing = T)
#######
fitlm5=lm(y~.-month_may-outother-edsecondary-jobteman,data = banktrain1)
sort(vif(fitlm5),decreasing=T)
summary(fitlm4)
summary(fitlm5)
fitlm6=lm(y~.-month_may-outother-edsecondary-jobteman-mssingle,data = banktrain1)
sort(vif(fitlm6),decreasing = T)
####fitlm5 has the lowest vif reached we will not consider fitlm6
####now we will take out vif >>5
#####now we will fit on logisitic regression model
fitglm=glm(y~.-month_may-outother-edsecondary-jobteman,family = "binomial",data = banktrain1)
summary(fitglm)
#####using the fit function of step fit on the model
fitglm=step(fitglm)
###thid automatically removes varibales and with no imp and taking only original vaariable
fitglm1=glm(y~ balance + duration + campaign + previous + jobbluecollar + 
              jobhm + jobunse + jobreun + msmarry + mssingle + month_jul_jun + 
              month_aug_jan_nov + month_feb_apr + month_sep_dec_oct + housingy + 
              loanno + concell + contele + edprimary + edtertiary + outsuccess, family="binomial",data = banktrain1)
formula(fitglm1)
summary(fitglm1)
 ###after getting aic of 12670,,ND=18543, RD=12626,,,,
#####so predicting on bank train 2 which dataset our model hasnt seen check for eroor and all....performance,,,,
predbanktrain2=predict(fitglm1,newdata = banktrain2,type= "response")
View(predbanktrain2)
##we have got values
###now we will attach the col to train 2 data for checking
banktrain2$score=predbanktrain2
View(banktrain2[,c("score","y")])##both columns only y and pscore
temp1=banktrain2[,c("y","score")]###storing it in object temp
View(temp1)
###now we will take for cutoff value as 0.5 for converting 1 and 0
library(ggplot2)
cutoff=0.5
predicted=as.numeric(banktrain2$score>cutoff)
View(predicted)
TP=sum(predicted==1&banktrain2$y==1)
FP=sum(predicted==0&banktrain2$y==0)
TN=sum(predicted==0&banktrain2$y==0)
FN=sum(predicted==0&banktrain2$y==1)
#####also calculate real positives and real negetive on data
P=TP+FN
N=TN+FP
total=P+N
total
temp2=View(cbind.data.frame(temp1,predicted))
temp2=cbind.data.frame(temp1,predicted)
View(temp2)
table(temp2$y,temp2$predicted)
 
########SENSITIVITY IS TAKING A HIT SO WE WILL ITERATE FOR WHICH CUTOFF BEST SUITS THE MODEL...
cutoffdata=data.frame(cutoff=0,TP=0,FP=0,FN=0,TN=0)###cutoff data matrix to choose
View(cutoffdata)
cutoffs=round(seq(0,1,length=100),3)
cutoffs
#now we will make a loop to take various cutoffs and record values
for (cutoff in cutoffs) {
  predicted1=as.numeric(banktrain2$score>cutoff)
  TP=sum(predicted1==1&banktrain2$y==1)
  FP=sum(predicted1==1&banktrain2$y==0)
  TN=sum(predicted1==0&banktrain2$y==0)
  FN=sum(predicted1==0&banktrain2$y==1)
  cutoffdata=rbind(cutoffdata,c(cutoff,TP,FP,TN,FN))
  
}
cutoffdata=cutoffdata[-1,]
View(cutoffdata)

###now we will add add formulae for criterias to check

cutoffdata=mutate(cutoffdata,
                  P=TP+FN,
                  N=TN+FP,
                  
                  Sn=TP/P,
                  Sp=TN/N,
                  precision=TP/(TP+FP),
                  recall=Sn,
                  
                  KS=abs((TP/P)-(FP/N)),
                  F5=(26*precision*recall)/((25*precision)+recall),
                  F.1=(1.01*precision*recall)/((.01*precision)+recall),
                  
                  M=(4*FP+FN)/(5*(P+N)),
                  
                  
)
                  View(cutoffdata)

###so now we will evaluate the best ks score highest
kscutoff=cutoffdata$cutoff[which.max(cutoffdata$KS)][1]                  
kscutoff
###we got max cut off is 0.071 as we will use to predict on test data
# visualise how these measures move across cutoffs
library(ggplot2)
ggplot(cutoffdata,aes(x=cutoff,y=Sp))+geom_line()

library(tidyr)

cutoff_long=cutoffdata %>% 
  gather(Measure,Value,Sn:M)

ggplot(cutoff_long,aes(x=cutoff,y=Value,color=Measure))+geom_line()


#####now with cutoff we will predict on test data
View(banktest)
banktest$score=predict(fitglm1,newdata = banktest,type="response")
View(banktest)
###now we  will check for score ot cutoff
cutofftest=0.071

banktestpredicted=as.numeric(banktest$score>cutofftest)
write.csv(banktestpredicted,"Hariom_chaturvedi_P5_part2.csv",row.names = F)
View(banktest)
banktestpredicted
###test is predicted and values are saved in the file
