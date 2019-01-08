library(anytime)
library(plotly)
library(data.table)
library(forecast)
library(ggplot2)
library(textreadr)
library(readxl)
library(xts)
library("naniar")
library(glmnet)
library(caret)
library(rpart)
require(data.table)
require(TunePareto)
require(glmnet)
library(skellam)

#Adjusting the dates of training and train datasets

testStart=as.Date('2018-12-31')
trainStart=as.Date("2013-01-02")
rem_miss_threshold=0.01 

source('/Users/alifurkanyavuz/Downloads/data_preprocessing.r')
source('/Users/alifurkanyavuz/Downloads/feature_extraction.r')
source('/Users/alifurkanyavuz/Downloads/Proje dosyalari/Performance_Metrics.r')
source('/Users/alifurkanyavuz/Downloads/Proje dosyalari/Train_Models.r')

matches_data_path='/Users/alifurkanyavuz/Downloads/Proje dosyalari/matches.rds'
odd_details_data_path='/Users/alifurkanyavuz/Downloads/Proje dosyalari/odds.rds'

# read data
matches_raw=readRDS(matches_data_path)
odd_details_raw=readRDS(odd_details_data_path)

# preprocess matches
matches=matches_data_preprocessing(matches_raw)

# preprocess odd data
odd_details=details_data_preprocessing(odd_details_raw,matches)

# extract open and close odd type features from multiple bookmakers. To merge features and data table that we get from the external source, the names of teams are adjusted 

features=extract_features.openclose(matches,odd_details,pMissThreshold=rem_miss_threshold,trainStart,testStart)
features$Home[features$Home=="manchester united"] <- "man-united"
features$Home[features$Home=="manchester-utd"] <- "man-united"
features$Home[features$Home=="manchester-united"] <- "man-united"
features$Home[features$Home=="manchester united"] <- "man-united"
features$Home[features$Home=="hull city"] <- "hull-city"
features$Home[features$Home=="manchester united"] <- "man-united"
features$Home[features$Home=="west ham"] <- "west-ham"
features$Home[features$Home=="newcastle utd"] <- "newcastle"
features$Home[features$Home=="aston villa"] <- "aston-villa"
features$Home[features$Home=="west brom"] <- "west-brom"
features$Home[features$Home=="manchester city"] <- "man-city"
features$Home[features$Home=="manchester-city"] <- "man-city"
features$Home[features$Home=="crystal palace"] <- "crystal-palace"
features$Home[features$Home=="stoke city"] <- "stoke-city"

features$Away[features$Away=="manchester united"] <- "man-united"
features$Away[features$Away=="manchester-utd"] <- "man-united"
features$Away[features$Away=="manchester-united"] <- "man-united"
features$Away[features$Away=="manchester united"] <- "man-united"
features$Away[features$Away=="hull city"] <- "hull-city"
features$Away[features$Away=="manchester united"] <- "man-united"
features$Away[features$Away=="west ham"] <- "west-ham"
features$Away[features$Away=="newcastle utd"] <- "newcastle"
features$Away[features$Away=="aston villa"] <- "aston-villa"
features$Away[features$Away=="west brom"] <- "west-brom"
features$Away[features$Away=="manchester city"] <- "man-city"
features$Away[features$Away=="manchester-city"] <- "man-city"
features$Away[features$Away=="crystal palace"] <- "crystal-palace"
features$Away[features$Away=="stoke city"] <- "stoke-city"

features[,ID:=NA]

features$ID<- with(features, paste0(Match_Date, "/", Home))

## Additional Data comes from 
dt_18_19=as.data.table(read.csv(file="/Users/Users/burak/Desktop/football-data/E0.csv"))
dt_17_18=as.data.table(read.csv(file="/Users/Users/burak/Desktop/football-data/E0-2.csv"))
dt_16_17=as.data.table(read.csv(file="/Users/Users/burak/Desktop/football-data/E0-3.csv"))
dt_15_16=as.data.table(read.csv(file="/Users/Users/burak/Desktopfootball-data/E0-4.csv"))
dt_14_15=as.data.table(read.csv(file="/Users/Users/burak/Desktopfootball-data/E0-5.csv"))
dt_13_14=as.data.table(read.csv(file="/Users/Users/burak/Desktopfootball-data/E0-6.csv"))

DT=rbind(dt_13_14[,1:23],dt_14_15[,1:23],dt_15_16[,1:23],dt_16_17[,1:23],dt_17_18[,1:23],dt_18_19[,1:23])

DT=DT[,-1]
colnames(dt_13_14)[0:62]==colnames(dt_18_19)
colnames(dt_13_14)==colnames(dt_14_15)
colnames(dt_14_15)[1:65]==colnames(dt_15_16)


DT[,c("Day","Month","Year"):=(tstrsplit(Date,'/'))]
DT[,Day:=as.numeric(Day)]
DT[,Year:=as.numeric(Year)]
DT[,Month:=as.numeric(Month)]
DT[Year<2000,Year:=Year+2000]

DT$Date<-with(DT,paste0(Day, "-", Month, "-", Year))

DT[,Date:=as.Date(Date,format=, "%d-%m-%Y")]
DT[,c("Day","Month","Year"):=NULL]
DT[,c("HTHG","HTAG","HTR"):=NULL]
DT=DT[,1:11]
colnames(DT)

DT[,replace_with_na()]
?replace_with_na

testStart=as.Date("2018-12-31")
trainStart=as.Date("2013-01-02")
train_DT=DT[Date>=trainStart & Date<testStart]

test_DT=DT[Date>=testStart]
test_DT=test_DT[,1:3]
test_check_DT=DT[Date>=testStart]


DT=as.data.table(DT)
DT[,HomeTeam := tolower(HomeTeam)]
DT[,AwayTeam := tolower(AwayTeam)]


DT$HomeTeam[DT$HomeTeam=="man city"]<- "man-city"
DT$HomeTeam[DT$HomeTeam=="man united"]<- "man-united"
DT$HomeTeam[DT$HomeTeam=="hull"]<- "hull-city"
DT$HomeTeam[DT$HomeTeam=="man city"]<- "man-united"
DT$HomeTeam[DT$HomeTeam=="west ham"]<- "west-ham"
DT$HomeTeam[DT$HomeTeam=="aston villa"]<- "aston-villa"
DT$HomeTeam[DT$HomeTeam=="west brom"]<- "west-brom"
DT$HomeTeam[DT$HomeTeam=="crystal palace"]<- "crystal-palace"
DT$HomeTeam[DT$HomeTeam=="stoke"]<- "stoke-city"

DT$AwayTeam[DT$AwayTeam=="man city"]<- "man-city"
DT$AwayTeam[DT$AwayTeam=="man united"]<- "man-united"
DT$AwayTeam[DT$AwayTeam=="hull"]<- "hull-city"
DT$AwayTeam[DT$AwayTeam=="man city"]<- "man-united"
DT$AwayTeam[DT$AwayTeam=="west ham"]<- "west-ham"
DT$AwayTeam[DT$AwayTeam=="aston villa"]<- "aston-villa"
DT$AwayTeam[DT$AwayTeam=="west brom"]<- "west-brom"
DT$AwayTeam[DT$AwayTeam=="crystal palace"]<- "crystal-palace"
DT$AwayTeam[DT$AwayTeam=="stoke"]<- "stoke-city"

DT[,ID:=NA]

DT$ID<- with(DT, paste0(Date, "/", HomeTeam))

#2 Datasets are merged (odds + match statistics)

DT_merged=merge(DT, features,by="ID")

DT_merged[,ID:=NULL]
DT_merged[,leagueId:=NULL]
DT_merged[,Date:=NULL]
DT_merged[,Home:=NULL]
DT_merged[,Away:=NULL]
colnames(DT_merged)
DT_merged = DT_merged[,-6]
DT_merged=DT_merged[,c(10,1,2,11,12,3:9,13:109)]
colnames(DT_merged)


testStart=as.Date("2018-12-31")
testEnd=as.Date("2018-12-31")
trainStart=as.Date("2013-01-02")

train_DT=DT_merged[Match_Date>=trainStart & Match_Date<testStart]

test_DT=DT_merged[Match_Date>=testStart]
test_DT=test_DT[,-c(6:12)]
test_check_DT=DT_merged[Match_Date>=testStart]

#Regression Tree Fitting
#Home Shoot Pred

train_home_shot=as.data.frame(train_DT)
train_home_shot=train_home_shot[,-c(1,5:8,10:13)]
fit_home_shot=rpart(HS~.,train_home_shot, method="anova")

predictions_home_shot=predict(fit_home_shot,test_DT)
predictions_home_shot

test_DT=test_DT[,HS:=predictions_home_shot]
#Away Shoot Pred

train_away_shot=as.data.frame(train_DT)
train_away_shot=train_away_shot[,-c(1,5:8,11:13)]
fit_away_shot=rpart(AS~.,train_away_shot, method="anova")

predictions_away_shot=predict(fit_away_shot,test_DT)
predictions_away_shot


test_DT=test_DT[,AS:=predictions_away_shot]


#Home shot target
train_home_shot_target=as.data.frame(train_DT)
train_home_shot_target=train_home_shot_target[,-c(1,5:8,12:13)]
fit_home_shot_target=rpart(HST~.,train_home_shot_target, method="anova")

predictions_home_shot_target=predict(fit_home_shot_target,test_DT)
predictions_home_shot_target

test_DT=test_DT[,HST:=predictions_home_shot_target]
#Away shot target
train_away_shot_target=as.data.frame(train_DT)
train_away_shot_target=train_away_shot_target[,-c(1,5:8,13)]
fit_away_shot_target=rpart(AST~.,train_away_shot_target, method="anova")

predictions_away_shot_target=predict(fit_away_shot_target,test_DT)
predictions_away_shot_target

test_DT=test_DT[,AST:=predictions_away_shot_target]


#Home goal pred

train_home_goal=as.data.frame(train_DT)

train_home_goal=train_home_goal[,-c(1,5,7:8,13)]

fit_home_goal=rpart(FTHG~.,train_home_goal, method="anova")

predictions_home_goal=predict(fit_home_goal,test_DT)
predictions_home_goal

test_DT=test_DT[,FTHG:=predictions_home_goal]

#Away goal pred

train_away_goal=as.data.frame(train_DT)

train_away_goal=train_away_goal[,-c(1,5,8,13)]

fit_away_goal=rpart(FTAG~.,train_away_goal, method="anova")

predictions_away_goal=predict(fit_away_goal,test_DT)
predictions_away_goal

test_DT=test_DT[,FTAG:=predictions_away_goal]
train_DT[,EFTHG := NA]
train_DT[,110] = as.numeric(train_DT[,EFTHG])

# Calculate expected goals for training data
denek = train_DT[,1:109]
denek = denek[,-c(1,5,7:11)]
denek = denek[,-c(101,102)]
denek[,EFTHG := NA]
denek[,101] = as.numeric(denek[,EFTHG])
for (i in 1:nrow(denek)) {
  test_train = denek[i]
  test_train[,4:=NA]
  train_train = denek[-i]
  train_train_home_goal = as.data.frame(train_train)
  fit_home_goal_train=rpart(FTHG~.,train_train_home_goal, method="anova")
  predictions_home_goal_train=predict(fit_home_goal_train,test_train)
  denek[i,101] = predictions_home_goal_train
}

denek_2 = train_DT[,1:109]
denek_2 = denek_2[,-c(1,5,6,8:11)]
denek_2 = denek_2[,-c(101,102)]
denek[,EFTAG := NA]
denek[,102] = as.numeric(denek[,EFTAG])

for (i in 1:nrow(denek_2)) {
  test_train = denek_2[i]
  test_train[,4:=NA]
  train_train = denek_2[-i]
  train_train_away_goal = as.data.frame(train_train)
  # train_train_away_goal = train_train_away_goal[,-c(1,5,6,13)]
  fit_away_goal_train=rpart(FTAG~.,train_train_away_goal, method="anova")
  predictions_away_goal_train=predict(fit_away_goal_train,test_train)
  denek[i,102] = predictions_away_goal_train
}

train_DT[,EFTHG:=denek[,101]]
train_DT[,EFTAG:=denek[,102]]


# divide data based on the provided dates 
train_features=train_DT
colnames(train_features)
train_features[,Match_Result:=NULL]
train_features=train_features[,FTR:=as.character(a)]
colnames(train_features)
train_features[,a:=NULL]
colnames(train_features)

test_features=test_DT[Match_Date>=testStart & Match_Date < '2019-01-04'] 
colnames(test_features)
colnames(test_DT)
test_features=test_features[,FTR:=NA]

colnames(test_features)


#MODEL TRAIN
not_included_feature_indices=c(1:5)
tune_lambda=TRUE
nofReplications=2
nlambda=40
nFolds=10
alpha=1
trace=T
set.seed(1)
colnames(glm_train_data)
colnames(glm_test_data)
test_features[,Match_Result:=NULL]
glm_features=train_features[complete.cases(train_features)]
train_class=glm_features$FTR
typeof(train_class)
train_class
colnames(test_features)
glm_train_data=glm_features[,-c(1:5,110),with=F]
glm_test_data=test_features[,-c(1:5,108),with=F]

glm_train_data=glm_train_data[complete.cases(glm_train_data)]
glm_test_data=glm_test_data[complete.cases(glm_test_data)]

colnames(glm_test_data)


#Skellam Distribution

#for training data
glm_train_data[,HomeProb:=NA]
glm_train_data[,AwayProb:=NA]
glm_train_data[,DrawProb:=NA]
colnames(glm_train_data)
glm_train_data[,HomeProb:=as.numeric(HomeProb)]
glm_train_data[,AwayProb:=as.numeric(AwayProb)]
glm_train_data[,DrawProb:=as.numeric(DrawProb)]


for (i in 1:nrow(glm_train_data)) {
  HomeGoal=as.numeric(glm_train_data[i,EFTHG])
  AwayGoal=as.numeric(glm_train_data[i,EFTAG])
  home_sum=0
  for (k in 1:100) {
    home_sum=home_sum+dskellam(k, HomeGoal, AwayGoal, log = FALSE)
  }
  glm_train_data[i,105] = home_sum
  
  away_sum=0
  for (j in 1:100) {
    away_sum=away_sum+dskellam(-j, HomeGoal, AwayGoal, log = FALSE)
  }
  glm_train_data[i,106] = away_sum
  
  draw=dskellam(0, HomeGoal, AwayGoal, log = FALSE)
  
  glm_train_data[i,107] = draw
  
}

glm_train_data[,Sum:=HomeProb+AwayProb+DrawProb]
glm_train_data[,model_odd1:=1/HomeProb]
glm_train_data[,model_odd2:=1/AwayProb]
glm_train_data[,model_oddX:=1/DrawProb]

#For the test data



glm_test_data[,HomeProb:=NA]
glm_test_data[,AwayProb:=NA]
glm_test_data[,DrawProb:=NA]
colnames(glm_test_data)
glm_test_data[,HomeProb:=as.numeric(HomeProb)]
glm_test_data[,AwayProb:=as.numeric(AwayProb)]
glm_test_data[,DrawProb:=as.numeric(DrawProb)]

for (i in 1:nrow(glm_test_data)) {
  HomeGoal=as.numeric(glm_test_data[i,FTHG])
  AwayGoal=as.numeric(glm_test_data[i,FTAG])
  home_sum=0
  for (k in 1:100) {
    home_sum=home_sum+dskellam(k, HomeGoal, AwayGoal, log = FALSE)
  }
  glm_test_data[i,103] = home_sum
  
  away_sum=0
  for (j in 1:100) {
    away_sum=away_sum+dskellam(-j, HomeGoal, AwayGoal, log = FALSE)
  }
  glm_test_data[i,104] = away_sum
  
  draw=dskellam(0, HomeGoal, AwayGoal, log = FALSE)
  
  glm_test_data[i,105] = draw
  
}

glm_test_data[,Sum:=HomeProb+AwayProb+DrawProb]
glm_test_data[,model_odd1:=1/HomeProb]
glm_test_data[,model_odd2:=1/AwayProb]
glm_test_data[,model_oddX:=1/DrawProb]

intermediate_glm_test_data=glm_test_data
intermediate_glm_train_data=glm_train_data

glm_test_data=intermediate_glm_test_data
colnames(intermediate_glm_train_data)
glm_train_data=intermediate_glm_train_data
colnames(glm_features)

glm_train_data=glm_train_data[,-c(1:6,103:108)]
colnames(glm_test_data)
glm_test_data=glm_test_data[,-c(97:106)]
glm_train_data

#cross validation part comes from instructor
if(tune_lambda){
  cvindices=generateCVRuns(train_class,nofReplications,nFolds,stratified=TRUE)
  glmnet_alldata = glmnet(as.matrix(glm_train_data), as.factor(train_class), family="multinomial", alpha = alpha, nlambda=nlambda)
  lambda_sequence = glmnet_alldata$lambda
  
  cvresult=vector('list',nofReplications*nFolds)
  iter=1
  for(i in 1:nofReplications) {
    thisReplication=cvindices[[i]]
    for(j in 1:nFolds){
      if(trace){
        cat(sprintf('Iteration %d: Fold %d of Replication %d\n',iter,j,i))
      }
      testindices=order(thisReplication[[j]])
      
      cvtrain=glm_train_data[-testindices]    
      cvtrainclass=train_class[-testindices]   
      cvtest=glm_train_data[testindices]
      cvtestclass=train_class[testindices] 
      
      inner_cv_glmnet_fit = glmnet(as.matrix(cvtrain),as.factor(cvtrainclass),family="multinomial", alpha = alpha,lambda=lambda_sequence)
      valid_pred = predict(inner_cv_glmnet_fit, as.matrix(cvtest), s = lambda_sequence, type = "response")
      order_of_class=attr(valid_pred,'dimnames')[[2]]
      new_order=c(which(order_of_class=='H'),which(order_of_class=='D'),which(order_of_class=='A'))
      foldresult=rbindlist(lapply(c(1:length(lambda_sequence)),function(x) { data.table(repl=i,fold=j,lambda=lambda_sequence[x],valid_pred[,new_order,x],result=cvtestclass)}))
      cvresult[[iter]]=foldresult
      iter=iter+1
    }
  }
  
  cvresult=rbindlist(cvresult)
  cvresult[,pred_id:=1:.N]
  outcome_for_rps=data.table::dcast(cvresult,pred_id~result,value.var='pred_id')
  outcome_for_rps[,pred_id:=NULL]
  outcome_for_rps[is.na(outcome_for_rps)]=0
  outcome_for_rps[outcome_for_rps>0]=1
  setcolorder(outcome_for_rps,c('H','D','A'))
  overall_results=data.table(cvresult[,list(repl,fold,lambda)],RPS=RPS_matrix(cvresult[,list(H,D,A)],outcome_for_rps))
  overall_results_summary=overall_results[,list(RPS=mean(RPS)),list(repl,fold,lambda)]
  overall_results_summary=overall_results_summary[,list(meanRPS=mean(RPS),sdRPS=sd(RPS)),list(lambda)]
  overall_results_summary[,RPS_mean_lb := meanRPS - sdRPS]
  overall_results_summary[,RPS_mean_ub := meanRPS + sdRPS]
  
  cv_lambda_min=overall_results_summary[which.min(meanRPS)]$lambda
  
  semin=overall_results_summary[lambda==cv_lambda_min]$RPS_mean_ub
  cv_lambda.1se=max(overall_results_summary[meanRPS<semin]$lambda)
  
  cvResultsSummary = list(lambda.min =cv_lambda_min, lambda.1se = cv_lambda.1se,
                          meanRPS_min=overall_results_summary[lambda==cv_lambda_min]$meanRPS,
                          meanRPS_1se=overall_results_summary[lambda==cv_lambda.1se]$meanRPS)
  
}

# fit final glmnet model with the lambda with minimum error
final_glmnet_fit = glmnet(as.matrix(glm_train_data),as.factor(train_class),family="multinomial", alpha = alpha,lambda=cvResultsSummary$lambda.min)
# obtain predictions
predicted_probabilities=predict(final_glmnet_fit, as.matrix(glm_test_data), type = "response")

#check order of predictions
order_of_class=attr(predicted_probabilities,'dimnames')[[2]]
new_order=c(which(order_of_class=='H'),which(order_of_class=='D'),which(order_of_class=='A'))

final_result=data.table(test_features[,list(matchId,FTR)],predicted_probabilities[,new_order,1])
final_result

final_results <- merge(final_result,matches,by='matchId')

#RPS calculations 
matrix_hda=matrix(data=NA,nrow=10,ncol = 3)
for (i in 1:10) {
  if(final_results[i,Match_Result]=="Home"){
    matrix_hda[i,1]=1
    matrix_hda[i,2]=0
    matrix_hda[i,3]=0
  }
  if(final_results[i,Match_Result]=="Tie"){
    matrix_hda[i,2]=1
    matrix_hda[i,1]=0
    matrix_hda[i,3]=0}
  if(final_results[i,Match_Result]=="Away"){
    matrix_hda[i,3]=1
    matrix_hda[i,1]=0
    matrix_hda[i,2]=0}
  
}
hda = as.data.table(matrix_hda)
RPS_final=RPS_single(final_results[,list(H,D,A)],hda)

##ROUND 20

testStart=as.Date("2018-12-29")
testEnd=as.Date("2018-12-31")
trainStart=as.Date("2013-01-02")

test_DT=DT_merged[Match_Date>=testStart & Match_Date<=testEnd]
test_DT=test_DT[,-c(6:12)]
test_check_DT=DT_merged[Match_Date>=testStart & Match_Date <=testEnd]

test_features=test_DT[Match_Date>=testStart & Match_Date <= testEnd] 
test_features=test_features[,FTR:=NA]



round21train = train_DT
train_DT = train_DT[-c((nrow(train_DT)-9):nrow(train_DT))]
glm_train_data = glm_train_data[-c((nrow(glm_train_data)-9):nrow(glm_train_data))]
train_class=train_class[-c((length(train_class)-9):length(train_class))]

#home shot
train_home_shot=as.data.frame(train_DT[,-c(108:110)])
train_home_shot=train_home_shot[,-c(1,5:7,9:11)]

fit_home_shot=rpart(HS~.,train_home_shot, method="anova")

predictions_home_shot=predict(fit_home_shot,test_DT)
predictions_home_shot

test_DT=test_DT[,HS:=predictions_home_shot]
#Away Shoot Pred

train_away_shot=as.data.frame(train_DT[,-c(108:110)])
train_away_shot=train_away_shot[,-c(1,5:7,10:11)]
fit_away_shot=rpart(AS~.,train_away_shot, method="anova")

predictions_away_shot=predict(fit_away_shot,test_DT)
predictions_away_shot


test_DT=test_DT[,AS:=predictions_away_shot]


#Home shot target
train_home_shot_target=as.data.frame(train_DT[,-c(108:110)])
train_home_shot_target=train_home_shot_target[,-c(1,5:7,11)]
fit_home_shot_target=rpart(HST~.,train_home_shot_target, method="anova")

predictions_home_shot_target=predict(fit_home_shot_target,test_DT)
predictions_home_shot_target

test_DT=test_DT[,HST:=predictions_home_shot_target]
#Away shot target
train_away_shot_target=as.data.frame(train_DT[,-c(108:110)])
train_away_shot_target=train_away_shot_target[,-c(1,5:7)]
fit_away_shot_target=rpart(AST~.,train_away_shot_target, method="anova")

predictions_away_shot_target=predict(fit_away_shot_target,test_DT)
predictions_away_shot_target

test_DT=test_DT[,AST:=predictions_away_shot_target]


#Home goal pred

train_home_goal=as.data.frame(train_DT[,-c(108:110)])

train_home_goal=train_home_goal[,-c(1,5,7)]

fit_home_goal=rpart(FTHG~.,train_home_goal, method="anova")

predictions_home_goal=predict(fit_home_goal,test_DT)
predictions_home_goal

test_DT=test_DT[,FTHG:=predictions_home_goal]

#Away goal pred

train_away_goal=as.data.frame(train_DT[,-c(108:110)])

train_away_goal=train_away_goal[,-c(1,5)]

fit_away_goal=rpart(FTAG~.,train_away_goal, method="anova")

predictions_away_goal=predict(fit_away_goal,test_DT)
predictions_away_goal

test_DT=test_DT[,FTAG:=predictions_away_goal]

test_features=test_DT[Match_Date>=testStart & Match_Date < testEnd] 
test_features=test_features[,FTR:=NA]

glm_test_data = test_features
glm_test_data=test_features[,-c(1:6),with=F]
#glm_test_data=test_features[,-c(1:5,108),with=F]

glm_test_data=glm_test_data[complete.cases(glm_test_data)]

glm_test_data[,HomeProb:=NA]
glm_test_data[,AwayProb:=NA]
glm_test_data[,DrawProb:=NA]
colnames(glm_test_data)
glm_test_data[,HomeProb:=as.numeric(HomeProb)]
glm_test_data[,AwayProb:=as.numeric(AwayProb)]
glm_test_data[,DrawProb:=as.numeric(DrawProb)]

for (i in 1:nrow(glm_test_data)) {
  HomeGoal=as.numeric(glm_test_data[i,FTHG])
  AwayGoal=as.numeric(glm_test_data[i,FTAG])
  home_sum=0
  for (k in 1:100) {
    home_sum=home_sum+dskellam(k, HomeGoal, AwayGoal, log = FALSE)
  }
  glm_test_data[i,104] = home_sum
  
  away_sum=0
  for (j in 1:100) {
    away_sum=away_sum+dskellam(-j, HomeGoal, AwayGoal, log = FALSE)
  }
  glm_test_data[i,105] = away_sum
  
  draw=dskellam(0, HomeGoal, AwayGoal, log = FALSE)
  
  glm_test_data[i,106] = draw
  
}

glm_test_data[,Sum:=HomeProb+AwayProb+DrawProb]
glm_test_data[,model_odd1:=1/HomeProb]
glm_test_data[,model_odd2:=1/AwayProb]
glm_test_data[,model_oddX:=1/DrawProb]

glm_test_data=glm_test_data[,-c(97:107)]

if(tune_lambda){
  cvindices=generateCVRuns(train_class,nofReplications,nFolds,stratified=TRUE)
  glmnet_alldata = glmnet(as.matrix(glm_train_data), as.factor(train_class), family="multinomial", alpha = alpha, nlambda=nlambda)
  lambda_sequence = glmnet_alldata$lambda
  
  cvresult=vector('list',nofReplications*nFolds)
  iter=1
  for(i in 1:nofReplications) {
    thisReplication=cvindices[[i]]
    for(j in 1:nFolds){
      if(trace){
        cat(sprintf('Iteration %d: Fold %d of Replication %d\n',iter,j,i))
      }
      testindices=order(thisReplication[[j]])
      
      cvtrain=glm_train_data[-testindices]    
      cvtrainclass=train_class[-testindices]   
      cvtest=glm_train_data[testindices]
      cvtestclass=train_class[testindices] 
      
      inner_cv_glmnet_fit = glmnet(as.matrix(cvtrain),as.factor(cvtrainclass),family="multinomial", alpha = alpha,lambda=lambda_sequence)
      valid_pred = predict(inner_cv_glmnet_fit, as.matrix(cvtest), s = lambda_sequence, type = "response")
      order_of_class=attr(valid_pred,'dimnames')[[2]]
      new_order=c(which(order_of_class=='H'),which(order_of_class=='D'),which(order_of_class=='A'))
      foldresult=rbindlist(lapply(c(1:length(lambda_sequence)),function(x) { data.table(repl=i,fold=j,lambda=lambda_sequence[x],valid_pred[,new_order,x],result=cvtestclass)}))
      cvresult[[iter]]=foldresult
      iter=iter+1
    }
  }
  
  cvresult=rbindlist(cvresult)
  cvresult[,pred_id:=1:.N]
  outcome_for_rps=data.table::dcast(cvresult,pred_id~result,value.var='pred_id')
  outcome_for_rps[,pred_id:=NULL]
  outcome_for_rps[is.na(outcome_for_rps)]=0
  outcome_for_rps[outcome_for_rps>0]=1
  setcolorder(outcome_for_rps,c('H','D','A'))
  overall_results=data.table(cvresult[,list(repl,fold,lambda)],RPS=RPS_matrix(cvresult[,list(H,D,A)],outcome_for_rps))
  overall_results_summary=overall_results[,list(RPS=mean(RPS)),list(repl,fold,lambda)]
  overall_results_summary=overall_results_summary[,list(meanRPS=mean(RPS),sdRPS=sd(RPS)),list(lambda)]
  overall_results_summary[,RPS_mean_lb := meanRPS - sdRPS]
  overall_results_summary[,RPS_mean_ub := meanRPS + sdRPS]
  
  cv_lambda_min=overall_results_summary[which.min(meanRPS)]$lambda
  
  semin=overall_results_summary[lambda==cv_lambda_min]$RPS_mean_ub
  cv_lambda.1se=max(overall_results_summary[meanRPS<semin]$lambda)
  
  cvResultsSummary = list(lambda.min =cv_lambda_min, lambda.1se = cv_lambda.1se,
                          meanRPS_min=overall_results_summary[lambda==cv_lambda_min]$meanRPS,
                          meanRPS_1se=overall_results_summary[lambda==cv_lambda.1se]$meanRPS)
  
}

final_glmnet_fit = glmnet(as.matrix(glm_train_data),as.factor(train_class),family="multinomial", alpha = alpha,lambda=cvResultsSummary$lambda.min)
predicted_probabilities=predict(final_glmnet_fit, as.matrix(glm_test_data), type = "response")
order_of_class=attr(predicted_probabilities,'dimnames')[[2]]
new_order=c(which(order_of_class=='H'),which(order_of_class=='D'),which(order_of_class=='A'))

final_result_20=data.table(test_features[,list(matchId,FTR)],predicted_probabilities[,new_order,1])
final_result_20

final_results_20 <- merge(final_result_20,matches,by='matchId')

#RPS calculation
matrix_hda=matrix(data=NA,nrow=10,ncol = 3)
for (i in 1:10) {
  if(final_results_20[i,Match_Result]=="Home"){
    matrix_hda[i,1]=1
    matrix_hda[i,2]=0
    matrix_hda[i,3]=0
  }
  if(final_results_20[i,Match_Result]=="Tie"){
    matrix_hda[i,2]=1
    matrix_hda[i,1]=0
    matrix_hda[i,3]=0}
  if(final_results_20[i,Match_Result]=="Away"){
    matrix_hda[i,3]=1
    matrix_hda[i,1]=0
    matrix_hda[i,2]=0}
  
}
hda = as.data.table(matrix_hda)
RPS_final_20=RPS_single(final_results_20[,list(H,D,A)],hda)

##Same procedures are applied to all remaining nodes. 