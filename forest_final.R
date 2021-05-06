
###############################
##                           ##    
##         Project           ##  
##                           ##
###############################

#install.packages("ROCR")

library(data.table)
#library(lubridate)
#library(SnowballC)
#library(tm)
library(randomForest)
library(plyr)
library(ROCR)

setwd('/Users/klarow/Desktop/ComputationalLabs/Project/Data')
rawdat <- fread("model_finalv1")
#rawdat <- fread("modelready.csv")
data <- data.table(rawdat)
head(data)

#get prior probabilities
perc_error = dim(data[data$ISERROR == 1.])[1]/dim(data)[1]
perc_corr = dim(data[data$ISERROR != 1.])[1]/dim(data)[1]

perc_error
perc_corr
dim(data[data$ISERROR == 1.])


#data <- orig[!(orig$CosCat=="Only 1 note"),]
#data <- data[!(data$CosDesc=="Only 1 note"),]


#Columns to make categorical
cat_names <-c("DESCRIPTION","CG_LABEL","CG_DESCRIPTION","ADMISSION_TYPE","ADMISSION_LOCATION"
              ,"DISCHARGE_LOCATION","INSURANCE","LANGUAGE","RELIGION","ETHNICITY",
              "CHARTTIMECAT","STORETIMECAT","ISERROR","CATEGORY")

#Columns to make numerical
float_names <- c("TIMEDIFF","CosDesc" ,"CosCat")

#make sure categorical columns are categorical
for(col in cat_names) {
  print(col)
  data[[col]] <- as.factor(data[[col]])
}

#make sure numeric columns are numeric
#na's will be filled in further below
for(col in float_names) {
  print(col)
  data[[col]] <- as.numeric(data[[col]])
}

#check to see which features are >53 categories and remove
for(col in names(data)) {
  print(col)
 str(data[[col]])
}


#remove features with too many factors or that aren't needed
data[,c("ROW_ID","SUBJECT_ID","CGID",
        "DESCRIPTION","CG_LABEL","HADM_ID","OldLanguage","TEXT")] <- NULL

#Test/Train Partition (assigned from python script)
train_df = data[data$Train == "TRUE"]

test_df = data[data$Train == "FALSE"]

#################################

#remove train columns now
train_df$Train <- NULL
test_df$Train <- NULL

#check all wanted features are there
names(train_df)
#head(test_df)
#################################

#fill in na with means for numerics and modes for categorical using roughfix

train_df_roughfix <- na.roughfix(train_df)
train_feat_roughfix <- train_df_roughfix
train_class <- train_feat_roughfix$ISERROR

#View(train_df_roughfix)
train_feat_roughfix$ISERROR <- NULL

test_feat_roughfix <- na.roughfix(test_df)
test_df_roughfix<- na.roughfix(test_df)
test_class <- test_feat_roughfix$ISERROR

test_feat_roughfix$ISERROR <- NULL


mdel <- randomForest(ISERROR ~ . , data = train_df_roughfix,
                               replace=TRUE,mtry = 17,ntree=400, 
                               sampsize = c("0" = 100,"1" = 100),
                               strata = train_df_roughfix$ISERROR)

## function to calculate the recall and precision
recprec <- function(confusion){
  rec <- confusion[4]/(confusion[4]+confusion[2])
  prec <- confusion[4]/(confusion[4]+confusion[3])
  res <- c(rec,prec)
  return(res)
}

#load in model if necessary
#load("/Users/klarow/Desktop/ComputationalLabs/Project/Scripts/mdel.RData")
#mdel

#Plot for Variable Importance
importance(mdel)
varImpPlot(mdel, main = "Feature Importance")

#How good are we at classifying errors as errors
#True Positive Rate
pred_test <-predict(mdel,test_df_roughfix)
pred_test

#Get recall and precision
conf_matrix_test <- table(test_class, pred_test)
conf_matrix_test
recprec(conf_matrix_test)

#output file looking at predicted vs actual class for error analysis
pred_rf <-  predict(mdel,test_df_roughfix)
pred_rf

test_df_roughfix$Prediction <- pred_rf

write.csv(test_df_roughfix, file = "forerroranalysis.csv", row.names=FALSE)



######### Selecting Parameters helping script
samp = c(100,200,400,500)
counter = 0
rec_vec <- c()
prec_vec <- c()

for(size in samp) {
  counter <- counter + 1
  tempfor <- randomForest(ISERROR ~ . , data = train_df_roughfix,
               replace=TRUE,mtry = 17,ntree=400, 
               sampsize = c("0" = size,"1" = size),
               strata = train_df_roughfix$ISERROR)
  pred_rf_temp <-  predict(tempfor,test_df_roughfix)
  conf_matrix_temp <- table(test_class, pred_rf_temp)
  results <- recprec(conf_matrix_temp)
  rec_vec <- append(rec_vec,results[1])
  prec_vec <- append(prec_vec,results[2])
}

sampleplot <- plot(rec_vec,prec_vec,col =1:4,"b")
text(rec_vec,prec_vec,labels = samp,cex = .7, pos = 2)

samp = c(100,200,400,500)
counter = 0
rec_vec <- c()
prec_vec <- c()

rec_vec_feat <- c()
prec_vec_feat <- c()

features <- c(3,5,7,9,13,15,17)
for(number in features) {
  counter <- counter + 1
  tempfor <- randomForest(ISERROR ~ . , data = train_df_roughfix,
                          replace=TRUE,mtry = number,ntree=400, 
                          sampsize = c("0" = 100,"1" = 100),
                          strata = train_df_roughfix$ISERROR)
  pred_rf_temp <-  predict(tempfor,test_df_roughfix)
  conf_matrix_temp <- table(test_class, pred_rf_temp)
  results <- recprec(conf_matrix_temp)
  rec_vec_feat <- append(rec_vec_feat,results[1])
  prec_vec_feat <- append(prec_vec_feat,results[2])
}

featureplot <- plot(rec_vec_feat,prec_vec_feat,col =1:4,"b")
text(rec_vec_feat,prec_vec_feat,labels = samp,cex = .7, pos = 2)


#Preciscion/Recall Curve
predict_obj <- prediction(pred_rf[,2], test_class, label.ordering = NULL)
#predict_obj
perf1 <- performance(predict_obj, "prec", "rec")

plot(perf1,col = "green", xlabel = "recall", 
     ylabel = "preciscion")

