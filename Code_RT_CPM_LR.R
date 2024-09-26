#Library
library("PLNmodels")
library(ggplot2)
library("lme4")
library("Matrix")
library("MASS")
library("glmmADMB")
library("optimx")
library("glmmTMB")
library("pscl")
library("R2admb")
library(caret)

#Data 
df_sample <- read.csv(file = "E:\\NCHRP_DATA\\Processed\\AM_Peak_All_Seg\\df_am_all_seg_1_5_pred_spat.csv",
                      header = TRUE,  sep = ",")

df_crash <- df_sample[df_sample$Crash_Prediction_Index == 1,]
df_nc <- df_sample[df_sample$Crash_Prediction_Index == 0,]

#under sampling: 1:5
df_ncs <- df_nc[sample(nrow(df_nc), size= 1025), ]
df_sample <- rbind(df_ncs, df_crash)


df_sample$Overcast <- ifelse(df_sample$Conditions == 'Clear', 0, 1)
df_sample$Rain <- ifelse(df_sample$Conditions == 'Rain, Partially cloudy', 1, 0)
df_sample$Seg_BO <- ifelse(df_sample$SegmentType == 1 , 0, 1)
df_sample$RouteId <- as.factor(df_sample$RouteId)
df_sample$LeftShoulderWidthC <- ifelse(df_sample$LeftShoulderWidth > 6 , 1, 0)
df_sample$RightShoulderWidthC <- ifelse(df_sample$RightShoulderWidth > 10 , 1, 0) #RSW greater than 10 as 1
df_sample$Ln_CF <- log(df_sample$Pred_XGB)

library(caTools)
set.seed(1)
ind = sample.split(Y = df_sample$Crash_Prediction_Index, SplitRatio = 0.7)

#subsetting into Train data
train = df_sample[ind,]
#subsetting into Test data
test = df_sample[!ind,]
table(train$Crash_Prediction_Index)
table(test$Crash_Prediction_Index)

#Model
summary(model_lr <- glm(Crash_Prediction_Index~ X5min_avg_volume  
              +  X5min_avg_speed 
             + X5min_std_speed 
             #+ Diff_Std_Speed
             #+  Diff_Avg_Speed
             #+ X5min_cv_speed
             + Diff_Avg_Occupancy_Down 
             #+ X5min_avg_occupancy_downstream 
             + factor(HSR_NHSR)
             + Visibility
             + Snow.Depth
             #+ Precipitation
             #+ factor(Overcast)
             #+ factor(Rain)
             #+ Temperature
             #+ Relative.Humidity
             #+ Wind.Speed
             + Cloud.Cover
             #+ factor(S4A_RURAL_OR_URBAN)
             #+ Segment.Length
             #+ factor(Turnout)
             + factor(Seg_BO)
             + factor(LeftShoulderWidthC)
             + factor(RightShoulderWidthC)
             + Ln_CF, 
             family="binomial", data=train))

pscl::pR2(model_lr)["McFadden"]
BIC(model_lr)


train$Pred_LR <- predict(model_lr, train, type="response")
train$Pred_LR.classes <- ifelse(train$Pred_LR > 0.5, 1, 0)
train_com <- train[complete.cases(train[ ,'Pred_LR.classes']),]
train_com <- train[,c('Crash_Prediction_Index', 'Pred_LR.classes')]
table(train_com$Crash_Prediction_Index, train_com$Pred_LR.classes)
confusionMatrix(table(train_com$Crash_Prediction_Index, train_com$Pred_LR.classes))

