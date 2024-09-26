#Read data
data <-read.table("..data.csv",header=T,sep=",")

#Process data
data <- data[data$RouteId == 'US_23_HSR_SB',]
data <- data[data$Period == 'AM',]
data$LogYear <- log(data$CrashYear)
data$LogSegLength <- log(data$SegmentLength)
data$Seg_BO <- ifelse(data$SegmentType == 1 , 0, 1)
data$hour <- as.factor(data$hour)
data$VAS_PTSU <- as.factor(data$VAS_PTSU)

#Model data
data$obs <- 1:nrow(data)
data$obs <- factor(data$obs)
set.seed(123)
ind = sample.split(Y = data$hour, SplitRatio = 0.7)

#subsetting into Train data
train = data[ind,]
train$obs <- 1:nrow(train)
train$obs <- factor(train$obs)
#subsetting into Test data
test = data[!ind,]
test$obs <- 1:nrow(test)
test$obs <- factor(test$obs)

#PLN Model:
summary(mod_pln <- glmer(TotalCrashes ~ LnVolume 
                         + LnAvgSpeed 
                         +  StdSpeed  
                         #+  Diff_Avg_Speed
                         +  AvgOccupancyDown
                         #+ Diff_Avg_Occupancy_Down
                         + factor(VAS_PTSU)
                         + factor(Turnout) 
                         #+ factor(RightShoulderWidthC)
                         + factor(LeftShoulderWidthC)
                         + factor(Seg_BO) 
                         + offset(LogYear) 
                         + offset(LogSegLength) +(1|obs)+ (1|hour),
                          family = poisson, data = data, control=glmerControl(optimizer="bobyqa")))

summary(mod_pln <- glmer(TotalCrashes ~  offset(LogYear) + offset(LogSegLength) +(1|obs)+ (1|hour),
                          family = poisson, data = data, control=glmerControl(optimizer="bobyqa")))


1-(logLik(mod_pln)/logLik(mod_pln_null))

