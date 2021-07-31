#########################
######Data Analysis######
#########################

#import merged data file
#note for users:replace the file path in line 7 with your own file path
ma_data<-read.csv("/Users/mayxia1020/Desktop/318finalproject/cleaned/masspublic_cleanvfinal.csv",header=TRUE)
names(ma_data)
head(ma_data)
dim(ma_data)

#check if we can include the binary categorical predictor school_type into the model
#non-vocational school
traditional<-ma_data[ma_data$school_type==0,]
#vocational school
vocational<-ma_data[ma_data$school_type==1,]

#check constant variance assumption
boxplot(ma_data$attend_college~ma_data$school_type,names=c("0 (traditional)","1 (vocational)"),
        main="Boxplot of Response (Percentage of Students Attending College)",
        xlab="School Type",ylab="Percentage of Students Attending College")

sd_traditional<-sd(traditional$attend_college)
sd_vocational<-sd(vocational$attend_college)
#compute the ratio between SD
sd_traditional/sd_vocational #1.08, pass the test

#check multicollinearity
#exclude response var and categorical var, also exclude school name and district code
data_multicheck<-ma_data[,-c(1,2,3,5)]
library(usdm)
vifstep(data_multicheck,th=10)

#after variable selection with VIF threshold=10
library(MASS)
model_full<-lm(attend_college~school_type+attendance_rate+artcourse+avgclass_size+African.American+Asian+Hispanic
               +Native.American+pacific_islander+multi_race+Males+nonbinary+grade9_passrate+expenditure+notnativespeaker
               +englishlearner+disability+econdisadv+teacher_retained+teacher_license+masscore_complete+sat_math,data=ma_data)
summary(model_full) #adjusted R squared: 0.81
AIC(model_full) #1887
BIC(model_full) #1974

# AIC
stepback_AIC<-step(model_full,direction="both",k=2) #1058

# BIC
n<-dim(ma_data)[1]
stepback_BIC<-step(model_full,direction="both",k=log(n)) #1101

# AIC model (model 1)
model1<-lm(attend_college ~ school_type + attendance_rate + avgclass_size + 
             African.American + Hispanic + Native.American + multi_race + 
             Males + grade9_passrate + englishlearner + disability + econdisadv + 
             teacher_retained + teacher_license + masscore_complete,data=ma_data)
summary(model1) #adjusted R squared 0.81
AIC(model1) #1877
BIC(model1) #1940

# BIC model (model 2)
model2<-lm(attend_college ~ school_type + attendance_rate + African.American + 
             Hispanic + multi_race + Males + grade9_passrate + econdisadv + 
             teacher_license,data=ma_data)
summary(model2) #adjusted R squared 0.8
AIC(model2)#1884
BIC(model2)#1924

## 5-fold cross validation
#define indexes for each fold 
set.seed(1)
id.fold1=sample(1:n,58,replace=FALSE)
remaining=setdiff(1:n,id.fold1)
set.seed(1)
id.fold2=sample(remaining,58,replace=FALSE)
remaining=setdiff(1:n,c(id.fold1,id.fold2))
set.seed(1)
id.fold3=sample(remaining,58,replace=FALSE)
remaining=setdiff(1:n,c(id.fold1,id.fold2,id.fold3))
set.seed(1)
id.fold4=sample(remaining,58,replace=FALSE)
id.fold5=setdiff(1:n,c(id.fold1,id.fold2,id.fold3,id.fold4))
id.list=list(id.fold1,id.fold2,id.fold3,id.fold4,id.fold5)

#cross-validation for AIC-based stepwise regression model
CV_score=0
K=5
for (i in 1:K) {
  fit=lm(attend_college ~ school_type + attendance_rate + avgclass_size + 
           African.American + Hispanic + Native.American + multi_race + 
           Males + grade9_passrate + englishlearner + disability + econdisadv + 
           teacher_retained + teacher_license + masscore_complete,data=ma_data[-id.list[[i]],])
  Yhat=predict(fit,newdata=ma_data[id.list[[i]],])
  CV_score=CV_score+(1/K)*(1/length(id.list[[i]]))*sum((ma_data$attend_college[id.list[[i]]]-Yhat)^2)
  
}
CV_score #43.27

#cross-validation for BIC-based stepwise regression model
CV_score=0
K=5
for (i in 1:K) {
  fit=lm(attend_college ~ school_type + attendance_rate + African.American + 
           Hispanic + multi_race + Males + grade9_passrate + econdisadv + 
           teacher_license,data=ma_data[-id.list[[i]],])
  Yhat=predict(fit,newdata=ma_data[id.list[[i]],])
  CV_score=CV_score+(1/K)*(1/length(id.list[[i]]))*sum((ma_data$attend_college[id.list[[i]]]-Yhat)^2)
  
}
CV_score #42.02

#model diagnostics
par(mfrow=c(2,2))
plot(lm(attend_college ~ school_type + attendance_rate + African.American + 
          Hispanic + multi_race + Males + grade9_passrate + econdisadv + 
          teacher_license,data=ma_data))

#studentized deleted residuals
final_model<-lm(attend_college ~ school_type + attendance_rate + African.American + 
                  Hispanic + multi_race + Males + grade9_passrate + econdisadv + 
                  teacher_license,data=ma_data)

summary(final_model)
rstudent(final_model) #adjusted R squared 0.8
par(mfrow=c(1,2))
#residual plot
plot(rstudent(final_model)~fitted(final_model),xlab="Fitted Values",
     ylab="Studentized Deleted Residuals",
     main="Studentized Deleted Residual Plot")
abline(h=0,col="red")

#qq plot
qqnorm(rstudent(final_model),ylab="Studentized Deleted Residuals")
qqline(rstudent(final_model),col="red")

#identify influential observations
influential_check<-cooks.distance(final_model)
#compare to F distribution
f_check<-qf(influential_check,9,279)
which(f_check>=0.5)

#remove 189th and 288th observation
newma_data<-ma_data[-c(189,288),]
dim(newma_data)
new_fit<-lm(attend_college ~ school_type + attendance_rate + African.American + 
              Hispanic + multi_race + Males + grade9_passrate + econdisadv + 
              teacher_license,data=newma_data)
summary(new_fit) #adjusted R squared 0.81
AIC(new_fit) #1855
BIC(new_fit) #1895

par(mfrow=c(1,2))
#new studentized residual plot
rstudent(new_fit) 
plot(rstudent(new_fit)~fitted(new_fit),xlab="Fitted Values",
     ylab="Studentized Deleted Residuals",
     main="Studentized Deleted Residual Plot")
abline(h=0,col="red")

#new qq plot
qqnorm(rstudent(new_fit),ylab="Studentized Deleted Residuals")
qqline(rstudent(new_fit),col="red")




