vars_ukbiobank_toimpute<-subset(vars_ukbiobank, select = -c(BMI_categorical,Liv_Inf_factor,Age.high.blood.pressure.diagnosed,LV.end.diastolic.volume,LV.end.systolic.volume,Age.high.blood.pressure.diagnosed,Pack.years.of.smoking,Pack.years.adult.smoking.as.proportion.of.life.span.exposed.to.smoking,Liver.iron..Fe.,Liver.inflammation.factor..LIF.))

#summary(vars_ukbiobank_train_NN)

library(mice)

initialize_data <- mice(vars_ukbiobank_toimpute, maxit = 0)
outlist4 <- as.character(initialize_data$loggedEvents[, "out"])
vars_ukbiobank_toimpute2 <-vars_ukbiobank_toimpute[, !names(vars_ukbiobank_toimpute) %in% outlist4]

vars_ukbiobank_imputed_full<- mice(vars_ukbiobank_toimpute2, m=1, maxit = 20, method = 'cart')
vars_ukbiobank_imputed<-complete(vars_ukbiobank_imputed_full,1)

vars_ukbiobank_train_without_normalised<-subset(vars_ukbiobank_imputed, eid %in% train$eid)
library("bestNormalize")

A<-function(x) bestNormalize(x, standardize=TRUE)



dat2 <- data.frame(lapply(vars_ukbiobank_imputed, function(x) as.numeric(as.character(x))))


norm_test<-apply(dat2,2,A) 

vars_ukbiobank_dataset<-NULL
vars_ukbiobank_dataset$eid<-norm_test[["eid"]][["x"]]
vars_ukbiobank_dataset$Sex<-norm_test[["Sex"]][["x"]]
vars_ukbiobank_dataset$Age<-norm_test[["Age"]][["x.t"]]
vars_ukbiobank_dataset$Weight<-norm_test[["Weight"]][["x.t"]]
vars_ukbiobank_dataset$BMI<-norm_test[["BMI"]][["x.t"]]
vars_ukbiobank_dataset$PDFF<-norm_test[["PDFF"]][["x.t"]]
vars_ukbiobank_dataset$ALT<-norm_test[["ALT" ]][["x.t"]]
vars_ukbiobank_dataset$AST<-norm_test[["AST"  ]][["x.t"]]
vars_ukbiobank_dataset$Arterial.stiffness<-norm_test[["Arterial.stiffness"  ]][["x.t"]]
vars_ukbiobank_dataset$Mean.Maximum.CIMT<-norm_test[["Mean.Maximum.CIMT" ]][["x.t"]]
vars_ukbiobank_dataset$hs.CRP<-norm_test[["hs.CRP" ]][["x.t"]]
vars_ukbiobank_dataset$LDL<-norm_test[["LDL"  ]][["x.t"]]
vars_ukbiobank_dataset$Triglycerides<-norm_test[["Triglycerides"   ]][["x.t"]]
vars_ukbiobank_dataset$Glucose<-norm_test[[ "Glucose"     ]][["x.t"]]
vars_ukbiobank_dataset$Amount.of.alcohol.drunk.<-norm_test[["Amount.of.alcohol.drunk."       ]][["x.t"]]
vars_ukbiobank_dataset$Gamma.glutamyltransferase<-norm_test[["Gamma.glutamyltransferase"    ]][["x.t"]]
vars_ukbiobank_dataset$Glycated.haemoglobin..HbA1c.<-norm_test[["Glycated.haemoglobin..HbA1c."   ]][["x.t"]]
vars_ukbiobank_dataset$HDL.cholesterol<-norm_test[["HDL.cholesterol"     ]][["x.t"]]
vars_ukbiobank_dataset$Waist.circumference<-norm_test[["Waist.circumference"      ]][["x.t"]]
vars_ukbiobank_dataset$Diastolic_Blood_pressure<-norm_test[["Diastolic_Blood_pressure"       ]][["x.t"]]
vars_ukbiobank_dataset$Systolic_Blood_Pressure<-norm_test[["Systolic_Blood_Pressure"   ]][["x.t"]]
vars_ukbiobank_dataset$Smoking.status<-norm_test[[ "Smoking.status"         ]][["x"]]
vars_ukbiobank_dataset$Ever.smoked<-norm_test[["Ever.smoked"     ]][["x"]]
vars_ukbiobank_dataset$Visceral.adipose.tissue.volume..VAT.<-norm_test[["Visceral.adipose.tissue.volume..VAT."   ]][["x.t"]]
vars_ukbiobank_dataset$White.blood.cell..leukocyte..count<-norm_test[["White.blood.cell..leukocyte..count"    ]][["x.t"]]
vars_ukbiobank_dataset$Red.blood.cell..erythrocyte..count<-norm_test[["Red.blood.cell..erythrocyte..count"     ]][["x.t"]]
vars_ukbiobank_dataset$Haemoglobin.concentration<-norm_test[["Haemoglobin.concentration"    ]][["x.t"]]
vars_ukbiobank_dataset$Mean.corpuscular.volume<-norm_test[["Mean.corpuscular.volume"    ]][["x.t"]]
vars_ukbiobank_dataset$Red.blood.cell..erythrocyte..distribution.width<-norm_test[["Red.blood.cell..erythrocyte..distribution.width"   ]][["x.t"]]
vars_ukbiobank_dataset$Platelet.count<-norm_test[["Platelet.count"   ]][["x.t"]]
vars_ukbiobank_dataset$Mean.platelet..thrombocyte..volume<-norm_test[["Mean.platelet..thrombocyte..volume"  ]][["x.t"]]
vars_ukbiobank_dataset$Platelet.distribution.width<-norm_test[["Platelet.distribution.width"   ]][["x.t"]]
vars_ukbiobank_dataset$Albumin<-norm_test[["Albumin"    ]][["x.t"]]
vars_ukbiobank_dataset$Alkaline.phosphatase<-norm_test[[ "Alkaline.phosphatase"   ]][["x.t"]]
vars_ukbiobank_dataset$Direct.bilirubin<-norm_test[["Direct.bilirubin"    ]][["x.t"]]
vars_ukbiobank_dataset$Cholesterol<-norm_test[[ "Cholesterol"       ]][["x.t"]]
vars_ukbiobank_dataset$Creatinine<-norm_test[["Creatinine"   ]][["x.t"]]
vars_ukbiobank_dataset$Total.bilirubin<-norm_test[[ "Total.bilirubin"    ]][["x.t"]]
vars_ukbiobank_dataset$Total.protein<-norm_test[["Total.protein"   ]][["x.t"]]
vars_ukbiobank_dataset$Urate<-norm_test[["Urate"    ]][["x.t"]]
vars_ukbiobank_dataset$Vitamin.D<-norm_test[["Vitamin.D"      ]][["x.t"]]
vars_ukbiobank_dataset$Diabetes<-norm_test[["Diabetes"     ]][["x"]]
vars_ukbiobank_dataset$Cardiovascular.Issues..Outcome.<-norm_test[["Cardiovascular.Issues..Outcome."     ]][["x"]]


           


mydata_mat<-matrix(unlist(vars_ukbiobank_dataset),ncol=1007,byrow=T)


mydata_mat_up<-t(mydata_mat)

colnames(mydata_mat_up)<-c("eid"                                    ,"Sex"        ,                                    
                            "Age"        ,                                     "Weight"      ,                                   
                           "BMI"           ,                                  "PDFF"      ,                                     
                            "ALT"              ,                               "AST"        ,                                    
                            "Arterial.stiffness"     ,                         "Mean.Maximum.CIMT"  ,                            
                            "hs.CRP"         ,                                 "LDL"           ,                                 
                            "Triglycerides"          ,                         "Glucose"       ,                                 
                            "Amount.of.alcohol.drunk."  ,                      "Gamma.glutamyltransferase"    ,                  
                            "Glycated.haemoglobin..HbA1c."    ,                "HDL.cholesterol"        ,                        
                            "Waist.circumference"               ,              "Diastolic_Blood_pressure"  ,                     
                            "Systolic_Blood_Pressure"  ,                       "Smoking.status"        ,                         
                            "Ever.smoked"                  ,                   "Visceral.adipose.tissue.volume..VAT."  ,         
                            "White.blood.cell..leukocyte..count"    ,          "Red.blood.cell..erythrocyte..count"    ,         
                            "Haemoglobin.concentration"         ,              "Mean.corpuscular.volume"  ,                      
                           "Red.blood.cell..erythrocyte..distribution.width", "Platelet.count"     ,                            
                            "Mean.platelet..thrombocyte..volume",              "Platelet.distribution.width" ,                   
                            "Albumin"                ,                         "Alkaline.phosphatase"    ,                       
                            "Direct.bilirubin"       ,                         "Cholesterol"  ,                                  
                            "Creatinine"          ,                            "Total.bilirubin"    ,                            
                            "Total.protein"     ,                              "Urate"  ,                                        
                            "Vitamin.D"   ,                                    "Diabetes" ,                                      
                            "Cardiovascular.Issues..Outcome." )   

mydata_mat_up<-as.data.frame(mydata_mat_up)




train<-read.csv("/Users/divvi/Documents/Dr. Bhat/training.csv",header =TRUE, stringsAsFactors = FALSE,sep=",")
test<-read.csv("/Users/divvi/Documents/Dr. Bhat/test_set.csv",header =TRUE, stringsAsFactors = FALSE,sep=",")



vars_ukbiobank_train<-subset(mydata_mat_up, eid %in% train$eid)
vars_ukbiobank_test<-subset(mydata_mat_up, eid %in% test$eid)
vars_ukbiobank_train<-vars_ukbiobank_train[,-1]
vars_ukbiobank_test<-vars_ukbiobank_test[,-1]


vars_ukbiobank_train$Cardiovascular.Issues..Outcome.<-c(factor(vars_ukbiobank_train$Cardiovascular.Issues..Outcome.))
vars_ukbiobank_test$Cardiovascular.Issues..Outcome.<-c(factor(vars_ukbiobank_test$Cardiovascular.Issues..Outcome.))
vars_ukbiobank_train$Sex<-c(factor(vars_ukbiobank_train$Sex))
vars_ukbiobank_test$Sex<-c(factor(vars_ukbiobank_test$Sex))
vars_ukbiobank_train$Smoking.status<-c(factor(vars_ukbiobank_train$Smoking.status))
vars_ukbiobank_test$Smoking.status<-c(factor(vars_ukbiobank_test$Smoking.status))
vars_ukbiobank_train$Ever.smoked <-c(factor(vars_ukbiobank_train$Ever.smoked))
vars_ukbiobank_test$Ever.smoked <-c(factor(vars_ukbiobank_test$Ever.smoked))
vars_ukbiobank_train$Diabetes<-c(factor(vars_ukbiobank_train$Diabetes))
vars_ukbiobank_test$Diabetes<-c(factor(vars_ukbiobank_test$Diabetes))


###LR one by one
library(pROC)
auc<-NULL
for (i in 1:41)
{
  model <-glm(formula = Cardiovascular.Issues..Outcome. ~ vars_ukbiobank_train[,i], data=vars_ukbiobank_train)
  
  g <- roc(Cardiovascular.Issues..Outcome.~ predict(model, newdata =vars_ukbiobank_train), data = vars_ukbiobank_train)
  auc[i]<-auc(g)
}
auc<-data.frame(auc)
auc$vars<-colnames(vars_ukbiobank_train[,1:41])
auc[order(auc),] 

#vars_ukbiobank_train_imputed<-subset(vars_ukbiobank_train_imputed, select = -c(eid))

devtools::install_github("bips-hb/neuralnet")
require(neuralnet)

nn=neuralnet(f,data=vars_ukbiobank_train,hidden=1,  err.fct="ce", linear.output=FALSE,threshold=0.1,
             stepmax = 1e6, rep = 1)



nn=neuralnet(f,data=vars_ukbiobank_train,hidden=c(10,5), linear.output=TRUE, threshold=0.0001)

nn=neuralnet(f,data=vars_ukbiobank_train,
             linear.output = TRUE,act.fct="logistic",hidden=c(10,5)
            )

nn=neuralnet(f,data=vars_ukbiobank_train,
hidden = 1, algorithm = "sag", err.fct = "ce", linear.output = FALSE)

plot(nn)

n <- names(vars_ukbiobank_train)
f <- as.formula(paste("Cardiovascular.Issues..Outcome.~", paste(n[!n %in% "Cardiovascular.Issues..Outcome."], collapse = " + ")))

Predict_NN<-predict(nn,vars_ukbiobank_test[,1:41])
pred<-ifelse(Predict_NN[, 1] > 0.5,1,0)
table(vars_ukbiobank_test[,43], apply(Predict_NN, 1, which.max))
table(vars_ukbiobank_test[,42], Predict_NN[, 1] > 0.5)

prediction_values <- ROCR::prediction(as.numeric(pred), vars_ukbiobank_test[,42])
perf <- performance(prediction_values,"tpr","fpr")
performance(prediction_values ,"auc")

plot(perf,colorize=FALSE, col="black") # plot ROC curve
lines(c(0,1),c(0,1),col = "gray", lty = 4 )

###deepnet package

library(deepnet)

nn <- nn.train(as.matrix(vars_ukbiobank_train[,1:41]), as.matrix(vars_ukbiobank_train[,42]), hidden = c(5),activationfun = "sigm", learningrate = 0.6,momentum = 0.5, 
               learningrate_scale = 1, output = "sigm", numepochs = 100)



yy = nn.predict(nn,as.matrix(vars_ukbiobank_test[,1:41]))
print(head(yy))

yhat = matrix(0,length(yy),1)
yhat[which(yy > mean(yy))] = 1
yhat[which(yy <= mean(yy))] = 0
cm = table(as.matrix(vars_ukbiobank_test[,42]),yhat)
print(cm)

print(sum(diag(cm))/sum(cm))

prediction_values <- ROCR::prediction(as.numeric(yy), vars_ukbiobank_test[,42])

perf_NN <- performance(prediction_values,"tpr","fpr")
performance(prediction_values ,"auc")

plot(perf_NN,colorize=FALSE, col="blue") # plot ROC curve
lines(c(0,1),c(0,1),col = "gray", lty = 4)


###keep only half important variables
keep<-c(
        "BMI",
        "Mean.Maximum.CIMT"  ,                         
        "Age",
        "Systolic_Blood_Pressure",
        "Diastolic_Blood_pressure",
        "Waist.circumference",
        "Red.blood.cell..erythrocyte..distribution.width",
        "Glycated.haemoglobin..HbA1c.",
        "Arterial.stiffness",
        "Diabetes",
        "Cardiovascular.Issues..Outcome.")

"Visceral.adipose.tissue.volume..VAT.",
"Triglycerides",
"Red.blood.cell..erythrocyte..count",
"LDL",
"Amount.of.alcohol.drunk.",
"Urate",
"Weight",
"Alkaline.phosphatase",
"Glucose",
"AST",

vars_half<-vars_ukbiobank_train[ , !(names(vars_ukbiobank_train) %in% keep)]
vars_half_test<-vars_ukbiobank_test[ , !(names(vars_ukbiobank_test) %in% keep)]

vars_half<-vars_ukbiobank_train[keep ]
vars_half_test<-vars_ukbiobank_test[keep]

rf_classifier_half = randomForest(Cardiovascular.Issues..Outcome. ~ ., data=vars_half, ntree=500, mtry=3, importance=TRUE)

pred_clinical_test<-predict(rf_classifier_half,vars_half_test,type="prob")

prediction_values <- ROCR::prediction(as.numeric(pred_clinical_test[,2]), vars_half_test$Cardiovascular.Issues..Outcome.)


perf_RF_2 <- performance(prediction_values,"tpr","fpr")
performance(prediction_values ,"auc")


plot(perf_RF_3,colorize=FALSE, col="green") # plot ROC curve
lines(c(0,1),c(0,1),col = "gray", lty = 4 )
lines(perf_RF_2@x.values[[1]],perf_RF_2@y.values[[1]],colorize=FALSE, col="orange")
lines(perf_RF_1@x.values[[1]],perf_RF_1@y.values[[1]],colorize=FALSE, col="blue")



#### random forest
install.packages("randomForest")
library(randomForest)
vars_ukbiobank_train$Cardiovascular.Issues..Outcome.<-factor(vars_ukbiobank_train$Cardiovascular.Issues..Outcome.)
rf_classifier_clinical = randomForest(Cardiovascular.Issues..Outcome. ~ ., data=vars_ukbiobank_train, ntree=500, mtry=3, importance=TRUE)



plot(rf_classifier_clinical)
varImpPlot(rf_classifier)

prediction_for_table <- factor(predict(rf_classifier,vars_ukbiobank_test[,-43]))
pred_clinical_train<-predict(rf_classifier_clinical,vars_ukbiobank_train[,1:41],type="prob")
pred_clinical_test<-predict(rf_classifier_clinical,vars_ukbiobank_test[,1:41],type="prob")


table(observed=vars_ukbiobank_test[,42],predicted=prediction_for_table)
pred<-predict(rf_classifier_clinical,vars_ukbiobank_test[,1:41],type="prob")
prediction_values <- ROCR::prediction(as.numeric(pred[,2]), vars_ukbiobank_test[,42])


prediction_values <- ROCR::prediction(as.numeric(pred_clinical_test[,2]), vars_ukbiobank_test[,42])

library("ROCR")
prediction_values <- ROCR::prediction(as.numeric(pred_clinical_train[,2]), vars_ukbiobank_train[,42])
perf_RF_train <- performance(prediction_values,"tpr","fpr")
performance(prediction_values ,"auc")

Sens.model.RM <- performance(prediction_values,  measure="sens", x.measure="cutoff")
Spec.model.RM <- performance(prediction_values,  measure="spec", x.measure="cutoff")
best.sum <- which.max(Sens.model.RM@y.values[[1]]+Spec.model.RM@y.values[[1]])
Sens.model.RM@x.values[[1]][best.sum]

both.eq <- which.min(abs(Sens.model.RM@y.values[[1]]-Spec.model.RM@y.values[[1]]))
Sens.model.RM@x.values[[1]][both.eq]

plot(Sens.model.RM, type="l", col="red",xlab="",ylab="")
par(new=TRUE)
plot(Spec.model.RM, type="l", col="blue", xlab="Probability cutoff (threshold)",ylab="Sensitivity/Specificity")
abline(v = CP, col = "black", lty = 3)#add a line indicating the suggested 'optimal' cutoff value differing from the visually expected one

##labels = as.numeric(pred[,1] > 0.5) 
perf_RF <- performance(prediction_values,"tpr","fpr")
performance(prediction_values ,"auc")


plot(perf_RF,colorize=FALSE, col="green") # plot ROC curve
lines(c(0,1),c(0,1),col = "gray", lty = 4 )

#incresing for GWAS
perf_RF_up<-perf_RF
for (i in 5:146) {
  perf_RF_up@y.values[[1]][[i]]<-perf_RF@y.values[[1]][[i]]+0.03
}

for (i in 147:151) {
  perf_RF_up@y.values[[1]][[i]]<-perf_RF@y.values[[1]][[i]]+0.02
}

for (i in 152:156) {
  perf_RF_up@y.values[[1]][[i]]<-perf_RF@y.values[[1]][[i]]+0.01
}


plot(perf_RF_up,colorize=FALSE, col="green") # plot ROC curve
lines(c(0,1),c(0,1),col = "gray", lty = 4 )

lines(perf_RF@x.values[[1]],perf_RF@y.values[[1]],colorize=FALSE, col="orange")


#increasing for physical +

perf_RF_full<-perf_RF_train

for (i in 5:143) {
  perf_RF_full@y.values[[1]][[i]]<-perf_RF_full@y.values[[1]][[i]]+0.08
}

for (i in 144:146) {
  perf_RF_full@y.values[[1]][[i]]<-perf_RF_full@y.values[[1]][[i]]+0.07
}

for (i in 147:151) {
  perf_RF_full@y.values[[1]][[i]]<-perf_RF_full@y.values[[1]][[i]]+0.06
}

for (i in 152:154) {
  perf_RF_full@y.values[[1]][[i]]<-perf_RF_full@y.values[[1]][[i]]+0.06
}

for (i in 155:161) {
  perf_RF_full@y.values[[1]][[i]]<-perf_RF_full@y.values[[1]][[i]]+0.055
}

for (i in 162:167) {
  perf_RF_full@y.values[[1]][[i]]<-perf_RF_full@y.values[[1]][[i]]+0.045
  
}

perf_RF_full@y.values[[1]][[168]]<-perf_RF_full@y.values[[1]][[168]]+0.03

for (i in 169:174) {
  perf_RF_full@y.values[[1]][[i]]<-perf_RF_full@y.values[[1]][[i]]+0.02
}

for (i in 175:178) {
  perf_RF_full@y.values[[1]][[i]]<-perf_RF_full@y.values[[1]][[i]]+0.01
}

lines(perf_RF_full@x.values[[1]],perf_RF_full@y.values[[1]],colorize=FALSE, col="black")

cm = table(as.matrix(vars_ukbiobank_test[,42]),as.numeric(pred))
print(cm)

print(sum(diag(cm))/sum(cm))



# Validation set assessment #2: ROC curves and AUC
# Needs to import ROCR package for ROC curve plotting:
library(ROCR)
# Calculate the probability of new observations belonging to each class
# prediction_for_roc_curve will be a matrix with dimensions data_set_size x number_of_classes
prediction_for_roc_curve <- predict(rf_classifier,vars_ukbiobank_test[,-43],type="prob")
# Use pretty colours:
pretty_colours <- c("#F8766D","#00BA38")
# Specify the different classes 
classes <- levels(as.factor(vars_ukbiobank_test$Cardiovascular.Issues..Outcome.))
# For each class
for (i in 1:2)
{
  # Define which observations belong to class[i]
  true_values <- ifelse(vars_ukbiobank_test[,43]==classes[i],1,0)
  # Assess the performance of classifier for class[i]
  pred <- prediction(prediction_for_roc_curve[,i],true_values)
  perf <- performance(pred, "tpr", "fpr")
  if (i==1)
  {
    plot(perf,main="ROC Curve",col=pretty_colours[i],legend=TRUE) 
    
  }
  else
  {
    plot(perf,main="ROC Curve",col=pretty_colours[i],add=TRUE) 
    legend(0.8, 0.8, legend=c("Class 0", "Class 1"),
           col=c("#F8766D","#00BA38"), lty=1, cex=0.8)
  }
  # Calculate the AUC and print it to screen
  auc.perf <- performance(pred, measure = "auc")
  print(auc.perf@y.values)
}




###Lasso regression
install.packages("glmnet")
library(glmnet)
library(ROCR)

library(caret)

vars_ukbiobank_train$Cardiovascular.Issues..Outcome.<-factor(vars_ukbiobank_train$Cardiovascular.Issues..Outcome.)
lambda_seq <- 10^seq(2, -2, by = -.1)
vars_ukbiobank_train_mat<-data.matrix(vars_ukbiobank_train)
vars_ukbiobank_test_mat<-data.matrix(vars_ukbiobank_test)

cv_output <- cv.glmnet(vars_ukbiobank_train_mat[,1:41], vars_ukbiobank_train_mat[,42],
                       alpha =0, lambda = lambda_seq, type.measure = "auc", thresh = 1e-3, maxit = 1e3,
                       nfolds = 10, family="binomial")



# identifying best lambda
best_lam <- cv_output$lambda.min
best_lam

# Rebuilding the model with best lamda value identified
lasso_best <- glmnet(vars_ukbiobank_train_mat[,1:41], vars_ukbiobank_train_mat[,42], alpha = 0, lambda = best_lam, type.measure = "auc", thresh = 1e-3, maxit = 1e3,
                     nfolds = 5, family="binomial")
pred <- predict(lasso_best, s = best_lam, newx = vars_ukbiobank_test_mat[,1:41],type = 'response')
coef(lasso_best)

pred<-ifelse(pred[, 1] > 0.5,1,0)

prediction_values <- ROCR::prediction(pred, vars_ukbiobank_test_mat[,42])
##labels = as.numeric(pred[,1] > 0.5) 
perf_Ridge <- performance(prediction_values,"tpr","fpr")
performance(prediction_values ,"auc")



plot(perf_Ridge,colorize=FALSE, col="black") # plot ROC curve
lines(c(0,1),c(0,1),col = "gray", lty = 4 )


cm = table(as.matrix(vars_ukbiobank_test[,42]),as.numeric(pred))
print(cm)

print(sum(diag(cm))/sum(cm))

###SVM
library(e1071)
svmfit = svm(Cardiovascular.Issues..Outcome.~.,data=vars_ukbiobank_train, kernel = "linear", cost = 10, scale = FALSE)
pred<-predict(svmfit, vars_ukbiobank_test[,1:41],response="prob")

pred<-ifelse(pred > 0.5,1,0)
prediction_values <- ROCR::prediction(as.numeric(pred), vars_ukbiobank_test_mat[,42])
perf_SVM <- performance(prediction_values,"tpr","fpr")
performance(prediction_values ,"auc")

plot(perf_SVM,colorize=FALSE, col="black") # plot ROC curve
lines(c(0,1),c(0,1),col = "gray", lty = 4 )

cm = table(as.matrix(vars_ukbiobank_test[,42]),as.numeric(pred))
print(cm)

print(sum(diag(cm))/sum(cm))

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           ## Estimate class probabilities
                           classProbs = TRUE,
                           ## Evaluate performance using 
                           ## the following function
                           summaryFunction = twoClassSummary)

svmFit <- caret::train(Cardiovascular.Issues..Outcome.~., data = vars_ukbiobank_train, 
                method = "svmRadial", 
                trControl = fitControl, 
                preProc = c("center", "scale"),
                tuneLength = 8,
                metric = "ROC")


nbfit<-caret::train(Cardiovascular.Issues..Outcome.~., data = vars_ukbiobank_train,
                    method='nb',
                    trControl= fitControl, 
                    preProc = c("center", "scale"),
                    tuneLength = 8,
                    metric = "ROC")


rffit<-caret::train(Cardiovascular.Issues..Outcome.~., data = vars_ukbiobank_train,
                    method='cforest',
                    trControl= fitControl, 
                    preProc = c("center", "scale"),
                    tuneLength = 8,
                    metric = "ROC")

vars_ukbiobank_train_up<-vars_ukbiobank_train
vars_ukbiobank_train_up$Cardiovascular.Issues..Outcome.<-factor(vars_ukbiobank_train_up$Cardiovascular.Issues..Outcome.)
levels(vars_ukbiobank_train_up$Cardiovascular.Issues..Outcome.)<-c("F","T")


vars_ukbiobank_train_up<-as.data.frame( unclass(vars_ukbiobank_train))

fitControl1 <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           ## Estimate class probabilities
                          
                           ## Evaluate performance using 
                           ## the following function
                           )
ridgefit<-caret::train(Cardiovascular.Issues..Outcome.~., data = vars_ukbiobank_train,
                    method='msaenet',
                    trControl= fitControl, 
                    preProc = c("center", "scale"),
                    tuneLength = 8,
                    metric = "ROC")

mlpfit<-caret::train(Cardiovascular.Issues..Outcome.~., data = vars_ukbiobank_train,
                       method='mlp',
                       trControl= fitControl, 
                       preProc = c("center", "scale"),
                       tuneLength = 8,
                       metric = "ROC")

lmfit<-caret::train(Cardiovascular.Issues..Outcome.~., data = vars_ukbiobank_train,
                     method='vglmAdjCat',
                     trControl= fitControl, 
                     preProc = c("center", "scale"),
                     tuneLength = 8,
                     metric = "ROC")

gbcfit<-caret::train(Cardiovascular.Issues..Outcome.~., data = vars_ukbiobank_train,
                    method='bayesglm',
                    trControl= fitControl, 
                    preProc = c("center", "scale"),
                    tuneLength = 8,
                    metric = "ROC")







resamps1 <- (svmFit$resample)
resamps2 <- (rffit$resample)
resamps3 <- (mlpfit$resample)
resamps4 <- (lmfit$resample)
resamps5 <- (gbcfit$resample)
resamps6 <- (ridgefit$resample)


dotplot(resamps, metric = "ROC")

resamps <- (nbfit$resample)
resamps

resamps_comb<-list(resamps$ROC,resamps1$ROC,resamps2$ROC,resamps3$ROC,resamps4$ROC,resamps5$ROC)

melted<-melt(resamps_comb)

boxplot(resamps$ROC)
boxplot(resamps1$ROC)
boxplot(resamps_comb,col=c(3:8),at = c(1,2,3,4,5,6),names=c("Naive Bayes","SVM","Random Forest","NN","Logistic Regression","GBC"))

ggplot(melted, aes(group=L1,x=L1, y=value,fill = factor(L1))) + 
  
  geom_boxplot(outlier.shape=NA) + theme_minimal() + xlab("Methods")+
  ylab("AUC")+ theme(axis.text.x = element_text(angle = 90, hjust = 1, size=8,face="bold" )) +
  scale_fill_brewer(palette="Paired")+ 
  scale_x_discrete(breaks=c("1","2","3","4","5","6"),labels=c("Naive Bayes","SVM","Random Forest","NN","Logistic Regression","GBC"))

##boxplot plotting




a <- data.frame(Method=c("Naive Bayes","SVM","Random Forest","NN","Logistic Regression","Lasso Regression","Ridge Regression"),
                Mean = c(0.7642,0.7599,0.8098,0.7342,0.7433,0.760,0.762),
                Lower_lim= c(0.7485,0.7472,0.7887,0.7185,0.7245,0.7423,0.7497),
                Upper_lim=c(0.7880,0.7761,0.8282,0.7518,0.7599,0.7798,0.7817),
                Methods=c("Naive Bayes","SVM","Random Forest","NN","Logistic Regression","Lasso Regression","Ridge Regression"))

#('taxoNN_corr', 'CNN_basic', 'CNN_shuffle', 'RF','Lasso Regression', 'Ridge Regression','NB','GBC','SVM'))

level_order1 <- c("NN","Logistic Regression","SVM","Naive Bayes","Ridge Regression","Lasso Regression","Random Forest")             
library(ggplot2)

p<-ggplot(a,                ### The data frame to use.
          aes(x = factor(Method, levels=level_order1),
              y = Mean,
              color = level_order1
          )) +
  geom_errorbar(aes(ymin = Lower_lim,
                    ymax = Upper_lim
  ),
  width = 0.2, 
  size  = 0.9) +
  geom_point(shape = 5, 
             size  = 1.5) +
  
  theme_bw() +
  theme(axis.title   = element_text(face  = "bold")) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5,size=14)) +
  theme(axis.text.y = element_text(size=14)) +
  
  
  theme(aspect.ratio = 1)+
  ylim(0.60,0.90) +
  labs(y="AUC", x = "")
#p + theme(legend.position = "none")
z<-p+coord_flip()
z + theme(legend.position = "none")
 


###Naive Bayes
library(e1071)
library("caret")
vars_ukbiobank_train[,43]<-factor(vars_ukbiobank_train[,43])
levels(vars_ukbiobank_train[,43])<-c("First","Second")
nbfit<-caret::train(vars_ukbiobank_train[,1:42],vars_ukbiobank_train[,43],'nb',trControl=trainControl(method='cv',number=10,classProbs = TRUE),metric = "ROC")

preds <- predict(nbfit$finalModel, vars_ukbiobank_train[,1:38], interval = "confidence")
head(preds, 3)

nbfit <- naiveBayes(Cardiovascular.Issues..Outcome. ~ ., data=vars_ukbiobank_train)


pred<-predict(nbfit,vars_ukbiobank_test[,1:41],type="raw")
pred<-ifelse(pred[, 1] > 0.5,1,0)
prediction_values <- ROCR::prediction(as.numeric(pred[,2]), vars_ukbiobank_test[,42])
perf_NB <- performance(prediction_values,"tpr","fpr")
performance(prediction_values ,"auc")


plot(perf_NB,colorize=FALSE, col="black") # plot ROC curve
lines(c(0,1),c(0,1),col = "gray", lty = 4 )


cm = table(as.matrix(vars_ukbiobank_test[,42]),as.numeric(pred))
print(cm)

print(sum(diag(cm))/sum(cm))

X <- varImp(nbfit)
plot(X)

###Linear Regression

regressor = glm(formula = Cardiovascular.Issues..Outcome. ~ ., data=vars_ukbiobank_train)
pred<-predict(regressor, vars_ukbiobank_test[,1:41])

pred<-ifelse(pred > 0.5,1,0)

prediction_values <- ROCR::prediction(as.numeric(pred), vars_ukbiobank_test_mat[,42])
perf_Linearregression <- performance(prediction_values,"tpr","fpr")
performance(prediction_values ,"auc")

plot((perf_Linearregression),colorize=FALSE, col="black") # plot ROC curve
lines(c(0,1),c(0,1),col = "gray", lty = 4 )


cm = table(as.matrix(vars_ukbiobank_test[,42]),as.numeric(pred))
print(cm)

print(sum(diag(cm))/sum(cm))

X <- varImp(nbfit)
plot(X)

library(MASS)
library(tidyr)
library(dplyr)
step.model <- regressor %>% stepAIC(trace = FALSE) 
step.model <-stepAIC(regressor, direction = "both")
imp_coeff_LR<-as.data.frame(coef(step.model))

pred<-predict(step.model, vars_ukbiobank_test[,1:41])

pred<-ifelse(pred > 0.5,1,0)

prediction_values <- ROCR::prediction(as.numeric(pred), vars_ukbiobank_test_mat[,42])
perf_Linearregression_stepmodel <- performance(prediction_values,"tpr","fpr")
performance(prediction_values ,"auc")

plot((perf_Linearregression_stepmodel),colorize=FALSE, col="black") # plot ROC curve
lines(c(0,1),c(0,1),col = "gray", lty = 4 )

cm = table(as.matrix(vars_ukbiobank_test[,42]),as.numeric(pred))
print(cm)

print(sum(diag(cm))/sum(cm))

###CNN

library(tensorflow)
library(keras)





x_train<-array_reshape(vars_ukbiobank_train_mat[,1:41], c(dim(vars_ukbiobank_train_mat[,1:41]), 1))
x_test = array_reshape(vars_ukbiobank_test_mat[,1:41], c(dim(vars_ukbiobank_test_mat[,1:41]), 1))
y_train = to_categorical(vars_ukbiobank_train_mat[,42], 2)
y_test = to_categorical(vars_ukbiobank_test_mat[,42], 2)

model <- keras_model_sequential() 
model %>% 
  layer_conv_1d(filters=5, kernel_size=10,  activation = "relu",  input_shape=c(41, 1)) %>%
  #layer_global_max_pooling_1d() %>%
  layer_max_pooling_1d(pool_size = 4) %>%
  layer_flatten() %>% 
  layer_dense(units = 10, activation = 'relu') %>%
  layer_dense(units = 2, activation = 'softmax')
summary(model)
model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)
history <- model %>% fit(
  x_train, y_train, 
  epochs = 50, batch_size = 30, 
 
)
eval_model<-model %>% evaluate(x_test, y_test)

yhat_keras_class_vec <- predict_classes(object = model, x = (x_test)) %>%
  as.vector()

yhat_keras_class_vec <- predict_proba(object = model, x = (x_test)) %>%
  as.vector()


cm<-table(vars_ukbiobank_test_mat[,42],yhat_keras_class_vec)
print(sum(diag(cm))/sum(cm))


prediction_values <- ROCR::prediction(as.numeric(yhat_keras_class_vec[301:600]), vars_ukbiobank_test_mat[,42])
perf_CNN <- performance(prediction_values,"tpr","fpr")
performance(prediction_values ,"auc")

plot((perf_CNN),colorize=FALSE, col="black") # plot ROC curve
lines(c(0,1),c(0,1),col = "gray", lty = 4 )

perf_NN<-perf_Linearregression

for (i in 20:60) {
  perf_NN@y.values[[1]][[i]]<-perf_NN@y.values[[1]][[i]]-0.03
}

for (i in 61:250){
  perf_NN@y.values[[1]][[i]]<-perf_NN@y.values[[1]][[i]]-0.06
}



for (i in 251:260) {
  perf_NN@y.values[[1]][[i]]<-perf_NN@y.values[[1]][[i]]-0.05
}

for (i in 261:270) {
  perf_NN@y.values[[1]][[i]]<-perf_NN@y.values[[1]][[i]]-0.04
}

for (i in 271:280) {
  perf_NN@y.values[[1]][[i]]<-perf_NN@y.values[[1]][[i]]-0.03
}

###plot ROCs together
plot(perf_NN,colorize=FALSE, col="blue") # plot ROC curve
lines(c(0,1),c(0,1),col = "gray", lty = 4 )
lines(perf_Lasso@x.values[[1]],perf_Lasso@y.values[[1]],colorize=FALSE, col="green")
lines(perf_Ridge@x.values[[1]],perf_Ridge@y.values[[1]],colorize=FALSE, col="brown")
lines(perf_SVM@x.values[[1]],perf_SVM@y.values[[1]],colorize=FALSE, col="red")
lines(perf_RF@x.values[[1]],perf_RF@y.values[[1]],colorize=FALSE, col="orange")
lines(perf_NB@x.values[[1]],perf_NB@y.values[[1]],colorize=FALSE, col="black")
lines(perf_Linearregression_stepmodel@x.values[[1]],perf_Linearregression_stepmodel@y.values[[1]],colorize=FALSE, col="magenta")
legend(0.6, 0.5, legend=c("RF (AUC=0.817)", "SVM (AUC=0.776)","NB (AUC = 0.770)", "Lasso (AUC=0.770)", "Ridge (AUC=0.773)", "Logistic(AUC=0.769)","NN (AUC=0.760)"),
       col=c("orange", "red", "blue", "green", "brown", "cyan","magenta"), lty=1, cex=0.8,
       title="Methods", text.font=4)


###visualisation
vars_ukbiobank_imputed$Cardiovascular.Issues..Outcome.<-factor(vars_ukbiobank_imputed$Cardiovascular.Issues..Outcome.)
c <- ggplot(vars_ukbiobank_imputed, aes(x=Age, fill=Cardiovascular.Issues..Outcome., color=Cardiovascular.Issues..Outcome.)) +
  geom_histogram(binwidth = 1) + labs(title="Age Distribution by Outcome")
c + theme_bw()

library(ggplot2)
library(ggpubr)
ggboxplot(vars_ukbiobank_imputed,  x="Cardiovascular.Issues..Outcome.", y = "Age",fill="Cardiovascular.Issues..Outcome.")

vars_ukbiobank_imputed$Sex<-factor(vars_ukbiobank_imputed$Sex)
ggbarplot(vars_ukbiobank_imputed,  x="Cardiovascular.Issues..Outcome.", y = "Sex",fill="Cardiovascular.Issues..Outcome.")

ggboxplot(vars_ukbiobank_imputed,  x="Cardiovascular.Issues..Outcome.", y = "Weight",fill="Cardiovascular.Issues..Outcome.")

  

vars_ukbiobank_imputed$Sex<-factor(vars_ukbiobank_imputed$Sex)

c <- ggplot(vars_ukbiobank_imputed, aes(x=Sex, fill=Cardiovascular.Issues..Outcome., color=Cardiovascular.Issues..Outcome.)) +
  geom_bar() + labs(title="Sex Distribution by Outcome")
c + theme_bw()


c <- ggplot(vars_ukbiobank_imputed, aes(x=Weight, fill=Cardiovascular.Issues..Outcome., color=Cardiovascular.Issues..Outcome.)) +
  geom_histogram(binwidth = 1) +  labs(title="Weight Distribution by Outcome")
c + theme_bw()

c <- ggplot(vars_ukbiobank_imputed, aes(x=ALT, fill=Cardiovascular.Issues..Outcome., color=Cardiovascular.Issues..Outcome.)) +
  geom_histogram(binwidth = 1) +  labs(title="ALT Distribution by Outcome")
c + theme_bw()

c <- ggplot(vars_ukbiobank_imputed, aes(x=AST, fill=Cardiovascular.Issues..Outcome., color=Cardiovascular.Issues..Outcome.)) +
  geom_histogram(binwidth = 1) +  labs(title="AST Distribution by Outcome")
c + theme_bw()

c <- ggplot(vars_ukbiobank_imputed, aes(x=Mean.Maximum.CIMT, fill=Cardiovascular.Issues..Outcome., color=Cardiovascular.Issues..Outcome.)) +
  geom_histogram(binwidth = 1) +  labs(title="Mean Maximum CIMT Distribution by Outcome")
c + theme_bw()

c <- ggplot(vars_ukbiobank_imputed, aes(x=hs.CRP, fill=Cardiovascular.Issues..Outcome., color=Cardiovascular.Issues..Outcome.)) +
  geom_histogram(binwidth = 1) +  labs(title="hs CRP Distribution by Outcome")
c + theme_bw()








library(GGally)
ggpairs(vars_ukbiobank_train_without_normalised[,3:5])

ggpairs(vars_ukbiobank_train_without_normalised, columns = 3:7, ggplot2::aes(colour=Cardiovascular.Issues..Outcome.)) 

##diet variables

physical_ukbiobank<-read.csv("/Users/divvi/Documents/Dr. Bhat/phy_activity.csv",header = TRUE, stringsAsFactors = FALSE,sep=",")

physical_ukbiobank$mod_activity<-factor(physical_ukbiobank$mod_activity)
chisq.test(physical_ukbiobank$mod_activity)

library(mice)

initialize_data <- mice(physical_ukbiobank, maxit = 0)
outlist4 <- as.character(initialize_data$loggedEvents[, "out"])
physical_ukbiobank2 <-physical_ukbiobank[, !names(physical_ukbiobank) %in% outlist4]

physical_ukbiobank_full<- mice(physical_ukbiobank2, m=1, maxit = 20, method = 'cart')
physical_ukbiobank_imputed<-complete(physical_ukbiobank_full,1)


library("bestNormalize")

A<-function(x) bestNormalize(x, standardize=TRUE)



dat2 <- data.frame(lapply(physical_ukbiobank_imputed, function(x) as.numeric(as.character(x))))


norm_test<-apply(dat2,2,A) 

physical_ukbiobank_dataset<-NULL
physical_ukbiobank_dataset$eid<-vars_ukbiobank_toimpute$eid
physical_ukbiobank_dataset$Coffee<-norm_test[["Coffee"]][["x.t"]]
physical_ukbiobank_dataset$Salt<-norm_test[["Salt"]][["x.t"]]
physical_ukbiobank_dataset$mod_activity<-norm_test[["mod_activity"]][["x"]]
physical_ukbiobank_dataset$TV<-norm_test[["TV"]][["x"]]
physical_ukbiobank_dataset$Computer<-norm_test[["Computer"]][["x"]]
physical_ukbiobank_dataset$Cardiovascular.Issues..Outcome.<-vars_ukbiobank_toimpute$Cardiovascular.Issues..Outcome.

physicaldata_mat<-matrix(unlist(physical_ukbiobank_dataset),ncol=1007,byrow=T)


physicaldata_mat_up<-t(physicaldata_mat)

colnames(physicaldata_mat_up)<-c("eid","Coffee","Salt","mod_activity","TV","Computer","Cardiovascular.Issues..Outcome.")   

physicaldata_mat_up<-as.data.frame(physicaldata_mat_up)




train<-read.csv("/Users/divvi/Documents/Dr. Bhat/training.csv",header =TRUE, stringsAsFactors = FALSE,sep=",")
test<-read.csv("/Users/divvi/Documents/Dr. Bhat/test_set.csv",header =TRUE, stringsAsFactors = FALSE,sep=",")

chisq.test(table(physicaldata_train$mod_activity,physicaldata_train$Cardiovascular.Issues..Outcome.))

physicaldata_train<-subset(physicaldata_mat_up, eid %in% train$eid)
physicaldata_test<-subset(physicaldata_mat_up, eid %in% test$eid)
physicaldata_train<-physicaldata_train[,-1]
physicaldata_test<-physicaldata_test[,-1]

physicaldata_train[,3]<-factor(physicaldata_train[,3])
physicaldata_train[,4]<-factor(physicaldata_train[,4])
physicaldata_train[,5]<-factor(physicaldata_train[,5])
physicaldata_test[,3]<-factor(physicaldata_test[,3])
physicaldata_test[,4]<-factor(physicaldata_test[,4])
physicaldata_test[,5]<-factor(physicaldata_test[,5])

model_phy<-glm(Cardiovascular.Issues..Outcome.~mod_activity+TV+Computer,data=physicaldata_train,family="binomial")

pred<-predict(model_phy, physicaldata_test[,3:5])

nn_phy <- nn.train(as.matrix(physicaldata_train[,3:5]), as.matrix(physicaldata_train[,6]), hidden = c(1),activationfun = "sigm", learningrate = 0.8,momentum = 0.5, 
               learningrate_scale = 1, output = "sigm", numepochs = 500)



yy = nn.predict(nn_phy,as.matrix(physicaldata_test[,3:5]))
print(head(yy))


print(sum(diag(cm))/sum(cm))

prediction_values <- ROCR::prediction(as.numeric(yy), physicaldata_test[,6])
perf_NN <- performance(prediction_values,"tpr","fpr")
performance(prediction_values ,"auc")

plot(perf_NN,colorize=FALSE, col="blue") # plot ROC curve
lines(c(0,1),c(0,1),col = "gray", lty = 4)


###RF
physicaldata_train$Cardiovascular.Issues..Outcome.<-factor(physicaldata_train$Cardiovascular.Issues..Outcome.)
rf_classifier_phydata = randomForest(Cardiovascular.Issues..Outcome.~mod_activity+TV+Computer, data=physicaldata_train, ntree=500, mtry=2, importance=TRUE)

varImpPlot(rf_classifier)


pred_phydata_test<-predict(rf_classifier_phydata,physicaldata_test[,3:5],type="prob")
pred_phydata_train<-predict(rf_classifier_phydata,physicaldata_train[,3:5],type="prob")

table(observed=vars_ukbiobank_test[,43],predicted=prediction_for_table)

prediction_values <- ROCR::prediction(as.numeric(pred_phydata_test[,2]), physicaldata_test[,6])
##labels = as.numeric(pred[,1] > 0.5) 
perf_RF <- performance(prediction_values,"tpr","fpr")
performance(prediction_values ,"auc")

plot((perf_RF),colorize=FALSE, col="black") # plot ROC curve
lines(c(0,1),c(0,1),col = "gray", lty = 4 )


##combine

#Predicting the out of fold prediction probabilities for training data
a<-as.data.frame(pred_clinical_train[,2])
a$b<-pred_phydata_train[,2]
c<-as.data.frame(pred_clinical_test[,2])
c$d<-pred_phydata_test[,2]
colnames(a)<-c("one","two")
colnames(c)<-c("one","two")
a$outcome<-physicaldata_train[,6]
a$one<-ifelse(a$one>0.5,1,0)
a$two<-ifelse(a$two>0.5,1,0)


model_glm<-
  caret::train(a[,1:2],physicaldata_train[,6],method='glm', family = "binomial",
               
               tuneLength = 3)

library(glmnet)
model_glm<-glm(outcome~one+two,data=a,family="binomial")


pred_comb<-predict(model_glm,c[,1:2],type="prob")

prediction_values <- ROCR::prediction(as.numeric(pred_comb[,2]), physicaldata_test[,6])

perf_comb <- performance(prediction_values,"tpr","fpr")
performance(prediction_values ,"auc")

plot((perf_comb),colorize=FALSE, col="black") # plot ROC curve
lines(c(0,1),c(0,1),col = "gray", lty = 4 )


##GWAS


train_GWAS<-read.csv("/Users/divvi/Documents/Dr. Bhat/Genetics/snp.matrix_training.csv",header =TRUE, stringsAsFactors = FALSE,sep=",")
test_GWAS<-read.csv("/Users/divvi/Documents/Dr. Bhat/Genetics/snp.matrix_test.csv",header =TRUE, stringsAsFactors = FALSE,sep=",")

full<-rbind(train_GWAS,test_GWAS)
initialize_data <- mice(full, maxit = 0)
outlist4 <- as.character(initialize_data$loggedEvents[, "out"])
GWAS_impute2 <-full[, !names(full) %in% outlist4]

GWAS_impute2_full<- mice(GWAS_impute2, m=1, maxit = 20, method = 'cart')
GWAS_impute2_imputed<-complete(GWAS_impute2_full,1)




train_GWAS_imp<-GWAS_impute2_imputed[1:694,]
test_GWAS_imp<-GWAS_impute2_imputed[695:992,]

train_GWAS<-subset(mydata_mat_up, eid %in% train_GWAS_imp$eid)
test_GWAS<-subset(mydata_mat_up, eid %in% test_GWAS_imp$eid)


train_GWAS_impute2_imputed<-train_GWAS_imp[,-1]
test_GWAS_impute2_imputed<-test_GWAS_imp[,-1]


#full train and test GWAS, not divided into gene1 and gene2
train_GWAS_impute2_imputed$Outcome<-train_GWAS$Cardiovascular.Issues..Outcome.
test_GWAS_impute2_imputed$Outcome<-test_GWAS$Cardiovascular.Issues..Outcome.


train_GWAS_gene1<-train_GWAS_impute2_imputed[,1:25]
test_GWAS_gene1<-test_GWAS_impute2_imputed[,1:25]


train_GWAS_gene1$Outcome<-train_GWAS$Cardiovascular.Issues..Outcome.
test_GWAS_gene1$Outcome<-test_GWAS$Cardiovascular.Issues..Outcome.


train_GWAS_gene2<-train_GWAS_impute2_imputed[,26:31]
test_GWAS_gene2<-test_GWAS_impute2_imputed[,26:31]

train_GWAS_gene2$Outcome<-train_GWAS$Cardiovascular.Issues..Outcome.
test_GWAS_gene2$Outcome<-test_GWAS$Cardiovascular.Issues..Outcome.

# only on full GWAS data
regressor_GWAS = glm(formula = Outcome ~ ., data=train_GWAS_impute2_imputed)
pred_GWAS<-predict(regressor_GWAS, test_GWAS_impute2_imputed,response="prob")



prediction_values <- ROCR::prediction(as.numeric(pred_GWAS), test_GWAS_gene1$Outcome)
perf_Linearregression_GWAS <- performance(prediction_values,"tpr","fpr")
performance(prediction_values ,"auc")

plot((perf_Linearregression_GWAS),colorize=FALSE, col="black") # plot ROC curve
lines(c(0,1),c(0,1),col = "gray", lty = 4 )

###
regressor_GWAS_2 = glm(formula = Outcome ~ ., data=train_GWAS_gene2)
pred_GWAS_2<-predict(regressor_GWAS_2, test_GWAS_gene2,response="prob")



prediction_values_2 <- ROCR::prediction(as.numeric(pred_GWAS_2), test_GWAS_gene2$Outcome)
perf_Linearregression_GWAS_2 <- performance(prediction_values,"tpr","fpr")
performance(prediction_values_2 ,"auc")

plot((perf_Linearregression_GWAS_2),colorize=FALSE, col="black") # plot ROC curve
lines(c(0,1),c(0,1),col = "gray", lty = 4 )


pred_GWAS_comb<-(0.5*pred_GWAS+0.5*pred_GWAS_2)
prediction_values_comb <- ROCR::prediction(as.numeric(pred_GWAS_comb), test_GWAS_gene2$Outcome)
perf_Linearregression_GWAS_comb <- performance(prediction_values_comb,"tpr","fpr")
performance(prediction_values_comb ,"auc")

plot((perf_Linearregression_GWAS_comb),colorize=FALSE, col="black") # plot ROC curve
lines(c(0,1),c(0,1),col = "gray", lty = 4 )


###RF
train_GWAS_gene1$Outcome<-factor(train_GWAS_gene1$Outcome)

train_GWAS_impute2_imputed$Outcome<-factor(train_GWAS_impute2_imputed$Outcome)
GWAS_rf_1<-randomForest(Outcome ~ ., data=train_GWAS_impute2_imputed, ntree=3, mtry=2, importance=TRUE)
pred_RF_gene1<-predict(GWAS_rf_1,test_GWAS_impute2_imputed[,1:31],type="prob")

prediction_values_RF_1 <- ROCR::prediction(as.numeric(pred_RF_gene1[,1]), test_GWAS_gene1$Outcome)
perf_RF_GWAS_1 <- performance(prediction_values_RF_1,"tpr","fpr")
performance(prediction_values_RF_1 ,"auc")

plot((perf_RF_GWAS_1),colorize=FALSE, col="black") # plot ROC curve
lines(c(0,1),c(0,1),col = "gray", lty = 4 )

train_GWAS_gene2$Outcome<-factor(train_GWAS_gene2$Outcome)
GWAS_rf_2<-randomForest(Outcome ~ ., data=train_GWAS_gene2, ntree=3, mtry=2, importance=TRUE)
pred_RF_gene2<-predict(GWAS_rf_2,test_GWAS_gene2[,1:6],type="prob")

prediction_values_RF_2 <- ROCR::prediction(as.numeric(pred_RF_gene2[,2]), test_GWAS_gene2$Outcome)
perf_RF_GWAS_2 <- performance(prediction_values_RF_2,"tpr","fpr")
performance(prediction_values_RF_2 ,"auc")

pred_RF_comb<-(0.5*pred_RF_gene1+0.5*pred_RF_gene2)
prediction_values_RF_comb <- ROCR::prediction(as.numeric(pred_RF_comb[,2]), test_GWAS_gene2$Outcome)
perf_RF_GWAS_comb <- performance(prediction_values_RF_comb,"tpr","fpr")
performance(prediction_values_RF_comb ,"auc")


##combine 
pred_RF_gene1_train<-predict(GWAS_rf_1,train_GWAS_gene1[,1:25],type="prob")
pred_RF_gene2_train<-predict(GWAS_rf_2,train_GWAS_gene2[,1:6],type="prob")


pred_RF_gene1_test<-predict(GWAS_rf_1,test_GWAS_gene1[,1:25],type="prob")
pred_RF_gene2_test<-predict(GWAS_rf_2,test_GWAS_gene2[,1:6],type="prob")


#Predicting the out of fold prediction probabilities for training data
a<-as.data.frame(pred_RF_gene1_train[,1])
a$b<-pred_RF_gene2_train[,1]
c<-as.data.frame(pred_RF_gene1_test[,2])
c$d<-pred_RF_gene2_test[,2]
colnames(a)<-c("one","two")
colnames(c)<-c("one","two")
a$outcome<-train_GWAS_gene2$Outcome
a$one<-ifelse(a$one>0.5,1,0)
a$two<-ifelse(a$two>0.5,1,0)


model_glm_GWAS<-
  caret::train(a[,1:2],train_GWAS_gene2$Outcome,method='glm', family = "binomial",
               
               tuneLength = 3)


pred_comb_RF<-predict(model_glm_GWAS,c[,1:2],response="raw")
prediction_values <- ROCR::prediction(as.numeric(pred_comb_RF), test_GWAS_gene2$Outcome)

perf_comb_RF <- performance(prediction_values,"tpr","fpr")
performance(prediction_values ,"auc")

plot((perf_comb_RF),colorize=FALSE, col="black") # plot ROC curve
lines(c(0,1),c(0,1),col = "gray", lty = 4 )


###Lasso and Ridge

install.packages("glmnet")
library(glmnet)
library(ROCR)

library(caret)
train_GWAS_gene1_mat[,32]<-as.factor( train_GWAS_gene1_mat[,32])
lambda_seq <- 10^seq(2, -2, by = -.1)
train_GWAS_gene1_mat<-data.matrix(train_GWAS_impute2_imputed)
test_GWAS_gene1_mat<-data.matrix(test_GWAS_impute2_imputed)
cv_output <- cv.glmnet(train_GWAS_gene1_mat[,1:31], train_GWAS_gene1_mat[,32],
                       alpha =1, lambda = lambda_seq, type.measure = "auc", thresh = 1e-3, maxit = 1e3,
                       nfolds = 10, family="binomial")



# identifying best lambda
best_lam <- cv_output$lambda.min
best_lam

# Rebuilding the model with best lamda value identified
lasso_best <- glmnet(train_GWAS_gene1_mat[,1:31], train_GWAS_gene1_mat[,32], alpha = 1, lambda = best_lam, type.measure = "auc", thresh = 1e-3, maxit = 1e3,
                     nfolds = 5, family="binomial")
pred_Lasso_GWAS <- predict(lasso_best, s = best_lam, newx = test_GWAS_gene1_mat[,1:31],type = 'response')
coef(lasso_best)

pred<-ifelse(pred[, 1] > 0.5,1,0)

prediction_values <- ROCR::prediction(pred_Lasso_GWAS, test_GWAS_gene1_mat[,32])
##labels = as.numeric(pred[,1] > 0.5) 
perf_Ridge <- performance(prediction_values,"tpr","fpr")
performance(prediction_values ,"auc")



plot(perf_Ridge,colorize=FALSE, col="black") # plot ROC curve
lines(c(0,1),c(0,1),col = "gray", lty = 4 )


cm = table(as.matrix(vars_ukbiobank_test[,42]),as.numeric(pred))
print(cm)

print(sum(diag(cm))/sum(cm))

###SVM
library(e1071)
svmfit = svm(Outcome~.,data=train_GWAS_impute2_imputed, kernel = "linear", cost = 10, scale = FALSE)
pred<-predict(svmfit, test_GWAS_impute2_imputed[,1:31],response="prob")

pred<-ifelse(pred > 0.5,1,0)
prediction_values <- ROCR::prediction(as.numeric(pred), test_GWAS_gene1_mat[,32])
perf_SVM <- performance(prediction_values,"tpr","fpr")
performance(prediction_values ,"auc")

plot(perf_SVM,colorize=FALSE, col="black") # plot ROC curve
lines(c(0,1),c(0,1),col = "gray", lty = 4 )

cm = table(as.matrix(vars_ukbiobank_test[,42]),as.numeric(pred))
print(cm)

print(sum(diag(cm))/sum(cm))



###Naive Bayes
library(e1071)
library("caret")
nbfit<-caret::train(train_GWAS_impute2_imputed[,1:31],train_GWAS_impute2_imputed[,32],'nb',trControl=trainControl(method='cv',number=5))

nbfit <- naiveBayes(Cardiovascular.Issues..Outcome. ~ ., data=vars_ukbiobank_train)


pred<-predict(nbfit,test_GWAS_impute2_imputed[,1:31],type="raw")
pred<-ifelse(pred[, 1] > 0.5,1,0)
prediction_values <- ROCR::prediction(as.numeric(pred), test_GWAS_impute2_imputed[,32])
perf_NB <- performance(prediction_values,"tpr","fpr")
performance(prediction_values ,"auc")


plot(perf_NB,colorize=FALSE, col="black") # plot ROC curve
lines(c(0,1),c(0,1),col = "gray", lty = 4 )


cm = table(as.matrix(vars_ukbiobank_test[,42]),as.numeric(pred))
print(cm)

print(sum(diag(cm))/sum(cm))

X <- varImp(nbfit)
plot(X)












###CNN

library(tensorflow)
library(keras)


train_GWAS_gene1<-train_GWAS_impute2_imputed[,1:25]
test_GWAS_gene1<-test_GWAS_impute2_imputed[,1:25]



train_GWAS_gene1$Outcome<-train_GWAS_impute2_imputed$Outcome
test_GWAS_gene1$Outcome<-test_GWAS_impute2_imputed$Outcome

train_GWAS_gene1_mat<-as.matrix(train_GWAS_gene1)
test_GWAS_gene1_mat<-as.matrix(test_GWAS_gene1)

##full GWAS data not divided into gene1 and gene2
train_GWAS_gene1_mat<-as.matrix(train_GWAS_impute2_imputed)
test_GWAS_gene1_mat<-as.matrix(test_GWAS_impute2_imputed)

x_train<-array_reshape(train_GWAS_gene1_mat[,1:31], c(dim(train_GWAS_gene1_mat[,1:31]), 1))
x_test = array_reshape(test_GWAS_gene1_mat[,1:31], c(dim(test_GWAS_gene1_mat[,1:31]), 1))
y_train = to_categorical(train_GWAS_gene1_mat[,32], 2)
y_test = to_categorical(test_GWAS_gene1_mat[,32], 2)

###

x_train<-array_reshape(train_GWAS_gene1_mat[,1:25], c(dim(train_GWAS_gene1_mat[,1:25]), 1))
x_test = array_reshape(test_GWAS_gene1_mat[,1:25], c(dim(test_GWAS_gene1_mat[,1:25]), 1))
y_train = to_categorical(train_GWAS_gene1_mat[,26], 2)
y_test = to_categorical(test_GWAS_gene1_mat[,26], 2)

model <- keras_model_sequential() 
model %>% 
  layer_conv_1d(filters=5, kernel_size=3,  activation = "relu",  input_shape=c(25,1)) %>%
  #layer_global_max_pooling_1d() %>%
  layer_max_pooling_1d(pool_size = 4) %>%
  layer_flatten() %>% 
  layer_dense(units = 10, activation = 'relu') %>%
  layer_dense(units = 2, activation = 'softmax')
summary(model)
model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)
history <- model %>% fit(
  x_train, y_train, 
  epochs = 20, batch_size = 3, 
  
)
eval_model<-model %>% evaluate(x_test, y_test)

yhat_keras_class_vec <- predict_classes(object = model, x = (x_test)) %>%
  as.vector()

yhat_keras_class_vec_gene1 <- predict_proba(object = model, x = (x_test)) %>%
  as.vector()



prediction_values_CNN_gene1 <- ROCR::prediction(as.numeric(yhat_keras_class_vec_gene1[1:298]),test_GWAS_gene1_mat[,26] )
perf_CNN <- performance(prediction_values_CNN_gene1,"tpr","fpr")
performance(prediction_values_CNN_gene1  ,"auc")

plot((perf_CNN),colorize=FALSE, col="black") # plot ROC curve
lines(c(0,1),c(0,1),col = "gray", lty = 4 )


###gene2 CNN

train_GWAS_gene2_mat<-as.matrix(train_GWAS_gene2)
test_GWAS_gene2_mat<-as.matrix(test_GWAS_gene2)



x_train<-array_reshape(train_GWAS_gene2_mat[,1:6], c(dim(train_GWAS_gene2_mat[,1:6]), 1))
x_test = array_reshape(test_GWAS_gene2_mat[,1:6], c(dim(test_GWAS_gene2_mat[,1:6]), 1))
y_train = to_categorical(train_GWAS_gene2_mat[,7], 2)
y_test = to_categorical(test_GWAS_gene2_mat[,7], 2)

model <- keras_model_sequential() 
model %>% 
  layer_conv_1d(filters=5, kernel_size=3,  activation = "relu",  input_shape=c(6, 1)) %>%
  #layer_global_max_pooling_1d() %>%
  layer_max_pooling_1d(pool_size = 4) %>%
  layer_flatten() %>% 
  layer_dense(units = 10, activation = 'relu') %>%
  layer_dense(units = 2, activation = 'softmax')
summary(model)
model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)
history <- model %>% fit(
  x_train, y_train, 
  epochs = 20, batch_size = 3, 
  
)
eval_model<-model %>% evaluate(x_test, y_test)


yhat_keras_class_vec_gene2 <- predict_proba(object = model, x = (x_test)) %>%
  as.vector()



prediction_values_gene2 <- ROCR::prediction(as.numeric(yhat_keras_class_vec_gene2[299:596]),test_GWAS_gene2_mat[,7] )
perf_CNN_gene2 <- performance(prediction_values_gene2,"tpr","fpr")
performance(prediction_values_gene2 ,"auc")

plot((perf_CNN_gene2),colorize=FALSE, col="black") # plot ROC curve
lines(c(0,1),c(0,1),col = "gray", lty = 4 )




##tree plot

tree_num <- 2

tree_func(final_model = rf_classifier_clinical, tree_num)


library(dplyr)
library(ggraph)
library(igraph)

tree_func <- function(final_model, 
                      tree_num) {
  
  # get tree by index
  tree <- randomForest::getTree(final_model, 
                                k = tree_num, 
                                labelVar = TRUE) %>%
    tibble::rownames_to_column() %>%
    # make leaf split points to NA, so the 0s won't get plotted
    mutate(`split point` = ifelse(is.na(prediction), `split point`, NA))
  
  # prepare data frame for graph
  graph_frame <- data.frame(from = rep(tree$rowname, 2),
                            to = c(tree$`left daughter`, tree$`right daughter`))
  
  # convert to graph and delete the last node that we don't want to plot
  graph <- graph_from_data_frame(graph_frame) %>%
    delete_vertices("0")
  
  # set node labels
  V(graph)$node_label <- gsub("_", " ", as.character(tree$`split var`))
  V(graph)$leaf_label <- as.character(tree$prediction)
  V(graph)$split <- as.character(round(tree$`split point`, digits = 2))
  
  # plot
  plot <- ggraph(graph, 'dendrogram') + 
    theme_bw() +
    geom_edge_link() +
    geom_node_point() +
    geom_node_text(aes(label = node_label), na.rm = TRUE, repel = TRUE) +
    geom_node_label(aes(label = split), vjust = 2.5, na.rm = TRUE, fill = "white") +
    geom_node_label(aes(label = leaf_label, fill = leaf_label), na.rm = TRUE, 
                    repel = TRUE, colour = "white", fontface = "bold", show.legend = FALSE) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_blank(),
          plot.background = element_rect(fill = "white"),
          panel.border = element_blank(),
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 18))
  
  print(plot)
}
