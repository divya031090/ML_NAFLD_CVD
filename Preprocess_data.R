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

