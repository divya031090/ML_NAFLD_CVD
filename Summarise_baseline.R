vars_ukbiobank<-read.csv("/Users/divvi/Documents/Dr. Bhat/CVD_cohort.csv",header = TRUE, stringsAsFactors = FALSE,sep=",")

vars_ukbiobank_cases<-vars_ukbiobank[which(vars_ukbiobank$Cardiovascular.Issues..Outcome.==1),]
vars_ukbiobank_controls<-vars_ukbiobank[which(vars_ukbiobank$Cardiovascular.Issues..Outcome.==0),]


missing_terms<-as.data.frame(sapply(vars_ukbiobank, function(x) sum(is.na(x))))
missing_terms_perc<-as.data.frame(sapply(vars_ukbiobank, function(x) (sum(is.na(x)))*100)/1007)


columns_of_interest<- (vars_ukbiobank)
p_value_class<-columns_of_interest %>% 
  summarise_each(funs(wilcox.test(., vars_ukbiobank$X)$p.value))
p_value_class<-t(p_value_class)

chisq.test(vars_ukbiobank$Sex,vars_ukbiobank$Cardiovascular.Issues..Outcome.)
table(vars_ukbiobank_cases$Sex)
table(vars_ukbiobank_controls$Sex)


chisq.test(vars_ukbiobank$Alcohol.Drinker.Status,vars_ukbiobank$Cardiovascular.Issues..Outcome.)
table(vars_ukbiobank_cases$Alcohol.Drinker.Status)
table(vars_ukbiobank_controls$Alcohol.Drinker.Status)

vars_ukbiobank$Ethnicity<-factor(vars_ukbiobank$Ethnicity)
chisq.test(vars_ukbiobank$Ethnicity,vars_ukbiobank$Cardiovascular.Issues..Outcome.)


IQR<-cbind(colnames(vars_ukbiobank_cases),summary(vars_ukbiobank_cases)[3,],summary(vars_ukbiobank_cases)[2,],summary(vars_ukbiobank_cases)[5,])

IQR<-cbind(colnames(vars_ukbiobank_controls),summary(vars_ukbiobank_controls)[3,],summary(vars_ukbiobank_controls)[2,],summary(vars_ukbiobank_controls)[5,])
write.table(IQR, file = "/Users/divvi/Documents/Dr. Bhat/IQR.csv", sep = ",", append = TRUE, quote = FALSE,
            col.names = FALSE, row.names = FALSE)

table(vars_ukbiobank_cases$Diabetes)
table(vars_ukbiobank_controls$Diabetes)


table(vars_ukbiobank_cases$Smoking.status)
table(vars_ukbiobank_controls$Smoking.status)


table(vars_ukbiobank_cases$Ever.smoked)
table(vars_ukbiobank_controls$Ever.smoked)


wilcox.test(vars_ukbiobank$Weight~vars_ukbiobank$Cardiovascular.Issues..Outcome.,data=vars_ukbiobank)$p.value

wilcox.test(vars_ukbiobank$PDFF~vars_ukbiobank$Cardiovascular.Issues..Outcome.,data=vars_ukbiobank)$p.value

wilcox.test(vars_ukbiobank$Age~vars_ukbiobank$Cardiovascular.Issues..Outcome.,data=vars_ukbiobank)$p.value

wilcox.test(vars_ukbiobank$BMI~vars_ukbiobank$Cardiovascular.Issues..Outcome.,data=vars_ukbiobank)$p.value
wilcox.test(vars_ukbiobank$ALT~vars_ukbiobank$Cardiovascular.Issues..Outcome.,data=vars_ukbiobank)$p.value
wilcox.test(vars_ukbiobank$AST~vars_ukbiobank$Cardiovascular.Issues..Outcome.,data=vars_ukbiobank)$p.value
wilcox.test(vars_ukbiobank$Liv_Inf_factor~vars_ukbiobank$Cardiovascular.Issues..Outcome.,data=vars_ukbiobank)$p.value
wilcox.test(vars_ukbiobank$Arterial.stiffness~vars_ukbiobank$Cardiovascular.Issues..Outcome.,data=vars_ukbiobank)$p.value
wilcox.test(vars_ukbiobank$Mean.Maximum.CIMT~vars_ukbiobank$Cardiovascular.Issues..Outcome.,data=vars_ukbiobank)$p.value

wilcox.test(vars_ukbiobank$hs.CRP~vars_ukbiobank$Cardiovascular.Issues..Outcome.,data=vars_ukbiobank)$p.value
wilcox.test(vars_ukbiobank$LDL~vars_ukbiobank$Cardiovascular.Issues..Outcome.,data=vars_ukbiobank)$p.value
wilcox.test(vars_ukbiobank$Triglycerides~vars_ukbiobank$Cardiovascular.Issues..Outcome.,data=vars_ukbiobank)$p.value


wilcox.test(vars_ukbiobank$Glucose~vars_ukbiobank$Cardiovascular.Issues..Outcome.,data=vars_ukbiobank)$p.value
wilcox.test(vars_ukbiobank$Amount.of.alcohol.drunk.~vars_ukbiobank$Cardiovascular.Issues..Outcome.,data=vars_ukbiobank)$p.value
wilcox.test(vars_ukbiobank$LV.end.diastolic.volume~vars_ukbiobank$Cardiovascular.Issues..Outcome.,data=vars_ukbiobank)$p.value


wilcox.test(vars_ukbiobank$LV.end.systolic.volume~vars_ukbiobank$Cardiovascular.Issues..Outcome.,data=vars_ukbiobank)$p.value
wilcox.test(vars_ukbiobank$Gamma.glutamyltransferase~vars_ukbiobank$Cardiovascular.Issues..Outcome.,data=vars_ukbiobank)$p.value
wilcox.test(vars_ukbiobank$Glycated.haemoglobin..HbA1c.~vars_ukbiobank$Cardiovascular.Issues..Outcome.,data=vars_ukbiobank)$p.value


wilcox.test(vars_ukbiobank$HDL.cholesterol~vars_ukbiobank$Cardiovascular.Issues..Outcome.,data=vars_ukbiobank)$p.value
wilcox.test(vars_ukbiobank$Waist.circumference~vars_ukbiobank$Cardiovascular.Issues..Outcome.,data=vars_ukbiobank)$p.value
wilcox.test(vars_ukbiobank$Diastolic_Blood_pressure~vars_ukbiobank$Cardiovascular.Issues..Outcome.,data=vars_ukbiobank)$p.value
wilcox.test(vars_ukbiobank$Systolic_Blood_Pressure~vars_ukbiobank$Cardiovascular.Issues..Outcome.,data=vars_ukbiobank)$p.value

#logistic regression
vars_ukbiobank$Sex<-factor(vars_ukbiobank$Sex)
vars_ukbiobank$Diabetes<-factor(vars_ukbiobank$Diabetes)
pvalues_logit<-NULL
logit_value<-NULL
for (i in 2:64)
{
fit<-glm(vars_ukbiobank$Cardiovascular.Issues..Outcome.~vars_ukbiobank[,i],data=vars_ukbiobank,family=binomial(logit))
p_val<-coef(summary(fit))[,4]
pvalues_logit<-rbind(pvalues_logit,p_val)
logit_value_col<-as.data.frame(exp(cbind(coef(fit), confint(fit)))  )
logit_value<-rbind(logit_value,logit_value_col)

}
pvalues_logit<-cbind(pvalues_logit,colnames(vars_ukbiobank)[2:64])
logit_value<-cbind(logit_value,colnames(vars_ukbiobank)[2:64])


###MVA
vars_ukbiobank$BMI_categorical<-factor(vars_ukbiobank$BMI_categorical)
fit2<-glm(vars_ukbiobank$Cardiovascular.Issues..Outcome.~vars_ukbiobank$Age+vars_ukbiobank$Systolic_Blood_Pressure+vars_ukbiobank$LDL+vars_ukbiobank$Waist.circumference+vars_ukbiobank$BMI_categorical,data=vars_ukbiobank,family=binomial(logit))
as.data.frame(exp(cbind(coef(fit2), confint(fit2)))  )

fit3<-glm(vars_ukbiobank$Cardiovascular.Issues..Outcome.~vars_ukbiobank$Age+vars_ukbiobank$Systolic_Blood_Pressure+vars_ukbiobank$LDL+vars_ukbiobank$Waist.circumference+vars_ukbiobank$BMI+vars_ukbiobank$Glycated.haemoglobin..HbA1c.,data=vars_ukbiobank,family=binomial(logit))
as.data.frame(exp(cbind(coef(fit3), confint(fit3)))  )

fit4<-glm(vars_ukbiobank$Cardiovascular.Issues..Outcome.~vars_ukbiobank$Age+vars_ukbiobank$Systolic_Blood_Pressure+vars_ukbiobank$LDL+vars_ukbiobank$Waist.circumference+vars_ukbiobank$BMI+vars_ukbiobank$Glycated.haemoglobin..HbA1c.+vars_ukbiobank$Visceral.adipose.tissue.volume..VAT.+vars_ukbiobank$Cholesterol+vars_ukbiobank$Urate,data=vars_ukbiobank,family=binomial(logit))
as.data.frame(exp(cbind(coef(fit4), confint(fit4)))  )


#sample training and testing data
gendata_ID<-read.csv("/Users/divvi/Documents/Dr. Bhat/overlaped_ID.csv",header =TRUE, stringsAsFactors = FALSE,sep="\t")
smp_size <- floor(0.7 * nrow(gendata_ID))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(gendata_ID)), size = smp_size)


train <- gendata_ID[train_ind, ]
test <- gendata_ID[-train_ind, ]
colnames(train[1])<-"eid"

train<-read.csv("/Users/divvi/Documents/Dr. Bhat/training.csv",header =TRUE, stringsAsFactors = FALSE,sep=",")
test<-read.csv("/Users/divvi/Documents/Dr. Bhat/test_set.csv",header =TRUE, stringsAsFactors = FALSE,sep=",")

vars_ukbiobank_train<-subset(vars_ukbiobank, eid %in% train$eid)
vars_ukbiobank_test<-subset(vars_ukbiobank, eid %in% test$eid)

fit3<-glm(vars_ukbiobank_train$Cardiovascular.Issues..Outcome.~vars_ukbiobank_train$Age+vars_ukbiobank_train$Systolic_Blood_Pressure+vars_ukbiobank_train$LDL+vars_ukbiobank_train$BMI+vars_ukbiobank_train$Glycated.haemoglobin..HbA1c..1,data=vars_ukbiobank_train,family=binomial(logit))
as.data.frame(exp(cbind(coef(fit3), confint(fit3)))  )


fit<-glm(vars_ukbiobank_train$Cardiovascular.Issues..Outcome.~vars_ukbiobank_train$BMI,data=vars_ukbiobank_train,family=binomial(logit))
coef(summary(fit))[,4]

VIF_uk<-as.data.frame(vif(glm(vars_ukbiobank_train$Cardiovascular.Issues..Outcome.~vars_ukbiobank_train$Age+vars_ukbiobank_train$Systolic_Blood_Pressure+vars_ukbiobank_train$LDL+vars_ukbiobank_train$BMI+vars_ukbiobank_train$Glycated.haemoglobin..HbA1c..1,data=vars_ukbiobank_train,family=binomial(logit))
))
