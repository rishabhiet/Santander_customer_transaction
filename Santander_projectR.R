rm(list=ls(all=T))
setwd("C:/Users/hp/Desktop/satender customer R")

#Load Libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')

#install.packages(x)
lapply(x, require, character.only = TRUE)
rm(x)

## Read the data
trans = read.csv("train.csv", header = T, na.strings = c(" ", "", "NA"))

###########################################Explore the data##########################################

#gives variable names
str(trans)
#gives number of variable and observation
dim(trans)
##################################Missing Values Analysis###############################################
missing_val = data.frame(apply(trans,2,function(x){sum(is.na(x))}))
missing_val$Columns = row.names(missing_val)
names(missing_val)[1] =  "Missing_percentage"
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(trans)) * 100
missing_val = missing_val[order(-missing_val$Missing_percentage),]
row.names(missing_val) = NULL
missing_val = missing_val[,c(2,1)]
write.csv(missing_val, "Miising_perc.csv", row.names = F)

ggplot(data = missing_val[1:3,], aes(x=reorder(Columns, -Missing_percentage),y = Missing_percentage))+
geom_bar(stat = "identity",fill = "grey")+xlab("Parameter")+
ggtitle("Missing data percentage (Train)") + theme_bw()


#FOUR METHOD USED IN PREPROCESSING ARE. 1) MISSING VALUE ANALYSIS 2) OUTLIER ANALYSIS 3) DATA SELECTION 4) DATA SCALING
#DATA DOES NOT HAVE ANY MISSING VALUE SO WE ARE NOT USE MISSING VALUE IMPUTATION


####################################DATA EXPLORATORY######################################################################
############################################Outlier Analysis#############################################
# ## BoxPlots - Distribution and Outlier Check
#OUTLIER ANALYSIS
#BY USING TWO WAYS OUTLIER CAN BE HANDLE.
#BY REMOVING THE OUTLIER VALUES.
#OR BY PUTTING OUTLIER AS NULL VALUE AND THE IMPUTE USING KNN IMPUTATION.
#SO WE ARE USING FIRST METHOD AS IS GIVES LESSER ERROR

numeric_index = sapply(trans,is.numeric) #selecting only numeric

numeric_data = trans[,numeric_index]

cnames = colnames(numeric_data)
# 
for (i in 1:length(cnames))
 {
   assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "target"), data = subset(trans))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
            labs(y=cnames[i],x="responded")+
          ggtitle(paste("Box plot of responded for",cnames[i])))
 }
# 
# ## Plotting plots together
# gridExtra::grid.arrange(gn1,gn5,gn2,ncol=3)
# gridExtra::grid.arrange(gn6,gn7,ncol=2)
# gridExtra::grid.arrange(gn8,gn9,ncol=2)
# 
# # #Remove outliers using boxplot method
# df = trans
# trans = df
# 
 val = trans$previous[trans$previous %in% boxplot.stats(trans$previous)$out]
# 
trans = trans[which(!trans$previous %in% val),]
#                                   
# # #loop to remove from all variables
 for(i in cnames){   print(i)
   val = trans[,i][trans[,i] %in% boxplot.stats(trans[,i])$out]
  #print(length(val))
   trans = trans[which(!trans[,i] %in% val),]
 }
#AFTER REMOVING OUTLIER WE GET 175073 OBSERVATION FROM 200000 OBSERVATIONS SO 24927 OBSERVATION ARE EXIST AS AN OUTLIER IN TRANS DATASET¶

# 
# #Replace all outliers with NA and impute
# #create NA on "custAge
# for(i in cnames){
#   val = trans[,i][trans[,i] %in% boxplot.stats(trans[,i])$out]
#   #print(length(val))
#   trans[,i][trans[,i] %in% val] = NA
# }
# 
# trans = knnImputation(trans, k = 3)

##################################Feature Selection################################################
## Correlation Plot 

#DATA SELECTION
#FOR DATA SELECTION WE HAVE SHOWN EARLIER THAT NO VARIABLE IS DEPENDENT WITH EACH OTHER CORRELATION VALUE IS COMES OUT TO BE 1
#WE HAVE FETCH OUT INITIAL TO 10 VARIABLES TO GET HEAT MAP TO GET FOW CORELATION FORM BUT IT SHOWS NO TWO ARE DEPENDENT ON EACH OTHER 

#corrgram(trans[,numeric_index], order = F,
         #upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

## Chi-squared Test of Independence
#factor_index = sapply(trans,is.factor)
#factor_data = trans[,factor_index]

#for (i in 1:10)
{
  #print(names(factor_data)[i])
  #print(chisq.test(table(factor_data$target,factor_data[,i])))
}


##################################Feature Scaling################################################
#Normality check
#DATA SCALING WILL BE DONE ACCORDING TO MACHINE LEARNING ALGORITHM FORM. IF THE ALGORITHM ASSOCIATED WITH DISTANCE THAN WE DO SCALING OF NUMERIC VARIBALE FIRST
qqnorm(trans$var_1)
hist(trans$var_1)

#Normalisation
cnames = c("custAge","campaign","previous","cons.price.idx","cons.conf.idx","euribor3m","nr.employed",
           "pmonths","pastEmail")

for(i in range(2,203)){
  print(i)
  trans[,i] = (trans[,i] - min(trans[,i]))/
    (max(trans[,i] - min(trans[,i])))
}

##Standardisation
# for(i in range(2,203)){
#   print(i)
#   trans[,i] = (trans[,i] - mean(trans[,i]))/
#                                  sd(trans[,i])
# }

#############################################Sampling#############################################
# ##Simple Random Sampling
# data_sample = trans[sample(nrow(trans), 4000, replace = F), ]
# 
# ##Stratified Sampling
# stratas = strata(trans, c("profession"), size = c(100, 199, 10, 5), method = "srswor")
# stratified_data = getdata(trans, stratas)
# 
# ##Systematic sampling
# #Function to generate Kth index
# sys.sample = function(N,n){
#   k = ceiling(N/n)
#   r = sample(1:k, 1)
#   sys.samp = seq(r, r + k*(n-1), k)
# }
# 
# lis = sys.sample(7414, 400) #select the repective rows
# 
# #Create index variable in the data
# trans$index = 1:200000
# 
# #Extract subset from whole data
# systematic_data = trans[which(trans$index %in% lis),]

###################################Model Development#######################################
#Clean the environment
rmExcept("trans")

#Divide data into train and test using stratified sampling method
set.seed(1234)
train.index = createDataPartition(trans$target, p = .80, list = FALSE)
train = trans[ train.index,]
test  = trans[-train.index,]

##Decision tree for classification
#Develop Model on training data
C50_model = C5.0(responded ~., train, trials = 100, rules = TRUE)

#Summary of DT model
summary(C50_model)

#write rules into disk
write(capture.output(summary(C50_model)), "c50Rules.txt")

#Lets predict for test cases
C50_Predictions = predict(C50_model, test[,-202], type = "class")

##Evaluate the performance of classification model
ConfMatrix_C50 = table(test$responded, C50_Predictions)
confusionMatrix(ConfMatrix_C50)

#recall
#Recall=(TP*100)/(TP+FN) 

#precision
#precision= (TP*100)/(TP+FP))


#recall = 19.53488372093023%
#precision = 19.174955160606554%
#AUC =54.89%
#accuracy =84%¶
###Random Forest
RF_model = randomForest(target ~ ., train, importance = TRUE, ntree = 500)

#Extract rules fromn random forest
#transform rf object to an inTrees' format
# treeList = RF2List(RF_model)  
# 
# #Extract rules
# exec = extractRules(treeList, train[,-202])  # R-executable conditions
# 
# #Visualize some rules
# exec[1:2,]
# 
# #Make rules more readable:
# readableRules = presentRules(exec, colnames(train))
# readableRules[1:2,]
# 
# #Get rule metrics
# ruleMetric = getRuleMetric(exec, train[,-17], train$target)  # get rule metrics
# 
# #evaulate few rules
# ruleMetric[1:2,]

#Presdict test data using random forest model
RF_Predictions = predict(RF_model, test[,-17])

##Evaluate the performance of classification model
ConfMatrix_RF = table(test$target, RF_Predictions)
confusionMatrix(ConfMatrix_RF)

#recall
#Recall=(TP*100)/(TP+FN) 

#precision
#precision= (TP*100)/(TP+FP))


#recall = 0.019542700801250732
#precision = 100.0
#AUC =50%
#accuracy =90%

#Logistic Regression
logit_model = glm(target ~ ., data = train, family = "binomial")

#summary of the model
summary(logit_model)

#predict using logistic regression
logit_Predictions = predict(logit_model, newdata = test, type = "target")

#convert prob
logit_Predictions = ifelse(logit_Predictions > 0.5, 1, 0)


##Evaluate the performance of classification model
ConfMatrix_RF = table(test$target, logit_Predictions)

#False Negative rate
FNR = FN/FN+TP 

#recall
#Recall=(TP*100)/(TP+FN) 

#precision
#precision= (TP*100)/(TP+FP))
#recall = 26.34356068008599
#precision = 69.05737704918033
#AUC =62.53%
#accuracy =92%

#HERE WE CAN NOT PREDICT THE DATA USING KNN AS KNN IS NOT ABLE
#TO PREDICT THIS DATASET BECAUSE DATASET CONTAIN VERY LARGE 
#NUMBER OF VARIABLES AS IT USES DISTANCE SO IT TAKES INFINITE 
#TIME TO PREDICT THIS DATASET""

##KNN Implementation
library(class)

#Predict test data
KNN_Predictions = knn(train[, 2:202], test[, 2:202], trans$target, k = 7)

#Confusion matrix
Conf_matrix = table(KNN_Predictions, test$target)

#Accuracy
sum(diag(Conf_matrix))/nrow(test)

#False Negative rate
FNR = FN/FN+TP 



#naive Bayes
library(e1071)

#Develop model
NB_model = naiveBayes(target ~ ., data = train)

#predict on test cases #raw
NB_Predictions = predict(NB_model, test[,2:202], type = 'class')

#Look at confusion matrix
Conf_matrix = table(observed = test[,202], predicted = NB_Predictions)
confusionMatrix(Conf_matrix)

#recall = 35.86085597029509%
#precision = 72.04554377699255%
#AUC =67.08%
#accuracy =92%¶

#statical way
mean(NB_Predictions == test$target)

#Kmeans implementation
irisCluster <- kmeans(train[,2:202], 2, nstart = 20)

table(irisCluster$cluster, train$responded)
################################SUMMARY###############################################################################################################################

"ERRORS
LOGISTIC REGRESSION:Results
recall = 26.34356068008599
precision = 69.05737704918033
AUC =62.53%
accuracy =92%

DECISION TREES:Results
recall = 19.53488372093023%
precision = 19.174955160606554%
AUC =54.89%
accuracy =84%

RANDOM FORREST:Results
n_estimators=100
recall = 0.019542700801250732
precision = 100.0
AUC =50%
accuracy =90%

NAIVE BAYES:Results
recall =  35.86085597029509%
precision =  72.04554377699255%
AUC =67.08%
accuracy =92%

KNN:Results
taking a very very long time can not use it to predict the result.    

""To get the most accurate model values of recall, precision, AUC should 
be high.
According to the question we have to predict the result based on recall ,
precision and AUC. so after analysing the results of all four machine 
learning algorithm we can see that Naive Bayes is giving all the 
three parameters equally good. In Random Forest precision is high but 
recall is very low"".    
