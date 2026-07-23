#dissertation topic 

library(MASS)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(skimr)
library(gt)
#library(moderndive)
#library(gapminder)
library(GGally)

#read data 
data<- read.csv("dataset.csv")

data %>% skim() %>% gt()
glimpse(data)


data_uk<- data %>% filter(`Region_of_Study`=="UK")
write.csv(data_uk, "data_dissertation.csv", row.names = FALSE)
getwd()
list.files()

glimpse(data_uk)
data_uk %>% skim() #74901 rows

########################################
# Salary #
##########

#Salary with numerical variables
data_numerical<- data_uk %>% select(Salary, Years_Since_Graduation, GPA, Age)
sum(is.na(data_numerical$Salary))
sum(is.na(data_numerical$GPA))
sum(is.na(data_numerical$Age))
sum(is.na(data_numerical$Years_Since_Graduation))

ggpairs(data_numerical)

#use boxplots instead cause my variables are discrete numerical 

library(gridExtra)
p1<- ggplot(data_uk, aes(x=factor(Age), y=Salary))+
  geom_boxplot()
p2 <- ggplot(data_uk, aes(x = factor(Years_Since_Graduation), y = Salary)) +
  geom_boxplot() 

grid.arrange(p1, p2, ncol=2)

#arrange the columns years since data shows that the median for students graduated more than 3 years is 0

data_arrange<- data_uk %>%
  arrange(Years_Since_Graduation) %>%  filter(Employment_Status=="Continuing Education" & Years_Since_Graduation>=1)
nrow(data_arrange) #is 17153 a big number? it is affecting the median tho, so we filter out or ignore?

#summary for numerical
summary(data_numerical)


###########################

#Salary with categorical variables

library(gridExtra)
p1<- ggplot(data_uk, aes(x=Internship_Experience, y=Salary))+
  geom_boxplot()
p2<- ggplot(data_uk, aes(x=Education_Level, y=Salary))+
  geom_boxplot()
p3<- ggplot(data_uk, aes(x=University_Ranking, y=Salary))+
  geom_boxplot()
p4<- ggplot(data_uk, aes(x=Visa_Type, y=Salary))+
  geom_boxplot()
p5<- ggplot(data_uk, aes(x=Field_of_Study, y=Salary))+
  geom_boxplot()
p6<- ggplot(data_uk, aes(x=Gender, y=Salary))+
  geom_boxplot()
grid.arrange(p1, p2, p3, p4,p5,p6, ncol=2)

#SALARY VS JOb_sector boxplot ]
ggplot(data_uk, aes(x=Job_Sector, y=Salary))+
  geom_boxplot()

#salary vs country of origin 
ggplot(data_uk, aes(x=Country_of_Origin, y=Salary))+
  geom_boxplot() #it doesnt really matter

#boxplot for salary only
boxplot(data_numerical$Salary)


#Slary vs Employment 
ggplot(data_uk, aes(x=Employment_Status, y=Salary))+
  geom_boxplot()




#All i did today was lookign at salary and others, next look for employement status with other variables.


########################################################################
# Emplyemnet Status #
#####################

# Employment status distribution

ggplot(data_uk,aes(x = Employment_Status, fill = Employment_Status)) +
  geom_bar()


# Employment by Field of Study

ggplot(data_uk, aes(x = Field_of_Study, fill = Employment_Status)) +
  geom_bar()


# Employment by Visa Type
ggplot(data_uk, aes(x = Visa_Type, fill = Employment_Status)) +
  geom_bar()

# Employment by Internship Experience
ggplot(data_uk, aes(x = Internship_Experience, fill = Employment_Status)) +
  geom_bar(position = "fill") # for proportion

# Employment by University Ranking
ggplot(data_uk, aes(x = University_Ranking, fill = Employment_Status)) +
  geom_bar(position = "fill") # for proportion



#Employement Class
data_uk %>%
  count(Employment_Status) %>%
  mutate(prop = n / sum(n))

#distribution of employemnt status, we can do pie chart, and in research questions we can look at it again
#after filtering out continuing education



###########################################################################

#Maybe in the future the dataset can be cleaned to 
emplyement_data <- data_uk %>%
  filter((Employment_Status == "Employed" | Employment_Status=="Unemployed"))


#basic EDA to explore Data. 



###########################################################################
##################                          ###############################
##################    Research Questions    ###############################
##################                          ###############################
###########################################################################



'''
How accurately can employment outcomes of international graduates in the UK
be predicted using statistical and machine learning classification Models?

Which classification methods provide the best predictive performance for
employment outcomes?
  
**K-nearest Neighbours (KNN)**

First we filtered the dataset to only Employed/ Unemployed outcomes, then we
splitted the datset by 10-fold-cross-validation

We applied 10‑fold cross‑validation to estimate the classification accuracy.




############################################################################
##################                           ###############################
##################    K-nearest Neighbours   ###############################
##################                           ###############################
############################################################################



KNN computes distance for every K, each time it compute the distance between the validation points
and all training points. We will be doing this for 100 K values and 10 folds, so we will be training 1000 KNN models. 
(for each K value, we train 10 models)
'''

#filter data
data_emp<- data_uk %>% 
  filter(Employment_Status!="Continuing Education") %>%
  select(-Salary, -Job_Sector,-Region_of_Study)

#training and test set 
Y<- as.factor(data_emp$Employment_Status)
x<- data_emp %>% select(-Employment_Status)

#change categorical variables to dummy variables
X_num <- model.matrix(~ ., data = x)[,-1]
continious_x<- c("GPA", "Age", "Years_Since_Graduation")

set.seed(1)
n <- nrow(data_emp)
train_index <- sample(1:n, size = 0.8*n)

#if we scaled before splitting, the test rows would help scale to prepare the training data
#we need to scale the continuous data only with the train data

train_mean<- colMeans(X_num[train_index, continious_x])
train_sd<- apply(X_num[train_index, continious_x], 2, sd)

X_scaled<- X_num
X_scaled[train_index, continious_x] <- scale(X_num[train_index, continious_x], center = train_mean, scale = train_sd )
X_scaled[- train_index, continious_x]<- scale(X_num[-train_index, continious_x], center = train_mean, scale = train_sd)

X_train <- X_scaled[train_index, ]
X_test  <- X_scaled[-train_index, ]

y_train <- Y[train_index]
y_test  <- Y[-train_index]


library(class)

set.seed(1)

# splitting into 10 folds

n <- nrow(X_train)
fold_indices <- sample(rep(1:10, length.out = n))
folds <- split(1:n, fold_indices)

K_vals <- 1:50 #LIST OF 50 k values, SHOULD I CHOOSE  A DIFFERENT NB 
cv_acc <- numeric(length(K_vals)) #vector of 50 values

for (i in seq_along(K_vals)) { #for each value of k 
  
  k_val <- K_vals[i]
  fold_acc <- numeric(10) #vector of length 10 
  
  for (f in 1:10) { #for each fold
    
    
    # Validation indices
    valid_ind <- folds[[f]] # fold f is the validation,the rest are training 
    train_ind <- setdiff(1:n, valid_ind)
    
    # Split the data
    X_train_fold <- X_train[train_ind, ] #train
    y_train_fold <- y_train[train_ind]
    
    X_valid_fold <- X_train[valid_ind, ]  #test 
    y_valid_fold <- y_train[valid_ind]
    
    #train KNN
    pred_valid <- knn(
      train = X_train_fold,
      test  = X_valid_fold,   
      cl    = y_train_fold,
      k     = k_val
    )
    
    #accuracy of this fold being validation
    fold_acc[f] <- mean(pred_valid == y_valid_fold)
    #10 different training, validation scenarios for each k
  }
  
  #cv accuracy for k.
  cv_acc[i] <- mean(fold_acc)
}

best_k<- which.max(cv_acc)
#we select K with the highest CV accuracy 

# Plot CV accuracy
plot(K_vals, cv_acc, type="b",
     main="10-fold Cross-Validation Accuracy vs k",
     xlab="k (neighbours)",
     ylab="CV Accuracy",
     cex.main=1.6,
     cex.lab=1.4,
     cex.axis=1.2)

# Put CV results into a table
results <- data.frame(
  K = K_vals,
  CV_Accuracy = cv_acc
)

# Show the top 10 K values, sorting accuracy from highest to lowest
head(results[order(-results$CV_Accuracy), ], 10)

#very computationally expensive, and takes  lot of time,

pred_knn <- knn(train = X_train, test = X_test, cl = y_train, k = best_k, prob = TRUE)
knn_acc<- mean(pred_knn == y_test)


#Confusion Matrix

confusionM_KNN<- table(Predicted=pred_knn, Actual=y_test)
confusionM_KNN

confusionM_KNN_table<- data.frame(
  Predicted=c("Employed","Unemployed"),
  Employed=c(confusionM_KNN[1,1], confusionM_KNN[2,1]),
  Unemployed=c(confusionM_KNN[1,2], confusionM_KNN[2,2])
) %>% gt() %>%
  tab_spanner(
    label = "Actual",
    
    columns = c("Employed","Unemployed")
  ) %>%
  cols_label(
    Predicted = "Predicted",
    Employed = "Employed",
    Unemployed = "Unemployed"
  )

confusionM_KNN_table


sensitivity<- confusionM_KNN[1,1]/(confusionM_KNN[1,1]+confusionM_KNN[-1,1])
specificity<- confusionM_KNN[-1,-1]/(confusionM_KNN[-1,-1]+confusionM_KNN[1,-1])
precision<- confusionM_KNN[1,1]/(confusionM_KNN[1,1]+confusionM_KNN[1,-1])
F1_score<- 2*((precision*sensitivity)/(precision+sensitivity))

metrics_table_KNN <- data.frame(
  Metric = c("Accuracy", "Sensitivity", "Specificity", "Precision", "F1 Score"),
  Value  = c(knn_acc, sensitivity, specificity, precision, F1_score)
)

metrics_table_KNN %>% gt()

cm_knn <- confusionMatrix(factor(pred_knn, levels = c("Employed","Unemployed")),
                          factor(y_test, levels = c("Employed","Unemployed")),
                          positive = "Employed")

cm_knn
###########################################################################
##################                          ###############################
##################   Classification tree    ###############################
##################                          ###############################
###########################################################################


#employement with 3 categorical values is used, cause when we filter cotinuing eductaion out, the tree consider 
#salary as the only predictor resulting ina  tree of only 1 split

library(rpart)
#install.packages("rpart.plot")
library(rpart.plot)


#first we split the dataset
set.seed(1)
n <- nrow(data_emp)
train_index <- sample(1:n, size = 0.8*n)

train_data_c <- data_emp[train_index, ]
test_data_c <- data_emp[-train_index, ]

train_data_c$Employment_Status <- as.factor(train_data_c$Employment_Status) #tells that this is categorical, so classification model

tree <- rpart(Employment_Status ~. , data=train_data_c, method="class",cp=0)
rpart.plot(tree)
#very large 
printcp(tree) # prune the tre,e find the best cot complecity 
plotcp(tree)

#find th eminimumm x error, the cv error
xerror_min<- min(tree$cptable[, "xerror"])

#now find the cp of that minimum
best_cp<- tree$cptable [which.min(tree$cptable[, "xerror"]), "CP"]
num_splits<- tree$cptable [which.min(tree$cptable[, "xerror"]), "nsplit"]

tree_prune<- prune(tree, cp=best_cp)
rpart.plot(tree_prune)
#Predict and evaluate perfromance
pred1 <- predict(tree_prune, newdata = test_data_c, type = "class")
mean(pred1 == test_data_c$Employment_Status)
#shouldi add arguments like minsplit and minbucket? how will this help? nothing changed

#after splitting data we get the pruning tree to be larger than without splitting 

#we can also use the 1SE rule for choosing the best cp

#1SE
min_xerror <- min(tree$cptable[, "xerror"])
min_xstd <- tree$cptable[which.min(tree$cptable[, "xerror"]), "xstd"]
smallest_Tree<- min_xerror+min_xstd
best_1se_index <- which(tree$cptable[, "xerror"] <= smallest_Tree)[1] #take the first value
best_cp_1se <- tree$cptable[best_1se_index, "CP"]
tree_prune_1se<- prune(tree, cp=best_cp_1se)
rpart.plot(tree_prune_1se)
#Predict and evaluate perfromance
pred2 <- predict(tree_prune_1se, newdata = test_data_c, type = "class")
mean(pred2 == test_data_c$Employment_Status)
#this 1SE gives a less crowded tree

###############################

library(randomForest)
# bagging

Model <- randomForest(Employment_Status ~. , data=train_data_c, mtry= ncol(train_data_c)-1,ntree=500) # nb of predictors 
#Predict and evaluate perfromance
pred3 <- predict(Model, newdata = test_data_c, type = "class")
mean(pred3 == test_data_c$Employment_Status)
# random forests
Model2 <- randomForest(Employment_Status ~. , data=train_data_c)
#Predict and evaluate perfromance
pred4 <- predict(Model2, newdata = test_data_c, type = "class")
mean(pred4 == test_data_c$Employment_Status)

results <- data.frame(
  Model = c("Tree (min xerror)", 
            "Tree (1-SE rule)", 
            "Bagging", 
            "Random Forest"),
  
  Accuracy = c(
    mean(pred1 == test_data_c$Employment_Status),
    mean(pred2 == test_data_c$Employment_Status),
    mean(pred3 == test_data_c$Employment_Status),
    mean(pred4 == test_data_c$Employment_Status)
  )
) 

results %>% gt() 

pred_1SE_tree<- predict(tree_prune_1se, newdata = test_data_c, type = "class")
accuracy_1se<- mean(pred_1SE_tree == test_data_c$Employment_Status)

#Confusion Matrix

confusionM_tree<- table(Predicted=pred_1SE_tree, Actual=test_data_c$Employment_Status)
confusionM_tree

confusionM_tree_table<- data.frame(
  Predicted=c("Employed","Unemployed"),
  Employed=c(confusionM_tree[1,1], confusionM_tree[2,1]),
  Unemployed=c(confusionM_tree[1,2], confusionM_tree[2,2])
) %>% gt() %>%
  tab_spanner(
    label = "Actual",
    
    columns = c("Employed","Unemployed")
  ) %>%
  cols_label(
    Predicted = "Predicted",
    Employed = "Employed",
    Unemployed = "Unemployed"
  )

confusionM_tree_table


sensitivity<- confusionM_tree[1,1]/(confusionM_tree[1,1]+confusionM_tree[-1,1])
specificity<- confusionM_tree[-1,-1]/(confusionM_tree[-1,-1]+confusionM_tree[1,-1])
precision<- confusionM_tree[1,1]/(confusionM_tree[1,1]+confusionM_tree[1,-1])
F1_score<- 2*((precision*sensitivity)/(precision+sensitivity))


metrics_table_tree <- data.frame(
  Metric_TREE = c("Accuracy", "Sensitivity", "Specificity", "Precision", "F1 Score"),
  Value  = c(accuracy_1se, sensitivity, specificity, precision, F1_score)
)

metrics_table_tree %>% gt()


library(caret)

cm_tree<- confusionMatrix(
  factor(pred_1SE_tree, levels = c("Employed","Unemployed")),
  factor(test_data_c$Employment_Status, levels = c("Employed","Unemployed")),
  positive = "Employed"
)


#install.packages("PRROC")

count_classes<- train_data_c %>%
  count(Employment_Status)
#we see that there's 11650 students classified as employed and 5230 student classified as unemployed
#this show an imbalanced data, so using AUC-ROC leads to misleading incorrect interpretation
#Instead we'll use precision-recall for imbalanced datalibrary(PRROC)

pred_test_probability<- predict(tree_prune_1se, newdata = test_data_c, type = "prob") 
precision_recall_curve<- pr.curve(scores.class0 = pred_test_probability[test_data_c$Employment_Status=="Employed", "Employed"],
                                  scores.class1 = pred_test_probability[test_data_c$Employment_Status=="Unemployed", "Employed"],
                                  curve = TRUE)

plot(precision_recall_curve)

roc_curve<-roc.curve(scores.class0 = pred_test_probability[test_data_c$Employment_Status=="Employed", "Employed"],
                     scores.class1 = pred_test_probability[test_data_c$Employment_Status=="Unemployed", "Employed"],
                     curve = TRUE)
plot(roc_curve)
auc(roc_curve)

'''
we did model selection by accurcay, and now we interpret by proc, pr curve and the other etrics found y confusion matrix 
the recall is how many actual unemployed cases are correctly identified, precision is were correct fromt he predicted unemployed
the pr curve measures how well the model identifies employed, a value of 0.9827009 mens the model maintains very high precision and very high recall simultaneously
STRONG PERFORMANCE ont he positive class employed

Roc curve, we use recall of the y axis (porpotion of employed predicted as employed) and false positive rate FPR on the xaxis which is how many actuall negative(unemployed) the model inccorrectly label as positive
in other words it is the proportion of unemployed predicted as employed.

'''


###########################################################
###############################################################################################################################################

#We can RE-fit this Model including Continuing education in the Employment Status.

###########################################################################
##################                        #################################
##################           SVM          #################################
##################                        #################################
###########################################################################

library(MASS)
library(e1071)

Model_svm <- svm(Employment_Status ~. , data=train_data_c,  type="C-classification", kernel="linear", cost=1) # default
predsvm <- predict(Model_svm, newdata = test_data_c, type = "class")
SVM_acc<- mean(predsvm == test_data_c$Employment_Status)

#model_svm_guassian<- svm(Employment_Status ~. , data=train_data_c,  type="C-classification", kernel="radial", cost=1) 
#predsvm_guassian<- predict(model_svm_guassian, newdata = test_data_c, type = "class")
#mean(predsvm_guassian == test_data_c$Employment_Status)  #better accuracy 

'''
In a SVM you are searching for two things: a hyperplane with the largest minimum margin, and a hyperplane that correctly separates as many instances as possible. 
The problem is that you will not always be able to get both things. The c parameter determines how great your desire is for the latter. 
I want to have a good strong classification so high c , didnt work, took too much time to run
'''

#cost_range = c(0.01, 0.1, 1, 10)
#model_tune<- tune.svm(Employment_Status ~. , data=train_data_c, type="C-classification", kernel="linear", cost=cost_range, tune.control(cross = 5))
#best_svm <- model_tune$best.model
#predsvm_tune <- predict(best_svm, newdata = test_data_c, type = "class")
#accuracy_svm<- mean(predsvm_tune == test_data_c$Employment_Status)

confusionM_SVM<- table(Predicted=predsvm, Actual=test_data_c$Employment_Status)
confusionM_SVM

cm_SVM<- confusionMatrix(
  factor(predsvm, levels = c("Employed","Unemployed")),
  factor(test_data_c$Employment_Status, levels = c("Employed","Unemployed")),
  positive = "Employed"
)

comparison_metrics <- data.frame(Model = c("KNN", "Tree (1-SE rule)", "SVM"),
                                 Accuracy = c(cm_knn$overall["Accuracy"],
                                              cm_tree$overall["Accuracy"],
                                              cm_SVM$overall["Accuracy"]),
                                 Sensitivity = c(cm_knn$byClass["Sensitivity"],
                                                 cm_tree$byClass["Sensitivity"],
                                                 cm_SVM$byClass["Sensitivity"]),
                                 Specificity = c(cm_knn$byClass["Specificity"],
                                                 cm_tree$byClass["Specificity"],
                                                 cm_SVM$byClass["Specificity"]),
                                 Precision = c(cm_knn$byClass["Pos Pred Value"],
                                               cm_tree$byClass["Pos Pred Value"],
                                               cm_SVM$byClass["Pos Pred Value"]),
                                 F1 = c(cm_knn$byClass["F1"],
                                        cm_tree$byClass["F1"],
                                        cm_SVM$byClass["F1"]))
comparison_metrics %>% gt()

confusionM_SVM_table<- data.frame(
  Predicted=c("Employed","Unemployed"),
  Employed=c(confusionM_SVM[1,1], confusionM_SVM[2,1]),
  Unemployed=c(confusionM_SVM[1,2], confusionM_SVM[2,2])
) %>% gt() %>%
  tab_spanner(
    label = "Actual",
    
    columns = c("Employed","Unemployed")
  ) %>%
  cols_label(
    Predicted = "Predicted",
    Employed = "Employed",
    Unemployed = "Unemployed"
  )

confusionM_SVM_table


sensitivity<- confusionM_SVM[1,1]/(confusionM_SVM[1,1]+confusionM_SVM[-1,1])
specificity<- confusionM_SVM[-1,-1]/(confusionM_SVM[-1,-1]+confusionM_SVM[1,-1])
precision<- confusionM_SVM[1,1]/(confusionM_SVM[1,1]+confusionM_SVM[1,-1])
F1_score<- 2*((precision*sensitivity)/(precision+sensitivity))

metrics_table_SVM <- data.frame(
  Metric_SVM = c("Accuracy", "Sensitivity", "Specificity", "Precision", "F1 Score"),
  Value  = c(SVM_acc, sensitivity, specificity, precision, F1_score)
)

metrics_table_SVM %>% gt()

#guassian kernel, glmnet for lasso 

#polynomial kernel, taking a lot of time so ignore it, might also ignore the radial and focus on tuning the svm with linear? 



################################################################################################
##################                                               ###############################
##################   Logistic Regression- Employment Status      ###############################
##################                                               ###############################
################################################################################################


#
#  Which factors are the most important predictors of employment outcomes?


#fit a binary logistic regression for the employment status of the categories( employed, Unemployed)

data_emp_logistic<- data_emp %>%
  mutate(Employment_binary= (ifelse(Employment_Status=="Employed",1,0) )) %>%
  select(-Employment_Status)

set.seed(1)
n<- nrow(data_emp_logistic)
train_ind <- sample(1:n, size = 0.8*n)
train_data<- data_emp_logistic[train_ind, ]
test_data<- data_emp_logistic[-train_ind, ]
full_model<- glm(Employment_binary ~ . , data=train_data, family = binomial)
summary(full_model)

#apply  selection
set.seed(1)
model_AIC<- stepAIC(full_model, direction = "both")
summary(model_AIC)   #one time it is giving gender another time it is not
model_no_gender <- update(model_AIC, . ~ . - Gender)
anova(model_no_gender, model_AIC, test = "Chisq") #remove gender

model_AIC_no_gender<- model_no_gender
predict_AIC_logistic<- ifelse(predict(model_AIC_no_gender, newdata=test_data, type="response") >0.5, 1,0) #this gives probabilities 
#glm cant output classes it gives probabilities, so convert to classes 0 and 1
mean(predict_AIC_logistic==test_data$Employment_binary)

set.seed(1)
model_BIC<- step(full_model, k= log(nrow(train_data)), direction = "both")
summary(model_BIC)
predict_BIC_logistic<- ifelse(predict(model_BIC, newdata=test_data, type="response") >0.5, 1,0)

mean(predict_BIC_logistic==test_data$Employment_binary) #same model as AIC, same accuracy 

#same same for BIC AND AIC

#BIC BETTER CAUSE WE DIDNT HAVE TO REMOVE GENDER, CONTINUE WITH BIC

#####################################
#######   LASSO regression   ########

library(glmnet)

set.seed(1)
y<- data_emp_logistic$Employment_binary
x<- model.matrix(
  Employment_binary ~ . ,
  data = data_emp_logistic
) [, -1]


X_train <- x[train_ind, ]
Y_train <- y[train_ind ] #y is a vector and not a matrix

X_test<- x[-train_ind, ]
Y_test<- y[-train_ind ]

set.seed(1)
lambda_ridge<- cv.glmnet(X_train, Y_train, alpha=0)
model_ridge<- glmnet(X_train, Y_train, family = "binomial", alpha=0, lambda = lambda_ridge$lambda.min)
#TO PLOT
model_r<- glmnet(X_train, Y_train, family = "binomial", alpha=0)
plot(model_r, label = TRUE)

set.seed(1)
lambda_other<- cv.glmnet(X_train, Y_train, alpha=0.5)
model_other<- glmnet(X_train, Y_train, family = "binomial", alpha=0.5, lambda = lambda_other$lambda.min)
#TO PLOT
model_o<- glmnet(X_train, Y_train, family = "binomial", alpha=0.5)
plot(model_o,label = TRUE)

set.seed(1)
lambda_lasso<- cv.glmnet(X_train,Y_train,alpha=1)
model_lasso <- glmnet(X_train, Y_train, family = "binomial", alpha=1, lambda = lambda_lasso$lambda.min)
#TO PLOT
model_l <- glmnet(X_train, Y_train, family = "binomial", alpha=1)
plot(model_l, label = TRUE)

plot(lambda_lasso)



result_ridge<- predict(model_ridge, s= lambda_ridge$lambda.min, type="class", newx = X_test)
result_other<- predict(model_other, s= lambda_other$lambda.min, type="class", newx = X_test)
result_lasso<- predict(model_lasso, s= lambda_lasso$lambda.min, type="class", newx = X_test)


#values_ridge<- cbind(Y_test, result_ridge)
#values_other<- cbind(Y_test, result_other)
#values_lasso<- cbind(Y_test, result_lasso)


results_model <- data.frame(
  Model = c("Ridge", 
            "Other", 
            "Lasso"),
  
  Accuracy = c(
    mean(Y_test==result_ridge),
    mean(Y_test==result_other),
    mean(Y_test==result_lasso)
  )
) 

results_model %>% gt() 

coef(model_ridge)
coef(model_other)
coef_lasso<- coef(model_lasso)


#COMPARE LASSO AND BIC
model_BIC_LASSO <- data.frame(
  Model = c(
    "Lasso",
    "BIC"),
  
  Accuracy = c(
    mean(Y_test==result_lasso),
    mean(test_data$Employment_binary==predict_BIC_logistic)
  )
) 

model_BIC_LASSO %>% gt()  #why we get 0 accuracy for AIC? look at it

#log odds of the model_lasso
exp(coef(model_lasso)) # >1 increase odds, < 1 reduces odds of employability

#####################################################################
#################   MODEL AFTER removing variables seleted by LASSO

nonzero_coeff<- rownames(coef_lasso)[as.numeric(coef_lasso) !=0 ] #remove age

model_glm <- glm(
  Employment_binary ~ . -Age -Visa_Type,
  data = train_data,
  family = binomial
)
summary(model_glm)

#Gender seems to be insignificant, remove it and compare by anova to see 


# Remove Gender
model_no_gender <- glm(
  Employment_binary ~ . -Age -Visa_Type - Gender,
  data = train_data,
  family = binomial
)
summary(model_no_gender)
#H0: Gender is not significant
#H1: Gender is significant, choose model_glm
anova(model_no_gender, model_glm, test = "Chisq") #p value >0.05 then we don't reject H0, so remove Gender

model_no_field <- glm(
  Employment_binary ~ . -Age - Gender - Visa_Type -Field_of_Study,
  data = train_data,
  family = binomial
)
#H0: field is not significant
#H1: field is significant, 
anova(model_no_field, model_no_gender, test = "Chisq") #remove field, p value>0.05

summary(model_no_field)

model_no_country <- glm(
  Employment_binary ~ . -Age - Gender - Visa_Type -Field_of_Study -Country_of_Origin,
  data = train_data,
  family = binomial
)
#H0: country is not significant
#H1: country is significant, 
anova(model_no_country, model_no_field, test = "Chisq") #remove country, p value>0.05

summary(model_no_country)

final_Lasso_model<- model_no_country

################################
#######   INTERACTIONS   #######

interaction_model <- glm(
  Employment_binary ~ GPA +
    Internship_Experience +
    Education_Level +
    University_Ranking +
    Language_Proficiency +
    Years_Since_Graduation +
    GPA:Internship_Experience+
    Internship_Experience:University_Ranking + Education_Level:Language_Proficiency,
  data = train_data,
  family = binomial
)

summary(interaction_model)

anova(final_Lasso_model, interaction_model, test = "Chisq") #simpler model, then more complicated model
#This means the interaction model fits the training data significantly better than the simpler model.
AIC(final_Lasso_model, interaction_model)


predict_final<- ifelse( predict(final_Lasso_model, newdata = test_data, type="response") >0.5, 1, 0)
predict_interaction<- ifelse( predict(interaction_model, newdata = test_data, type="response") >0.5, 1, 0)
mean(predict_final==test_data$Employment_binary)  #same as the AIC after removing gender, same as BIC ONE
mean(predict_interaction==test_data$Employment_binary)

# the final selected model 

exp(coef(interaction_model))


library(caret)


confusionMatrix(
  factor(predict_interaction, levels = c(0,1)),
  factor(test_data$Employment_binary, levels = c(0,1)),
  positive = "1"
)


confusionMatrix(
  factor(predict_interaction, levels = c(0,1)),
  factor(test_data$Employment_binary, levels = c(0,1)),
  positive = "1"
)$table  #this gives confusion matrix, without the precision, f1 and others 

prob_interaction<- predict(interaction_model, newdata = test_data, type = "response")
library(PRROC)
pr_interaction<- pr.curve(scores.class0 = prob_interaction[test_data$Employment_binary==1],
                            scores.class1 = prob_interaction[test_data$Employment_binary==0],
                            curve = TRUE)
plot(pr_interaction)

roc_interaction<- roc.curve(scores.class0 = prob_interaction[test_data$Employment_binary==1],
                            scores.class1 = prob_interaction[test_data$Employment_binary==0],
                            curve = TRUE)
plot(roc_interaction)

#######################################################################################
##################                                      ###############################
##################   Multilinear Regression- Salary     ###############################
##################                                      ###############################
#######################################################################################

#only employed people
data_salary<- data_uk %>% filter(Salary!=0) %>%
  select( - Job_Sector, - Employment_Status, - Region_of_Study ) 


#using AIC
set.seed(1)
n<- nrow(data_salary)
train_ind_s <- sample(1:n, size = 0.8*n)
train_data_s<- data_salary[train_ind_s, ]
test_data_s<- data_salary[-train_ind_s, ]

fullmodel <- lm(Salary ~ . , data=train_data_s)
summary(fullmodel)

AIC_model<- stepAIC(fullmodel, direction = "backward")
summary(AIC_model)

predict_AIC<- predict(AIC_model, newdata = test_data_s)
mse_AIC<- mean((test_data_s$Salary- predict_AIC)^2)

par(mfrow=c(2,2))
plot(AIC_model, main="Before Log")


#############################
### LOG SALARY AIC

model_log_salary<- lm(log(Salary) ~ . , data=train_data_s) #salary i sright-skewed
AIC_log_salary<- stepAIC(model_log_salary, direction = "backward")
summary(AIC_log_salary)

predict_AIC_s_log<- exp(predict(AIC_log_salary, newdata = test_data_s))
mse_AIC_log<- mean((test_data_s$Salary- predict_AIC_s_log)^2)

par(mfrow=c(2,2))
plot(AIC_log_salary, main="After Log")

#continue with no log 

#if we do BIC no log, we get the same result as AIC no log
#If we do BIC with log it gives slightly different result than the AIC log


final_model_salary<- AIC_model

###################################################
############### Additive model
library(mgcv)
salary_add <- gam(
  Salary ~ s(GPA) + s(Years_Since_Graduation) + s(Age) + Internship_Experience +
    Education_Level + University_Ranking + Language_Proficiency + Field_of_Study + Gender+ Visa_Type +
    Country_of_Origin,
  data=train_data_s,
  method="ML"
)
par(mfrow=c(2,2))
plot(salary_add)


summary(salary_add)
salary_no_gender <-  gam(
  Salary ~ s(GPA) + s(Years_Since_Graduation) + s(Age) + Internship_Experience +
    Education_Level + University_Ranking + Language_Proficiency + Field_of_Study + Visa_Type +
    Country_of_Origin,
  data=train_data_s,
  method="ML"
)

summary(salary_no_gender)
#H0: Gender is not significant
#H1: Gender is significant, choose model_glm
anova(salary_no_gender, salary_add, test = "Chisq") #p value >0.05 then we don't reject H0, so remove Gender

salary_no_visa <- gam(
  Salary ~ s(GPA) + s(Years_Since_Graduation) + s(Age) + Internship_Experience +
    Education_Level + University_Ranking + Language_Proficiency + Field_of_Study + 
    Country_of_Origin,
  data=train_data_s,
  method="ML"
)
#H0: Visa is not significant
#H1: Visa is significant, 
anova(salary_no_visa, salary_no_gender, test = "Chisq") #remove visa, p value>0.05

summary(salary_no_visa)

salary_no_field <- gam(
  Salary ~ s(GPA) + s(Years_Since_Graduation) + s(Age) + Internship_Experience +
    Education_Level + University_Ranking + Language_Proficiency +
    Country_of_Origin,
  data=train_data_s,
  method="ML"
)
#H0: field is not significant
#H1: field is significant, 
anova(salary_no_field, salary_no_visa, test = "Chisq") #remove field, p value>0.05

summary(salary_no_field)

salary_no_country <- gam(
  Salary ~ s(GPA) + s(Years_Since_Graduation) + s(Age) + Internship_Experience +
    Education_Level + University_Ranking + Language_Proficiency ,
  data=train_data_s,
  method="ML"
)
#H0: country is not significant
#H1: country is significant, 
anova(salary_no_country, salary_no_field, test = "Chisq") #remove country, p value>0.05

summary(salary_no_country)

salary_no_years_age <- gam(
  Salary ~ s(GPA) + Internship_Experience +
    Education_Level + University_Ranking + Language_Proficiency ,
  data=train_data_s,
  method="ML"
)
#H0: country is not significant
#H1: country is significant, 
anova(salary_no_years_age, salary_no_country, test = "Chisq") #remove country, p value>0.05

summary(salary_no_years_age)

final_GAM_salary<- gam(
  Salary ~ s(GPA) + Internship_Experience +
    Education_Level + University_Ranking + Language_Proficiency ,
  data=train_data_s,
  method="REML"
)
summary(final_GAM_salary)


plot(final_GAM_salary)
plot(final_GAM_salary, residuals = TRUE)

# GAM predictions
predict_GAM <- predict(final_GAM_salary, newdata=test_data_s)

mse_GAM <- mean((test_data_s$Salary - predict_GAM)^2)

R2_AIC<- cor(test_data_s$Salary, predict_AIC)^2
R2_GAM<- cor(test_data_s$Salary, predict_GAM)^2
data.frame(
  Model = c("AIC Linear", "GAM"),
  RMSE = c(sqrt(mse_AIC), sqrt(mse_GAM)),
  R2=c(R2_AIC, R2_GAM)
)
#gam 5216.299 and AIC 5364.680
#GAM SHOWS higher R2 is better.


#TO DO LIST: fit roc for the employment, residual plots for salary, 
#ask does it matter which direction we use?
#update R 
