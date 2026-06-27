#dissertation topic 

library(MASS)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(skimr)
library(gt)
library(moderndive)
library(gapminder)
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



###########################################################################

#Maybe in the future the dataset can be cleaned to 
emplyement_data <- data_uk %>%
  filter((Employment_Status == "Employed" | Employment_Status=="Unemployed"))


#basic EDA to explore Data. 


###########################################################################
###########################################################################
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

KNN computes distance for every K, each time it compute the distance between the validation points
and all training points. We will be doing this for 100 K values and 10 folds, so we will be training 1000 KNN models. 
(for each K value, we train 10 models)
'''

#filter data
data_emp<- data_uk %>% 
  filter(Employment_Status!="Continuing Education") %>%
  select(-Salary, -Job_Sector,-Region_of_Study)

#Response and predictor variables
Y<- as.factor(data_emp$Employment_Status)
x<- data_emp %>% select(-Employment_Status, -Region_of_Study)

#change categorical variables to dummy variables
X_num <- model.matrix(~ ., data = x)[,-1]
X_scaled<- scale(X_num) #scaling

library(class)

set.seed(1)

# splitting into 10 folds

n <- nrow(X_scaled)
fold_indices <- sample(rep(1:5, length.out = n))
folds <- split(1:n, fold_indices)

K_vals <- 1:50 #LIST OF 100 k values, SHOULD I CHOOSE  A DIFFERENT NB 
cv_acc <- numeric(length(K_vals)) #vector of 100 values

for (i in seq_along(K_vals)) { #for each value of k 
  
  k_val <- K_vals[i]
  fold_acc <- numeric(5) #vector of length 10 
  
  for (f in 1:5) { #for each fold
    
      
    # Validation indices
    valid_ind <- folds[[f]] # fold f is the validation,the rest are training 
    train_ind <- setdiff(1:n, valid_ind)
    
    # Split the data
    X_train_fold <- X_scaled[train_ind, ] #train
    y_train_fold <- Y[train_ind]
    
    X_valid_fold <- X_scaled[valid_ind, ]  #test 
    y_valid_fold <- Y[valid_ind]
    
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

'''
very computationally expensive, and takes  lot of time, so we try leave-on-out CV where for each K value we fit only 1 model

library(class)

K_vals <- 1:100
cv_acc <- numeric(length(K_vals))

for (i in seq_along(K_vals)) {
  
  k_val <- K_vals[i]
  
  pred <- knn.cv(train = X_scaled, cl = Y, k = k_val)
  
  cv_acc[i] <- mean(pred == Y)
}

this also took so long and didnt run 




###########################################################################
##################                          ###############################
##################   Classification tree    ###############################
##################                          ###############################
###########################################################################


employement with 3 categorical values is used, cause when we filter cotinuing eductaion out, the tree consider 
salary as the only predictor resulting ina  tree of only 1 split

'''

library(rpart)
install.packages("rpart.plot")
library(rpart.plot)


#first we split the dataset
set.seed(1)
n <- nrow(data_emp)
train_index <- sample(1:n, size = 0.7*n)

train_data <- data_emp[train_index, ]
test_data  <- data_emp[-train_index, ]

train_data$Employment_Status <- as.factor(train_data$Employment_Status)

tree <- rpart(Employment_Status ~. , data=train_data, method="class",cp=0)
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
pred1 <- predict(tree_prune, newdata = test_data, type = "class")
mean(pred1 == test_data$Employment_Status)
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
pred2 <- predict(tree_prune_1se, newdata = test_data, type = "class")
mean(pred2 == test_data$Employment_Status)
#this 1SE gives a less crowded tree

###############################

library(randomForest)
# bagging

Model <- randomForest(Employment_Status ~. , data=train_data, mtry= ncol(train_data)-1,ntree=500) # nb of predictors 
#Predict and evaluate perfromance
pred3 <- predict(Model, newdata = test_data, type = "class")
mean(pred3 == test_data$Employment_Status)
# random forests
Model2 <- randomForest(Employment_Status ~. , data=train_data)
#Predict and evaluate perfromance
pred4 <- predict(Model2, newdata = test_data, type = "class")
mean(pred4 == test_data$Employment_Status)

results <- data.frame(
  Model = c("Tree (min xerror)", 
            "Tree (1-SE rule)", 
            "Bagging", 
            "Random Forest"),
  
  Accuracy = c(
    mean(pred1 == test_data$Employment_Status),
    mean(pred2 == test_data$Employment_Status),
    mean(pred3 == test_data$Employment_Status),
    mean(pred4 == test_data$Employment_Status)
  )
) 

results %>% gt() 

Pred_tree<- mean(pred1 == test_data$Employment_Status)

#We can RE-fit this Model including Continuing education in the Employment Status.

###########################################################################
##################                        #################################
##################           SVM          #################################
##################                        #################################
###########################################################################

library(MASS)
library(e1071)
Model_svm <- svm(Employment_Status ~. , data=train_data,  type="C-classification", kernel="linear", cost=1)
predsvm <- predict(Model_svm, newdata = test_data, type = "class")
mean(predsvm == test_data$Employment_Status)

cost_range <- c(0.1,0.5,1,2,5,10)
model_tune<- tune.svm(Employment_Status ~. , data=train_data, type="C-classification", kernel="linear", cost=cost_range)
best_svm <- model_tune$best.model
predsvm_tune <- predict(best_svm, newdata = test_data, type = "class")
mean(predsvm_tune == test_data$Employment_Status)

#took ages to run 

#TDL: Compare all Classification models, fit the logistic regression model, fit GAM/ linear model
#maybe try different performance metrics? or no need? ROC?
#confusion matrix + ROC,
#class specific performance : specificty.....
#accuracy mfor most accurate model
