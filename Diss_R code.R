#dissertation topic 

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
