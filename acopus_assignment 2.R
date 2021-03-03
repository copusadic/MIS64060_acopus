

#######working directory and load data##########

##set working directory### THIS SCRIPT CREATES A TEXT FILE - CHECK YOUR WD###
setwd("C:/Users/copus/Desktop/Classes/Machine Learning/Assignments/Assignment 2")

##import data
UBData <- read.csv('C:/Users/copus/Desktop/Classes/Machine Learning/Assignments/Assignment 2/UniversalBank.csv')




########load all the libraries######


library(tidyverse)  ##can you even function without tidyverse
library(caret) ##knn 
library(class)  ##knn
library(gmodels) ##model fitting
library(ggplot2) ##there's a plot later
library(psych) ##because why not
library(dplyr) ##something didn't run correctly unless I installed this???

#######part1,2,3##############################


##dummy coding for three levels
Education <- as.data.frame(dummy.code(UBData$Education))
head(Education)
Education <- rename(Education, Education_1 ="1")
Education <- rename(Education, Education_2 = "2")
Education <- rename(Education, Education_3 = "3" )
head(Education)

##update UB dataframe
UBData <- cbind(UBData, Education)
head(UBData)

##subset data
UB <- subset.data.frame(UBData, select = -c(ID, ZIP.Code, Education))
head(UB) #look at the data

loan_outcome <- UB%>% select(Personal.Loan)#create new df for outcome
UB <- UB%>% select(-Personal.Loan)#remove outcome from df

#make PL a factor
loan_outcome[,'Personal.Loan'] <- factor(loan_outcome[,'Personal.Loan']) #outcome = factor
class(loan_outcome$Personal.Loan) #verify previous action


## add df with new data person
new.df <-data.frame(Age = 40, Experience = 10, Income = 84, Family = 2, CCAvg = 2, Education_1 = 0, Education_2 = 1, Education_3 = 0, Mortgage = 0, Securities.Account = 0, CD.Account = 0, Online = 1, CreditCard = 1.) #create df with new customer

##start kNN -train/test sets
set.seed(123)

##60% of sample
smp_size <- floor(0.60*nrow(UB))
train_ind <- sample(seq_len(nrow(UB)), size = smp_size)

##create test and train sets
loan_pred_train <- UB[train_ind, ]
loan_pred_test <- UB[-train_ind, ]

##split outcome variable
loan_outcome_train <- loan_outcome[train_ind, ]
loan_outcome_test <- loan_outcome[-train_ind, ]

###Run kNN classification
loan_pred_knn <- knn(train = loan_pred_train, test = loan_pred_test, cl = loan_outcome_train, k=1)

##loan outcome test into dataframe
loan_outcome_test <- data.frame(loan_outcome_test)

# merge "loan_pred_knn" and "loan_outcome_test" 
loan_comparison <- data.frame(loan_pred_knn, loan_outcome_test)

# specify column names for "loan_comparison"
names(loan_comparison) <- c("PredictedLoan", "ObservedLoan")

# inspect "loan_comparison" 
head(loan_comparison)

# create table examining model accuracy
CrossTable(x = loan_comparison$ObservedLoan, y = loan_comparison$PredictedLoan, prop.chisq=FALSE, prop.c = FALSE, prop.r = FALSE, prop.t = FALSE)

##same thing different package
loan_pred_caret <- train(loan_pred_train, loan_outcome_train, method = "knn", preProcess = c("center","scale"))
loan_pred_caret

#plot(loan_pred_caret) #plot pretty line
ggplot(data=loan_pred_caret, aes(x ="#neighbors", y= Accuray(Bootstrap), group = 1))+
         geom_line(color = "royalblue3", size = 1.25)+
         geom_point()+
  xlim(5, 9)+
  theme_classic(base_size = 14)





knnPredict <- predict(loan_pred_caret, newdata = loan_pred_test) 
confusionMatrix(knnPredict, loan_outcome_test$loan_outcome_test) #confusion matrix


###

nn <- knn (train = UB[,1:13], test = new.df,
           cl = loan_outcome[,1],k=1,
           prob =TRUE,
           use.all = TRUE)
nn

knn.pred.new <- knn(UB[,1:13], new.df,
                    cl = loan_outcome [,1],k=1,
                    prob = TRUE,
                    use.all = TRUE)
row.names(UB)[attr(UB, "nn.index")]

knn.pred.new

#######part 4###########################
new4.df <-data.frame(Age = 40, Experience = 10, Income = 84, Family = 2, CCAvg = 2, Education_1 = 0, Education_2 = 1, Education_3 = 0, Mortgage = 0, Securities.Account = 0, CD.Account = 0, Online = 1, CreditCard = 1.) #create df with new customer


knnPredict <- predict(loan_pred_caret, newdata = loan_pred_test) 
confusionMatrix(knnPredict, loan_outcome_test$loan_outcome_test) #confusion matrix


###

nn <- knn (train = UB[,1:13], test = new4.df,
           cl = loan_outcome[,1],k=9,
           prob =TRUE,
           use.all = TRUE)
nn

knn.pred.new <- knn(UB[,1:13], new4.df,
                    cl = loan_outcome [,1],k=9,
                    prob = TRUE,
                    use.all = TRUE)
row.names(UB)[attr(UB, "nn.index")]

knn.pred.new




#######part 5################################
set.seed(123)

UB1 <- UBData
UB1 <- subset.data.frame(UBData, select = -c(ID, ZIP.Code, Education))


loan_outcome1 <- UB1%>% select(Personal.Loan)#create new df for outcome
UB2 <- UB1 %>% relocate(Personal.Loan, .after=last_col())
head(UB2) #look at the data


Test_Index = createDataPartition(UB2$Personal.Loan,p=0.2, list=FALSE) # 20% reserved for Test
Test_Data = UB2[Test_Index,]
TraVal_Data = UB2[-Test_Index,] # Validation and Training data is rest
Train_Index = createDataPartition(TraVal_Data$Personal.Loan,p=0.50, list=FALSE) # 50% of remaining data as training
Train_Data = TraVal_Data[Train_Index,]
Validation_Data = TraVal_Data[-Train_Index,] # rest as validation
summary(Train_Data)
summary(Validation_Data)
summary(Test_Data)



## Normalization
# Copy the original data
train.norm.df <- Train_Data
valid.norm.df <- Validation_Data
traval.norm.df <- TraVal_Data
# use preProcess() from the caret package to normalize Age, Exp, Income.
norm.values <- preProcess(Train_Data[, 1:3], method=c("center", "scale"))
train.norm.df[, 1:3] <- predict(norm.values, Train_Data[, 1:3]) # Replace first 3 columns with normalized values
valid.norm.df[, 1:3] <- predict(norm.values, Validation_Data[, 1:3])
traval.norm.df[, 1:3] <- predict(norm.values, traval.norm.df[, 1:3])
test.norm.df <- predict(norm.values, Test_Data[, 1:14])
summary(train.norm.df)
var(train.norm.df[, 1:3])
summary(valid.norm.df)
var(valid.norm.df[, 1:3])


###Run kNN classification
loan_pred_knn1 <- knn(train = traval.norm.df, test = test.norm.df, cl = traval.norm.df$Personal.Loan, k=9)

##loan outcome test into dataframe
loan_outcome_test1 <- data.frame(loan_outcome_test1)

# merge "loan_pred_knn" and "loan_outcome_test" 
loan_comparison1 <- data.frame(loan_pred_knn1, loan_outcome_test1)

# specify column names for "loan_comparison"
names(loan_comparison1) <- c("PredictedLoan", "ObservedLoan")

# inspect "loan_comparison" 
head(loan_comparison1)


CrossTable(x = loan_comparison1$ObservedLoan, y = loan_comparison1$PredictedLoan, prop.chisq=FALSE, prop.c = FALSE, prop.r = FALSE, prop.t = FALSE)


###############Assignment Answers############
sink("./acopus_assignment2.txt", append = T)

print("Amy D Copus, Assignment 2")
print("Question 1.:")
knn.pred.new
print("This new customer would be classified as 0, not accepting the loan.")
print(" ")
print("Question 2.:")
loan_pred_caret
print("The choice of k that balances between overfitting and ignoring predictor information is 9.")
print(" ")
print("Question 3.:")
CrossTable(x = loan_comparison$ObservedLoan, y = loan_comparison$PredictedLoan, prop.chisq=FALSE, prop.c = FALSE, prop.r = FALSE, prop.t = FALSE)
print(" ")
print("Question 4.:")
knn.pred.new
print("The 2nd new customer would be classified as 0, not accepting the loan.")
print(" ")
print("Question 5.:")
CrossTable(x = loan_comparison1$ObservedLoan, y = loan_comparison1$PredictedLoan, prop.chisq=FALSE, prop.c = FALSE, prop.r = FALSE, prop.t = FALSE)
print("The original model had a True Postive (TP) of 80 and a False Negative (FN) of 108 while the second model only had a TP of 10 and a FN of 178. The accuracy of the original model was (1722+80)/2000=.901 while the accuracy of the second model was (1714+10)/2000=.862.The recall or sensitivity of the original model was 1722/(1722+108)=.941 and the recall of the second model was 1714/(1714+178)=.906. The specificity of the original model was 80/(80+90)=.471 and the specificity of the second model was 10/(10+98)=.093. The confusion matrix of the train-valid-test method seems to somehow be a worse model than the confusion matrix of the train-test method. ")
sink() ##stop recording output##




########end of project######