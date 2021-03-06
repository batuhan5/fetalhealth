#multinomial logistic regresyon 
""
df <- fetal_health
names(df)
any(is.na(df))# bo� de�er yok
library(nnet)
library(tidyverse)
table(df$fetal_health)
df <- df %>% mutate(
  histogram_number_of_zeroes = as.factor(histogram_number_of_zeroes),
  histogram_tendency = as.factor(histogram_tendency),
  fetal_health = as.factor(fetal_health)
)
View(df)
class(df$fetal_health) # factor
names(table(df$fetal_health))
# Train ve Test ayr�m�
trainTestSplit <- function(data,dvName,seed){
  tbl <- table(data[,dvName])
  classes <- names(tbl)
  minClass <- min(tbl)
  lengthClass <- length(tbl)  
  train <- data.frame()  
  test <- data.frame()
  for (i in 1:lengthClass) {
    selectedClass <- data[,dvName] == classes[i]
    set.seed(seed)
    sampleIndex <- sample(1:nrow(data[selectedClass, ]),size = minClass*0.8)
    train <- rbind(train, data[selectedClass, ][sampleIndex, ])
    test <- rbind(test , data[selectedClass, ][-sampleIndex, ])
    }
  return(list(train,test))
}
train <- trainTestSplit(df,"fetal_health",125)[[1]]
test <- trainTestSplit(df,"fetal_health",125)[[2]]
table(train$fetal_health)
table(test$fetal_health)
#Ke�if�i veri analizi
"korelasyon testi yerine ki-kare testi kulland�m ��nk� kategorik verileri s�rekli verilerle tahmin etmeye �al���yorum"
"chisq hipotezleri:
Ho:bu de�i�kenler birbirinden ba��ms�zd�r
Ha:bu de�i�kenler birbirine ba�l�d�r."
#fetal_movement
chisq.test(table(train$fetal_health,train$fetal_movement))#p-value 0.05ten k���k
#yani de�i�kenler birbirine ba�l�
#accelerations
chisq.test(table(train$fetal_health,train$accelerations))#p-value 0.05ten k���k
#prolongued decelerations
chisq.test(table(train$fetal_health,train$prolongued_decelerations))#p-value 0.05ten k���k
#uterine_contractions
chisq.test(table(train$fetal_health,train$uterine_contractions))#p-value 0.05ten k���k
#Multinomial logistic regression
library(nnet)
library(tidyverse)
library(e1071)
library(caret)
modelBase <- multinom(fetal_health ~ . , data = train)
summary(modelBase)
modelBase$fitted.values
modelBase$decay
#Farkl� Model Kar��la�t�rmalar�
model2 <- multinom(fetal_health ~ accelerations +fetal_movement+uterine_contractions+
        light_decelerations+severe_decelerations+prolongued_decelerations, data = train)
summary(model2)
summary(modelBase)

#Model �zerinden Tahminler
caret::varImp(modelBase)
predModelBase <- predict(modelBase,test)
caret::confusionMatrix(predModelBase,test$fetal_health,mode = "prec_recall")
#Oran E�itlemeleri
testOranlarEsit <- data.frame()
View(test)
table(test$fetal_health)
test[test$fetal_health == "2", ]
sampleIndex_0 <- sample(1:nrow(test[test$fetal_health == "2", ]), size = 40)
sampleIndex_2 <- sample(1:nrow(test[test$fetal_health == "3", ]),size = 40)
testOranlarEsit <- rbind(testOranlarEsit, test[test$fetal_health == "2", ][sampleIndex_0 , ])
testOranlarEsit <- rbind(testOranlarEsit, test[test$fetal_health == "3", ][sampleIndex_2 , ])
testOranlarEsit <- rbind(testOranlarEsit, test[test$fetal_health == "1", ][sampleIndex_0 , ])
table(testOranlarEsit$fetal_health)
predModelBaseOR <- predict(modelBase, testOranlarEsit)
caret::confusionMatrix(predModelBaseOR,testOranlarEsit$fetal_health,mode = "prec_recall")

