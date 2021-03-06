# k�t� huylu  meme kanseri karar a�a�lar� tahmin modeli
"Veri Seti Tan�t�m�:
id kolonu hastalar�n numaras�,diagnosis kolonundaki M ve B de�erleri M ise t�m�r�n k�t� huylu ,B ise t�m�r�n iyi huylu oldu�unu g�sterir.
di�er kolonlarda t�m�r�n �ap� vs gibi fiziksel �zellikleri yer al�r."
df <- breast.cancer
which(is.na(df)) # X kolonunda bo� de�erler var onlar� kald�r�yorum
df$X <- NULL
which(is.na(df)) #integer(0) sonucunu verdi yani veri setinde bo� de�er kalmad�
df$id <- NULL # hasta numaralar�na ihtiyac�m olmad��� i�in kald�r�yorum
library(rpart)
library(rattle)
library(caret)
# Train ve Test Ayr�m�
set.seed(165)
trainIndex <- sample(1:nrow(df),size = 0.8*nrow(df))
trainSet <- df[trainIndex, ]
testSet <- df[-trainIndex, ]
nrow(trainSet)
nrow(testSet)
table(testSet$diagnosis)
table(trainSet$diagnosis)
#Model Olu�turma
class(testSet$diagnosis)
class(trainSet$diagnosis) # ikisi de factor s�n�f�nda
modelEntropy <- rpart(diagnosis ~ . , data = trainSet,method = "class",
      parms = list(split = "information"))
modelGini <- rpart(diagnosis ~ . , data = trainSet, method = "class",
                   parms = list(split = "gini"))
modelEntropy
modelGini

#G�rselle�tirme
fancyRpartPlot(modelEntropy)
fancyRpartPlot(modelGini)

#Model Detaylar�
summary(modelEntropy)
summary(modelGini)

#Karar A�a�lar� Hiper Parametreleri
modelEntropyHyper <- rpart(diagnosis ~ . , data = trainSet,method = "class",
                      parms = list(split = "information"),
                      control = rpart.control(minsplit = 40,cp=0.02,maxdepth = 5))
modelEntropyHyper
## Tahmin
predModelEntropy <- predict(modelEntropy,testSet,type = "class")

predModelGini <- predict(modelGini,testSet, type = "class")
predModelEntropyHyper <- predict(modelEntropyHyper,testSet,type = "class")
confusionMatrix(predModelEntropy,testSet$diagnosis)
confusionMatrix(predModelEntropy,testSet$diagnosis,mode = "prec_recall")
confusionMatrix(predModelEntropy,testSet$diagnosis,mode = "prec_recall",positive = "M")

#Model Tuning
library(e1071)
trControl <- trainControl(method = "cv", number = 5,search="random")
trControl2 <- trainControl(method = "cv",number = 5,search = "grid")
modelCp <- train(diagnosis ~ . , data = trainSet,
                 method = "rpart",
                 tuneLength = 20,
                 trControl = trControl)
modelCp #complexity parameter � tune etmek i�in rpart metodunu kulland�m
modelMD <- train(diagnosis ~ . , data = trainSet,
                 method = "rpart2",
                 tuneLength = 20,
                 trControl = trControl)
modelMD #max tree depth=4
modelMDGrid <- train(diagnosis ~ . ,data = trainSet,
                     method = "rpart2",
                     tuneGrid = expand.grid(maxdepth = 3:20),
                     trControl = trControl2)
modelTuneMin <- tune.rpart(diagnosis ~ . , data = trainSet,
                           minsplit = 10:15,minbucket = 5:10,cp=seq(0.0,0.2,by=0.01))
modelTuneMin
#Tune Edilmi� Model �zerinden Tahminler
predMDGrid <- predict(modelMDGrid$finalModel, testSet,type = "class")
predCP <- predict(modelCp$finalModel,testSet,type = "class")
predMD <- predict(modelMD$finalModel,testSet,type = "class")
predMin <- predict(modelTuneMin$best.model,testSet,type = "class")

confusionMatrix(predMDGrid,testSet$diagnosis,mode = "prec_recall",positive = "M")
confusionMatrix(predCP,testSet$diagnosis,mode = "prec_recall",positive = "M")
confusionMatrix(predMD,testSet$diagnosis,mode = "prec_recall",positive = "M")
confusionMatrix(predMin,testSet$diagnosis,mode = "prec_recall",positive = "M")

#predCP en iyi k�t� huylu t�mor� taahmin eden model

