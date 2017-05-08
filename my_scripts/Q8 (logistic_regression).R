######################################
# 5.01.2017
# Logistic regression on ChIP-seq data
# BISC 577
######################################

## Install packages
install.packages("caret")
install.packages("e1071")
install.packages("ROCR")
source("https://bioconductor.org/biocLite.R")
biocLite("Biostrings")


## Initialization
library(DNAshapeR)
library(caret)
library(ROCR)
library(Biobase)
library(Biostrings)
workingPath <- "C:/Users/nodne/Desktop/BISC577/CTCF/"

## Generate data for the classifcation (assign Y to bound and N to non-bound)
# bound
boundFasta <- readDNAStringSet(paste0(workingPath, "bound_30.fa"))
sequences <- paste(boundFasta)
boundTxt <- data.frame(seq=sequences, isBound="Y")

# non-bound
nonboundFasta <- readDNAStringSet(paste0(workingPath, "unbound_30.fa"))
sequences <- paste(nonboundFasta)
nonboundTxt <- data.frame(seq=sequences, isBound="N")

# merge two datasets
writeXStringSet( c(boundFasta, nonboundFasta), paste0(workingPath, "ctcf.fa"))
exp_data <- rbind(boundTxt, nonboundTxt)


## DNAshapeR prediction
pred <- getShape(paste0(workingPath, "ctcf.fa"))


## Encode feature vectors
seqFeatures <- c("1-mer")
seqShapeFeatures <- c("1-mer", "1-shape")
seqVector <- encodeSeqShape(paste0(workingPath, "ctcf.fa"), pred, seqFeatures)
seqShapeVector <- encodeSeqShape(paste0(workingPath, "ctcf.fa"), pred, seqShapeFeatures)
seqDf <- data.frame(isBound = exp_data$isBound, seqVector)
seqShapeDf <- data.frame(isBound = exp_data$isBound, seqShapeVector)

## Logistic regression
# Set parameters for Caret
trainControl <- trainControl(method = "cv", number = 10, 
                             savePredictions = TRUE, classProbs = TRUE)

# Perform prediction
seqModel <- train(isBound~ ., data = seqDf, trControl = trainControl,
               method = "glm", family = binomial, metric ="ROC")
summary(seqModel)

seqShapeModel <- train(isBound~ ., data = seqShapeDf, trControl = trainControl,
                       method = "glm", family = binomial, metric ="ROC")
summary(seqShapeModel)

## Plot AUROC
seqPrediction <- prediction( seqModel$pred$Y, seqModel$pred$obs )
seqPerformance <- performance( seqPrediction, "tpr", "fpr" )
plot(seqPerformance) + title("sequence")

seqShapePrediction <- prediction( seqShapeModel$pred$Y, seqShapeModel$pred$obs )
seqShapePerformance <- performance( seqShapePrediction, "tpr", "fpr" )
plot(seqShapePerformance) + title("sequence + shape")

## Caluculate AUROC
seqAuc <- performance(seqPrediction, "auc")
seqAuc <- unlist(slot(seqAuc, "y.values"))
seqAuc

seqShapeAuc <- performance(seqShapePrediction, "auc")
seqShapeAuc <- unlist(slot(seqShapeAuc, "y.values"))
seqShapeAuc
