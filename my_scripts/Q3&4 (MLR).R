######################################
# 04.30.2017
# MLR of Mad, Max, and Myc
# BISC 577
######################################

## Install packages
# Bioconductor
source("https://bioconductor.org/biocLite.R")
biocLite()
# DNAshapeR
biocLite("DNAshapeR")
# Caret
install.packages("caret")

## Initialization
library(DNAshapeR)
library(caret)

## Predict DNA shapes
Mad_fasta <- "C:/Users/nodne/Desktop/BISC577/gcPBM/Mad.txt.fa"
Max_fasta <- "C:/Users/nodne/Desktop/BISC577/gcPBM/Max.txt.fa"
Myc_fasta <- "C:/Users/nodne/Desktop/BISC577/gcPBM/Myc.txt.fa"
Mad_pred <- getShape(Mad_fasta)
Max_pred <- getShape(Max_fasta)
Myc_pred <- getShape(Myc_fasta)

## Encode feature vectors
featureType <- c("1-mer")
Mad_1_mer <- encodeSeqShape(Mad_fasta, Mad_pred, featureType)
Max_1_mer <- encodeSeqShape(Max_fasta, Max_pred, featureType)
Myc_1_mer <- encodeSeqShape(Myc_fasta, Myc_pred, featureType)

featureType <- c("1-mer", "1-shape")
Mad_1_mer_shape <- encodeSeqShape(Mad_fasta, Mad_pred, featureType)
Max_1_mer_shape <- encodeSeqShape(Max_fasta, Max_pred, featureType)
Myc_1_mer_shape <- encodeSeqShape(Myc_fasta, Myc_pred, featureType)

## Build MLR model by using Caret
# Data preparation
Mad_exp <- "C:/Users/nodne/Desktop/BISC577/gcPBM/Mad.txt"
Max_exp <- "C:/Users/nodne/Desktop/BISC577/gcPBM/Max.txt"
Myc_exp <- "C:/Users/nodne/Desktop/BISC577/gcPBM/Myc.txt"
Mad_data <- read.table(Mad_exp)
Max_data <- read.table(Max_exp)
Myc_data <- read.table(Myc_exp)
Mad_1_mer_df <- data.frame(affinity=Mad_data$V2, Mad_1_mer)
Max_1_mer_df <- data.frame(affinity=Max_data$V2, Max_1_mer)
Myc_1_mer_df <- data.frame(affinity=Myc_data$V2, Myc_1_mer)
Mad_1_mer_shape_df <- data.frame(affinity=Mad_data$V2, Mad_1_mer_shape)
Max_1_mer_shape_df <- data.frame(affinity=Max_data$V2, Max_1_mer_shape)
Myc_1_mer_shape_df <- data.frame(affinity=Myc_data$V2, Myc_1_mer_shape)

# Arguments setting for Caret
trainControl <- trainControl(method = "cv", number = 10, savePredictions = TRUE)

# Prediction with L2-regularized
Mad_1_mer_model <- train(affinity~., data = Mad_1_mer_df, trControl=trainControl, 
                      method = "glmnet", tuneGrid = data.frame(alpha = 0, lambda = c(2^c(-15:15))))
Max_1_mer_model <- train(affinity~., data = Max_1_mer_df, trControl=trainControl, 
                         method = "glmnet", tuneGrid = data.frame(alpha = 0, lambda = c(2^c(-15:15))))
Myc_1_mer_model <- train(affinity~., data = Myc_1_mer_df, trControl=trainControl, 
                         method = "glmnet", tuneGrid = data.frame(alpha = 0, lambda = c(2^c(-15:15))))
Mad_1_mer_shape_model <- train(affinity~., data = Mad_1_mer_shape_df, trControl=trainControl, 
                         method = "glmnet", tuneGrid = data.frame(alpha = 0, lambda = c(2^c(-15:15))))
Max_1_mer_shape_model <- train(affinity~., data = Max_1_mer_shape_df, trControl=trainControl, 
                         method = "glmnet", tuneGrid = data.frame(alpha = 0, lambda = c(2^c(-15:15))))
Myc_1_mer_shape_model <- train(affinity~., data = Myc_1_mer_shape_df, trControl=trainControl, 
                         method = "glmnet", tuneGrid = data.frame(alpha = 0, lambda = c(2^c(-15:15))))

# View results
Mad_1_mer_model
Max_1_mer_model
Myc_1_mer_model
Mad_1_mer_shape_model
Max_1_mer_shape_model
Myc_1_mer_shape_model


