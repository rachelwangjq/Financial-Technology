## Initialization
rm(list = ls()) # Clear the memory
library("pROC") # The package needed for plotting ROC curves


## Load data
# Load the data from the data file. The first row is variable names.
# To avoid trouble we do not use "string as factor" option
Data_set <- read.csv("File1_IS_data.csv",header = TRUE) 

#check the data type of each column in the data set
str(Data_set)

# Transform "defaulted" into a binomial response, which gives 1 if defaulted or zero otherwise.
Data_set$default <- Data_set$default == "Defaulted" 


##Q1 
#First logistic regression
model1 <- glm(default ~ fico, family = "binomial", data = Data_set)
# family = "binominal" tells R to run a logistic regression.
summary(model1)

##Q2 
#a. Get the fitted default probability
Data_set$predicted_p <- predict(model1, type = "response")
#'type = "response"' tells R to give estimated probability of default directly

#b. Now we can draw a ROC curve.
pdf("Figure1.pdf", width = 6, height = 6) 
# This command allows you to output your figure as a pdf file directly. 
# The directory where the pdf will be saved is your working directory.
ROC1 <- roc(default ~ predicted_p, data = Data_set) # Calculate the ROC curve
plot(ROC1) # Plot the ROC curve
dev.off() # This command tells R to stop writing on the PDF

#c. calculate the area under the ROC curve
# With package pROC you can use the function auc()
auc(default ~ predicted_p, data = Data_set)

##d. Calculating the percentage of false positives and true positive with cut-off 0.1: 
#For any one who has estimated probability of default being 0.1, we label them as 'default'.
Data_set$Predicted_default <- Data_set$predicted_p > 0.1
Num_predicted_pos <- sum(Data_set$Predicted_default == TRUE)
Num_correct_pos_pred <- sum(Data_set$Predicted_default == TRUE & Data_set$default == TRUE)
true_positive <- Num_correct_pos_pred/Num_predicted_pos
false_positive <- 1-true_positive
#display the result for percentage of `correct' positive
cat("true positive rate:",true_positive)
cat("false positive rate:", false_positive)


##Q3. Out-of-sample Analysis
#a. Create a subsample data set with first 9000 samples. The remaining data goes to test data
Data_set_training <- Data_set[1:9000,]
Data_set_test <- Data_set[9001:nrow(Data_set),]
names(Data_set_test)
model2 <- glm(default ~ fico, family = "binomial", data = Data_set_training)
summary(model2)

#b. Compare the performance
Data_set_test$predicted_p = predict(model2, newdata = Data_set_test, type = "response")
pdf("Figure2.pdf", width = 6, height = 6)
# Calculate the ROC curve
ROC1 <- roc(default ~ predicted_p, data = Data_set) 
ROC2 <- roc(default ~ predicted_p, data = Data_set_test)

plot(ROC1, col = "red") # Plot the ROC curve, 'col = "red"' sets the color of the 
# curve to be red.
plot(ROC2, add = TRUE, col = "blue") # The argument 'add = TURE' makes sure that the curve is added
# to the original figure created by plot above, instead of replacing the old one.
dev.off()


##e. A more complicated model
model3 <- glm(default ~fico + loan_amnt + int_rate + verification_status, family = "binomial", data = Data_set)
summary(model3)
Data_set$predicted_p_new <- predict(model3, type = "response")

# Now we can draw a new ROC curve.
pdf("Figure3.pdf", width = 6, height = 6) # This command allows you to output
# your figure as a pdf file directly. The directory where the pdf will be saved 
# is your working directory.
ROC3 <- roc(default ~ predicted_p_new, data = Data_set) # Calculate the ROC curve
plot(ROC1, col = "red") # Plot the ROC curve
plot(ROC3, add = TRUE, col = "green")
dev.off() # This command tells R to stop writing on the PDF
