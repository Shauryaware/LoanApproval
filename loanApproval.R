loan_data <- read.csv("loandataset.csv")#Importing the dataset
#Understanding the dataset paramters
head(loan_data)
str(loan_data)
dim(loan_data)
summary(loan_data)
#target data is loan_status and predictor are all the other features
#Checking for missing data (loan amount,loan amount term and credit history has missing values)
colSums(is.na(loan_data))
#Checking the type of data 
summary(loan_data$loanamount)
summary(loan_data$loan_amount_term)
table(loan_data$credit_history, useNA = "ifany")
#Replacing the missing value with numerical value to prevent errors and estimate a typical value
# Handle missing values in loanamount with median value(mean gets more affected by outliers more)
loan_data$loanamount[is.na(loan_data$loanamount)] <-
  median(loan_data$loanamount, na.rm = TRUE)

# Handle missing values in loan_amount_term
loan_data$loan_amount_term[is.na(loan_data$loan_amount_term)] <-
  median(loan_data$loan_amount_term, na.rm = TRUE)

# Handle missing values in credit_history(since it is binary and 1s are more so using mode putting 1 in missing places)
loan_data$credit_history[is.na(loan_data$credit_history)] <- 1
#Checking for duplicate records(no duplicate data)
sum(duplicated(loan_data))
# Removing the loan ID because it is only an identifier
loan_data$loan_id <- NULL
# Converting categorical variables to factors(factors are r's way of representing categorical data(fixed set of levels))
loan_data$gender <- as.factor(loan_data$gender)
loan_data$married <- as.factor(loan_data$married)
loan_data$dependents <- as.factor(loan_data$dependents)
loan_data$education <- as.factor(loan_data$education)
loan_data$self_employed <- as.factor(loan_data$self_employed)
loan_data$property_area <- as.factor(loan_data$property_area)
loan_data$loan_status <- as.factor(loan_data$loan_status)
levels(loan_data$gender)#Checking an example of factors
# Using boxplot to check the variance of date and check for extreme outliers(
boxplot(loan_data$applicantincome,
        main = "Applicant Income")

boxplot(loan_data$coapplicantincome,
        main = "Coapplicant Income")

boxplot(loan_data$loanamount,
        main = "Loan Amount")

boxplot(loan_data$loan_amount_term,
        main = "Loan Amount Term")
# Splitting data into training and testing sets(Doing this so test data is not involved in scaling)
set.seed(123)

train_index <- sample(
  1:nrow(loan_data),
  size = 0.8 * nrow(loan_data)
)

train_data <- loan_data[train_index, ]
test_data <- loan_data[-train_index, ]

# Checking dimensions
dim(train_data)
dim(test_data)
# Calculating scaling parameters from training data

applicant_mean <- mean(train_data$applicantincome)
applicant_sd <- sd(train_data$applicantincome)

coapplicant_mean <- mean(train_data$coapplicantincome)
coapplicant_sd <- sd(train_data$coapplicantincome)

loanamount_mean <- mean(train_data$loanamount)
loanamount_sd <- sd(train_data$loanamount)
# Calculating scaling parameters from training data
applicant_mean <- mean(train_data$applicantincome)
applicant_sd <- sd(train_data$applicantincome)

coapplicant_mean <- mean(train_data$coapplicantincome)
coapplicant_sd <- sd(train_data$coapplicantincome)

loanamount_mean <- mean(train_data$loanamount)
loanamount_sd <- sd(train_data$loanamount)
# Scaling training data
train_data$applicantincome <-
  (train_data$applicantincome - applicant_mean) / applicant_sd

train_data$coapplicantincome <-
  (train_data$coapplicantincome - coapplicant_mean) / coapplicant_sd

train_data$loanamount <-
  (train_data$loanamount - loanamount_mean) / loanamount_sd
# Scaling testing data using training parameters

test_data$applicantincome <-
  (test_data$applicantincome - applicant_mean) / applicant_sd

test_data$coapplicantincome <-
  (test_data$coapplicantincome - coapplicant_mean) / coapplicant_sd

test_data$loanamount <-
  (test_data$loanamount - loanamount_mean) / loanamount_sd
# Encoding categorical predictor variables(Using dummy encoding to encode the categorical data)

train_encoded <- model.matrix(
  ~ gender + married + dependents + education +
    self_employed + property_area,
  data = train_data
)[, -1]

test_encoded <- model.matrix(
  ~ gender + married + dependents + education +
    self_employed + property_area,
  data = test_data
)[, -1]
# Creating final training predictor dataset

train_final <- cbind(
  train_encoded,
  train_data[, c(
    "applicantincome",
    "coapplicantincome",
    "loanamount",
    "loan_amount_term",
    "credit_history"
  )]
)

# Creating final testing predictor dataset
#Merging the numerical data with the categorical data
test_final <- cbind(
  test_encoded,
  test_data[, c(
    "applicantincome",
    "coapplicantincome",
    "loanamount",
    "loan_amount_term",
    "credit_history"
  )]
)
# Separating target variable

y_train <- train_data$loan_status
y_test <- test_data$loan_status
# Creating training data

train_model_data <- data.frame(
  train_final,
  loan_status = y_train
)

# Building logistic regression model

model <- glm(
  loan_status ~ .,
  data = train_model_data,
  family = binomial
)

# Displaying model summary

summary(model)


# Predicting probabilities on test data

probabilities <- predict(
  model,
  newdata = data.frame(test_final),
  type = "response"
)

# Converting probabilities into classes

predicted_class <- ifelse(
  probabilities >= 0.5,
  levels(y_train)[2],
  levels(y_train)[1]
)

# Converting predictions to factor

predictions <- factor(
  predicted_class,
  levels = levels(y_test)
)

# Displaying predictions

head(predictions)


# Creating confusion matrix

confusion_matrix <- table(
  Actual = as.character(y_test),
  Predicted = as.character(predictions)
)

print(confusion_matrix)


# Calculating accuracy

accuracy <- mean(
  as.character(predictions) ==
    as.character(y_test)
)

cat("Accuracy:", accuracy, "\n")


# Getting class names

class_names <- levels(y_test)

positive_class <- class_names[2]
negative_class <- class_names[1]


# Calculating true positive

TP <- confusion_matrix[
  positive_class,
  positive_class
]

# Calculating true negative

TN <- confusion_matrix[
  negative_class,
  negative_class
]

# Calculating false positive

FP <- confusion_matrix[
  negative_class,
  positive_class
]

# Calculating false negative

FN <- confusion_matrix[
  positive_class,
  negative_class
]


# Calculating precision

if ((TP + FP) == 0) {
  precision <- 0
} else {
  precision <- TP / (TP + FP)
}


# Calculating recall

if ((TP + FN) == 0) {
  recall <- 0
} else {
  recall <- TP / (TP + FN)
}


# Calculating F1 score

if ((precision + recall) == 0) {
  f1_score <- 0
} else {
  f1_score <- 2 * precision * recall /
    (precision + recall)
}


# Displaying performance measures

cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1 Score:", f1_score, "\n")
# Creating a new applicant

new_applicant <- data.frame(
  gender = factor(
    levels(train_data$gender)[1],
    levels = levels(train_data$gender)
  ),
  
  married = factor(
    levels(train_data$married)[1],
    levels = levels(train_data$married)
  ),
  
  dependents = factor(
    levels(train_data$dependents)[1],
    levels = levels(train_data$dependents)
  ),
  
  education = factor(
    levels(train_data$education)[1],
    levels = levels(train_data$education)
  ),
  
  self_employed = factor(
    levels(train_data$self_employed)[1],
    levels = levels(train_data$self_employed)
  ),
  
  applicantincome = 5000,
  
  coapplicantincome = 1500,
  
  loanamount = 150,
  
  loan_amount_term = 360,
  
  credit_history = 1,
  
  property_area = factor(
    levels(train_data$property_area)[1],
    levels = levels(train_data$property_area)
  )
)


# Scaling numerical values

new_applicant$applicantincome <-
  (new_applicant$applicantincome -
     applicant_mean) / applicant_sd

new_applicant$coapplicantincome <-
  (new_applicant$coapplicantincome -
     coapplicant_mean) / coapplicant_sd

new_applicant$loanamount <-
  (new_applicant$loanamount -
     loanamount_mean) / loanamount_sd


# Encoding categorical variables

new_encoded <- model.matrix(
  ~ gender + married + dependents + education +
    self_employed + property_area,
  data = new_applicant
)[, -1, drop = FALSE]


# Adding missing columns

missing_columns <- setdiff(
  colnames(train_encoded),
  colnames(new_encoded)
)

for (column in missing_columns) {
  new_encoded <- cbind(
    new_encoded,
    0
  )
  
  colnames(new_encoded)[ncol(new_encoded)] <- column
}


# Removing extra columns

extra_columns <- setdiff(
  colnames(new_encoded),
  colnames(train_encoded)
)

if (length(extra_columns) > 0) {
  new_encoded <- new_encoded[
    ,
    !colnames(new_encoded) %in% extra_columns,
    drop = FALSE
  ]
}


# Arranging columns in the same order as training data

new_encoded <- new_encoded[
  ,
  colnames(train_encoded),
  drop = FALSE
]


# Creating final data for prediction

new_final <- cbind(
  new_encoded,
  new_applicant[, c(
    "applicantincome",
    "coapplicantincome",
    "loanamount",
    "loan_amount_term",
    "credit_history"
  )]
)


# Making prediction

new_probability <- predict(
  model,
  newdata = data.frame(new_final),
  type = "response"
)


# Displaying probability

cat(
  "Loan approval probability:",
  new_probability,
  "\n"
)


# Predicting loan status

if (new_probability >= 0.5) {
  
  cat(
    "Predicted loan status:",
    positive_class,
    "\n"
  )
  
} else {
  
  cat(
    "Predicted loan status:",
    negative_class,
    "\n"
  )
}