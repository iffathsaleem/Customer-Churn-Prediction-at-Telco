data <- read.csv("Telco_customer_churn - Copy.csv")

data$Total.Charges[is.na(data$Total.Charges)] <- 0
data$Monthly.Charges[is.na(data$Monthly.Charges)] <- 0

data$Multiple.Lines[data$Multiple.Lines == 'No phone service'] <- 'No'
data$Online.Security[data$Online.Security == 'No internet service'] <- 'No'
data$Online.Backup[data$Online.Backup == 'No internet service'] <- 'No'
data$Device.Protection[data$Device.Protection == 'No internet service'] <- 'No'
data$Tech.Support[data$Tech.Support == 'No internet service'] <- 'No'
data$Streaming.TV[data$Streaming.TV == 'No internet service'] <- 'No'
data$Streaming.Movies[data$Streaming.Movies == 'No internet service'] <- 'No'

str(data)
summary(data)


# Setting the variables with more than two categories to factor
# lm() in regression automatically creates the dummy variables

data$Internet.Service <- factor(data$Internet.Service) 
data$Payment.Method <- factor(data$Payment.Method)
data$Contract <-factor(data$Contract)
data$Gender <- factor(data$Gender)
data$Senior.Citizen <- factor(data$Senior.Citizen)
data$Partner <- factor(data$Partner)
data$Dependents <- factor(data$Dependents)
data$Phone.Service <- factor(data$Phone.Service)
data$Multiple.Lines <- factor(data$Multiple.Lines)
data$Online.Security <- factor(data$Online.Security)
data$Online.Backup <- factor(data$Online.Backup)
data$Device.Protection <- factor(data$Device.Protection)
data$Tech.Support <- factor(data$Tech.Support)
data$Streaming.TV <- factor(data$Streaming.TV)
data$Streaming.Movies <- factor(data$Streaming.Movies)
data$Paperless.Billing <- factor(data$Paperless.Billing)
data$Churn.Label <- factor(data$Churn.Label)
data$Churn.Reason <- factor(data$Churn.Reason)


# Data Partition

RanNum <- runif(7043)
length(RanNum)

Index <- order(RanNum)
Index

Train <- data[Index[1:4543],]
Test <- data[Index[4543:7043],]

colSums(is.na(Train))  # Counts missing values in each column

# Model building

Model3 <- lm(Churn.Score~Total.Charges+Monthly.Charges+Zip.Code+Gender+Senior.Citizen+Partner+Dependents+Tenure.Months
             +Phone.Service+Multiple.Lines+Internet.Service+Online.Security+Online.Backup+Device.Protection
             +Tech.Support+Streaming.TV+Streaming.Movies+Contract+Paperless.Billing+Payment.Method+Churn.Label+CLTV, data = Train)

summary(Model3)

# Define the null model (intercept only)
Model4 <- lm(Churn.Score ~ 1, data = Train)


# Perform forward selection
forward_model <- step(Model4, 
                      scope = list(lower = Model4, upper = Model3), 
                      direction = "forward")

# Summarize the selected model
summary(forward_model)

# Perform backward elimination
backward_model <- step(Model3, direction = "backward")
summary(backward_model)

# stepwise regression
stepwise_model <- step(Model4, 
                       scope = list(lower = Model4, upper = Model3), 
                       direction = "both")
summary(stepwise_model)

CustChurnRegression <- lm(Churn.Score~Churn.Label+Online.Backup+Gender+Phone.Service+Device.Protection, data = Train)

#abline(CustChurnRegression,col='red')

summary(CustChurnRegression)

plot(CustChurnRegression)

predict = predict(CustChurnRegression, newdata =Test)
predict

Test$PREDICTED.Churn.Score=predict

summary(b)
