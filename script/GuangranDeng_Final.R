library(car)
library(MASS)
library(ggplot2)
library(dplyr)

# Read the data from a file
raw_data <- read.csv("raw_data/final_raw.csv",sep = ',', header = T,
                          colClasses = c("character","character","factor","numeric",
                                         "numeric","numeric","numeric","numeric",
                                         "factor","factor","numeric"))
raw_data$CLA <- sapply(raw_data$CLA,switch,'Yes' = 1,'No' = 0)
raw_data$DISEASE <- gsub(' ', '', raw_data$DISEASE) %>% as.factor()
write.table(raw_data, 'clean_data/final_clean.csv', row.names = F, sep = ',', quote = FALSE)

raw_data$PARITY = relevel(raw_data$PARITY, "Primiparous")
raw_data$DISEASE = relevel(raw_data$DISEASE, "NoDisease")
raw_data$SNP = relevel(raw_data$SNP, "AA")
head(raw_data)
dim(raw_data)
str(raw_data)

# Initial model
logit.model = glm(CLA ~ PARITY + AGE + BCS0 + BCS30 + BCS45 + BCS60 + DISEASE + SNP + MILK, 
                  data = raw_data, family = "binomial")

# Evaluate Multi-Collinearity
vif(logit.model)

# Variable selection - Stepwise selection
step <- stepAIC(logit.model, direction="both")
step$anova

# Running the logistics model after model selection
logit.model.f = glm(CLA ~ PARITY + AGE + BCS30 + BCS60 + MILK, 
                    data = raw_data, family = "binomial")

# Wald test to evaluate coefficients and test the significance of coeffcients
summary(logit.model.f)

# Likelihood ratio test to test the significance of predictors
modelF = glm(CLA ~ PARITY + AGE + BCS30 + BCS60 + MILK, data = raw_data, family = "binomial") ## full model
DF = deviance(modelF)
modelR1 = glm(CLA ~ PARITY + AGE + BCS30 + BCS60, data = raw_data, family = "binomial") ## reduced model 1
DR1 = deviance(modelR1)
modelR2 = glm(CLA ~ PARITY + AGE + BCS30 + MILK, data = raw_data, family = "binomial") ## reduced model 2
DR2 = deviance(modelR2)
modelR3 = glm(CLA ~ PARITY + AGE + BCS60 + MILK, data = raw_data, family = "binomial") ## reduced model 3
DR3 = deviance(modelR3)
modelR4 = glm(CLA ~ PARITY + BCS30 + BCS60 + MILK, data = raw_data, family = "binomial") ## reduced model 4
DR4 = deviance(modelR4)
modelR5 = glm(CLA ~ AGE + BCS30 + BCS60 + MILK, data = raw_data, family = "binomial") ## reduced model 5
DR5 = deviance(modelR5)

anova(modelR1,modelF, test = "Chisq")
anova(modelR2,modelF, test = "Chisq")
anova(modelR3,modelF, test = "Chisq")
anova(modelR4,modelF, test = "Chisq")
anova(modelR5,modelF, test = "Chisq")

# Predictions
predictions <- data.frame(Observed = raw_data$CLA, Predicted = round(fitted(logit.model.f),0))
predictions$Observed <- predictions$Observed %>% as.character() %>% sapply(switch, '1' = 'Yes','0' = 'No')
predictions$Predicted <- predictions$Predicted %>% as.character() %>% sapply(switch, '1' = 'Yes','0' = 'No')
write.table(predictions, 'clean_data/final_predictions.csv', row.names = F, sep = ',', quote = FALSE)

# Outliers
outlierTest(logit.model.f)

# High leverage
hii <- data.frame(hatvalues = hatvalues(logit.model.f)) 
ggplot(hii, aes(x = seq_along(hii$hatvalues), y = hatvalues(logit.model.f))) +
  geom_point(size = 2, color = "blue", alpha = 0.5) +
  geom_hline(yintercept = 2*6/785, color = "red") +
  geom_text(aes( 0, 2*6/785, label = 0.0153, vjust = -1), size = 3) + 
  labs(x = "Index", y = "Hat Values", title = "High Leverage Points") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave('figures/High Leverages Figure.jpg')

# Cook's distance
cd <- data.frame(cooksdistances = cooks.distance(logit.model.f))
ggplot(cd, aes(x = seq_along(cd$cooksdistances), y = cooks.distance(logit.model.f))) +
  geom_point(size = 2, color = "blue", alpha = 0.5) +
  geom_hline(yintercept = 4/(785-6), color = "red") +
  geom_text(aes( 0, 4/(785-6), label = 0.005, vjust = -1), size = 3) + 
  labs(x = "Index", y = "Cooks Distance", title = "Influential Points") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave('figures/Influential Points Figure.jpg')
