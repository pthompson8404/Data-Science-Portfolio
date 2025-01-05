##Phillip Thompson
##DSC520-500 Regression Analysis
##Multiple Linear Regression Analysis


##Generate data set
library(data.table)
set.seed(12345)
adosleep <- data.table(
  SOLacti = rnorm(150, 4.4, 1.3)^2,
  DBAS =  rnorm(150, 72, 26),
  DAS =  rnorm(150, 125, 32),
  Female = rbinom(150, 1, .53),
  Stress = rnorm(150, 32, 11))
adosleep[, SSQ := rnorm(150,
                        (.36*3/12.5)*SOLacti +
                          (.16*3/26)*DBAS +
                          (.20*3/11)*Stress,2.6)]
adosleep[, MOOD := rnorm(150,
                         (-.07/12.5)*SOLacti +
                           (.29/3)*SSQ +
                           (.14/26)*DBAS +
                           (.21/32)*DAS + 
                           (.12/32)*SSQ*(DAS-50) +
                           (.44/.5)*Female +
                           (.28/11)*Stress, 2)]
adosleep[, Female := factor(Female, levels=0:1, labels = c("Males", "Females"))]
# Display the synthetic data set adosleep
##       SOLacti     DBAS       DAS  Female   Stress        SSQ     MOOD
##   1: 26.63786 29.89746 141.71303   Males 34.46721  0.0351776 3.135512
##   2: 28.32694 86.25835 125.31340 Females 40.65050 10.8613493 5.763634
##   3: 18.12976 77.07734 110.90316   Males 27.34301  5.6395828 2.695476
##   4: 14.51956 51.03105 163.38367 Females 27.95713  5.2300021 4.148444
##   5: 26.91175 69.17577 121.24101 Females 12.56278  5.4454510 3.648391
##  ---                                                                 
## 146: 26.03265 76.77716 141.02786   Males 27.77196  7.6259684 7.044688
## 147:  5.70503 16.09193  95.89703 Females 16.28171  1.0325290 1.557178
## 148: 30.30006 55.80287  54.68933 Females 42.59759  4.8613652 4.189277
## 149: 30.96719 52.09858 139.03345 Females 38.75160 12.9784500 8.158078
## 150: 20.97913 84.07204  92.05873 Females 27.94590  5.8835742 5.623150

###### Step 3: Inspect variables and identify outliers

library(car)
library(ggplot2)


testdistr <- function(variable) {
  
  ggplot(adosleep, aes(x = .data[[variable]])) +
    geom_histogram(bins = 20, fill = "blue", color = "black") +
    labs(x = variable, y = "Frequency") +
    theme_minimal()
}


variables <- c("MOOD", "SSQ", "SOLacti", "DAS")


plot_list <- lapply(variables, testdistr)
plot_list


###### Step 4:Correlation matrix and plot heatmap

library(ggplot2)
library(reshape2)

## Correlations
cor_table <- cor(adosleep[, c("SSQ", "MOOD", "Stress", "SOLacti", "DAS", "DBAS")])


cor_melted <- melt(cor_table)

## Plot heatmap
ggplot(cor_melted, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "green", high = "red") +
  theme_bw() +
  labs(x = "", y = "") +
  coord_equal()

###### Step 5: Create table of graphs and standardize the predictors: ** The assignment calls for the use of an egl table, I used the following code: egltable(adosleep[, c("SSQ", "MOOD", "Stress", "SOLacti", "DAS", "DBAS")])  .Can you tell me what was wrong/how to avoid this sort of error in the future?
library(psych)

# Standardize predictors
adosleep$SSQ_std <- as.vector(scale(adosleep$SSQ))
adosleep$MOOD_std <- as.vector(scale(adosleep$MOOD))
adosleep$Stress_std <- as.vector(scale(adosleep$Stress))
adosleep$SOLacti_std <- as.vector(scale(adosleep$SOLacti))
adosleep$DAS_std <- as.vector(scale(adosleep$DAS))
adosleep$DBAS_std <- as.vector(scale(adosleep$DBAS))

# Create the table of descriptive statistics
adosleep_stats <- adosleep[, c("SSQ_std", "MOOD_std", "Stress_std", "SOLacti_std", "DAS_std", "DBAS_std")]
describe(adosleep_stats)

###### Step 6: Model creations

## Model 1: Covariates only
model1 <- lm(MOOD ~ SSQ_std + Stress_std + SOLacti_std + DAS_std + DBAS_std, data = adosleep)

## Model 2: Model 1 + main constructs of interest without interactions
model2 <- lm(MOOD ~ SSQ_std + Stress_std + SOLacti_std + DAS_std + DBAS_std +
               SSQ_std * DAS_std + Stress_std * DAS_std, data = adosleep)

## Model 3: Model 2 + interaction between subjective sleep quality and global dysfunctional beliefs
model3 <- lm(MOOD ~ SSQ_std + Stress_std + SOLacti_std + DAS_std + DBAS_std +
               SSQ_std * DAS_std + Stress_std * DAS_std + SSQ_std * DBAS_std, data = adosleep)

###### Step 7: Combination of the results of the three models

library(texreg)

## Combine model results into one table
model_table <- screenreg(list(model1, model2, model3))
model_table

###### Step 8:

# Extract p-values for SSQ and DAS variables from each model
p_values <- c(
  model1$coefficients["SSQ"],
  model2$coefficients["SSQ"],
  model3$coefficients["SSQ"],
  model1$coefficients["DAS"],
  model2$coefficients["DAS"],
  model3$coefficients["DAS"]
)

# Determine which models have p-values less than 0.001
significant_models <- c(
  model1$p.value < 0.001,
  model2$p.value < 0.001,
  model3$p.value < 0.001
)

# Print the results
cat("Model 1: SSQ p-value =", model1$p.value["SSQ"], "\n")
cat("Model 2: SSQ p-value =", model2$p.value["SSQ"], "\n")
cat("Model 3: SSQ p-value =", model3$p.value["SSQ"], "\n")
cat("\n")
cat("Model 1: DAS p-value =", model1$p.value["DAS"], "\n")
cat("Model 2: DAS p-value =", model2$p.value["DAS"], "\n")
cat("Model 3: DAS p-value =", model3$p.value["DAS"], "\n")
cat("\n")
cat("Significant Models:", paste(which(significant_models), collapse = ", "), "\n")



###### Step 9:
## Check variance inflation factors (VIF)
vif_values <- car::vif(model3, type = 'predictor')
vif_values

## Check distribution of residuals
residuals <- residuals(model3)
hist(residuals, breaks = 20, col = "blue", main = "Distribution of Residuals", xlab = "Residuals")

## Interpretation of findings: VIF measures the degree of multicollinearity between the predictor variables in the regression model.  High values indicate high correlation among predictors.  This can cause instability and unreliability in the regression coefficients.  All VIF's are below 1.52 which indicates considerable collinearity.


###### Step 10:

## Remove standardized variables
adosleep[, c("SSQ_std", "MOOD_std", "Stress_std", "SOLacti_std", "DAS_std", "DBAS_std") := NULL]

## Refit Model 3 on raw data
model3_raw <- lm(MOOD ~ SSQ + Stress + SOLacti + DAS + DBAS +
                   SSQ * DAS + Stress * DAS + SSQ * DBAS, data = adosleep)

###### Step 11: Plot the graph ** I am unable to determine exactly what is causing the issue on this particular step. Would you be able to help me determine where I went wrong or reference a particular point in the textbook I can revisit to fix this??

library(ggplot2)

# Remove missing values
adosleep.complete <- na.omit(adosleep)

# Convert DAS to factor
adosleep.complete$DAS <- as.factor(adosleep.complete$DAS)

library(ggplot2)
library(cowplot)

# Create a new dataset for plotting
adosleep.newdat <- adosleep[, .(SSQ, MOOD, DAS)]

# Convert DAS to a factor with ordered levels
adosleep.newdat$DAS <- factor(adosleep.newdat$DAS, levels = unique(adosleep.newdat$DAS))

# Plot the graph
ggplot(adosleep.newdat, aes(x = SSQ, y = MOOD, color = DAS)) +
  geom_line(size = 2, group = 1) +
  scale_x_continuous("Subjective sleep quality\n(higher is worse)") +
  ylab("Negative Mood") +
  scale_color_brewer(palette = "Set1") +
  theme_cowplot() +
  theme(
    legend.position = c(.85, .15),
    legend.key.width = unit(2, "cm")
  )

