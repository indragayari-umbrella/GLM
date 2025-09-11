# GLM
GLM Analysis Post Hoc test and Visualization
GLM Analysis Post Hoc Tests and Visualization
Dr. Indrajit Gayari
2025-09-06
Generalized Linear Models (GLM) in R
This R Markdown document provides a comprehensive guide to performing GLM analysis, post-hoc tests, least square means calculations, and creating publication-quality graphics.

# Load required packages
library(tidyverse)    # Data manipulation and visualization
library(lme4)         # For mixed models (if needed)
library(car)          # For ANOVA tables
library(emmeans)      # For post-hoc tests and least square means
library(multcomp)     # For multiple comparisons
library(ggpubr)       # For publication-quality graphics
library(broom)        # For tidy model output
library(performance)  # For model diagnostics

# Set theme for publication-quality graphics
theme_set(theme_pubr())
Steps for importing your own dataset for Analysis
## Here, in this chunk ## is the comment # is the code
## First check current directory first
#getwd()
##Set working directory to a specific path
#setwd("C:/Users/YourName/Projects/MyAnalysis")

## Import data from CSV file
## Replace 'your_data.csv' with your actual file name
#data <- read.csv("your_data.csv")

##Display the first few rows of the data
#head(data) %>% kable()

##Check data structure
#str(data)

# Summary statistics
#summary(data)
Example Dataset
I am create a sample dataset for demonstration purpose:

set.seed(123)

# Create a sample dataset
n <- 120
data <- data.frame(
  treatment = rep(c("A", "B", "C", "D"), each = n/4),
  dose = rep(c("Low", "Medium", "High"), times = n/3),
  response = c(
    rpois(n/4, lambda = 10),  # Treatment A
    rpois(n/4, lambda = 15),  # Treatment B  
    rpois(n/4, lambda = 20),  # Treatment C
    rpois(n/4, lambda = 25)   # Treatment D
  ),
  covariate = rnorm(n, mean = 50, sd = 10)
)

# Convert factors
data$treatment <- factor(data$treatment)
data$dose <- factor(data$dose, levels = c("Low", "Medium", "High"))

head(data)
##   treatment   dose response covariate
## 1         A    Low        8  46.07446
## 2         A Medium        9  50.07051
## 3         A   High       14  25.05165
## 4         A    Low       10  40.22704
## 5         A Medium       10  56.28569
## 6         A   High       15  49.15860
Descriptive Statistics
# Summary statistics
summary(data)
##  treatment     dose       response       covariate    
##  A:30      Low   :40   Min.   : 3.00   Min.   :16.89  
##  B:30      Medium:40   1st Qu.:12.00   1st Qu.:41.32  
##  C:30      High  :40   Median :17.00   Median :49.14  
##  D:30                  Mean   :17.52   Mean   :49.08  
##                        3rd Qu.:22.25   3rd Qu.:56.73  
##                        Max.   :34.00   Max.   :77.08
# Count by treatment and dose
table(data$treatment, data$dose)
##    
##     Low Medium High
##   A  10     10   10
##   B  10     10   10
##   C  10     10   10
##   D  10     10   10
Fitting a GLM Model
Here I am fitting a Poisson GLM (appropriate for count data). For Gaussian data, use family = gaussian (link = “identity”). For Binomial data, use family = binomial(link = “logit”)

# Fit GLM with Poisson family
model <- glm(response ~ treatment * dose + covariate, 
             data = data, 
             family = poisson(link = "log"))

# Model summary
summary(model)
## 
## Call:
## glm(formula = response ~ treatment * dose + covariate, family = poisson(link = "log"), 
##     data = data)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.0280  -0.5778  -0.0295   0.6055   1.5839  
## 
## Coefficients:
##                        Estimate Std. Error z value Pr(>|z|)    
## (Intercept)            2.146330   0.141850  15.131  < 2e-16 ***
## treatmentB             0.399427   0.133093   3.001  0.00269 ** 
## treatmentC             0.772879   0.124594   6.203 5.53e-10 ***
## treatmentD             1.055836   0.119119   8.864  < 2e-16 ***
## doseMedium            -0.204672   0.152957  -1.338  0.18086    
## doseHigh               0.013508   0.145615   0.093  0.92609    
## covariate              0.002225   0.002074   1.073  0.28335    
## treatmentB:doseMedium  0.268353   0.192690   1.393  0.16372    
## treatmentC:doseMedium  0.174088   0.184445   0.944  0.34525    
## treatmentD:doseMedium  0.143000   0.175892   0.813  0.41622    
## treatmentB:doseHigh    0.006237   0.186796   0.033  0.97337    
## treatmentC:doseHigh   -0.074592   0.176033  -0.424  0.67176    
## treatmentD:doseHigh   -0.069123   0.170360  -0.406  0.68493    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for poisson family taken to be 1)
## 
##     Null deviance: 383.983  on 119  degrees of freedom
## Residual deviance:  84.633  on 107  degrees of freedom
## AIC: 663.52
## 
## Number of Fisher Scoring iterations: 4
# ANOVA table
car::Anova(model, type = "III")
## Analysis of Deviance Table (Type III tests)
## 
## Response: response
##                LR Chisq Df Pr(>Chisq)    
## treatment       102.528  3     <2e-16 ***
## dose              2.523  2     0.2833    
## covariate         1.150  1     0.2835    
## treatment:dose    3.037  6     0.8042    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
Model Diagnostics
# Check for overdispersion
# If residual deviance >> degrees of freedom, consider quasipoisson
dispersion_test <- model$deviance / model$df.residual
cat("Dispersion parameter:", dispersion_test, "\n")
## Dispersion parameter: 0.7909599
# Residual plots
par(mfrow = c(2, 2))
plot(model)


Check for influential observations
influence_measures <- influence.measures(model)
summary(influence_measures)
## Potentially influential observations of
##   glm(formula = response ~ treatment * dose + covariate, family = poisson(link = "log"),      data = data) :
## 
##     dfb.1_ dfb.trtB dfb.trtC dfb.trtD dfb.dsMd dfb.dsHg dfb.cvrt dfb.tB:M
## 9    0.01   0.00     0.00     0.00     0.00    -0.58    -0.02     0.00   
## 17   0.08   0.01     0.01     0.00    -0.60    -0.01    -0.12     0.47   
## 106 -0.56  -0.10    -0.10    -0.40    -0.04     0.07     0.82     0.07   
##     dfb.tC:M dfb.tD:M dfb.tB:H dfb.tC:H dfb.tD:H dffit   cov.r   cook.d hat  
## 9    0.00     0.00     0.45     0.48     0.50    -0.82    0.61_*  0.03   0.10
## 17   0.49     0.53     0.00     0.00     0.01    -0.83    0.62_*  0.03   0.10
## 106  0.13     0.30    -0.03    -0.02     0.17    -1.13_*  0.75    0.07   0.20
# Check multicollinearity (for continuous predictors)
# vif(model) # Only for continuous predictors in this case
Calculating Least-Squares Means (Estimated Marginal Means)
# Calculate least-squares means for treatment
lsmeans_treatment <- emmeans(model, ~ treatment)
lsmeans_treatment
##  treatment emmean     SE  df asymp.LCL asymp.UCL
##  A           2.19 0.0616 Inf      2.07      2.31
##  B           2.68 0.0481 Inf      2.59      2.78
##  C           3.00 0.0408 Inf      2.92      3.08
##  D           3.27 0.0356 Inf      3.20      3.34
## 
## Results are averaged over the levels of: dose 
## Results are given on the log (not the response) scale. 
## Confidence level used: 0.95
# For dose
lsmeans_dose <- emmeans(model, ~ dose)
lsmeans_dose
##  dose   emmean     SE  df asymp.LCL asymp.UCL
##  Low      2.81 0.0404 Inf      2.73      2.89
##  Medium   2.75 0.0421 Inf      2.67      2.84
##  High     2.79 0.0406 Inf      2.71      2.87
## 
## Results are averaged over the levels of: treatment 
## Results are given on the log (not the response) scale. 
## Confidence level used: 0.95
# For interaction
lsmeans_interaction <- emmeans(model, ~ treatment:dose)
lsmeans_interaction
##  treatment dose   emmean     SE  df asymp.LCL asymp.UCL
##  A         Low      2.26 0.1030 Inf      2.05      2.46
##  B         Low      2.65 0.0842 Inf      2.49      2.82
##  C         Low      3.03 0.0700 Inf      2.89      3.17
##  D         Low      3.31 0.0606 Inf      3.19      3.43
##  A         Medium   2.05 0.1130 Inf      1.83      2.27
##  B         Medium   2.72 0.0812 Inf      2.56      2.88
##  C         Medium   3.00 0.0719 Inf      2.86      3.14
##  D         Medium   3.25 0.0624 Inf      3.13      3.37
##  A         High     2.27 0.1040 Inf      2.07      2.47
##  B         High     2.67 0.0830 Inf      2.51      2.84
##  C         High     2.97 0.0717 Inf      2.83      3.11
##  D         High     3.26 0.0622 Inf      3.13      3.38
## 
## Results are given on the log (not the response) scale. 
## Confidence level used: 0.95
Post-hoc Tests
# Pairwise comparisons for treatment
posthoc_treatment <- pairs(lsmeans_treatment, adjust = "tukey")
posthoc_treatment
##  contrast estimate     SE  df z.ratio p.value
##  A - B      -0.491 0.0786 Inf  -6.244  <.0001
##  A - C      -0.806 0.0739 Inf -10.900  <.0001
##  A - D      -1.080 0.0711 Inf -15.192  <.0001
##  B - C      -0.315 0.0629 Inf  -5.006  <.0001
##  B - D      -0.590 0.0598 Inf  -9.855  <.0001
##  C - D      -0.274 0.0541 Inf  -5.071  <.0001
## 
## Results are averaged over the levels of: dose 
## Results are given on the log (not the response) scale. 
## P value adjustment: tukey method for comparing a family of 4 estimates
# Pairwise comparisons for dose
posthoc_dose <- pairs(lsmeans_dose, adjust = "tukey")
posthoc_dose
##  contrast      estimate     SE  df z.ratio p.value
##  Low - Medium    0.0583 0.0585 Inf   0.996  0.5791
##  Low - High      0.0209 0.0573 Inf   0.364  0.9295
##  Medium - High  -0.0375 0.0585 Inf  -0.640  0.7978
## 
## Results are averaged over the levels of: treatment 
## Results are given on the log (not the response) scale. 
## P value adjustment: tukey method for comparing a family of 3 estimates
# Compact letter display for treatment
cld_treatment <- cld(lsmeans_treatment, Letters = letters, adjust = "tukey")
cld_treatment
##  treatment emmean     SE  df asymp.LCL asymp.UCL .group
##  A           2.19 0.0616 Inf      2.04      2.35  a    
##  B           2.68 0.0481 Inf      2.56      2.80   b   
##  C           3.00 0.0408 Inf      2.90      3.10    c  
##  D           3.27 0.0356 Inf      3.18      3.36     d 
## 
## Results are averaged over the levels of: dose 
## Results are given on the log (not the response) scale. 
## Confidence level used: 0.95 
## Conf-level adjustment: sidak method for 4 estimates 
## P value adjustment: tukey method for comparing a family of 4 estimates 
## significance level used: alpha = 0.05 
## NOTE: If two or more means share the same grouping symbol,
##       then we cannot show them to be different.
##       But we also did not show them to be the same.
# Compact letter display for dose
cld_dose <- cld(lsmeans_dose, Letters = letters, adjust = "tukey")
cld_dose
##  dose   emmean     SE  df asymp.LCL asymp.UCL .group
##  Medium   2.75 0.0421 Inf      2.65      2.85  a    
##  High     2.79 0.0406 Inf      2.69      2.89  a    
##  Low      2.81 0.0404 Inf      2.72      2.91  a    
## 
## Results are averaged over the levels of: treatment 
## Results are given on the log (not the response) scale. 
## Confidence level used: 0.95 
## Conf-level adjustment: sidak method for 3 estimates 
## P value adjustment: tukey method for comparing a family of 3 estimates 
## significance level used: alpha = 0.05 
## NOTE: If two or more means share the same grouping symbol,
##       then we cannot show them to be different.
##       But we also did not show them to be the same.
Publication-Quality Visualizations
1. Mean Plot with Error Bars
# Create data frame for plotting
plot_data <- as.data.frame(lsmeans_treatment)

# Create mean plot with confidence intervals
mean_plot <- ggplot(plot_data, aes(x = treatment, y = emmean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) +
  labs(title = "Least-Squares Means by Treatment",
       x = "Treatment", 
       y = "Estimated Marginal Mean (log scale)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

mean_plot


2. Interaction Plot
# Get interaction means
interaction_data <- as.data.frame(lsmeans_interaction)

# Create interaction plot
interaction_plot <- ggplot(interaction_data, 
                           aes(x = dose, y = emmean, color = treatment, group = treatment)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.1) +
  labs(title = "Treatment × Dose Interaction",
       x = "Dose Level", 
       y = "Estimated Marginal Mean",
       color = "Treatment") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "top")

interaction_plot


3. Bar Plot with Significance Letters
# Prepare data with significance letters
cld_data <- as.data.frame(cld_treatment)

# Create bar plot with significance letters
bar_plot <- ggplot(cld_data, aes(x = treatment, y = emmean, fill = treatment)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.2) +
  geom_text(aes(label = .group, y = emmean + SE + 0.5), vjust = 0) +
  labs(title = "Treatment Means with Standard Errors",
       x = "Treatment", 
       y = "Estimated Marginal Mean",
       fill = "Treatment") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")

bar_plot


4. Multi-panel Publication Figure
# Arrange multiple plots
multi_panel <- ggarrange(mean_plot, bar_plot, interaction_plot,
                         ncol = 2, nrow = 2,
                         labels = c("A", "B", "C"),
                         common.legend = FALSE)

# Add title
multi_panel <- annotate_figure(multi_panel,
                               top = text_grob("GLM Analysis Results", 
                                               color = "black", 
                                               face = "bold", 
                                               size = 14))

multi_panel


Exporting Results
##Save plots (uncomment to use)
#ggsave("mean_plot.png", mean_plot, width = 8, height = 6, dpi = 300)
#ggsave("interaction_plot.png", interaction_plot, width = 8, height = 6, dpi = 300)
#ggsave("multi_panel_plot.png", multi_panel, width = 10, height = 8, dpi = 300)

##Save results to CSV
#write.csv(as.data.frame(lsmeans_treatment), "treatment_means.csv")
#write.csv(as.data.frame(posthoc_treatment), "treatment_comparisons.csv")
