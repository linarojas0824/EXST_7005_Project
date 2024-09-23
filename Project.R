# Load Dataset
library(readxl)
df1 <- read_excel('Dataset_Suicide.xlsx', sheet = 'Sex, Race, Age')
#===============================================================
# Dataset cleaning
# Clean the NA data
df2 <- na.omit(df1)
# Select the year of interest
final_year <- 2018
df_final <- df2[df2$YEAR %in% final_year,]
#===============================================================
# Numerical Summary
# Load the libraries
library(easypackages) 
libraries("lsr","psych","car","agricolae",
          "tidyverse","knitr") 
libraries("kableExtra","emmeans","multcomp",
          "gtsummary","lindia","rstatix","broom")
# Summary 
# Summary by Age range
MidSummary = df_final %>%
  group_by(RACE) %>%
  summarise(n = n(), mean = mean(ESTIMATE),
            sd = sd(ESTIMATE), med = median(ESTIMATE),
            min = min(ESTIMATE), max = max(ESTIMATE),
            mad = mad(ESTIMATE), 
            IQR = IQR(ESTIMATE),
            .groups = "drop")
MidSummary
## Save the file
write.csv(MidSummary, "./SummaryByAge_Range.csv")
#===============================================================
# Graphical Representation (Choose with plot representation will be better)
# Load the library to create the plots
library(PerformanceAnalytics)
#===============================================================
# Probability distribution for Ages
df_race <- df_final %>%
  group_by(`RACE NO`) %>%
  summarise(Estimate = sum(ESTIMATE))
#===============================================================
# Histogram Plot Representation

#===============================================================
# Boxplot Representation
#Create boxplot for age and estimate
# I need to convert the variable into non numerical first
df_final$`AGE No.` <- factor(df_final$`AGE No.`)
# I can create the plot
plot1 <- ggplot(df_final, aes(x = `AGE No.`, y = ESTIMATE)) +
  stat_boxplot(geom = "errorbar", width = 0.3) +
  geom_boxplot() + stat_summary(fun = mean,colour = "darkred", geom = "point", shape = 18,
                                size = 3, show.legend = FALSE) + 
  labs(x = "Age",y = "Estimate") + theme_classic()
plot1
# Example of scatter plot
plot1 <- ggplot(df_final, aes(x = df_final$`AGE No.`, y = ESTIMATE)) +
  geom_point() + theme_classic()
plot1

#===============================================================
# Category by Race

#===============================================================
# ANOVA assumptions
# Homoscedasticity
## Change the names to categorical
df_final$Race_No_Category <- factor(df_final$`RACE NO`, levels = 1:5, labels = c("Category1", "Category2", "Category3", "Category4","Category5"))
## Do the test on the catergorical variable
grade_levene <- leveneTest(ESTIMATE~Race_No_Category,df_final)
grade_levene
# Normality of residuals
plot2=ggplot(df_final, aes(sample=ESTIMATE), color=Race_No_Category) + 
  stat_qq() + stat_qq_line() +
  facet_wrap(~ Race_No_Category, scales = "free") + 
  ggtitle('Normal Q-Q Plot for Estimates by Race') 
plot2
#===============================================================
# ANOVA
Race.aov<- aov(ESTIMATE~Race_No_Category, data=df_final) 
summary(Race.aov)
## Residual from the model
Race.aov.res = residuals(object = Race.aov )
## Information about the residuals
describe(Race.aov.res)
## Test for the residuals KS test
ks.test(Race.aov.res, "pnorm", mean(Race.aov.res), sd(Race.aov.res))

#===============================================================
#Post-hoc ANOVA
posthocPairwiseT(Race.aov,p.adjust.methods='none')
TukeyHSD(Race.aov,conf.level = 0.95)