# Experiment 1 code and plot
# Null Hypothesis - Gibbernium doesn't effect the reaction time of the samples
# Alternative Hypothesis - Gibbernium does effect the reaction time  of the samples 

Experiment1 <- read.table("Student number 156 Experiment 1.txt", header=TRUE)
str(Experiment1)
# This is to inset a name to the data set (156) and the str function is to make sure the code is working

Experiment1$sex<-as.factor(Experiment1$sex)
str(Experiment1)

Experiment1$treatment<-as.factor(Experiment1$treatment)
str(Experiment1)
# as.factor function is used to convert the object into a factor

boxplot(
  Experiment1$reaction_time_ms~Experiment1$sex*Experiment1$treatment, besides= TRUE,
  ylab = "Reaction Time (ms)",
  xlab = "Samples Treated/Control",
  col =c ("blue","green"),
  names =c ("Female control", "Male control", "Drugs treated Female", "Drug treated Male")
  )
# This is to represent the data as a box plot
# The ("Female control", "Male control", "Drugs treated Female", "Drug treated Male") are the axis names for this boxplot

legend("topright",
       c("Female", "Male"),
       fill = c("blue", "green"),
       )

names("Female control", "Male control", "Drug treated Female", "Drug treated Male") <- axis

title("Effects of Gibbernuim on reaction time for male and female samples")
# all this code/functions is to label the whole data plot
# legend is created to known which colour represents which category

print(Experiment1)
anova<-aov(Experiment1$reaction_time_ms~Experiment1$sex*Experiment1$treatment)

summary(anova)
TukeyHSD(anova)
# The anova function is to reveal if I accept or reject the null hypothesis
# As for the Tukey code, this is to see if there is a difference in any of the sample groups



# Experiment 2 code and plot

Experiment2<-matrix(c(38,10,340,20,40,70), ncol=3, nrow=2)
rownames(Experiment2)<-c("Control", "Drug")
colnames(Experiment2)<-c("Decrease", "No Change", "Increase")
Experiment2
# This section of code is to input the data in Rcloud in a 2x3 matrix
# The rownames() and colnames() function is to label my diagram/table

# Null Hypothesis - Gibberium doesn't elevate blood pressure
# Alternative Hypothesis - Gibberium does elevate blood pressure

barplot(
  Experiment2,
  beside = TRUE,
  legend.text = TRUE,
  xlab = 'Effects on the Treatment',
  ylab = 'Number of people',
  ylim =c (0,400)
)
# This section of code is to make a barplot to represent our data in a clear way/visualise the effects of Gibbernium on blood pressure
# xlab and ylab are the codes to name the axis, so the graph is labeled
# the ylim function/code is to extend the y axis because one of the bars was higher than 300 (before the code ran)

title("Effects of Gibbernuim on blood pressure")
# this function is to title the plot/barplot

chisq.test(Experiment2)
# This chisq.test code is to see the results of the data and check if we should reject ot accept the null hypothesis
# Also allows us to see if there is a relationship between the use of Gibberium and an individual's blood pressure
