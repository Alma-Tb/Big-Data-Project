Data <- read.csv("G:/my_doc/moaa/ainshams/Year4/TERM2/BigData/Dataset.csv" ,header = TRUE)
install.packages("ggplot2")
install.packages("corrplot")
install.packages("stats")
install.packages("caret")
install.packages("lattice")
library(ggplot2)
library(corrplot)
library(dplyr)
library(reshape2)
library(gplots)
#-|
#-|
#-|    REALLLLLY IMPORTANT NOTEEEEEEE!!!!!!!!! PLEASE CHECK IT!!!
#-|
#-U

#Please note that the run of the Hypothesis Testing , Pre Processing and The Analytical Techniques on google collab through this link #Please check the run on this link https://colab.research.google.com/drive/1HANwJ8_JwXXWUNaHffnfrpLL8upckkHX?usp=sharing#scrollTo=EQrgpekFOO1h
#--------------------------------------------------------EDA---------------------------------------------------------#

summary(Data)


print("Missing Values:")
print(colSums(is.na(Data)))
#no null values
duplicates_count <- sum(duplicated(Data))
print(paste("Number of duplicates:", duplicates_count))
#no duplicates


#Pie chart for the student's Target percentages
count <- table(Data$Target)
percentages <- prop.table(count) * 100
#This line calculates the percentage distribution of the manufacturing locations. prop.table() is used to calculate the proportion of each count relative to the total count, and then this proportion is multiplied by 100 to convert it to a percentage. The result is stored in the percentages variable.
lbls <- paste(names(percentages), round(percentages, 1), "%", sep = "")
pie(percentages, labels = lbls, col = rainbow(length(percentages)))
title("Students'Graduation Status")
#almost 50% of the students in this dataset did in fact graduate the next highest percent dropped out while the rest are still enrolled students

#Student's age distribution during enrollment
plot(density(Data$Age.at.enrollment,adjust=2) , main = "Rating Distribution")
#The distribution of age at enrollment is positively skewed, indicating that the majority of students enrolled at a relatively young age. 


#Graduation based on the course enrolled
course_status_table <- table(Data$Course, Data$Target)


barplot(course_status_table, beside = TRUE, col = rainbow(nrow(course_status_table)),
        legend.text = c('Biofuel Production Technologies',
                        'Animation and Multimedia Design',
                        'Social Service (Evening Attendance)',
                        'Agronomy',
                        'Communication Design',
                        'Veterinary Nursing',
                        'Informatics Engineering',
                        'Equiniculture',
                        'Management',
                        'Social Service',
                        'Tourism',
                        'Nursing',
                        'Oral Hygiene',
                        'Advertising and Marketing Management',
                        'Journalism and Communication',
                        'Basic Education',
                        'Management (Evening Attendance)'), 
        args.legend = list(title = "Course", x = "topright", bty = "n"),
        main = "Students' Graduation Status by Course",
        xlab = "Status", ylab = "Count")
#Nursing course produced the highest number of graduates while management course has the highest number of droputs.


#Graduation based on previous qualifications 
previous_qualification_status <- table(Data$Previous.qualification, Data$Target)


rownames(previous_qualification_status) <- c('Secondary Education',
                                             'Higher Education—Bachelor’s Degree',
                                             'Higher Education—Degree',
                                             'Higher Education—Master’s Degree',
                                             'Higher Education—Doctorate',
                                             'Frequency of Higher Education',
                                             '12th Year of Schooling—Not Completed',
                                             '11th Year of Schooling—Not Completed',
                                             'Other—11th Year of Schooling',
                                             '10th Year of Schooling',
                                             '10th Year of Schooling—Not Completed',
                                             'Basic Education 3rd Cycle (9th/10th/11th year) or Equivalent',
                                             'Basic Education 2nd Cycle (6th/7th/8th year) or Equivalent',
                                             'Technological Specialization Course',
                                             'Higher Education—Degree (1st cycle)',
                                             'Professional Higher Technical Course',
                                             'Higher Education—Master’s Degree (2nd Cycle)')


barplot(previous_qualification_status, beside = TRUE, col = rainbow(nrow(previous_qualification_status)),
        legend.text = rownames(previous_qualification_status), 
        args.legend = list(title = "Previous Qualification", x = "topright", bty = "n"),
        main = "Students' Graduation Status by Previous Qualification",
        xlab = "Status", ylab = "Count")
#Most of the students in the data finished secondary education.



#Graduation based on gender
gender_status_table <- table(Data$Gender, Data$Target)


barplot(gender_status_table, beside = TRUE, col = rainbow(nrow(gender_status_table)),
        legend.text = c("Female (0)", "Male (1)"), 
        args.legend = list(title = "Gender", x = "topright", bty = "n"),
        main = "Students' Graduation Status by Gender",
        xlab = "Status", ylab = "Count")
#According to the data, a higher number of graduates are female. By alot.
#However, females also have the highest number of dropouts, although the difference compared to males is small.


#Graduation based on gender Marital Status
marital_status_table  <- table(Data$Marital.status, Data$Target)

barplot(marital_status_table, beside = TRUE, col = rainbow(nrow(marital_status_table)),
        legend.text = c("Single", "Married", "Divorced", "Widowed"), 
        args.legend = list(title = "Marital Status", x = "topright", bty = "n"),
        main = "Students' Graduation Status by Marital Status",
        xlab = "Status", ylab = "Count")

#most of the students overall are single 




# Grduation Status based on mother and father qualifications
colors_target <- c("Graduate" = "#1f77b4", "Enrolled" = "#ff7f0e", "Dropout" = "#2ca02c")
cols <- c("Mother.s.qualification", "Father.s.qualification")

for (col in cols) {
  order <- names(sort(table(Data[[col]]), decreasing = TRUE))
  p <- ggplot(Data, aes(x = reorder({{col}}, -table(Data[[col]])[[1]]), fill = Target)) +
    geom_bar(position = "dodge") +
    labs(x = col, y = "Number of students") +
    scale_fill_manual(values = colors_target) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    guides(fill = guide_legend(title = "Target", ncol = 1)) +
    ggtitle(paste("Count plot of", col)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    coord_flip()
  
  print(p)
}
#similar results (not significant----probably)

# Grduation Status based on mother and father occupation
colors_target <- c("Graduate" = "#1f77b4", "Enrolled" = "#ff7f0e", "Dropout" = "#2ca02c")
cols <- c("Mother.s.occupation", "Father.s.occupation")

for (col in cols) {
  order <- names(sort(table(Data[[col]]), decreasing = TRUE))
  p <- ggplot(Data, aes(x = reorder({{col}}, -table(Data[[col]])[[1]]), fill = Target)) +
    geom_bar(position = "dodge") +
    labs(x = col, y = "Number of students") +
    scale_fill_manual(values = colors_target) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    guides(fill = guide_legend(title = "Target", ncol = 1)) +
    ggtitle(paste("Count plot of", col)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    coord_flip()
  
  print(p)
}
#again similar results (not significant----probably)

#Graduation based on Nationality
Nationality_Target <-table(Data$Target,Data$Nacionality)
Nationality_Names <-c("Portugese", "German", "Spanish", "Italian", "Dutch", "English", "Lithunian", "Angolan",
                      "Verdean", "Guinean", "Mozmbcan", "Santoman", "Turkish", "Brazilian", "Romanian",
                      "Moldovan", "Mexican", "Ukrainian", "Russian", "Cuban", "Colombian")

barplot(Nationality_Target,
        beside = TRUE,
        col = c("blue", "orange","green"),
        legend.text = c("Dropouts", "Enrolled", "Graduated"),
        ylab = "Number of students",
        main = "Total number of students by nationality",
        names = Nationality_Names,las = 2)
#the majority of the students are portugese (thus this column is not significant)

#Graduation based on age
Data$Target <- as.integer(factor(Data$Target))

Target_labels <- c("Dropout", "Enrolled", "Graduate")
boxplot(Data$Age.at.enrollment ~ Target,col = c("red", "white","yellow") , data = Data, xlab = 'Target', ylab = 'Age', main = 'Relationship between Age and Target',names = Target_labels)

student_status_labels <- c("Dropouts", "Enrolled", "Graduated")
debtor_status_labels <- c("No Debt", "In Debt")
#most of the students in all states are of young age

#Graduation based on Financial obligations
counts <- table(Data$Target, Data$Debtor)

barplot(counts,col = c("blue","orange","green"),
        legend.text = student_status_labels,
        xlab = "Debtor Status", ylab = "Count",
        main = "Impact of Financial Obligations on Student Educational Status",
        names.arg = debtor_status_labels)
#most of the in debt students are droputs and most of the students that don't have any financial obligations graduated (so it's a significant column)

#Graduation based on Financial aid
scholarship_status_labels <- c("No Scholarship", "Scholarship Holder")
scholarship_Counts <- table(Data$Target,Data$Scholarship.holder)

barplot(scholarship_Counts, beside = TRUE, col = c("blue","orange","green"),
        legend.text = student_status_labels,
        args.legend = list(title = "Student Status"),
        xlab = "Scholarship Status", ylab = "Count",
        main = "Impact of Financial Aids on Student Educational Status",
        names.arg = scholarship_status_labels,
        ylim = c(0, max(counts) + 10))
#student's with scholarships have higher potential (very significant)


#Displaying the correlation between the columns using heatmap
correlation_matrix <- cor(Data)
melted_cormat <- melt(correlation_matrix)

ggplot(data = melted_cormat, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  labs(title = "Correlation Heatmap", x = "", y = "")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



heatmap(as.matrix(sorted_corr), Colv=NA, Rowv=NA, col = heat.colors(256), scale="column", margins=c(5,10))
title('Factors Ranker')





unique_values <- unique(Data$Target)
print(unique_values)
#"Dropout"  "Graduate" "Enrolled" are the unique values
#We will transform these values to numerical values 
Data$Target <- factor(Data$Target, levels = c("Dropout", "Graduate", "Enrolled"), labels = c(1, 2, 3))
head(Data)



# Calculate the correlation matrix
target_column <- Data$Target
Data_without_target <- select(Data, -Target)
target_numeric <- as.numeric(as.character(target_column))
correlation_matrix <- cor(target_column, Data_without_target)
print(correlation_matrix)

print("correlation with Target")
print(correlation_matrix)

#--------------------------------------------------------EDA---------------------------------------------------------#
#--------------------------------------------------------Hypothesis Testing---------------------------------------------------------#

#Please check the run on this link https://colab.research.google.com/drive/1HANwJ8_JwXXWUNaHffnfrpLL8upckkHX?usp=sharing#scrollTo=EQrgpekFOO1h
# **Hypothesis Testing**

str(Data)
# Daytime/Evening Attendance VS Dropout Rate

unique_values <- unique(Data$Target)
cat("Unique values in 'Target' column:\n")
print(unique_values)


DroupoutData <- Data
DroupoutData$Target <- factor(DroupoutData$Target, levels = c("Dropout", "Graduate", "Enrolled"), labels = c(1, 2, 3))

head(DroupoutData)

library(stats)

# Example: Daytime vs. Evening Attendance and Dropout Rates
# 1—daytime, 0—evening

#target 1 dropout, 2 draguate , 3 enrolled

# Select only dropout cases (optional)
dropout_data <- DroupoutData[DroupoutData$Target == 1, ]

# Extract the numeric codes from the factor levels (assuming you want to use these for the test)
daytime_group <- as.numeric(DroupoutData[DroupoutData$`Daytime.evening.attendance` == 1, "Target"])
evening_group <- as.numeric(DroupoutData[DroupoutData$`Daytime.evening.attendance` == 0, "Target"])

# Check for missing values in daytime and evening groups
#anyNA(daytime_group)
#anyNA(evening_group)

# Remove rows with missing values in Target for both attendance groups
#DroupoutData <- DroupoutData[!is.na(DroupoutData$Target), ]

# Proceed with Wilcoxon Rank Sum test
wilcox.test.results <- wilcox.test(daytime_group, evening_group)

# Interpret results
if (wilcox.test.results$p.value < 0.05) {
  cat("Reject null hypothesis: Daytime and evening attendance have a statistically significant effect on dropout rates (Wilcoxon Rank Sum, p-value =", wilcox.test.results$p.value, ").\n")
} else {
  cat("Fail to reject null hypothesis: No evidence of a significant difference in dropout rates based on attendance time (Wilcoxon Rank Sum, p-value =", wilcox.test.results$p.value, ").\n")
}

# Create a contingency table
contingency_table <- table(Data$`Daytime.evening.attendance`, Data$Target)

# Perform Chi-square test
chisq.test.results <- chisq.test(contingency_table)

# Interpret results
if (chisq.test.results$p.value < 0.05) {
  cat("Reject null hypothesis: There is a statistically significant association between daytime/evening attendance and dropout rates (Chi-square test, p-value =", chisq.test.results$p.value, ").\n")
} else {
  cat("Fail to reject null hypothesis: No evidence of a significant association between daytime/evening attendance and dropout rates (Chi-square test, p-value =", chisq.test.results$p.value, ").\n")
}
#  Gender VS Dropout Rate

library(stats)

# Assuming "Target" is the dropout label and "Gender" is the gender variable

# Select only dropout cases (optional)
dropout_data <- DroupoutData[DroupoutData$Target == 1, ]

# 1. T-test (assuming normality of dropout rates for each gender)
male_group <- as.numeric(DroupoutData[DroupoutData$Gender == 1, "Target"])
female_group <- as.numeric(DroupoutData[DroupoutData$Gender == 0, "Target"])


t.test.results <- t.test(male_group, female_group)

# Interpret results (look at p-value)
cat("T-test results:\n")
if (t.test.results$p.value < 0.05) {
  cat("Reject null hypothesis: Gender has a statistically significant effect on dropout rates (t-test, p-value =", t.test.results$p.value, ").\n")
} else {
  cat("Fail to reject null hypothesis: No evidence of a significant difference in dropout rates between genders (t-test, p-value =", t.test.results$p.value, ").\n")
}

# 2. Wilcoxon Rank Sum Test (alternative non-parametric test)
wilcox.test.results <- wilcox.test(male_group, female_group)

# Interpret results (look at p-value)
cat("\nWilcoxon Rank Sum test results:\n")
if (wilcox.test.results$p.value < 0.05) {
  cat("Reject null hypothesis: Gender has a statistically significant effect on dropout rates (Wilcoxon Rank Sum, p-value =", wilcox.test.results$p.value, ").\n")
} else {
  cat("Fail to reject null hypothesis: No evidence of a significant difference in dropout rates between genders (Wilcoxon Rank Sum, p-value =", wilcox.test.results$p.value, ").\n")
}
# Check Column Names

column_names <- names(DroupoutData)
print(column_names)


str(DroupoutData)
# Scholarship VS Dropout Rate

str(DroupoutData)

library(stats)

# Check for missing values (optional, but recommended)
anyNA(DroupoutData$Target)
anyNA(DroupoutData$Scholarship.holder)

# Handle missing values if necessary (e.g., remove rows, impute)

# Check data type of Dropout and International (optional, but informative)
class(DroupoutData$Target)
class(DroupoutData$Scholarship.holder )

# Assuming Dropout can be represented numerically (e.g., 1 = Dropout)
Scholarship_students <-as.numeric( DroupoutData[DroupoutData$Scholarship.holder  == 1, "Target"] )#yes
Regular_students <-as.numeric( DroupoutData[DroupoutData$Scholarship.holder  == 0 , "Target"]) #no

wilcox.test.results <- wilcox.test(Scholarship_students, Regular_students)

# Interpret results (look at p-value)
cat("Wilcoxon Rank Sum test results (Scholarships holder & Dropout):\n")
if (wilcox.test.results$p.value < 0.05) {
  cat("Reject null hypothesis: There is a statistically significant relationship between scholarship status and dropout rates (Wilcoxon Rank Sum, p-value =", wilcox.test.results$p.value, ").\n")
} else {
  cat("Fail to reject null hypothesis: No evidence of a significant relationship between scholarship status and dropout rates (Wilcoxon Rank Sum, p-value =", wilcox.test.results$p.value, ").\n")
}
# International VS Dropout Rate (Esma Dropped it sil)


library(stats)

# Check for missing values (optional, but recommended)
anyNA(DroupoutData$Target)
anyNA(DroupoutData$Marital.status)

# Handle missing values if necessary (e.g., remove rows, impute)

# Check data type of Dropout and International (optional, but informative)
class(DroupoutData$Target)
class(DroupoutData$Marital.status)

# Assuming Dropout can be represented numerically (e.g., 1 = Dropout)
international_students <-as.numeric( DroupoutData[DroupoutData$International == 1, "Target"] )#yes
domestic_students <-as.numeric( DroupoutData[DroupoutData$International == 0 , "Target"]) #no

t.test.results <- t.test(international_students, domestic_students)

# Interpret results (look at p-value)
cat("T-test results:\n")
if (t.test.results$p.value < 0.05) {
  cat("Reject null hypothesis: International students have a statistically significant difference in dropout rates compared to domestic students (Wilcoxon Rank Sum, p-value =", wilcox.test.results$p.value, ").\n")
} else {
  cat("Fail to reject null hypothesis: No evidence of a significant difference in dropout rates between international and domestic students (Wilcoxon Rank Sum, p-value =", wilcox.test.results$p.value, ").\n")
}



wilcox.test.results <- wilcox.test(international_students, domestic_students)

# Interpret results (look at p-value)
cat("Wilcoxon Rank Sum test results (International Status & Dropout):\n")
if (wilcox.test.results$p.value < 0.05) {
  cat("Reject null hypothesis: International students have a statistically significant difference in dropout rates compared to domestic students (Wilcoxon Rank Sum, p-value =", wilcox.test.results$p.value, ").\n")
} else {
  cat("Fail to reject null hypothesis: No evidence of a significant difference in dropout rates between international and domestic students (Wilcoxon Rank Sum, p-value =", wilcox.test.results$p.value, ").\n")
}
# MaritalStatus VS Dropout Rate


#	1—Single
#2—Married
#3—Widower
#4—Divorced
#5—Facto union
#6—Legally separated
dropout_rates_by_marital_status <- DroupoutData %>%
  filter(Target == 1) %>%  # Filter for Target = 1 (assuming "Target" indicates dropout)
  group_by(Marital.status) %>%
  summarise(count = n())

print(dropout_rates_by_marital_status)

library(stats)

# Assuming "Dropout" is a factor with levels like "1", "2", "3" (indicating dropout status)
# Assuming "Marital.status" is a factor with levels 1 to 6 representing marital statuses

# Check for missing values (optional, but recommended)
#anyNA(DroupoutData$Target)
#anyNA(DroupoutData$Marital.status)

# Handle missing values if necessary (e.g., remove rows, impute)

# Check data type of Dropout and Marital.status (optional, but informative)
#class(DroupoutData$Target)
#class(DroupoutData$Marital.status)

# Chi-square test for relationship between marital status and dropout
contingency_table <- table(DroupoutData$Target, DroupoutData$Marital.status)
chi_square_results <- chisq.test(contingency_table)

# Interpret results (look at p-value)
cat("Chi-square test results (Marital Status & Dropout):\n")
if (chi_square_results$p.value < 0.05) {
  cat("Reject null hypothesis: There is a statistically significant relationship between marital status and dropout rates (chi-square, p-value =", chi_square_results$p.value, ").\n")
} else {
  cat("Fail to reject null hypothesis: No evidence of a significant relationship between marital status and dropout rates (chi-square, p-value =", chi_square_results$p.value, ").\n")
}
# Special  Needs VS Dropout Rate

# Assuming "DropoutData" is your data frame

# Check for missing values (optional but recommended)
anyNA(DroupoutData$Target)
anyNA(DroupoutData$`Educational.special.needs`)  # Escape the dot in the variable name

# Handle missing values if necessary (e.g., remove rows, impute)

# Contingency table (shows co-occurrence of dropout rates and special needs)
contingency_table <- table(DroupoutData$Target, DroupoutData$`Educational.special.needs`)

# Chi-square test to assess the relationship
chi_square_results <- chisq.test(contingency_table)

# Interpret results (look at p-value)
cat("Chi-square test results:\n")
if (chi_square_results$p.value < 0.05) {
  cat("Reject null hypothesis: There is a statistically significant relationship between special needs and dropout rates (chi-square, p-value =", chi_square_results$p.value, ").\n")
} else {
  cat("Fail to reject null hypothesis: No evidence of a significant relationship between special needs and dropout rates (chi-square, p-value =", chi_square_results$p.value, ").\n")
}


# "Target"(1: dropout, 0: non-dropout)

Special_needs <-as.numeric( DroupoutData[DroupoutData$Educational.special.needs== 1, "Target"] )#yes
regular <-as.numeric( DroupoutData[DroupoutData$Educational.special.needs == 0 , "Target"]) #no

# Wilcoxon test
wilcox.test.results <- wilcox.test(Special_needs, regular)

# Interpret results (look at p-value)
cat("\nWilcoxon Rank Sum test results:\n")
if (wilcox.test.results$p.value < 0.05) {
  cat("Reject null hypothesis: There is a statistically significant relationship between special needs and dropout rates (Wilcoxon Rank Sum, p-value =", wilcox.test.results$p.value, ").\n")
} else {
  cat("Fail to reject null hypothesis: No evidence of a significant relationship between special needs and dropout rates (Wilcoxon Rank Sum, p-value =", wilcox.test.results$p.value, ").\n")
}


# Age VS Dropout Rate

# T-test (assuming equal variances)
age_t_test <- t.test(DroupoutData$Age.at.enrollment ~ DroupoutData$Target, var.equal = TRUE)

# OR Wilcoxon rank sum test (if normality assumption is violated)
age_wilcox <- wilcox.test(DroupoutData$Age.at.enrollment ~ DroupoutData$Target)

# Print test results
cat("\nAge at enrollment vs Dropout Rate:", "\n")
if (exists("age_t_test")) {
  summary(age_t_test)
} else {
  summary(age_wilcox)
}
# Approved and Enrolled units VS Dropout Rate

str(DroupoutData$Target)

# Recode Target (excluding graduates)
DroupoutData$Dropout <- ifelse(DroupoutData$Target == "Dropout", 1, 0)

# ANOVA model (testing Dropout vs. Non-Dropout)
dropout_model <- aov(Dropout ~ Curricular.units.1st.sem..approved. + Curricular.units.1st.sem..enrolled., data = DroupoutData)

# Summary of ANOVA model
summary(dropout_model)

# Tukey's HSD test for pairwise comparisons (optional)
tukey_hsd <- TukeyHSD(dropout_model)
summary(tukey_hsd)
#Tuition fees up to date VS Dropout Rate
# Splitting Data


if (!require("caret")) install.packages("caret")

library(caret)
library(ggplot2)
library(lattice)

c# Create a vector indicating the row indices for the training set
train_index <- createDataPartition(data$Target, p = 0.7, list = FALSE)


# Split the data into training and testing sets
train_data <- data[train_index, ]
test_data <- data[-train_index, ]


# Check the dimensions of the resulting data sets
print(dim(train_data))
print(train_data)
print(dim(test_data))
print(test_data)



#--------------------------------------------------------Hypothesis Testing---------------------------------------------------------#
#--------------------------------------------------------Pre-Processing-------------------------------------------------------------#
#Please check the run on this link https://colab.research.google.com/drive/1HANwJ8_JwXXWUNaHffnfrpLL8upckkHX?usp=sharing#scrollTo=EQrgpekFOO1h

str(Data)
dim(Data)

#Remove rows with null values
data <- na.omit(Data)

# Identify outliers using z-score
z_scores <- scale(data$Marital.status)
outliers <- abs(z_scores) > 3
data <- data[!outliers, ]

str(data)
dim(data)

# Print the column names of your dataframe
column_names <- names(data)
print(column_names)


# Convert unique values of target_column to integers
data$Target <- as.integer(factor(data$Target))

print(data)

if (!require("dplyr")) install.packages("dplyr")


library(dplyr)

# Drop columns
#Nationality: Since its correlation is very close to zero (-0.004740), it may not have a significant impact on the target variable.
#Mother's qualification: With a correlation of -0.038346, it appears to have a weak relationship with the target variable.
#Father's qualification: Similarly, with a correlation of 0.000329, it seems to have little influence on the target variable.
#Educational special needs: This column has a low correlation of -0.007353, suggesting it may not strongly affect the target variable.
#International: With a correlation of 0.003934, this column has minimal impact on the target variable.
#Unemployment rate: This column's correlation of 0.008627 indicates a weak relationship with the target variable.
#Inflation rate: With a correlation of -0.026874, it has a relatively low impact on the target variable.
data <- select(data,-Nacionality, -Mother.s.qualification,  -Father.s.qualification, -Educational.special.needs, -International, -Unemployment.rate, -Inflation.rate, -GDP, -Mother.s.occupation,-Father.s.occupation, -Course)

print(data)

head(Data)

# Print the column names of your dataframe
column_names <- names(data)
print(column_names)


# Assuming 'data' is your dataframe
data$Curricular_units1st72nd_sem_approved <- data$Curricular.units.1st.sem..approved. * data$Curricular.units.2nd.sem..approved.
data$Interaction_CU_1st_2nd_Grade <- data$Curricular.units.1st.sem..grade. * data$Curricular.units.2nd.sem..grade.


data$Total_CU_Approved <- data$Curricular.units.1st.sem..approved. + data$Curricular.units.2nd.sem..approved.
data$Total_CU_Grade <- (data$Curricular.units.1st.sem..grade. + data$Curricular.units.2nd.sem..grade.)/ 2

columns_to_drop <- c('Curricular.units.1st.sem..approved.' , 'Curricular.units.2nd.sem..approved.',
                     'Curricular.units.1st.sem..grade.', 'Curricular.units.2nd.sem..grade.')

data <- data[, !names(data) %in% columns_to_drop]

#--------------------------------------------------------Pre-Processing-------------------------------------------------------------#
#--------------------------------------------------------Analytical Techniques-------------------------------------------------------------#
#Please check the run on this link https://colab.research.google.com/drive/1HANwJ8_JwXXWUNaHffnfrpLL8upckkHX?usp=sharing#scrollTo=EQrgpekFOO1h

#  ***DATA ANALYTICS TECHNIQUES***



if (!require("caret")) install.packages("caret")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("e1071")) install.packages("e1071")
if (!require("text2vec")) install.packages("text2vec")



# install.packages("tidyverse")
# install.packages("caret")
# install.packages("e1071")
# install.packages("text2vec")


unique_values <- unique(Data$Target)
cat("Unique values in 'Target' column:\n")
print(unique_values)


DroupoutData <- Data
DroupoutData$Target <- factor(DroupoutData$Target, levels = c("Dropout", "Graduate", "Enrolled"), labels = c(1, 2, 3))

head(DroupoutData)
#*K-MEANS*

#our lab code
#has accuracy 87.7%
new_data <- data[, c("Gender", "Age.at.enrollment")]

kc <- kmeans(new_data, centers = 3)  # Assuming 3 clusters
kc
kc$cluster
kc$centers

# Compare the original target variable with the clustering result
table(data$Target, kc$cluster)

# Plot the clusters and their centers
plot(new_data, col = kc$cluster)
points(kc$centers, col = 1:3, pch = 8, cex = 2)


# 77.4%
#our lab code

new_data <- data[, c("Course", "Previous.qualification")]

kc <- kmeans(new_data, centers = 3)  # Assuming 3 clusters
kc
kc$cluster
kc$centers

# Compare the original target variable with the clustering result
table(data$Target, kc$cluster)

# Plot the clusters and their centers
plot(new_data, col = kc$cluster)
points(kc$centers, col = 1:3, pch = 8, cex = 2)


# 97.3%
new_data <- data[, c("Tuition.fees.up.to.date", "Curricular.units.1st.sem..grade.")]

kc <- kmeans(new_data, centers = 3)  # Assuming 3 clusters
kc
kc$cluster
kc$centers

# Compare the original target variable with the clustering result
table(data$Target, kc$cluster)

# Plot the clusters and their centers
plot(new_data, col = kc$cluster)
points(kc$centers, col = 1:3, pch = 8, cex = 2)


# 87.5%
new_data <- data[, c("Age.at.enrollment", "Scholarship.holder")]

kc <- kmeans(new_data, centers = 3)  # Assuming 3 clusters
kc
kc$cluster
kc$centers

# Compare the original target variable with the clustering result
table(data$Target, kc$cluster)

# Plot the clusters and their centers
plot(new_data, col = kc$cluster)
points(kc$centers, col = 1:3, pch = 8, cex = 2)


#87.0%
new_data <- data[, c("Curricular.units.1st.sem..enrolled.", "Curricular.units.1st.sem..grade.")]

kc <- kmeans(new_data, centers = 3)  # Assuming 3 clusters
kc
kc$cluster
kc$centers

# Compare the original target variable with the clustering result
table(data$Target, kc$cluster)

# Plot the clusters and their centers
plot(new_data, col = kc$cluster)
points(kc$centers, col = 1:3, pch = 8, cex = 2)

# 91.6%
new_data <- data[, c("Curricular.units.1st.sem..grade.", "Curricular.units.2nd.sem..grade.")]

kc <- kmeans(new_data, centers = 3)  # Assuming 3 clusters
kc
kc$cluster
kc$centers

# Compare the original target variable with the clustering result
table(data$Target, kc$cluster)

# Plot the clusters and their centers
plot(new_data, col = kc$cluster)
points(kc$centers, col = 1:3, pch = 8, cex = 2)

# 87.7%
new_data <- data[, c("Age.at.enrollment", "Daytime.evening.attendance")]

kc <- kmeans(new_data, centers = 3)  # Assuming 3 clusters
kc
kc$cluster
kc$centers

# Compare the original target variable with the clustering result
table(data$Target, kc$cluster)

# Plot the clusters and their centers
plot(new_data, col = kc$cluster)
points(kc$centers, col = 1:3, pch = 8, cex = 2)

# 96.3%
new_data <- data[, c("Previous.qualification", "Scholarship.holder")]

kc <- kmeans(new_data, centers = 3)  # Assuming 3 clusters
kc
kc$cluster
kc$centers

# Compare the original target variable with the clustering result
table(data$Target, kc$cluster)

# Plot the clusters and their centers
plot(new_data, col = kc$cluster)
points(kc$centers, col = 1:3, pch = 8, cex = 2)


# 93.1%
new_data <- data[, c("Curricular.units.1st.sem..enrolled.", "Curricular.units.1st.sem..grade.")]

kc <- kmeans(new_data, centers = 3)  # Assuming 3 clusters
kc
kc$cluster
kc$centers

# Compare the original target variable with the clustering result
table(data$Target, kc$cluster)

# Plot the clusters and their centers
plot(new_data, col = kc$cluster)
points(kc$centers, col = 1:3, pch = 8, cex = 2)

student_data <- data[, c("Gender", "Marital.status")]


# 35.7%

# Choose relevant predictor variables
predictor_vars <- c("Marital.status", "Application.mode", "Application.order", "Course", "Daytime.evening.attendance",
                    "Previous.qualification", "Mother.s.occupation", "Father.s.occupation", "Displaced", "Debtor",
                    "Tuition.fees.up.to.date", "Gender", "Scholarship.holder", "Age.at.enrollment",
                    "Curricular.units.1st.sem..credited.", "Curricular.units.1st.sem..enrolled.",
                    "Curricular.units.1st.sem..evaluations.", "Curricular.units.1st.sem..approved.",
                    "Curricular.units.1st.sem..grade.", "Curricular.units.1st.sem..without.evaluations.",
                    "Curricular.units.2nd.sem..credited.", "Curricular.units.2nd.sem..enrolled.",
                    "Curricular.units.2nd.sem..evaluations.", "Curricular.units.2nd.sem..approved.",
                    "Curricular.units.2nd.sem..grade.", "Curricular.units.2nd.sem..without.evaluations.",
                    "GDP")

# Extract predictor variables and target variable
predictors <- data[, predictor_vars]

# Perform k-means clustering
kc <- kmeans(predictors, centers = 3)

# Display clustering results
print(kc)

# Compare the clustering result with the original target variable
table(data$Target, kc$cluster)

# Plot the clusters
plot(predictors[, c("Marital.status", "Application.mode")], col = kc$cluster)
points(kc$centers[, c("Marital.status", "Application.mode")], col = 1:3, pch = 8, cex = 2)

#*Decision Tree*

if (!require("party")) install.packages("party")


# install.packages("party")
library(party)

install.packages("partykit")

#Lab code
ind<-sample(2,nrow(data),prob=c(0.7,0.3),replace=TRUE)
train.data<-data[ind==1,]
test.data<-data[ind==2,]
data.tree<- ctree(Target ~ ., data = train.data)
plot(data.tree,type="simple")
testPred<-predict(data.tree,newdata=test.data)
table(testPred,test.data$Target)
testPred

# Calculate the confusion matrix
cm <- table(testPred, test.data$Target)

# Calculate accuracy
accuracy <- sum(diag(cm)) / sum(cm)
print(paste("Accuracy: ", round(accuracy*100, 2), "%", sep=""))





# Select relevant predictor variables
predictor_vars <- c("Course", "Daytime.evening.attendance",
                    "Previous.qualification", "Displaced", "Debtor",
                    "Gender", "Scholarship.holder", "Age.at.enrollment",
                    "Curricular.units.1st.sem..credited.", "Curricular.units.1st.sem..enrolled.",
                    "Curricular.units.1st.sem..evaluations.", "Curricular.units.1st.sem..approved.",
                    "Curricular.units.1st.sem..grade.", "Curricular.units.1st.sem..without.evaluations.",
                    "Curricular.units.2nd.sem..credited.", "Curricular.units.2nd.sem..enrolled.",
                    "Curricular.units.2nd.sem..evaluations.", "Curricular.units.2nd.sem..approved.",
                    "Curricular.units.2nd.sem..grade.", "Curricular.units.2nd.sem..without.evaluations.",
                    "GDP")

# Extract predictor variables and target variable
predictors <- data[, predictor_vars]
target <- data$Target

# Split the data into training and testing sets
set.seed(123)  # For reproducibility
ind <- sample(2, nrow(data), prob = c(0.7, 0.3), replace = TRUE)
train.data <- data[ind == 1, ]
test.data <- data[ind == 2, ]

# Build the classification tree
#library(partykit)
data.tree <- ctree(Target ~ ., data = train.data)
plot(data.tree, type = "simple")

# Make predictions on the test data
testPred <- predict(data.tree, newdata = test.data)

# Display the confusion matrix
conf_matrix <- table(testPred, test.data$Target)
print(conf_matrix)

# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)

# Print accuracy
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))


# #0%

# # Splitting the data into training and testing sets
# ind <- sample(2, nrow(data), prob = c(0.7, 0.3), replace = TRUE)
# train.data <- data[ind == 1, ]
# test.data <- data[ind == 2, ]

# # Building the decision tree model
# data.tree <- ctree(Target ~ Age.at.enrollment + Father.s.occupation + Tuition.fees.up.to.date + Curricular.units.1st.sem..credited., data = train.data)

# # Plotted Tree
# plot(data.tree, type = "simple")

# # Making predictions on the test data
# testPred <- predict(data.tree, newdata = test.data)

# # Confusion matrix
# table(testPred, test.data$Target)
# # Calculate accuracy
# accuracy <- mean(testPred == test.data$Target)

# # Print accuracy
# print(paste("Accuracy:", round(accuracy * 100, 2), "%"))
#*SVM*

#install.packages("e1071")
library(e1071)

#our lab code
# Training the SVM model
svm_model <- svm( Target ~ ., data = train.data, kernel = "radial")

# Making predictions
pred <- predict(svm_model, test.data)

# Creating a confusion matrix "too long"
tab <- table(Predicted = pred, Actual = test.data$Target)
print (tab)

# Calculate accuracy
#accuracy <- sum(diag(tab)) / sum(tab)
#print(paste("Accuracy:", accuracy))
#*Naive Bayes*

#our lab code
library(e1071)

# Train the naive Bayes classifier
classifier <- naiveBayes(Target ~ ., data = train.data)

# Make predictions on the test data
output <- predict(classifier,test.data)

# Create a confusion matrix
tab <- table( output, test.data$Target)
print(tab)

# Calculate accuracy
accuracy <- sum(diag(tab)) / sum(tab)
print(paste("Accuracy: ", round(accuracy*100, 2), "%", sep=""))

#*Random Forest*

if (!require("caret")) install.packages("caret")
if (!require("randomForest")) install.packages("randomForest")



install.packages("randomForest")
install.packages("caret")


train_matrix <- as.matrix(train_data[, -which(names(train_data) %in% "Target")])
test_matrix <- as.matrix(test_data[, -which(names(test_data) %in% "Target")])

train_labels <- train_data$Target
test_labels <- test_data$Target

train_data_rf <- data.frame(train_matrix, class = factor(train_labels))
test_data_rf <- data.frame(test_matrix, class = factor(test_labels))

# Train the model
rf_model <- randomForest(class ~ ., data = train_data_rf, ntree = 100)

# Predict on test data
predicted_classes_rf <- predict(rf_model, newdata = test_data_rf)

# Calculate accuracy
accuracy_rf <- sum(predicted_classes_rf == test_data_rf$class) / nrow(test_data_rf)
print(paste("Accuracy:", round(accuracy_rf * 100, 2), "%"))



# Import necessary libraries
library(caret)
library(randomForest)
library(e1071)  # for svm()

# Prepare training and testing data
train_data <- data.frame(train_matrix, class = factor(train_labels))
test_data <- data.frame(test_matrix, class = factor(test_labels))

# Train the Random Forest model
rf_model <- randomForest(class ~ ., data = train_data, ntree = 100)

# Predict on test data
predicted_classes_rf <- predict(rf_model, newdata = test_data)

# Calculate Random Forest accuracy
rf_accuracy <- sum(predicted_classes_rf == test_data$class) / nrow(test_data)

# Train the SVM model
svm_model <- svm(class ~ ., data = train_data, kernel = "radial")

# Predict on test data
predicted_classes_svm <- predict(svm_model, newdata = test_data)

# Calculate SVM accuracy
svm_accuracy <- sum(predicted_classes_svm == test_data$class) / nrow(test_data)

# Calculate Naive Bayes accuracy
# Assuming you have already trained and predicted using a Naive Bayes model
# nb_accuracy <- sum(predicted_classes_nb == test_data$class) / nrow(test_data)

# Create a data frame to hold the accuracies
accuracy_df <- data.frame(
  Classification.Algorithm = c("SVM", "Naïve Bayes", "Random Forest"),
  Accuracy.Value = c(svm_accuracy, nb_accuracy, rf_accuracy)
)

# Print the data frame
print(accuracy_df)


# Import necessary libraries
library(caret)
library(randomForest)

# Prepare training and testing data
train_data <- data.frame(train_matrix, class = factor(train_labels))
test_data <- data.frame(test_matrix, class = factor(test_labels))

# Train the Random Forest model
rf_model <- randomForest(class ~ ., data = train_data, ntree = 100)

# Predict on test data
predicted_classes_rf <- predict(rf_model, newdata = test_data)

# Calculate Random Forest accuracy
rf_accuracy <- sum(predicted_classes_rf == test_data$class) / nrow(test_data)

# Calculate SVM accuracy
svm_accuracy <- sum(diag(tab)) / sum(tab)

# Calculate Naive Bayes accuracy
nb_accuracy <- sum(diag(tab)) / sum(tab)

# Create a data frame to hold the accuracies
accuracy_df <- data.frame(
  Classification.Algorithm = c("SVM", "Naïve Bayes", "Random Forest"),
  Accuracy.Value = c(svm_accuracy, nb_accuracy, rf_accuracy)
)

# Print the data frame
print(accuracy_df)
#*Linear Regression*

# our lab code
fit <- lm(Target ~ ., data = data)
print(fit)

# Predict the target variable
predicted_values <- predict(fit, newdata = data)

# Calculate R-squared
rss <- sum((predicted_values - mean(target))^2)
tss <- sum((target - mean(target))^2)
rsquared <- rss / tss
print(rsquared)


install.packages("glmnet")

library(glmnet)

# Split the data into training and testing sets
set.seed(123)  # for reproducibility
training_indices <- sample(1:nrow(data), nrow(data)*0.7)
train_data <- data[training_indices, ]
test_data <- data[-training_indices, ]

# Create a model matrix (removes NA values and converts categorical variables into dummy variables)
model_matrix <- model.matrix(Target ~ ., data = train_data)

# Fit the linear regression model
fit <- glmnet(model_matrix, train_data$Target, family = "gaussian")

# You can use cross-validation to choose the best lambda (penalty parameter)
cv_fit <- cv.glmnet(model_matrix, train_data$Target, family = "gaussian")
best_lambda <- cv_fit$lambda.min

# Refit the model with the best lambda
fit <- glmnet(model_matrix, train_data$Target, family = "gaussian", lambda = best_lambda)

# Now, let's predict on the test data
test_model_matrix <- model.matrix(Target ~ ., data = test_data)
predictions <- predict(fit, newx = test_model_matrix)

# Calculate Mean Squared Error (MSE)
mse <- mean((predictions - test_data$Target)^2)
cat("Mean Squared Error (MSE):", mse, "\n")


# Calculate residuals
residuals <- predicted_values - target

# Calculate RMSE
rmse <- sqrt(mean(residuals^2))

# Display RMSE
print(rmse)

# Build the linear regression model
fit <- lm(target ~ ., data = predictors)



attributes(data)
attributes(fit)

#residuals(data)
residuals(fit)

summary(fit)

#*Multinomial Logistic Regresion*

# Check unique values of the Target variable as the logistic regression takes only 0 and 1
unique(data$Target)


#our lab code
library(nnet)
model <- multinom(Target ~ ., data = train_data)

# Making predictions on the test data
predictions <- predict(model, newdata = test_data)

# Evaluate the model
accuracy <- sum(predictions == test_data$Target) / nrow(test_data)
accuracy


#another code to check if the accruacy is the same
# Fit the logistic regression model
fit <- glmnet(model_matrix, train_data$Target, family = "multinomial")

# You can use cross-validation to choose the best lambda (penalty parameter)
cv_fit <- cv.glmnet(model_matrix, train_data$Target, family = "multinomial")
best_lambda <- cv_fit$lambda.min

# Refit the model with the best lambda
fit <- glmnet(model_matrix, train_data$Target, family = "multinomial", lambda = best_lambda)

# Now, let's predict on the test data
test_model_matrix <- model.matrix(Target ~ ., data = test_data)
predictions <- predict(fit, newx = test_model_matrix, type = "response")

# Convert predictions to class labels
predicted_labels <- colnames(predictions)[apply(predictions, 1, which.max)]

# Evaluate the accuracy
accuracy <- mean(predicted_labels == test_data$Target)
cat("Accuracy:", accuracy, "\n")
#*XGB BOOST CLASSIFIER*

install.packages("xgboost")
library(xgboost)

# Create a vector indicating the row indices for the training set
train_index <- createDataPartition(data$Target, p = 0.7, list = FALSE)

# Split the data into training and testing sets
train_data <- data[train_index, ]
test_data <- data[-train_index, ]


X_train <- train_data[, predictor_vars]
Y_train <- train_data$Target


# Convert the data to xgb.DMatrix format
dtrain <- xgb.DMatrix(data = train_matrix, label = train_labels)
dtest <- xgb.DMatrix(data = test_matrix, label = test_labels)

# Train the model
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 100,
  watchlist = list(train = dtrain, test = dtest),
  print_every_n = 10,
  early_stopping_rounds = 10
)

# Predict on test data
preds <- predict(xgb_model, newdata = dtest)
pred_labels <- matrix(preds, ncol = length(unique(train_labels)), byrow = TRUE)
predicted_classes <- max.col(pred_labels) - 1

# Calculate accuracy
accuracy <- sum(predicted_classes == test_labels) / length(test_labels)
print(paste("Accuracy:", accuracy))




---
  
  
  
  **NOT** NEEDED

# #0.6%
# # Select relevant predictor variables
# predictor_vars <- c("Course", "Daytime.evening.attendance",
#                     "Previous.qualification", "Displaced", "Debtor",
#                     "Gender", "Scholarship.holder", "Age.at.enrollment",
#                     "Curricular.units.1st.sem..credited.", "Curricular.units.1st.sem..enrolled.",
#                     "Curricular.units.1st.sem..evaluations.", "Curricular.units.1st.sem..approved.",
#                     "Curricular.units.1st.sem..grade.", "Curricular.units.1st.sem..without.evaluations.",
#                     "Curricular.units.2nd.sem..credited.", "Curricular.units.2nd.sem..enrolled.",
#                     "Curricular.units.2nd.sem..evaluations.", "Curricular.units.2nd.sem..approved.",
#                     "Curricular.units.2nd.sem..grade.", "Curricular.units.2nd.sem..without.evaluations.",
#                     "GDP")

# # Extract predictor variables and target variable
# predictors <- data[, predictor_vars]
# target <- data$Target

# # Split the data into training and testing sets
# ind <- sample(2, nrow(data), prob = c(0.7, 0.3), replace = TRUE)
# train.data <- data[ind == 1, ]
# test.data <- data[ind == 2, ]

# # Build the classification tree
# data.tree <- ctree(Target ~ ., data = train.data)

# # Plot the tree
# plot(data.tree, type = "simple")

# # Make predictions on the test data
# testPred <- predict(data.tree, newdata = test.data)

# # Display the confusion matrix
# table(testPred, test.data$Target)
# # Calculate accuracy
# accuracy <- mean(testPred == test.data$Target)

# # Print accuracy
# print(paste("Accuracy:", round(accuracy * 100, 2), "%"))



# # Convert the data to xgb.DMatrix format
# dtrain <- xgb.DMatrix(data = train_matrix, label = train_labels)
# dtest <- xgb.DMatrix(data = test_matrix, label = test_labels)

# # Train the model
# xgb_model <- xgb.train(
#   params = params,
#   data = dtrain,
#   nrounds = 100,
#   watchlist = list(train = dtrain, test = dtest),
#   print_every_n = 10,
#   early_stopping_rounds = 10
# )

# # Predict on test data
# preds <- predict(xgb_model, newdata = dtest)
# pred_labels <- matrix(preds, ncol = length(unique(train_labels)), byrow = TRUE)
# predicted_classes <- max.col(pred_labels) - 1

# # Evaluate model performance
# confusionMatrix(as.factor(predicted_classes), as.factor(test_labels))


# # Choose relevant predictor variables
# predictor_vars <- c("Marital.status", "Application.mode", "Application.order", "Course", "Daytime.evening.attendance", "Previous.qualification",
#                     "Mother.s.occupation", "Father.s.occupation", "Displaced", "Debtor", "Tuition.fees.up.to.date", "Gender",
#                     "Scholarship.holder", "Age.at.enrollment", "Curricular.units.1st.sem..credited.", "Curricular.units.1st.sem..enrolled.",
#                     "Curricular.units.1st.sem..evaluations.", "Curricular.units.1st.sem..approved.", "Curricular.units.1st.sem..grade.",
#                     "Curricular.units.1st.sem..without.evaluations.", "Curricular.units.2nd.sem..credited.", "Curricular.units.2nd.sem..enrolled.",
#                     "Curricular.units.2nd.sem..evaluations.", "Curricular.units.2nd.sem..approved.", "Curricular.units.2nd.sem..grade.",
#                     "Curricular.units.2nd.sem..without.evaluations.", "GDP")

# # Extract predictor variables and target variable
# predictors <- data[, predictor_vars]
# target <- data$Target

# # Build the linear regression model
# fit <- lm(Target ~ ., data = data)

# # Display the model
# print(fit)

# # Predict the target variable
# predicted_values <- predict(fit, newdata = data)

# # Calculate accuracy (you may need to modify this based on your definition of accuracy)
# accuracy <- mean(predicted_values == target)

# # Display accuracy
# print(accuracy)


# library(xgboost)

# # Convert the target variable to a factor with appropriate levels
# train_data$Target <- factor(train_data$Target)

# # Get feature names
# feature_names <- names(train_data[, -which(names(train_data) %in% "Target")])

# # Set up the data matrix and labels for training
# dtrain <- xgb.DMatrix(data = as.matrix(train_data[, -which(names(train_data) %in% "Target")]),
#                       label = as.numeric(train_data$Target) - 1)  # XGBoost expects labels to start from 0

# # Set up parameters for the XGBoost model
# params <- list(
#   objective = "multi:softmax",  # Multi-class classification
#   num_class = 3,  # Number of classes in the target variable
#   eval_metric = "merror"  # Evaluation metric: multi-class classification error rate
# )

# # Train the XGBoost model
# xgb_model <- xgboost(params = params,
#                       data = dtrain,
#                       nrounds = 10)  # Number of boosting rounds, you can adjust this

# # Now, let's predict on the test data
# # Ensure test data has the same feature names
# test_data <- test_data[, feature_names, drop = FALSE]
# colnames(test_data) <- feature_names
# dtest <- xgb.DMatrix(data = as.matrix(test_data))
# predictions <- predict(xgb_model, dtest)

# # Convert predictions back to original labels
# predicted_labels <- as.integer(predictions) + 1  # Adding 1 to match the original label encoding

# # Evaluate the accuracy
# accuracy <- mean(predicted_labels == test_data$Target)
# cat("Accuracy:", accuracy, "\n")

# # You can also explore feature importance if needed
# importance_matrix <- xgb.importance(feature_names = feature_names,
#                                     model = xgb_model)
# print(importance_matrix)

# # Evaluate the accuracy
# accuracy <- mean(predicted_labels == test_data$Target)
# cat("Accuracy:", accuracy, "\n")

# # Check the predicted labels
# #print(predicted_labels)

# # Check the test data
# #print(test_data)




# library(party)

# # Split the data into training and testing sets
# set.seed(123)  # for reproducibility
# ind <- sample(2, nrow(data), prob = c(0.7, 0.3), replace = TRUE)
# train.data <- data[ind == 1,]
# test.data <- data[ind == 2,]

# # Train a decision tree on the training data
# student.tree <- ctree(Target ~ ., data = train.data)

# # Plot the decision tree
# plot(student.tree, type = "simple")

# # Predict the target values for the test data
# testPred <- predict(student.tree, newdata = test.data)

# # Compare the predicted values with the actual values
# confusion_matrix <- table(testPred, test.data$Target)

# # Print the confusion matrix
# print(confusion_matrix)

# # Print the predicted values
# print(testPred)

# # Calculate accuracy
# accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

# # Print accuracy
# print(paste("Accuracy:", round(accuracy * 100, 2), "%"))


# # Import necessary libraries
# library(partykit)
# library(dplyr)

# # Select relevant predictor variables
# predictor_vars <- names(data)[!names(data) %in% "Target"]

# # Extract predictor variables and target variable
# predictors <- data[, predictor_vars]
# target <- data$Target

# # Split the data into training and testing sets
# set.seed(123)  # For reproducibility
# ind <- sample(2, nrow(data), prob = c(0.7, 0.3), replace = TRUE)
# train.data <- data[ind == 1, ]
# test.data <- data[ind == 2, ]

# # Build the classification tree
# data.tree <- ctree(Target ~ ., data = train.data)

# # Make predictions on the test data
# testPred <- predict(data.tree, newdata = test.data)

# # Display the confusion matrix
# conf_matrix <- table(testPred, test.data$Target)
# print(conf_matrix)

# # Calculate accuracy
# accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)

# # Print accuracy
# print(paste("Accuracy:", round(accuracy * 100, 2), "%"))

#--------------------------------------------------------Analytical Techniques-------------------------------------------------------------#
