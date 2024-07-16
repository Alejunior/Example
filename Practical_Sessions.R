
# Installing packages

install.packages("readr")
install.packages("dplyr")
install.packages("flextable")
install.packages("ggplot2")
install.packages("corrplot")

# Loading packages

library(readr)
library(dplyr)
library(flextable)
library(ggplot2)
library(corrplot)

# Loading dataset

data <- read.csv("dataset_plos.csv")

View(data)

str(data)

summary(data$sex) #it does not make sense because sex is a categorical variables

# Data transformaation

data_transf <- data %>%
    mutate(gender = ifelse(sex == 1, "Male", "Female"))

# Gender table

gender_count <- round(prop.table(table(data_transf$gender))* 100, 2)
gender_count

# check if gender_count is a data frame
is.data.frame(gender_count)   

# transform data frame as a data frame
gender_count <- as.data.frame(gender_count) 

# Display gender table using flextable

gender_table <- gender_count %>% 
    flextable() %>% 
    theme_vanilla() %>% # change the visualisation using vanilla theme
    set_header_labels(Var1 = "Gender", Freq = "Percentage") %>% # change column header name
    set_caption(caption = "Table 1: Gender distribution of Nairobi population in 2008") %>% # add title to the table
    save_as_image("gender_table.png") # save (export) table as a image

gender_table

# Number and the percentage of participants we have by sex and treatment

data_transf <- data %>%
    mutate(gender = ifelse(sex == 1, "Male", "Female"), 
           
           hiv_treatment = ifelse(treatment == 1, "On ART",
                                  ifelse(treatment == 2, "Not on ART",
                                         ifelse(treatment == 3, "Treatment unknown",
                                                ifelse(treatment == 4, "Not on ART - defaulted", NA))))
    )

table(data$treatment)

table(data_transf$hiv_treatment)

dfgender_trt <- data_transf %>% # creating database for gender and hiv treatment
    select(gender,
           hiv_treatment) %>% # renaming column name
    rename(Gender = `gender`,
           HIV_Treatment = `hiv_treatment`)

dfgender_trt

gender_trt <- proc_freq(dfgender_trt, "HIV_Treatment", "Gender",
                        include.row_percent = TRUE,
                        include.column_percent = TRUE,
                        include.table_percent = TRUE) %>% 
    theme_vanilla()

gender_trt

# Displaying a scatterplot between age and Systolique BP

df <- read.csv("df.csv")
View(df)

str(df)

dfage_tas <- df %>% # creating database 
    select(id,
           age,
           tas)

plotage_tas <- ggplot(dfage_tas, aes(x = age, y = tas)) +
    geom_point(color = "darkblue") + # Change the color of the points
    geom_smooth(method = "lm", se = FALSE, color = "red") + # Add a regression line
    labs(title = "Scatter Plot showing the association between age and the systolic BP", # Add a title
         x = "Age", # Label for x-axis
         y = "Systolic BP") + # Label for y-axis
    theme_minimal() +
    theme(panel.background = element_rect(fill = 'white'),
          panel.grid = element_blank(),
          axis.line = element_line(color = 'black'))

plotage_tas

# Save the plot

ggsave("plotage_tas.png", width = 8, height = 6)

# To know all the colors available in R

colors()

# Heapmap displaying the correlation between variable 

# Numerical values
# Remove the 'id' column

df_numeric <- df[, sapply(df, is.numeric)]

# Calculate the correlation matrix
cor_matrix <- cor(df_numeric, use = "complete.obs")
cor_matrix

# Create the heatmap
corrplot(cor_matrix, method = "color", type = "lower", 
         tl.col = "black", tl.srt = 30, 
         addCoef.col = "black", number.cex = 0.7, 
         col = colorRampPalette(c("darkblue", "white", "darkred"))(200))

# Gender distribution by age group

## Let us create the variable age_group

summary(data$age)

custom_bins <- c(12, 32, 52, 72, 93)

custom_labels <- c("12-32", "33-52", "53-72", "73-93") ### Custom intervals for the age

data_transf <- data %>%
    mutate(gender = ifelse(sex == 1, "Male", "Female"), 
           
           hiv_treatment = ifelse(treatment == 1, "On ART",
                                  ifelse(treatment == 2, "Not on ART",
                                         ifelse(treatment == 3, "Treatment unknown",
                                                ifelse(treatment == 4, "Not on ART - defaulted", NA)))),
           
           age_group = cut(age, breaks = custom_bins, labels = custom_labels, right = FALSE)
    )

## Let us create the dataset for that

data_genderage <- data_transf %>% 
    select(
        gender,
        age,
        age_group
    ) %>% 
    rename(Gender = `gender`,
           Age_Group = `age_group`)

gender_agegroup <- proc_freq(data_genderage, "Age_Group", "Gender",
                        include.row_percent = TRUE,
                        include.column_percent = TRUE,
                        include.table_percent = TRUE) %>% 
    theme_vanilla()

gender_agegroup

summary(data_genderage$age)

table(data_genderage$Age_Group, useNA = "ifany")

# Proportions for each age_group

age_group_summary <- data_genderage %>%
    filter(!is.na(Age_Group)) %>%
    count(Age_Group) %>%
    mutate(prop = round(n / sum(n) * 100, 2))

age_group_summary

# Display plot the barplot for age_group

ggplot(age_group_summary, aes(x = Age_Group, y = prop)) +
    geom_bar(stat = "identity", fill = "#256D75", color = "white") +
    geom_text(aes(label = paste0(round(prop, 2), "%")), vjust = -0.2, size = 3.5) +
    labs(title = "Age group distribution of the study population", x = "Age_Group", y = "Proportion (%)") +
    theme_minimal() +
    theme(panel.background = element_rect(fill = 'white'),
          panel.grid = element_blank(),
          axis.line = element_line(color = 'black'),
          text = element_text(size = 12))

ggsave("age_group.png", width = 8, height = 6)

# Display an histogram for age

hist_age <- ggplot(data, aes(x = age)) + 
    geom_histogram(binwidth = 5, fill = "#256D75", color = "black", alpha = 0.7) +
    labs(x = "Age", y = "Frequency") +
    theme_minimal() +
    theme(panel.background = element_rect(fill = 'white'),
          panel.grid = element_blank(),
          axis.line = element_line(color = 'black'),
          text = element_text(size = 12))

hist_age

# An alternative to visualise age using a density plot

density_age <- ggplot(data, aes(x = age)) + 
    geom_density(fill = "#256D75") +
    labs(x = "Age", y = "Density") +
    theme_minimal() +
    theme(panel.background = element_rect(fill = 'white'),
          panel.grid = element_blank(),
          axis.line = element_line(color = 'black'))

density_age

# Age_Group and gender
# Calculate summary statistics

agender_summary <- data_genderage %>%
    filter(!is.na(Age_Group)) %>%
    filter(!is.na(Gender)) %>%
    group_by(Age_Group, Gender) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    group_by(Age_Group) %>%
    mutate(percentage = count / sum(count) * 100) %>%
    ungroup()

## Bar plot of the age distribution based on gender

ggplot(agender_summary, aes(x = Age_Group, y = percentage, fill = Gender)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = paste0(round(percentage, 2), "%")), 
              position = position_dodge(width = 0.9), vjust = -0.4, size = 2.8) +
    labs(title = "Gender distribution by age group", x = "Age Group", y = "Percentage") +
    scale_fill_manual(values = c('pink', "#256D75")) +
    theme_minimal() +
    theme(panel.background = element_rect(fill = 'white'),
          panel.grid = element_blank(),
          axis.line = element_line(color = 'black')) +
    scale_y_continuous(limits = c(0, max(agender_summary$percentage) + 5))

ggsave("age_gender.png", width = 8, height = 6)

# Gender distribution using pie chart

data_genderage %>% 
    filter(!is.na(Gender)) %>%
    count(Gender) %>%
    mutate(prop = n / sum(n) * 100) %>%
    ggplot(aes(x = "", y = prop, fill = Gender)) +
    geom_bar(stat = "identity", width = 1, color = "black") +
    geom_text(aes(label = paste0(round(prop, 1), "%")), position = position_stack(vjust = 0.5)) +
    coord_polar("y") +
    scale_fill_manual(values = c("pink", "#256D75")) +
    labs(title = "", fill = "Gender") + 
    theme_void() +
    theme(legend.position = "right")

ggsave("sex_pie.png", width = 6, height = 6, dpi = 300)
