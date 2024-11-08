#Calculating the spearman correlation analysis of soil variables in data1, data2 and data3 and plotting the results

# Calculate the correlation matrix using Spearman method

View(Finer_Sample1_Gituamba_SoilChemistry)

sample1 <- Finer_Sample1_Gituamba_SoilChemistry
view(sample1)
sample1$LOCATION <- NULL

#Export Gituamba_Soil_data
getwd()
write_xlsx(sample1, path = "C:/Users/Mwanza/Desktop/Banana_Eugene1/Parametric_Statistics_R_TRAINING/Gituamba_Soil_data.xlsx")

#calculate the spearman correlation for sample1

cor_matrix <- cor(sample1, method = "spearman")
View(cor_matrix)

#installing the corrplot package

install.packages("corrplot")
library("corrplot")

# Plot the correlation matrix  for sample1 using corrplot
corrplot(cor_matrix, method = "number", type = "upper", order = "hclust")


View(Finer_Sample2_Mangu_SoilChemistry)

sample2 <- Finer_Sample2_Mangu_SoilChemistry
sample2$LOCATION <- NULL
view(sample2)

#Exporting my Mangu_data as an Excel file
write_xlsx(sample2, path = "C:/Users/Mwanza/Desktop/Banana_Eugene1/Parametric_Statistics_R_TRAINING/Mangu_Soil_data.xlsx")

# Calculate the spearman correlation for sample2
cor_matrix <- cor(sample2, method = "spearman")
View(cor_matrix)

# Plot the correlation matrix  for sample2 using corrplot
corrplot(cor_matrix, method = "number", type = "upper", order = "hclust")

View(Finer_Sample3_Ngenda_SoilChemistry)
sample3 <- Finer_Sample3_Ngenda_SoilChemistry
sample3$LOCATION <- NULL

# Calculate the spearman correlation for sample3
cor_matrix <- cor(sample3, method = "spearman")
View(cor_matrix)

# Plot the correlation matrix  for sample3 using corrplot
corrplot(cor_matrix, method = "number", type = "upper", order = "hclust")

#calculating the mean of columns in data frame of sample 1 (Gituamba)
str(Finer_Sample1_Gituamba_SoilChemistry)
View(Finer_Sample1_Gituamba_SoilChemistry)
data1 <- Finer_Sample1_Gituamba_SoilChemistry
View(data1)
data1$LOCATION <- NULL

str(data1$pH)
str(data1)
mean(data1$pH)
sd(data1$pH)

# Calculate the confidence interval for the pH
confidence_interval <- t.test(data1$pH)$conf.int

# Print the result
cat("95% Confidence Interval for pH:", confidence_interval[1], "to", confidence_interval[2], "\n")

# Calculate the standard error of the mean (SEM) for pH
sem_pH <- sd(data1$pH) / sqrt(length(data1$pH))

#print the result
cat("Standard Error of the Mean (SEM) for pH:", sem_pH, "\n")

mean(data1$Ec_H2O)

# Calculate the standard error of the mean (SEM) for Ec_H2O
sem_Ec_H2O <- sd(data1$Ec_H2O) / sqrt(length(data1$Ec_H2O))

#print the result
cat("Standard Error of the Mean (SEM) for Ec_H2O:", sem_Ec_H2O, "\n")

sd(data1$Ec_H2O)

mean(data1$`%TOC`)
sd(data1$`%TOC`)

# Calculate the standard error of the mean (SEM) for TOC
sem_TOC <- sd(data1$`%TOC`) / sqrt(length(data1$`%TOC`))

#print the result
cat("Standard Error of the Mean (SEM) for TOC:", sem_TOC, "\n")


mean(data1$O.M)
sd(data1$O.M)

# Calculate the standard error of the mean (SEM) for OM
sem_OM <- sd(data1$O.M) / sqrt(length(data1$O.M))

#print the result
cat("Standard Error of the Mean (SEM) for OM:", sem_OM, "\n")

mean(data1$`%N`)
sd(data1$`%N`)

# Calculate the standard error of the mean (SEM) for N
sem_N <- sd(data1$`%N`) / sqrt(length(data1$`%N`))

#print the result
cat("Standard Error of the Mean (SEM) for N:", sem_N, "\n")

mean(data1$`%K`)
sd(data1$`%K`)

# Calculate the standard error of the mean (SEM) for K
sem_K <- sd(data1$`%K`) / sqrt(length(data1$`%K`))

#print the result
cat("Standard Error of the Mean (SEM) for K:", sem_K, "\n")

mean(data1$`P2O5mg/kg`)
sd(data1$`P2O5mg/kg`)


# Calculate the standard error of the mean (SEM) for P
sem_P <- sd(data1$`P2O5mg/kg`) / sqrt(length(data1$`P2O5mg/kg`))

#print the result
cat("Standard Error of the Mean (SEM) for P:", sem_P, "\n")

#calculating the mean of columns in data frame of sample 2 (Mangu)
str(Finer_Sample2_Mangu_SoilChemistry)
View(Finer_Sample2_Mangu_SoilChemistry)
data2 <- Finer_Sample2_Mangu_SoilChemistry
data2$LOCATION <- NULL
View(data2)

mean(data2$pH)
sd(data2$pH)

# Calculate the standard error of the mean (SEM) for pH
sem_pH2 <- sd(data2$pH) / sqrt(length(data2$pH))

#print the result
cat("Standard Error of the Mean (SEM) for pH:", sem_pH2, "\n")

mean(data2$Ec_H2O)
sd(data2$Ec_H2O)

# Calculate the standard error of the mean (SEM) for pH
sem_Ec_H2O2 <- sd(data2$Ec_H2O) / sqrt(length(data2$Ec_H2O))

#print the result
cat("Standard Error of the Mean (SEM) for H2O:", sem_Ec_H2O2, "\n")

mean(data2$`%TOC`)
sd(data2$`%TOC`)

# Calculate the standard error of the mean (SEM) for TOC
sem_TOC2 <- sd(data2$`%TOC`) / sqrt(length(data2$`%TOC`))

#print the result
cat("Standard Error of the Mean (SEM) for TOC:", sem_TOC2, "\n")

mean(data2$O.M)
sd(data2$O.M)

# Calculate the standard error of the mean (SEM) for OM
sem_OM2 <- sd(data2$O.M) / sqrt(length(data2$O.M))

#print the result
cat("Standard Error of the Mean (SEM) for OM:", sem_OM2, "\n")

mean(data2$`%N`)
sd(data2$`%N`)

# Calculate the standard error of the mean (SEM) for N
sem_N2 <- sd(data2$`%N`) / sqrt(length(data2$`%N`))

#print the result
cat("Standard Error of the Mean (SEM) for N:", sem_N2, "\n")

mean(data2$`%K`)
sd(data2$`%K`)


# Calculate the standard error of the mean (SEM) for K
sem_K2 <- sd(data2$`%K`) / sqrt(length(data2$`%K`))

#print the result
cat("Standard Error of the Mean (SEM) for K:", sem_K2, "\n")

mean(data2$`P205mg/kg`)
sd(data2$`P205mg/kg`)


# Calculate the standard error of the mean (SEM) for P
sem_P2 <- sd(data2$`P205mg/kg`) / sqrt(length(data2$`P205mg/kg`))

#print the result
cat("Standard Error of the Mean (SEM) for P:", sem_P2, "\n")


##calculating the mean of columns in data frame of sample 3 (Ngenda)
str(Finer_Sample3_Ngenda_SoilChemistry)
View(Finer_Sample3_Ngenda_SoilChemistry)
data3 <- Finer_Sample3_Ngenda_SoilChemistry
data3$LOCATION <- NULL
View(data3)
mean(data3$pH)
sd(data3$pH)

# Calculate the standard error of the mean (SEM) for pH
sem_pH3 <- sd(data3$pH) / sqrt(length(data3$pH))

#print the result
cat("Standard Error of the Mean (SEM) for pH:", sem_pH3, "\n")

mean(data3$EC_H2O)
sd(data3$EC_H2O)

# Calculate the standard error of the mean (SEM) for Ec_H2O
sem_H2O3<- sd(data3$EC_H2O) / sqrt(length(data3$EC_H2O))

#print the result
cat("Standard Error of the Mean (SEM) for H2O:", sem_H2O3, "\n")

mean(data3$`%TOC`)
sd(data3$`%TOC`)


# Calculate the standard error of the mean (SEM) for TOC
sem_TOC3<- sd(data3$`%TOC`) / sqrt(length(data3$`%TOC`))

#print the result
cat("Standard Error of the Mean (SEM) for TOC:", sem_TOC3, "\n")


mean(data3$`%OM`)
sd(data3$`%OM`)

# Calculate the standard error of the mean (SEM) for OM
sem_OM3<- sd(data3$`%OM`) / sqrt(length(data3$`%OM`))

#print the result
cat("Standard Error of the Mean (SEM) for OM:", sem_OM3, "\n")

mean(data3$`%N`)
sd(data3$`%N`)

# Calculate the standard error of the mean (SEM) for N
sem_N3<- sd(data3$`%N`) / sqrt(length(data3$`%N`))

#print the result
cat("Standard Error of the Mean (SEM) for N:", sem_N3, "\n")

mean(data3$`%K`)
sd(data3$`%K`)

# Calculate the standard error of the mean (SEM) for K
sem_K3<- sd(data3$`%K`) / sqrt(length(data3$`%K`))

#print the result
cat("Standard Error of the Mean (SEM) for N:", sem_K3, "\n")


mean(data3$`P205mg/kg`)
sd(data3$`P205mg/kg`)


# Calculate the standard error of the mean (SEM) for P
sem_P3<- sd(data3$`P205mg/kg`) / sqrt(length(data3$`P205mg/kg`))

#print the result
cat("Standard Error of the Mean (SEM) for P:", sem_P3, "\n")

#Install packages
install.packages("ggplot2")
install.packages("multcompView")
install.packages("ggsignif")
install.packages("dplyr")

library(ggplot2)
library(multcompView)
library(ggsignif)
library(dplyr)


#Calculating ANOVA , TUKEY'S HSD and CREATING BOX PLOTS FOR ALL SOIL VARIABLES

#vIEW and check the structure of pH DATA
View(pH_DATA)
str(pH_DATA)

Soil_pH <- pH_DATA
Soil_pH$...1 <- NULL
View(Soil_pH)
str(Soil_pH)

pH_VALUES <- Soil_pH
View(pH_VALUES)
str(pH_VALUES)

#Combine the pH columns into a single vector
pH_data <- c(pH_VALUES$Gituamba, pH_VALUES$Mangu, pH_VALUES$Ngenda)
str(pH_data)
View(pH_data)

#Create a grouping variable
Location <- rep(c("Gituamba", "Mangu", "Ngenda"), each = 26)

# Combine the data into a data frame
pH_data_df <- data.frame(Location = factor(Location), pH = as.numeric(pH_data))


# Calculate the quartiles and IQR
Q1 <- quantile(pH_data_df$pH, 0.25)
Q3 <- quantile(pH_data_df$pH, 0.75)
IQR <- Q3 - Q1

# Define the boundaries for outliers
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Filter out outliers
pH_data_filtered <- pH_data_df[pH_data_df$pH >= lower_bound & pH_data_df$pH <= upper_bound, ]

# Perform one-way ANOVA for the pH variable with the grouping variable
anova_model <- aov(pH ~ Location, data = pH_data_filtered)

# Display ANOVA results
summary(anova_model)

# Perform Tukey's post hoc analysis
posthoc <- TukeyHSD(anova_model)

# Display post hoc results
print(posthoc)

#compact letter display
cld_pH <- multcompLetters4(anova_model, posthoc)
print(cld_pH)

#Creating a table with the summarised data and the compact letter display
# table with factors and 3rd quantile
pH_K <- group_by(pH_data_filtered,Location) %>%
  summarise(mean=mean(pH), quant = quantile(pH, probs = 0.75)) %>%
  arrange(desc(mean))

# extracting the compact letter display and adding to the pH_K table
cld_pH <- as.data.frame.list(cld_pH$Location)
pH_K$cld_pH <- cld_pH$Letters

print(pH_K)


# Create a box plot with significance letters
pH_boxplot <- ggplot(pH_data_filtered, aes(x = Location, y = pH, fill = Location)) +
  geom_boxplot() +
  labs(x = "Location",
       y = "pH") +
  theme_minimal() + geom_text(data = pH_K, aes(x = Location, y = quant, label = cld_pH), size = 5, vjust=-1, hjust =-1) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#Print the pH box plot 
print(pH_boxplot)


#Structure for EC_H2O DATA
str(EC_H2O)

SoilWater_EC <- EC_H2O
View(SoilWater_EC)
str(SoilWater_EC)

SoilWater_EC$...1 <- NULL
View(SoilWater_EC)

# Combine the SOIL water content columns into a single vector
EC <- c(SoilWater_EC$Gituamba, SoilWater_EC$Mangu, SoilWater_EC$Ngenda)

View(EC)
str(EC)

# Create a grouping variable
Location <- rep(c("Gituamba", "Mangu", "Ngenda"), each = 26)

# Combine the data into a data frame
Soil_EC <- data.frame(Location = factor(Location), Soilwater_Ec = as.numeric(EC))
View(Soil_EC)

# Perform one-way ANOVA for the SoilEc_H20 variable with the grouping variable
anova_model <- aov(Soilwater_Ec ~ Location, data = Soil_EC)


# Display ANOVA results
summary(anova_model)

# Perform Tukey's post hoc analysis
posthoc <- TukeyHSD(anova_model)

# Display post hoc results
print(posthoc)

# compact letter display
cld_Soil_EC <- multcompLetters4(anova_model, posthoc)
print(cld_Soil_EC)

#Creating a table with the summarised data and the compact letter display
# table with factors and 3rd quantile
Soil_EC_K <- group_by(SoilEc_H20,Location) %>%
  summarise(mean=mean(Soilwater_Ec), quant = quantile(Soilwater_Ec, probs = 0.75)) %>%
  arrange(desc(mean))

#Extracting the compact letter display and adding to the Soil_EC_K table
cld_Soil_EC <- as.data.frame.list(cld_Soil_EC$Location)
Soil_EC_K$cld_Soil_EC <- cld_Soil_EC$Letters

print(Soil_EC_K)

# Create a box plot with significance letters
SoilWater_EC_boxplot <- ggplot(Soil_EC, aes(x = Location, y = Soilwater_Ec , fill = Location)) +
  geom_boxplot() +
  labs(x = "Location",
       y = "Soilwater_Ec (dS /m)") +
  theme_minimal() + geom_text(data = EcH20_K, aes(x = Location, y = quant, label = cld_Ec_H20), size = 5, vjust=-1, hjust =-1) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#Print the EC box plot 
print(SoilWater_EC_boxplot)


View(Total_Carbon_content)

# Combine the total_carbon columns into a single vector
carbon_content <- c(Total_Carbon_content$Gituamba, Total_Carbon_content$Mangu, Total_Carbon_content$Ngenda)

View(carbon_content)
str(carbon_content)

# Create a grouping variable
Location <- rep(c("Gituamba", "Mangu", "Ngenda"), each = 26)


# Combine the data into a data frame
SoilCarbonContent_df <- data.frame(Location = factor(Location), Carbon_content = as.numeric(carbon_content))

# Perform one-way ANOVA for the carbon_content variable with the grouping variable
anova_model <- aov(Carbon_content ~ Location, data = SoilCarbonContent_df)

# Display ANOVA results
summary(anova_model)

# Perform Tukey's post hoc analysis
posthoc <- TukeyHSD(anova_model)

# Display post hoc results
print(posthoc)


# compact letter display
cld_Carbon_content <- multcompLetters4(anova_model, posthoc)
print(cld_Carbon_content)

#Creating a table with the summarised data and the compact letter display
# table with factors and 3rd quantile
Carbon_content_K <- group_by(SoilCarbonContent_df,Location) %>%
  summarise(mean=mean(Carbon_content), quant = quantile(Carbon_content, probs = 0.75)) %>%
  arrange(desc(mean))

# extracting the compact letter display and adding to the carbon_content_K table
cld_Carbon_content <- as.data.frame.list(cld_Carbon_content$Location)
Carbon_content_K$cld_Carbon_content <- cld_Carbon_content$Letters

print(Carbon_content_K)


# Create a box plot with significance letters
Carbon_content_boxplot <- ggplot(SoilCarbonContent_df, aes(x = Location, y = Carbon_content , fill = Location)) +
  geom_boxplot() +
  labs(x = "Location",
       y = "Carbon_Content (%)") +
  theme_minimal() + geom_text(data = Carbon_content_K, aes(x = Location, y = quant, label = cld_Carbon_content), size = 5, vjust=-1, hjust =-1) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#Print the carbon_content box plot 
print(Carbon_content_boxplot)


View(Total_Organic_matter)

# Combine the total_organic_matter columns into a single vector
soil_organic_matter <- c(Total_Organic_matter$Gituamba, Total_Organic_matter$Mangu, Total_Organic_matter$Ngenda)

# Create a grouping variable
Location <- rep(c("Gituamba", "Mangu", "Ngenda"), each = 26)

#Combine the data into a data frame
SoilOrganicMatter_df <- data.frame(Location = factor(Location), Organic_matter_content = as.numeric(soil_organic_matter))


# Perform one-way ANOVA for the Organic_matter_content variable with the grouping variable
anova_model <- aov(Organic_matter_content ~ Location, data = SoilOrganicMatter_df)


# Display ANOVA results
summary(anova_model)

# Perform Tukey's post hoc analysis
posthoc <- TukeyHSD(anova_model)

# Display post hoc results
print(posthoc)

# compact letter display
cld_OM <- multcompLetters4(anova_model, posthoc)
print(cld_OM)

#Creating a table with the summarised data and the compact letter display
# table with factors and 3rd quantile
OM_K <- group_by(SoilOrganicMatter_df,Location) %>%
  summarise(mean=mean(Organic_matter_content), quant = quantile(Organic_matter_content, probs = 0.75)) %>%
  arrange(desc(mean))

# extracting the compact letter display and adding to the OM_K table
cld_OM <- as.data.frame.list(cld_OM$Location)
OM_K$cld_OM <- cld_OM$Letters

print(OM_K)


# Create a box plot with significance letters
OM_boxplot <- ggplot(SoilOrganicMatter_df, aes(x = Location, y = Organic_matter_content, fill = Location)) +
  geom_boxplot() +
  labs(x = "Location",
       y = "Organic_matter (%)") +
  theme_minimal() + geom_text(data = OM_K, aes(x = Location, y = quant, label = cld_OM), size = 5, vjust=-1, hjust =-1) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#Print the OM box plot
print(OM_boxplot)


View(Total_Nitrogen_content)
str(Total_Nitrogen_content)

# Combine the total_nitrogen_matter columns into a single vector
soil_nitrogen_content <- c(Total_Nitrogen_content$Gituamba, Total_Nitrogen_content$Mangu, Total_Nitrogen_content$Ngenda)

# Create a grouping variable
Location <- rep(c("Gituamba", "Mangu", "Ngenda"), each = 26)

# Combine the data into a data frame
N_data_df <- data.frame(Location = factor(Location), Nitrogen_content = as.numeric(soil_nitrogen_content))

# Perform one-way ANOVA for the Nitrogen_content variable with the grouping variable
anova_model <- aov(Nitrogen_content ~ Location, data = N_data_df)

# Display ANOVA results
summary(anova_model)

# Perform Tukey's post hoc analysis
posthoc <- TukeyHSD(anova_model)

# Display post hoc results
print(posthoc)

#compact letter display
cld_N <- multcompLetters4(anova_model, posthoc)
print(cld_N)

#Creating a table with the summarised data and the compact letter display
# table with factors and 3rd quantile
N_K <- group_by(N_data_df,Location) %>%
  summarise(mean=mean(Nitrogen_content), quant = quantile(Nitrogen_content, probs = 0.75)) %>%
  arrange(desc(mean))

# extracting the compact letter display and adding to the pH_K table
cld_N <- as.data.frame.list(cld_N$Location)
N_K$cld_N <- cld_N$Letters

print(N_K)

# Create a box plot with significance letters
N_boxplot <- ggplot(N_data_df, aes(x = Location, y = Nitrogen_content, fill = Location)) +
  geom_boxplot() +
  labs(x = "Location",
       y = "Nitrogen_content (%)") +
  theme_minimal() + geom_text(data = N_K, aes(x = Location, y = quant, label = cld_N), size = 5, vjust=-1, hjust =-1) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#Print the Nitrogen_content box plot 
print(N_boxplot)


View(Total_Potassium_content)

# Combine the total_potassium columns into a single vector
soil_potassium_content <- c(Total_Potassium_content$Gituamba, Total_Potassium_content$Mangu, Total_Potassium_content$Ngenda)

# Create a grouping variable
Location <- rep(c("Gituamba", "Mangu", "Ngenda"), each = 26)

# Combine the data into a data frame
K_data_df <- data.frame(Location = factor(Location), Potassium_content = as.numeric(soil_potassium_content))

# Perform one-way ANOVA for the Potassium_content variable with the grouping variable
anova_model <- aov(Potassium_content ~ Location, data = K_data_df)

# Display ANOVA results
summary(anova_model)

# Perform Tukey's post hoc analysis
posthoc <- TukeyHSD(anova_model)

# Display post hoc results
print(posthoc)

#compact letter display
cld_K <- multcompLetters4(anova_model, posthoc)
print(cld_K)

#Creating a table with the summarised data and the compact letter display
# table with factors and 3rd quantile
K_K <- group_by(K_data_df,Location) %>%
  summarise(mean=mean(Potassium_content), quant = quantile(Potassium_content, probs = 0.75)) %>%
  arrange(desc(mean))

# extracting the compact letter display and adding to the pH_K table
cld_K <- as.data.frame.list(cld_K$Location)
K_K$cld_K <- cld_K$Letters

print(K_K)

# Create a box plot with significance letters
K_boxplot <- ggplot(K_data_df, aes(x = Location, y = Potassium_content, fill = Location)) +
  geom_boxplot() +
  labs(x = "Location",
       y = "Potassium_content (%)") +
  theme_minimal() + geom_text(data = K_K, aes(x = Location, y = quant, label = cld_K), size = 5, vjust=-1, hjust =-1) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#Print the Potassium_content box plot 
print(K_boxplot)

View(Total_Phosphorus_content)

# Combine the total_phosphorus_content columns into a single vector
soil_phosphorus_content <- c(Total_Phosphorus_content$Gituamba, Total_Phosphorus_content$Mangu, Total_Phosphorus_content$Ngenda)

# Create a grouping variable
Location <- rep(c("Gituamba", "Mangu", "Ngenda"), each = 26)

# Combine the data into a data frame
P_data_df <- data.frame(Location = factor(Location), Phosphorous_content = as.numeric(soil_phosphorus_content))

#Perform one-way ANOVA for the Phosphorous_content variable with the grouping variable
anova_model <- aov(Phosphorous_content ~ Location, data = P_data_df)

# Display ANOVA results
summary(anova_model)

# Perform Tukey's post hoc analysis
posthoc <- TukeyHSD(anova_model)

# Display post hoc results
print(posthoc)

#compact letter display
cld_P <- multcompLetters4(anova_model, posthoc)
print(cld_P)

#Creating a table with the summarised data and the compact letter display
# table with factors and 3rd quantile
P_K <- group_by(P_data_df,Location) %>%
  summarise(mean=mean(Phosphorous_content), quant = quantile(Phosphorous_content, probs = 0.75)) %>%
  arrange(desc(mean))

# extracting the compact letter display and adding to the pH_K table
cld_P <- as.data.frame.list(cld_P$Location)
P_K$cld_P <- cld_P$Letters

print(P_K)

# Create a box plot with significance letters
P_boxplot <- ggplot(P_data_df, aes(x = Location, y = Phosphorous_content, fill = Location)) +
  geom_boxplot() +
  labs(x = "Location",
       y = "Phosphorous_content (mg/kg)") +
  theme_minimal() + geom_text(data = P_K, aes(x = Location, y = quant, label = cld_P), size = 5, vjust=-1, hjust =-1) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#Print the Phosphorous_content box plot 
print(P_boxplot)


View(Total_Potassium_content)

# Combine the total_potassium columns into a single vector
soil_potassium_content <- c(Total_Potassium_content$Gituamba, Total_Potassium_content$Mangu, Total_Potassium_content$Ngenda)

# Create a grouping variable
Location <- rep(c("Gituamba", "Mangu", "Ngenda"), each = 26)

# Combine the data into a data frame
P_data_df <- data.frame(Location = factor(Location), Phosporous_content = as.numeric(soil_potassium_content))

# Perform one-way ANOVA for the Potassium_content variable with the grouping variable
anova_model <- aov(Potassium_content ~ Location, data = K_data_df)

# Display ANOVA results
summary(anova_model)

# Perform Tukey's post hoc analysis
posthoc <- TukeyHSD(anova_model)

# Display post hoc results
print(posthoc)

#compact letter display
cld_K <- multcompLetters4(anova_model, posthoc)
print(cld_K)

#Creating a table with the summarised data and the compact letter display
# table with factors and 3rd quantile
K_K <- group_by(K_data_df,Location) %>%
  summarise(mean=mean(Potassium_content), quant = quantile(Potassium_content, probs = 0.75)) %>%
  arrange(desc(mean))

# extracting the compact letter display and adding to the pH_K table
cld_K <- as.data.frame.list(cld_K$Location)
K_K$cld_K <- cld_K$Letters

print(K_K)

# Create a box plot with significance letters
K_boxplot <- ggplot(K_data_df, aes(x = Location, y = Potassium_content, fill = Location)) +
  geom_boxplot() +
  labs(title = "Soil Potassium_content Analysis",
       x = "Location",
       y = "Potassium_content (%)") +
  theme_minimal() + geom_text(data = K_K, aes(x = Location, y = quant, label = cld_K), size = 3, vjust=-1, hjust =-1) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#Print the potassium_content box plot 
print(K_boxplot)


























































