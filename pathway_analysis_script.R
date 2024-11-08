##Functional Pathway analysis for 16S profiles
View(pathway_data)

#load packages
install.packages("tidyverse")
install.packages("writexl")
install.packages("ggsci")
install.packages("ggpubr")


library(tidyverse)
library(writexl)
library(ggsci)
library(ggpubr)

#Filter rows where both Gituamba and Mangu have values above 450,000
pathway_data_filtered <- subset(pathway_data, pathway_data[, 2] >= 200000 & pathway_data[, 3] >= 200000)
View(pathway_data_filtered)

#Write pathway_data_filtered to a CSV file
#Exporting my filtered data as an Excel file
write_xlsx(pathway_data_filtered, path = "C:/Users/Mwanza/Desktop/Banana_Eugene1/Parametric_Statistics_R_TRAINING/pathway_data_filtered.xlsx")

#Calculate percentages of abundances of the pathways in the  filtered data
pathway_data_filtered$Gituamba_percent <- (pathway_data_filtered$Gituamba / sum(pathway_data_filtered$Gituamba)) * 100
pathway_data_filtered$Mangu_percent <- (pathway_data_filtered$Mangu / sum(pathway_data_filtered$Mangu)) * 100

view(pathway_data_filtered)

#Remove columns 2 and 3
pathway_data_filtered1 <- pathway_data_filtered[ , -c(2, 3)]
view(pathway_data_filtered1)  

# Rename column 2
colnames(pathway_data_filtered1)[2] <- "Gituamba"

# Rename column 3
colnames(pathway_data_filtered1)[3] <- "Mangu"

view(pathway_data_filtered1)

# Prepare the pathway data for plotting
Pathway_data_long <- pathway_data_filtered1 %>%
  pivot_longer(-pathway, names_to = "Location", values_to = "Relative_Abundance")
##Plotting the most abundant 16S pathways
ggplot(Pathway_data_long, aes(x = Relative_Abundance, y = Location, fill = pathway)) +
  geom_bar(stat = "identity") +
  labs(x = "Relative Abundance (%)", y = "Location", fill = "pathway") +
  theme_minimal() +
  coord_flip()  + 
  scale_fill_igv() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#Perform a two sample two-sided t-test on the functional pathways in the two locations

t.test(pathway_data$Gituamba, pathway_data$Mangu, alternative = "two.sided", var.equal = FALSE)
