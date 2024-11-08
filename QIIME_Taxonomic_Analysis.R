#install packages
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("tibble")
install.packages("writexl")
install.packages("ggsci")

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyverse)
library(tibble)
library(writexl)
library(ggsci)

View(QIIME_2_Level2_Phylum)
Mydata <- QIIME_2_Level2_Phylum
View(Mydata)


# My data for Bacteria at Phylum level
B_data <- tribble(
  ~Study_site, ~k__Bacteria_p_Proteobacteria, ~k__Bacteria_p_Planctomycetes, ~k__Archaea_p_Euryarchaeota, ~k__Bacteria_p_Nitrospirae, ~k__Bacteria_p_Tenericutes, ~k__Bacteria_p_Actinobacteria, ~k__Bacteria_p_Acidobacteria, ~k__Bacteria_p_Bacteroidetes, ~k__Bacteria_p_Chloroflexi, ~k__Bacteria_p_Unknown, ~k__Bacteria_p_Verrucomicrobia, ~k__Bacteria_p_MBNT15, ~k__Bacteria_p_Firmicutes, ~k__Bacteria_p_Cyanobacteria, ~k__Bacteria_p_Caldatribacteriota, ~k__Bacteria_p_Gemmatimonadetes, ~k__Bacteria_p_Thermodesulfobacteria, ~k__Bacteria_p_Actinobacteria_Firmicutes, ~k__Archaea_p_Crenarchaeota, ~k__Bacteria_p_candidate_division_Zixibacteria, ~k__Bacteria_p_Candidatus_Cloacimonetes, ~k__Bacteria_p_Cloacimonadota, ~k__Bacteria_p_Spirochaetes, ~k__Bacteria_p_Synergistetes, ~k__Bacteria_p_Deinococcus_Thermus, ~k__Bacteria_p_Firmicutes_Proteobacteria, ~k__Bacteria_p_Candidatus_Omnitrophica, ~k__Bacteria_p_Elusimicrobia, ~k__Bacteria_p_candidate_division_NC10, ~k__Bacteria_p_Armatimonadetes, ~k__Bacteria_p_WPS_2, ~k__Bacteria_p_Candidatus_Melainabacteria, ~k__Bacteria_p_Thermotogae, ~k__Bacteria_p_Aquificae, ~k__Bacteria_p_Chlamydiae, ~k__Bacteria_p_SAR324_clade_Marine_group_B, ~k__Bacteria_p_Actinobacteria_Firmicutes_Proteobacteria, ~k__Bacteria_p_Cyanobacteria_Proteobacteria, ~k__Bacteria_p_Fibrobacteres, ~k__Bacteria_p_NB1_j, ~k__Bacteria_p_RCP2_54, ~k__Bacteria_p_Caldiserica, ~k__Bacteria_p_Rhodothermaeota, ~k__Bacteria_p_Chlorobi, ~k__Bacteria_p_Ignavibacteriae, ~k__Bacteria_p_Chlamydiae_Firmicutes, ~k__Bacteria_p_Actinobacteria_Proteobacteria, ~k__Bacteria_p_Abditibacteriota, ~k__Bacteria_p_Lentisphaerae, ~k__Bacteria_p_Bacteroidetes_Firmicutes, ~k__Bacteria_p_Fusobacteria,
  "Gituamba", 308782, 11332, 413, 953, 116, 69946, 17982, 17645, 8700, 330, 5045, 645, 31407, 42845, 331, 2090, 0, 38, 1165, 101, 31, 9, 189, 238, 75, 177, 27, 9, 56, 816, 157, 728, 4, 91, 762, 26, 16, 55, 701, 43, 17, 33, 87, 209, 326, 0, 56, 52, 17, 6, 0,
  "Mangu", 130390, 5447, 223, 423, 70, 27628, 4266, 7866, 3190, 196, 1406, 141, 14591, 26520, 71, 626, 15, 7, 199, 24, 12, 6, 138, 50, 0, 45, 14, 0, 7, 198, 196, 513, 32, 23, 336, 39, 0, 9, 334, 0, 25, 14, 30, 37, 150, 11, 0, 8, 0, 0, 15
)

# Transpose the phyla data
transposed_data <- B_data %>%
  pivot_longer(-Study_site) %>%
  pivot_wider(names_from = Study_site, values_from = value)

# View the transposed data
View(transposed_data)

#Checking my working directory
getwd()

#Exporting my transposed data as an Excel file
write_xlsx(transposed_data, path = "C:/Users/Mwanza/Desktop/Banana_Eugene1/Parametric_Statistics_R_TRAINING/Bacteria_data.xlsx")

View(Bacteria_Phyla_data)
str(Bacteria_Phyla_data)

#Filter out rows where both Gituamba and Mangu Amplicon sequences are less than 500
Bacteria_Phyla_filtered_data <- Bacteria_Phyla_data[Bacteria_Phyla_data$Gituamba >= 500 & Bacteria_Phyla_data$Mangu >= 500, ]
view(Bacteria_Phyla_filtered_data)

#Exporting my filtered data as an Excel file
write_xlsx(Bacteria_Phyla_filtered_data, path = "C:/Users/Mwanza/Desktop/Banana_Eugene1/Parametric_Statistics_R_TRAINING/Bacteria_filtered_data.xlsx")

View(Bacteria_Phyla_filtered_data)

# Calculate percentages of Abundances in the Bacteria Phyla filtered data
Bacteria_Phyla_filtered_data$Gituamba_percent <- (Bacteria_Phyla_filtered_data$Gituamba / sum(Bacteria_Phyla_filtered_data$Gituamba)) * 100
Bacteria_Phyla_filtered_data$Mangu_percent <- (Bacteria_Phyla_filtered_data$Mangu / sum(Bacteria_Phyla_filtered_data$Mangu)) * 100

#Exporting my filtered data with Relative Abundance values as an Excel file
write_xlsx(Bacteria_Phyla_filtered_data, path = "C:/Users/Mwanza/Desktop/Banana_Eugene1/Parametric_Statistics_R_TRAINING/Bacteria_filtered_RA_data.xlsx")

View(Bacteria_filtered_RA_data)

# Prepare the Phyla data for plotting
Bacteria_data_long <- Bacteria_filtered_RA_data %>%
  pivot_longer(-Bacterial_Phyla, names_to = "Location", values_to = "Relative_Abundance")

view(Bacteria_data_long)

ggplot(Bacteria_data_long, aes(x = Relative_Abundance, y = Location, fill = Bacterial_Phyla)) +
  geom_bar(stat = "identity") +
  labs(x = "Relative Abundance (%)", y = "Location", fill = "Bacterial Phyla") +
  theme_minimal() +
  coord_flip()  + 
  scale_fill_igv() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

##Genus data (Qiime Level 6)
View(Bacteria_genus_data)
str(Bacteria_genus_data)

# Transpose the Bacteria Genera dataset
Bacteria_genus_data_transposed <- Bacteria_genus_data %>%
  pivot_longer(cols = -c(Location), names_to = "Variable", values_to = "Abundance")

# Filter out Bacterial Genera /variables  with abundances less than 1000 for both Gitumba and Mangu
Bacteria_data_genus_filtered <- Bacteria_genus_data_transposed %>%
  group_by(Location, Variable) %>%
  filter(all(Abundance >= 1000))

view(Bacteria_data_genus_filtered)

#Exporting my filtered data as an Excel file
write_xlsx(Bacteria_data_genus_filtered, path = "C:/Users/Mwanza/Desktop/Banana_Eugene1/Parametric_Statistics_R_TRAINING/Bacteria_genus_filtered_data.xlsx")

View(Bacteria_genus_filtered_data)

#Subsetting my datasets by location first

view(Gituamba_data)

Bacterial_Gituamba_data <- data.frame(
  Location = c(rep("Gituamba", 60)),
  Bacterial_genera = c("Luteitalea", "Pseudomonas", "Aetherobacter", "Candidatus_Koribacter", "Ohtaekwangia", "Gaiella", "Pantoea", "Streptomyces", "Actinomarinicola", "Luteibacter", "Aciditerrimonas", "Actinoallomurus", "Ktedonobacter", "Hyphomicrobium", "Gemmata", "Mesorhizobium", "Mycolicibacterium", "Haliangium", "Brasilonema", "Polyangium_brachysporum_group", "Sphingobium", "Solirubrobacter", "Brevitalea", "Bacillus", "Leptothrix", "Fimbriiglobus", "Bradyrhizobium", "Reyranella", "Terrimonas", "Baekduia", "Stella", "Phenylobacterium", "Desulfofundulus", "Conexibacter", "Burkholderia", "Rhizobium", "Nocardioides", "Mixta", "Vicinamibacter", "Acidibacter", "Rhodoplanes", "Romboutsia", "Serratia", "Pseudolabrys", "Devosia", "Sphingomonas", "Stenotrophomonas", "Mitsuaria", "Leucobacter", "Lactococcus", "Klebsiella", "Aurantimicrobium", "Raoultella", "Kosakonia", "Achromobacter", "Lewinella", "Chryseolinea", "Candidatus_Solibacter", "Aeromicrobium", "Lactiplantibacillus"),
  Abundance = c(1375, 2209, 1009, 6589, 2366, 7299, 136142, 9243, 2264, 1198, 1451, 1909, 1440, 1079, 1964, 1155, 1001, 3651, 42207, 1294, 1855, 4213, 1049, 5454, 5083, 1513, 1432, 1340, 1163, 1106, 6403, 1494, 1365, 1598, 14347, 4386, 1451, 1595, 1850, 1907, 1441, 1396, 25410, 1424, 1242, 8060, 2426, 1607, 1568, 1006, 12721, 1084, 4813, 2253, 1360, 1142, 1585, 1843, 1009, 3724)
)

view(Bacterial_Gituamba_data)

# Sort the data by Abundance in descending order
Bacterial_Gituamba_sorted_data <- Bacterial_Gituamba_data[order(-Bacterial_Gituamba_data$Abundance), ]

View(Bacterial_Gituamba_sorted_data)

#Select the first 22 rows and then select only the columns for Bacterial_genera and Abundance
Bacteria_Genera_top_22 <- head(Bacterial_Gituamba_sorted_data, 22)[, c("Bacterial_genera", "Abundance")]

view(Bacteria_Genera_top_22)

View(Mangu_data)

# Sort the data by Abundance in descending order
Bacterial_Mangu_sorted_data <- Mangu_data[order(-Mangu_data$Abundance), ]

View(Bacterial_Mangu_sorted_data)

#Exporting my sorted data as an Excel file
write_xlsx(Bacterial_Gituamba_sorted_data, path = "C:/Users/Mwanza/Desktop/Banana_Eugene1/Parametric_Statistics_R_TRAINING/Bacteria_GG_sorted.xlsx")
write_xlsx(Bacterial_Mangu_sorted_data, path = "C:/Users/Mwanza/Desktop/Banana_Eugene1/Parametric_Statistics_R_TRAINING/Bacteria_MG_sorted.xlsx")

# Create data frames for Gituamba and Mangu bacterial abundance
Bacteria_Gituamba <- data.frame(
  Bacteria = c("Pantoea", "Brasilonema", "Serratia", "Burkholderia", "Klebsiella", "Streptomyces", "Sphingomonas", "Gaiella", "Candidatus_Koribacter", "Stella", "Bacillus", "Leptothrix", "Raoultella", "Rhizobium", "Solirubrobacter", "Lactiplantibacillus", "Haliangium", "Stenotrophomonas", "Ohtaekwangia", "Actinomarinicola", "Kosakonia", "Pseudomonas"),
  Gituamba = c(136142, 42207, 25410, 14347, 12721, 9243, 8060, 7299, 6589, 6403, 5454, 5083, 4813, 4386, 4213, 3724, 3651, 2426, 2366, 2264, 2253, 2209)
)

#Export Bacteria Genus data for Gituamba
write_xlsx(Bacteria_Gituamba, path = "C:/Users/Mwanza/Desktop/Banana_Eugene1/Parametric_Statistics_R_TRAINING/Bacteria_Genus_Gituamba.xlsx")

Bacteria_Mangu <- data.frame(
  Bacteria = c("Pantoea", "Brasilonema", "Serratia", "Burkholderia", "Klebsiella", "Stella", "Raoultella", "Leptothrix", "Gaiella", "Bacillus", "Rhizobium", "Sphingomonas", "Lactiplantibacillus", "Candidatus_Koribacter", "Streptomyces", "Solirubrobacter", "Kosakonia", "Haliangium", "Stenotrophomonas", "Pseudomonas", "Conexibacter", "Mitsuaria"),
  Mangu = c(41544, 26143, 17596, 8737, 7210, 3904, 3253, 2664, 2582, 2477, 2328, 2210, 2092, 1650, 1344, 1614, 1364, 1344, 1223, 1109, 1146, 1109)
)

#Export Bacteria Genus data for Mangu
write_xlsx(Bacteria_Mangu, path = "C:/Users/Mwanza/Desktop/Banana_Eugene1/Parametric_Statistics_R_TRAINING/Bacteria_Mangu.xlsx")

# Calculate relative abundances (%)
Bacteria_Gituamba$Gituamba_percent <- Bacteria_Gituamba$Gituamba / sum(Bacteria_Gituamba$Gituamba) * 100
Bacteria_Mangu$Mangu_percent <- Bacteria_Mangu$Mangu / sum(Bacteria_Mangu$Mangu) * 100

view(Bacteria_Gituamba)
view(Bacteria_Mangu)

#Export subsetted data for RA
write_xlsx(Bacteria_Gituamba, path = "C:/Users/Mwanza/Desktop/Banana_Eugene1/Parametric_Statistics_R_TRAINING/Bacteria_Gituamba_GRA.xlsx")
write_xlsx(Bacteria_Mangu, path = "C:/Users/Mwanza/Desktop/Banana_Eugene1/Parametric_Statistics_R_TRAINING/Bacteria_Mangu_GRA.xlsx")

# Combine the data frames
Bacterial_Genera_combined_df <- merge(Bacteria_Genera_Gituamba_Relative_Abundance, Bacteria_Genera_Mangu_Relative_Abundance, by = "Bacterial_Genera", all = TRUE)
view(Bacterial_Genera_combined_df)

#Export the combined df
write_xlsx(Bacterial_Genera_combined_df, path = "C:/Users/Mwanza/Desktop/Banana_Eugene1/Parametric_Statistics_R_TRAINING/Bacteria_Genera_Combined_RA.xlsx")

#Prepare the data for plotting
Bacteria_G_data_long <- Bacterial_Genera_combined_df %>%
  pivot_longer(-Bacterial_Genera, names_to = "Location", values_to = "Relative_Abundance")

View(Bacteria_G_data_long)

ggplot(Bacteria_G_data_long, aes(x = Relative_Abundance, y = Location, fill = Bacterial_Genera)) +
  geom_bar(stat = "identity") +
  labs(x = "Relative Abundance (%)", y = "Location", fill = "Bacterial Genera") +
  theme_minimal() +
  coord_flip()  + 
  scale_fill_igv() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

##Creating a taxonomic R 3D Pie chart
#Load the package
install.packages("plotrix")
library(plotrix)

# Create data for the graph.
Amplicon_taxon <- c(50, 117, 247,508, 1440,2748)
labels <- c("Phyla", "Class", "Order", "Family", "Genus", "Species")
piepercent<- round(100 * Amplicon_taxon / sum(Amplicon_taxon), 1)

# Plot the chart.
pie(Amplicon_taxon, labels = piepercent,col = rainbow(length(Amplicon_taxon)))
legend("topright",c("Phyla", "Class", "Order", "Family", "Genus", "Species") ,
       cex = 0.8, fill = rainbow(length(Amplicon_taxon)))
