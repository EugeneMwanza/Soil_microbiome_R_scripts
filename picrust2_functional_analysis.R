##METHOD 1 #This method didn't work
#Load ggpicrust2 package
install.packages("ggpicrust2")
install.packages("tidyverse")
install.packages("lme4", type = "source")
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("edgeR")
BiocManager::install("ALDEx2")
library(ggpicrust2)
library(tidyverse)
library(edgeR)
library(ALDEx2)

#Analysis kegg pathway abundance instead of ko within the pathway
#Load metadata
getwd()
metadata <- read_delim("C:/Users/Mwanza/Desktop/Banana_Eugene1/Parametric_Statistics_R_TRAINING/metadata.txt", delim = "\t",escape_double = FALSE, trim_ws = TRUE)
head(metadata) 
colnames(metadata)
# Check the number of samples per group
table(metadata[[group]])

Samples <- samples
group <- "sample_id"

KO_metagemome <- ("C:/Users/Mwanza/Desktop/Banana_Eugene1/Parametric_Statistics_R_TRAINING/pred_metagenome_unstrat.tsv")  
KO_daa_results_list <- ggpicrust2(KO_metagemome, metadata = metadata,group = group,pathway = "KO",daa_method = "LinDA", order = "pathway_class", ko_to_kegg = TRUE, x_lab = "pathway_name", p.adjust = "BH",select = NULL,reference = NULL)

## Sample usage of the ko2kegg_abundance function
ko_abundance_file <- "pred_metagenome_unstrat.tsv"
kegg_abundance <- ko2kegg_abundance(ko_abundance_file)

#Differential abundance(DA) analysis
##you can use ko2_kegg_abundance output
abundance <- ko2kegg_abundance(ko_abundance_file)

#daa_method default is "ALDex2"
daa_results_df <- pathway_daa(abundance = abundance,metadata = metadata, group = group ,daa_method = "ALDEx2",select = NULL, p.adjust = "BH",  reference = NULL)


##METHOD 2 ## This method also didn't work because of the nature of my dataset.

# I want to analyze the abundance of KEGG pathways instead of KO within the pathway, please set `ko_to_kegg` to TRUE.
# KEGG pathways typically have more descriptive explanations.

##Load packages
install.packages("ggpicrust2")
install.packages("readr")
install.packages("tibble")
install.packages("tidyverse")
install.packages("ggprism")
install.packages("patchwork")


if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("DESeq2")
BiocManager::install("metagenomeSeq")
BiocManager::install("Maaslin2")

library(readr)
library(ggpicrust2)
library(tibble)
library(tidyverse)
library(ggprism)
library(patchwork)
library(DESeq2)
library(metagenomeSeq)
library(Maaslin2)


# Load metadata as a tibble
metadata <- read_delim("C:/Users/Mwanza/Desktop/Banana_Eugene1/Parametric_Statistics_R_TRAINING/metadata.txt", delim = "\t",escape_double = FALSE, trim_ws = TRUE)
head(metadata) 
colnames(metadata)

# Load KEGG pathway abundance
# data(kegg_abundance)
kegg_abundance <- ko2kegg_abundance("C:/Users/Mwanza/Desktop/Banana_Eugene1/Parametric_Statistics_R_TRAINING/pred_metagenome_unstrat.tsv") 

# Perform pathway differential abundance analysis (DAA) using ALDEx2 method
# Please change group to "your_group_column" if you are not using example dataset
daa_results_df <- pathway_daa(abundance = kegg_abundance, metadata = metadata, group = "Location", daa_method = "ALDEx2", select = NULL, reference = NULL) 


##Inspecting the input files
kegg_abundance_matrix <- as.matrix(kegg_abundance)
metadata_df <- as.data.frame(metadata)
group_info <- as.factor(metadata_df$Location)
str(group_info)

# Filter out features with less than 3 nonzero values
# Replace "Column1" and "Column2" with the actual column names in your data frame
Kegg_filtered_abundance <- kegg_abundance[kegg_abundance$`S1-S1-L001-001` >= 1000 & kegg_abundance$`S2-S1-L001-001` >= 1000, ]

# Running ALDEx2
daa_results_df <- pathway_daa(
  abundance = Kegg_filtered_abundance, 
  metadata = metadata_df, 
  group = "Location", 
  daa_method = "ALDEx2", 
  select = NULL, 
  reference = NULL
)

