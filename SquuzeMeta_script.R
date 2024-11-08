##Taxonomic and functional analysis of Shotgun data from SqueezeMeta
##Loading SQM tools for analysis of SqueezeMeta outputs

install.packages("SQMtools")
library('SQMtools')

getwd()

#Loading data into R to form A SQM object 
Ngenda = loadSQM ("C:/Users/Mwanza/Desktop/Banana_Eugene1/Banana_Soil_Microbiome_R_analysis/D3_sample")

# Matrix genera vs. abundances
genus_tax=Ngenda$taxa$genus$abund

# Matrix with COGs vs. abundances
COG_table=Ngenda$functions$COG$abund


#Plot taxonomy at the phylum level:
plotTaxonomy(Ngenda, rank ='phylum', count='percent', ignore_unmapped = TRUE,ignore_unclassified = TRUE)

#KEGG functional analysis
plotFunctions(Ngenda, fun_level = 'KEGG', count = 'tpm', N = 15)

# Extract the orf annotated with that KEGG
orf = Ngenda$orfs$table[Ngenda$orfs$table$`KEGG ID` == 'K03088']

# Extract the sequence
sequence=Ngenda$orfs$seqs[rownames(orf)]

#Picking all contigs from Actinomycetota and plotting their taxonomy at the genus level and their most abundant functions/Kegg profile of Actinomycetota
Actino=subsetTax(Ngenda, 'phylum',tax='Actinomycetota')
plotTaxonomy(Actino, 'genus','percent', N=10 )
plotFunctions(Actino, fun_level = 'KEGG',count = 'copy_number')

#Most abundant functions
plotFunctions(Ngenda, fun_level = "KEGG", count = "tpm" ,N = 25 ,fun = NULL, samples = NULL,ignore_unmapped = TRUE,ignore_unclassified = TRUE, gradient_col = c("ghostwhite", "dodgerblue4") ,  base_size = 11,metadata_groups = NULL) 

#Most abundant Taxa
plotTaxonomy(Ngenda, rank = "genus", count = "percent", N = 15, tax = NULL , others = TRUE ,samples = NULL , nocds = "treat_separately", ignore_unmapped = TRUE,ignore_unclassified = TRUE ,  no_partial_classifications = FALSE,  rescale = FALSE,  color = NULL,  base_size = 11,max_scale_value = NULL, metadata_groups = NULL )

#Exploring specific functions
#Metabolism
Metabolism = subsetFun(Ngenda, fun = 'Metabolism', rescale_copy_number = F) 
plotTaxonomy(Metabolism, 'genus','percent', N = 10, rescale = T, others = T)
plotFunctions(Metabolism, fun_level = 'KEGG',count = 'tpm')

#Genetic information processing
GIP = subsetFun(Ngenda, fun = 'Genetic Information Processing', rescale_copy_number = F) 
plotTaxonomy(GIP, 'genus','percent', N = 10, rescale = T, others = T)
plotFunctions(GIP, fun_level = 'KEGG',count = 'tpm')

#Cellular Processes
CP = subsetFun(Ngenda, fun = 'Cellular Processes', rescale_copy_number = F) 
plotTaxonomy(CP, 'genus','percent', N = 10, rescale = T, others = T)
plotFunctions(CP, fun_level = 'KEGG',count = 'tpm')

#Environmental Information Processing
EIP = subsetFun(Ngenda, fun = 'Environmental Information Processing', rescale_copy_number = F) 
plotTaxonomy(EIP, 'genus','percent', N = 10, rescale = T, others = T)
plotFunctions(EIP, fun_level = 'KEGG',count = 'tpm')

#Organismal systems
OS = subsetFun(Ngenda, fun = 'Organismal systems', rescale_copy_number = F) 
plotTaxonomy(OS, 'genus','percent', N = 10, rescale = T, others = T)
plotFunctions(OS, fun_level = 'KEGG',count = 'tpm')

#Human diseases
HD = subsetFun(Ngenda, fun = 'Human diseases', rescale_copy_number = F) 
plotTaxonomy(HD, 'genus','percent', N = 10, rescale = T, others = T)
plotFunctions(HD, fun_level = 'KEGG',count = 'tpm')

#Computing alpha diversity using SQM object
# Tranpose the matrix to have samples in rows.
Ngenda_T = t(Ngenda$taxa$genus$abund)

# Install and load the vegan package
install.packages("vegan")
library(vegan)

# Shannon diversity index
shannon_diversity <- diversity(Ngenda_T, index = "shannon")
print(shannon_diversity)

# Simpson diversity index 
simpson_diversity <- diversity(Ngenda_T, index = "simpson")
print(simpson_diversity)

# Compute observed species (richness) 
species_richness <- specnumber(Ngenda_T)

# Compute Chao1
chao1_estimates <- estimateR(Ngenda_T)
print(chao1_estimates)

Kegg_pathways <- (Ngenda$misc$KEGG_paths)

##Creating a taxonomic R 3D Pie chart
#Load the package
install.packages("plotrix")
library(plotrix)


# Create data for the graph.
Shotgun_taxon <- c(102, 238, 435,808, 1081,2791)
labels <- c("Phyla", "Class", "Order", "Family", "Genus", "Species")
piepercent<- round(100 * Shotgun_taxon / sum(Shotgun_taxon), 1)

# Plot the chart.
pie(Shotgun_taxon, labels = piepercent,col = rainbow(length(Shotgun_taxon)))
legend("topright",c("Phyla", "Class", "Order", "Family", "Genus", "Species") ,
       cex = 0.8, fill = rainbow(length(Shotgun_taxon)))
