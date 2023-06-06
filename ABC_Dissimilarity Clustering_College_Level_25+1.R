# Set working directory to College Level folder on hard drive
# Find and replace of "ABC" to college of choice in this script
setwd("C:/R Files/CCCS Cluster Analysis/Age 25+ College Level/ABC")
# set up project for each college in this directory.
# Import data file and examine data
ABC <- read.csv("ABC.csv")
# save(ABC, file = "filename.Rdata")
save(ABC, file = "ABC.Rdata")
load("ABC.Rdata")
View(ABC)

# Make cohort_person_uid row_names and not a numeric column anymore.
rownames(ABC) <- ABC[,1]
ABC <- subset(ABC, select = -cohort_person_uid)

# Change true/false//yes/no variables to logical -- Vectors coming in must be binary (1s and 0s) format!
ABC$SKILLS_BUILDER <- as.logical(ABC$SKILLS_BUILDER)
ABC$RETAINED_NEXT_SPRING <- as.logical(ABC$RETAINED_NEXT_SPRING)
ABC$RESIDENCY <- as.logical(ABC$RESIDENCY)
ABC$FIRST_GEN <- as.logical(ABC$FIRST_GEN)

# make annual_wage variable numeric if read in with $ sign from dataset
# ABC$ANNUAL_WAGE <- as.numeric(ABC$ANNUAL_WAGE)

# check to make sure variable has missing values
# is.na(ABC$ANNUAL_WAGE)

# Look at structure of current DF vectors
str(ABC)
summary(ABC)

# Clustering
install.packages("cluster")
library("cluster", lib.loc="C:/Users/s02783268/Anaconda3/envs/rstudio/lib/R/library")
require(cluster)
# Compute pairwise dissimilarities (distances) between observations in the ABC data set recognizing mixed data types using Gower option.
# gower_dist will be the resultant distance matrix using daisy function.
gower_dist <- daisy(ABC, metric = "gower")  # will get message, "setting 'logical variables [Cols #s] to type 'asymm', which just
                                            # means binary (yes/no)
saveRDS(gower_dist, file = "gower_dist")
readRDS("C:/R Files/CCCS Cluster Analysis/Age_25+ College Level/ABC/gower_dist")
# Build pam_fit cluster model using PAM (Partitioning Around Medoids) algorithm function to cluster observations from distance matrix
# into up to 10 clusters using PAM.
# Calculate silouette width for k clusters
sil_width <- c(NA)
for(i in 2:10){
  pam_fit <- pam(gower_dist, diss = TRUE, k = i)
  sil_width[i] <- pam_fit$silinfo$avg.width
}

# Plot silhouette width of 9 clusters (higher silhouette width is better to identify ideal number of clusters)
plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil_width)
# Number of clusters with the highest silhouette width is 2

# re-run cluster algorithm with 2 clusters since it has the highest silhouette width
pam_fit_ABC <- pam(gower_dist, diss = TRUE, k = 2)
saveRDS(pam_fit_ABC, file = "pam_fit_ABC.RDS")
saveRDS(pam_fit, file = "pam_fit.RDS")
# Look at pam_fit_ABC data structure and identify the two adult learners serving as the medoids
str(pam_fit_ABC)
#
# join clustering to dataset in new dataset
combined_ABC_cluster_ds <- cbind(ABC, pam_fit_ABC$clustering)
# Identify index of cluster column and change column name to "Cluster"
colnames(combined_ABC_cluster_ds)  # Returns indices of columns in DF
colnames(combined_ABC_cluster_ds)[22] <- "Cluster"
saveRDS(combined_ABC_cluster_ds, file = "combined_ABC_cluster_ds.RDS")

# Splitting out clusters
# Splitting out Cluster 1
ABC_Cluster1 <- combined_ABC_cluster_ds[which(pam_fit_ABC$clustering == 1),]
# 
saveRDS(ABC_Cluster1, file = "ABC_Cluster1.RDS")

# Summarize first cluster and output
summary(ABC_Cluster1)
# summary_ABC_Cluster1 <- summary(ABC_Cluster1)  # if necessary to use for reporting later
# View(summary_ABC_Cluster1)

# Splitting out Remaining cluster(s)
ABC_Cluster2 <- combined_ABC_cluster_ds[which(pam_fit_ABC$clustering == 2),]
saveRDS(ABC_Cluster2, file = "ABC_Cluster2.RDS")
# str(ABC_Cluster2)
summary(ABC_Cluster2)
# summary_ABC_Cluster2 <- summary(ABC_Cluster2)  # if necessary to use for reporting later

# Visualization of the two gower distance clusters using t-SNE plotting (t-distributed stochastic neighborhood embedding)
# returns two-dimensional scale
install.packages("Rtsne")
library(Rtsne)
require(Rtsne)
tsne_obj_ABC <- Rtsne(gower_dist, is_distance = TRUE)
saveRDS(tsne_obj_ABC, file = "tsne_obj_ABC")
# Setting up plot file for the two main clusters
library(dplyr)
#
tsne_data_ABC <- tsne_obj_ABC$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit_ABC$clustering),
         person_uid = ABC$person_uid)
#
saveRDS(tsne_data_ABC, file = "tsne_data_ABC")
#
install.packages("ggplot2")
library("ggplot2")
require(ggplot2)

# Running plot with persona names of clusters
ggplot(aes(x = X, y = Y), data = tsne_data_ABC) +
  geom_point(aes(color = cluster)) + scale_color_hue(labels = c("Older Males with Money but Less School Time",
                                                                "Younger Females with Less Money but More School Time ")) +
  theme(legend.position = "bottom")

# Split out remaining clusters, if any exist
# ABC_Cluster3 <- combined_ABC_cluster_ds[which(pam_fit_ABC$clustering == 3),]
# ABC_Cluster4 <- combined_ABC_cluster_ds[which(pam_fit_ABC$clustering == 4),]
# ABC_Cluster5 <- combined_ABC_cluster_ds[which(pam_fit_ABC$clustering == 5),]
# ABC_Cluster6 <- combined_ABC_cluster_ds[which(pam_fit_ABC$clustering == 6),]
# ABC_Cluster7 <- combined_ABC_cluster_ds[which(pam_fit_ABC$clustering == 7),]
# ABC_Cluster8 <- combined_ABC_cluster_ds[which(pam_fit_ABC$clustering == 8),]

# Summarize each remaining cluster one at a time and then copy and paste into Excel
# summary(ABC_Cluster3)
# summary(ABC_Cluster4)
# summary(ABC_Cluster5)
# summary(ABC_Cluster6)
# summary(ABC_Cluster7)
# summary(ABC_Cluster8)

# Save remaining cluster files
# saveRDS(ABC_Cluster3, file = "ABC_Cluster3.RDS")
# saveRDS(ABC_Cluster4, file = "ABC_Cluster4.RDS")
# saveRDS(ABC_Cluster5, file = "ABC_Cluster5.RDS")
# saveRDS(ABC_Cluster6, file = "ABC_Cluster6.RDS")
# saveRDS(ABC_Cluster7, file = "ABC_Cluster7.RDS")
# saveRDS(ABC_Cluster8, file = "ABC_Cluster8.RDS")

# Print summaries to Excel -- see https://stackoverflow.com/questions/37940271/export-r-output-to-excel

# Open cluster files for further analysis if necessary
# ABC_Cluster1 <- readRDS("C:/R Files/CCCS Cluster Analysis/ABC_Cluster1.RDS")
# ABC_Cluster2 <- readRDS("C:/R Files/CCCS Cluster Analysis/ABC_Cluster2.RDS")
# ABC_Cluster3 <- readRDS("C:/R Files/CCCS Cluster Analysis/ABC_Cluster3.RDS")
# ABC_Cluster4 <- readRDS("C:/R Files/CCCS Cluster Analysis/ABC_Cluster4.RDS")
# ABC_Cluster1 <- ABC_Cluster1[-c(1)]
# ABC_Cluster2 <- ABC_Cluster2[-c(1)]
# ABC_Cluster3 <- ABC_Cluster3[-c(1)]
# ABC_Cluster4 <- ABC_Cluster4[-c(1)]
# ABC_Cluster5 <- ABC_Cluster5[-c(1)]
# ABC_Cluster6 <- ABC_Cluster6[-c(1)]
# ABC_Cluster7 <- ABC_Cluster7[-c(1)]
# ABC_Cluster8 <- ABC_Cluster8[-c(1)]

#
# Run the table summary function in the following steps to compare programs across clusters. 
# See https://cran.r-project.org/web/packages/gtsummary/vignettes/tbl_summary.html
 install.packages("gtsummary")
 library(gtsummary)

# Run summaries individually, click on "Zoom" feature in Viewer, and then copy and paste into excel
 ABC_Cluster1 %>% tbl_summary()
 ABC_Cluster2 %>% tbl_summary()
# ABC_Cluster3 %>% tbl_summary()
# ABC_Cluster4 %>% tbl_summary()
# ABC_Cluster5 %>% tbl_summary()
# ABC_Cluster6 %>% tbl_summary()
# ABC_Cluster7 %>% tbl_summary()
# ABC_Cluster8 %>% tbl_summary()

# Run summary of all ABC adult learners. click on "Zoom" feature in Viewer, and then copy and paste into excel for cluster comparison
 combined_ABC_cluster_ds %>% tbl_summary()

# Data prep for compareGrops function
# Make cohort_person_uid a column again in the combined data/cluster dataframe
combined_ABC_cluster_ds <- cbind(rownames(combined_ABC_cluster_ds), data.frame(combined_ABC_cluster_ds,
                                                                               row.names=NULL))

# Re-name column to "cohort_person_uid" so it can be identified in compareGroups function
# install.packages("data.table", dependencies=TRUE)
# library(data.table)
setnames(combined_ABC_cluster_ds, "rownames(combined_ABC_cluster_ds)", "cohort_person_uid")
saveRDS(combined_ABC_cluster_ds, file = "combined_ABC_cluster_ds.RDS")

#############################################
# Cluster comparison for all variables (Program variable already summarized)
# See https://medium.com/@vieille.francois/compare-clusters-with-comparegroups-package-in-r-4cac20a0c00e
# Also: http://rstudio-pubs-static.s3.amazonaws.com/234038_ad78275f926c4df3b90650308dfde194.html
install.packages("compareGroups", dependencies = TRUE)
# 1. Create raw comparison data with mean and standard deviation/qty's for continuous variables
#    and %s for non-continuous variables, which is the default in the function
comparegroups.main = compareGroups(Cluster ~ . -cohort_person_uid , data = combined_ABC_cluster_ds)
# 2. Create comparison data with median and quartiles for specified continuous variables
# (e.g., AGE). Use "2" as the argument for desired variables.
comparegroups.main = compareGroups(Cluster ~ . -cohort_person_uid , data = combined_ABC_cluster_ds,
                                   method = c(AGE = 2, CREDITS_ATTEMPTED_FALL_TERM = 2,
                                              COHORT_TERM_PASS_RATE= 2, OVERALL_PASS_RATE = 2,
                                              TERM_COUNT = 2, ANNUAL_WAGE = 2))

# Create comparison table from each dataset, 
# Run for each time comparegroups.main is created above, i.e., once for means and once for medians.
comparegroups.main.table = createTable(
     x        = comparegroups.main,
     show.all = T
   )

# Run export2xls function for each time comparegroups.main.table is created
# Export comparegroups.main.table to Excel and name exported file, mean_comparegrps:
export2xls(comparegroups.main.table, file = "mean_comparegrps.xlsx", header.labels = c(p.overall = "p-value"))
# Export comparegroups.main.table to Excel and name exported file, median_comparegrps:
export2xls(comparegroups.main.table, file = "median_comparegrps.xlsx", header.labels = c(p.overall = "p-value"))

# Run this only if prefer to have html format
# Render this table into markdown format using export2md() function using knitr.
# Storing the result in comparegroups.html variable to modify content later.
comparegroups.html = suppressWarnings(
  export2md(
    x             = comparegroups.main.table,
    caption       = "",
    header.labels = c(
      "all"       = "All",
      "p.overall" = "p-value"
    )
  )
)

# print html table version to screen
comparegroups.html

# write combined (data set tagged with clusters) to excel 
install.packages("writexl")
library("writexl", lib.loc="C:/Users/s02783268/Anaconda3/envs/rstudio/lib/R/library")
write_xlsx(combined_ABC_cluster_ds,"Q:\\IR-BI\\Institutional Research\\Projects\\BIR-000980 Adult Learners
           Study\\College Level Datasets\\ABC\\combined_ABC_cluster_ds.xlsx")
