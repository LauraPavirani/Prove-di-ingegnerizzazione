library(tidyverse)
options(warn = -1)

############################
#   READ INPUT ARGUMENTS
############################

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 7) {
  stop("Usage:
       Rscript xmeans_preprocessing.R 
       <input_csv> 
       <coord_cols> 
       <feature_cols> 
       <min_elements_in_cluster> 
       <minimum_n_of_clusters> 
       <maximum_n_of_clusters> 
       <maximum_iterations>")
}

############################
# INPUT FILE
############################

input_file <- args[1]

if (!file.exists(input_file)) {
  stop("Input file does not exist")
}

df <- read_csv(input_file, show_col_types = FALSE) %>% drop_na()

############################
# COLUMNS
############################

coord_cols <- strsplit(args[2], ",")[[1]]
selected_features <- strsplit(args[3], ",")[[1]]

missing_cols <- setdiff(c(coord_cols, selected_features), colnames(df))
if (length(missing_cols) > 0) {
 stop(paste("Missing columns:", paste(missing_cols, collapse = ", ")))
}

# These are available for downstream use (e.g. post-processing, joining results)
coords <- df[, coord_cols]
data_vars <- df[, selected_features]  
  
############################
# PARAMETERS
############################

min_elements_in_cluster  <- as.numeric(args[4])
minimum_n_of_clusters    <- as.numeric(args[5])
maximum_n_of_clusters    <- as.numeric(args[6])
maximum_iterations       <- as.numeric(args[7])

############################
# OUTPUT FOLDER
############################

outfolder <- "xmeans_clusters_output"
if (!dir.exists(outfolder)) {
  dir.create(outfolder, recursive = TRUE)
}



############################
# JAVA FEATURE STRING
############################

features2 <- paste0("\"", selected_features, "\"", collapse = " ")

############################
# RUN X-MEANS
############################

command <- paste0(
  "java -jar ./XmeanCluster.jar ",
  "\"", input_file, "\" ",
  min_elements_in_cluster, " ",
  minimum_n_of_clusters,   " ",
  maximum_n_of_clusters,   " ",
  maximum_iterations,      " ",
  outfolder,               " ",
  features2
)

message("Running X-Means with command:\n", command)

XMeanCluster_execution <- system(
  command,
  intern               = TRUE,
  ignore.stdout        = FALSE,
  ignore.stderr        = FALSE,
  wait                 = TRUE,
  input                = NULL,
  show.output.on.console = TRUE,
  minimized            = FALSE,
  invisible            = TRUE
)

############################
# CHECK RESULT
############################
execution_success <- length(which(grepl(pattern = "OK MaxEnt", x = XMeanCluster_execution))) > 0



###########################
# CLUSTER INTERPRETATION
###########################

cluster_file <- file.path(outfolder, "clustering_table_xmeans.csv")

if (!file.exists(cluster_file)) {
  stop("Missing clustering_table_xmeans.csv")
}

df <- read.csv(cluster_file, header = TRUE)

df <- df %>%
  rename(cluster = clusterid)

data_with_clusters <- df

# fix cluster 0 (if present)
max_cluster <- max(data_with_clusters$cluster, na.rm = TRUE)
data_with_clusters$cluster[data_with_clusters$cluster == 0] <- max_cluster + 1


clustering_data <- data_with_clusters[, selected_features, drop = FALSE]



# Calculate centroids for each cluster
cluster_centroids <- data_with_clusters %>%
  group_by(cluster) %>%
  summarise(across(all_of(names(clustering_data)), mean, na.rm = TRUE))

cluster_centroids <- cluster_centroids %>% select(-cluster)

# Quantiles
feature_quantiles <- apply(clustering_data, 2, quantile)

# Prepare the centroids matrix with "M" for medium
centroid_labels <- matrix("M", nrow=nrow(cluster_centroids), ncol=ncol(cluster_centroids))


###############################################################################
######                  INTERPRETATION OF QUANTILES                      ######
###############################################################################


# Filling labeled centroids
for (centroid_idx in 1:nrow(cluster_centroids)) {
  for (feat in 1:(ncol(clustering_data))) {
    if (cluster_centroids[centroid_idx,feat]<feature_quantiles[3,feat]){    
      centroid_labels[centroid_idx, feat] <- "L"
    }
    else if (cluster_centroids[centroid_idx,feat]>feature_quantiles[4,feat]) {   
      centroid_labels[centroid_idx, feat] <- "H"
    }
    
  }
  
}

# Counting L, M, H to decide the level of attention
c_H <- matrix(nrow = nrow(centroid_labels), ncol=1)
c_M <- matrix(nrow = nrow(centroid_labels), ncol=1)
c_L <- matrix(nrow = nrow(centroid_labels), ncol=1)

# Creating empty vector for attention interpretation centroids
cluster_attention_level <- matrix(nrow = nrow(centroid_labels), ncol=1)

# Counting the letters from centroid_labels
for (r in 1:nrow(centroid_labels)) {
  c_H[r] <- sum(centroid_labels[r,] == "H")
  c_M[r] <- sum(centroid_labels[r,] == "M")
  c_L[r] <- sum(centroid_labels[r,] == "L")
}  

# Assignment 
for (i in 1:nrow(cluster_attention_level)) {
  if (c_H[i]>c_L[i] & c_H[i]>c_M[i]) {
    cluster_attention_level[i] <- "high attention"
  }
  else if (c_L[i]>=c_H[i] & c_L[i]>c_M[i]) {
    cluster_attention_level[i] <- "low attention"
  }
  else{ 
    cluster_attention_level[i] <- "medium attention"
  }
}

###############################################################################


data_with_clusters$distance_class_interpretation <- NA
for (i in 1:nrow(cluster_centroids)) {
  indici <- which(data_with_clusters$cluster == i)
  interpretation <- cluster_attention_level[i]
  data_with_clusters$distance_class_interpretation[indici] <- interpretation
}


write.csv(data_with_clusters,
          file = file.path(outfolder, "cluster_Xmeans_output.csv"),
          row.names = FALSE)
