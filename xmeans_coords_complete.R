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
       <maximum_iterations>
       [invert_vars]
       [standardize_vars]
       [time]")
}

############################
# INPUT FILE
############################

input_file <- args[1]

if (!file.exists(input_file)) {
  stop("Input file does not exist")
}

df <- read.csv(input_file, header = TRUE, stringsAsFactors = FALSE)

df <- df %>% drop_na()

############################
# COLUMNS
############################

coord_cols <- strsplit(args[2], ",")[[1]]
selected_features <- strsplit(args[3], ",")[[1]]

missing_cols <- setdiff(c(coord_cols, selected_features), colnames(df))
if (length(missing_cols) > 0) {
  stop(paste("Missing columns:", paste(missing_cols, collapse = ", ")))
}

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
# OPTIONAL PREPROCESSING ARGS
############################

invert_vars <- if (length(args) >= 8 && nchar(trimws(args[8])) > 0) {
  strsplit(args[8], ",")[[1]]
} else {
  c()
}

standardize_vars <- if (length(args) >= 9 && nchar(trimws(args[9])) > 0) {
  strsplit(args[9], ",")[[1]]
} else {
  c()
}

time <- if (length(args) >= 10 && nchar(trimws(args[10])) > 0) {
  paste0("_", trimws(args[10]))
} else {
  ""
}

############################
# OUTPUT FOLDER
############################

outfolder <- "xmeans_clusters_output"
if (!dir.exists(outfolder)) {
  dir.create(outfolder, recursive = TRUE)
}

############################
# VARIABLE PRE-PROCESSING (STRADA 1)
# -> ONLY VALUES CHANGE, NOT NAMES
############################

processed_data <- data.frame(matrix(nrow = nrow(data_vars), ncol = 0))

for (var_name in colnames(data_vars)) {
  
  x <- data_vars[[var_name]]
  
  # INVERSION (NO NAME CHANGE)
  if (var_name %in% invert_vars) {
    x <- ifelse(x == 0, 0, 1 / x)
  }
  
  # STANDARDIZATION (NO NAME CHANGE)
  if (var_name %in% standardize_vars) {
    x <- scale(x)[,1]
  }
  
  # KEEP ORIGINAL NAME ALWAYS
  processed_data[[var_name]] <- x
}

############################
# SAVE PREPROCESSED DATASET
############################

input_basename <- tools::file_path_sans_ext(basename(input_file))
processed_filename <- paste0(outfolder, "/", input_basename, "_processing.csv")

processed_output <- cbind(coords, processed_data)

write.csv(
  processed_output,
  file = processed_filename,
  row.names = FALSE,
  quote = FALSE
)

cat("Preprocessed dataset saved to:", processed_filename, "\n")

############################
# JAVA FEATURE STRING (UNCHANGED NAMES)
############################

features2 <- paste0("\"", selected_features, "\"", collapse = " ")

############################
# RUN X-MEANS
############################

command <- paste0(
  "java -jar ./XmeanCluster.jar ",
  "\"", processed_filename, "\" ",
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
  show.output.on.console = TRUE,
  invisible            = TRUE
)

############################
# CHECK RESULT
############################

execution_success <- length(which(grepl("OK MaxEnt", XMeanCluster_execution))) > 0

############################
# CLUSTER INTERPRETATION
############################

cluster_file <- file.path(outfolder, "clustering_table_xmeans.csv")

if (!file.exists(cluster_file)) {
  stop("Missing clustering_table_xmeans.csv")
}

df_2 <- read.csv(cluster_file, header = TRUE)

df_2 <- df_2 %>%
  rename(cluster = clusterid)

data_with_clusters <- df_2

max_cluster <- max(data_with_clusters$cluster, na.rm = TRUE)
data_with_clusters$cluster[data_with_clusters$cluster == 0] <- max_cluster + 1

clustering_data <- data_with_clusters[, selected_features, drop = FALSE]

############################
# CENTROIDS
############################

cluster_centroids <- data_with_clusters %>%
  group_by(cluster) %>%
  summarise(across(all_of(names(clustering_data)), mean, na.rm = TRUE)) %>%
  select(-cluster)

feature_quantiles <- apply(clustering_data, 2, quantile)

centroid_labels <- matrix("M",
                          nrow = nrow(cluster_centroids),
                          ncol = ncol(clustering_data))

for (centroid_idx in 1:nrow(cluster_centroids)) {
  for (feat in 1:ncol(clustering_data)) {
    
    if (cluster_centroids[centroid_idx, feat] < feature_quantiles[3, feat]) {
      centroid_labels[centroid_idx, feat] <- "L"
    }
    else if (cluster_centroids[centroid_idx, feat] > feature_quantiles[4, feat]) {
      centroid_labels[centroid_idx, feat] <- "H"
    }
  }
}

############################
# ATTENTION LEVEL
############################

c_H <- rowSums(centroid_labels == "H")
c_M <- rowSums(centroid_labels == "M")
c_L <- rowSums(centroid_labels == "L")

cluster_attention_level <- character(length(c_H))

for (i in 1:length(c_H)) {
  if (c_H[i] > c_L[i] & c_H[i] > c_M[i]) {
    cluster_attention_level[i] <- "high attention"
  } else if (c_L[i] >= c_H[i] & c_L[i] > c_M[i]) {
    cluster_attention_level[i] <- "low attention"
  } else {
    cluster_attention_level[i] <- "medium attention"
  }
}

############################
# ASSIGN BACK
############################

data_with_clusters$distance_class_interpretation <- NA

for (i in 1:nrow(cluster_centroids)) {
  indici <- which(data_with_clusters$cluster == i)
  data_with_clusters$distance_class_interpretation[indici] <- cluster_attention_level[i]
}

############################
# OUTPUT FINAL FILE
############################

write.csv(
  data_with_clusters,
  file = file.path(outfolder, paste0("cluster_Xmeans_output", time, ".csv")),
  row.names = FALSE
)