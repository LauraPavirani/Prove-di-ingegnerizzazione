rm(list = ls())

library(tidyverse)
options(warn = -1)


############################
#   READ INPUT ARGUMENTS
############################

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 5) {
  stop("Usage: RScript multikmeans_coords_raw_dataset.R <input_csv> <k_values> <max_iter> <coords> <features> [invert] [standardize] [time]")
}

############################
#   INPUT FILE
############################

input_file <- args[1]

if (!file.exists(input_file)) {
  stop("Input file does not exist")
}

############################
#   K VALUES 
############################

if (grepl(":", args[2])) {
  parts <- as.numeric(strsplit(args[2], ":")[[1]])
  k_values <- seq(
    from = parts[1],
    to = parts[2],
    by = ifelse(length(parts) == 3, parts[3], 1)
  )
} else {
  k_values <- as.numeric(strsplit(args[2], ",")[[1]])
}

############################
#   K-MEANS ITERATIONS
############################

N <- as.numeric(args[3])

############################
#   FEATURES + COORDINATES
############################

coord_cols <- strsplit(args[4], ",")[[1]]  
selected_features <- strsplit(args[5], ",")[[1]] 

invert_vars <- if (length(args) >= 6) strsplit(args[6], ",")[[1]] else c()
standardize_vars <- if (length(args) >= 7) strsplit(args[7], ",")[[1]] else c()


############################
#   OUTPUT FOLDER
############################

output_MKM <- "output_MKM"

if (!dir.exists(output_MKM)) {
  dir.create(output_MKM, recursive = TRUE)
  cat("Created output folder:", output_MKM, "\n")
} else {
  cat("Output folder already exists:", output_MKM, "\n")
}
############################
#   TIME (optional)
############################

time <- if (length(args) >= 8 && nchar(trimws(args[8])) > 0) {
  paste0("_", trimws(args[8]))
} else {
  ""
}


cat("time used:", time, "\n")

############################
#   DATA LOADING
############################

variables_risk1_standardized <- read_csv(input_file) %>% drop_na()

selection <- variables_risk1_standardized

############################
#   VARIABLE PRE-PROCESSING
############################

# Coordinates 
coords <- selection[, coord_cols]

# ONLY FEATURES
data_vars <- selection[, selected_features]

processed_data <- data.frame(matrix(nrow = nrow(data_vars), ncol = 0))

for (var_name in colnames(data_vars)) {
  
  x <- data_vars[[var_name]]
  new_name <- var_name
  
  # INVERSION (with zero handling)
  if (var_name %in% invert_vars) {
    x <- ifelse(x == 0, 0, 1 / x)
    new_name <- paste0(new_name, "_inv")
  }
  
  # STANDARDIZATION
  if (var_name %in% standardize_vars) {
    x <- scale(x)[,1]
    new_name <- paste0(new_name, "_std")
  }
  
  processed_data[[new_name]] <- x
}

# update final dataset
selection <- cbind(coords, processed_data)


############################
#   UPDATE FEATURE NAMES
############################

processed_feature_names <- selected_features

for (i in seq_along(selected_features)) {
  
  name <- selected_features[i]
  new_name <- name
  
  if (name %in% invert_vars) {
    new_name <- paste0(new_name, "_inv")
  }
  
  if (name %in% standardize_vars) {
    new_name <- paste0(new_name, "_std")
  }
  
  processed_feature_names[i] <- new_name
}

selected_features <- processed_feature_names

############################
#   SAVE PREPROCESSED DATASET
############################

# file name: input + _processing.csv
input_basename <- tools::file_path_sans_ext(basename(input_file))
processed_filename <- paste0(output_MKM, "/", input_basename, "_processing.csv")

# dataset with coordinates + preprocessed variables
processed_output <- cbind(coords, processed_data)

write.csv(
  processed_output,
  file = processed_filename,
  row.names = FALSE
)

cat("Preprocessed dataset saved to:", processed_filename, "\n")

############################
#   K VALIDATION
############################

v_check <- as.data.frame(selection[, selected_features])
max_k_allowed <- floor(nrow(v_check) / 2)

if (any(k_values > max_k_allowed)) {
  invalid_ks <- k_values[k_values > max_k_allowed]
  stop(paste0(
    "ERROR: K too large: ",
    paste(invalid_ks, collapse = ", ")
  ))
}

############################
#   MULTI K-MEANS
############################

bics <- c()

for (k in k_values) {
  
  cat("####I'm analyzing ", k, "centroids\n")
  
  selected_features_coords <- selection[, selected_features]
  
  # clustering_data is directly the feature dataset
  clustering_data <- as.data.frame(selected_features_coords)
  
  ############################
  # centroids
  ############################
  
  centroids <- matrix(nrow = k, ncol = ncol(clustering_data))
  
  for (centroid_idx in 1:nrow(centroids)) {
    centroids[centroid_idx, ] <- as.numeric(clustering_data[centroid_idx, ])
  }
  
  km <- kmeans(as.matrix(clustering_data), centers = as.matrix(centroids), iter.max = N)
  
  selected_features_coords$distance_class <- km$cluster
  clustering_data$distance_class <- km$cluster
  
  centroids <- as.matrix(km$centers)
  
  ############################
  # STANDARD DEVIATION
  ############################
  
  
  feature_names <- selected_features
  
  centroid_sd <- matrix(nrow = k, ncol = (ncol(clustering_data) - 1))
  
  for (centroid_idx in 1:k) {
    cluster_points <- clustering_data[clustering_data$distance_class == centroid_idx, 1:(ncol(clustering_data) - 1)]
    
    if (nrow(cluster_points) > 1) {
      centroid_sd[centroid_idx, ] <- apply(cluster_points, 2, sd)
    } else {
      centroid_sd[centroid_idx, ] <- 0
    }
  }
  
  centroid_sd_df <- as.data.frame(centroid_sd)
  names(centroid_sd_df) <- paste0(feature_names, "_sd")
  
  ############################
  # QUANTILES
  ############################
  
  # feature_quantiles <- apply(clustering_data, 2, quantile)
  
  # better option...
  feature_quantiles <- apply(clustering_data[, feature_names], 2, quantile)   # this does not include the distance_class column in the quantile computation
  
  centroid_labels <- matrix("M", nrow = nrow(centroids), ncol = length(feature_names),
                               dimnames = list(NULL, feature_names))
  

  for (centroid_idx in 1:nrow(centroids)) {
    for (feat in feature_names) {
      if (centroids[centroid_idx, feat] < feature_quantiles["50%", feat]) {
        centroid_labels[centroid_idx, feat] <- "L"
      } else if (centroids[centroid_idx, feat] > feature_quantiles["75%", feat]) {
        centroid_labels[centroid_idx, feat] <- "H"
      }
    }
  }
  
  ############################
  # INTERPRETATION
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
  # OUTPUT centroids
  ############################
  
  centroids_df <- as.data.frame(centroids)
  centroid_labels_df <- as.data.frame(centroid_labels)
  
  names(centroids_df) <- feature_names
  names(centroid_labels_df) <- paste0(feature_names, "_label")
  
  centroid_id <- data.frame(centroid_id = 1:nrow(centroids_df))
  
  centroids_annotated <- centroid_id
  
  for (i in seq_along(feature_names)) {
    centroids_annotated[[feature_names[i]]] <- centroids_df[[i]]
    centroids_annotated[[paste0(feature_names[i], "_sd")]] <- centroid_sd_df[[i]]
    centroids_annotated[[paste0(feature_names[i], "_label")]] <- centroid_labels_df[[i]]
  }
  
  centroids_annotated$attention_level <- cluster_attention_level
  
  write.csv(
    centroids_annotated,
    file =paste0(output_MKM, "/centroids_", k, "_k_values", time, ".csv"),
    row.names = FALSE
  )
  
  ############################
  # OUTPUT DATASET
  ############################
  
  clustering_data$distance_class_interpretation <- cluster_attention_level[clustering_data$distance_class]
  
  data_no_coords <- clustering_data[, !(names(clustering_data) %in% names(coords)), drop = FALSE]
  final_dataset <- cbind(coords, data_no_coords)
  
  write.csv(
    final_dataset,
    file = paste0(output_MKM, "/clustering_", k, "_k_values", time, ".csv"),
    row.names = FALSE
  )
  
  ############################
  # UNIF
  ############################
  
  centroid_distribution<-km$size
  #### CALCULATING ChiSqr
  if (length(which(centroid_distribution<=2))>0 || 
      ( (min(centroid_distribution)/max(centroid_distribution) ) <0.007) 
  ){
    cat("Unsuitable distribution: low uniformity:",(min(centroid_distribution)/max(centroid_distribution))," --- outliers: ",length(which(centroid_distribution<=2)),"\n")
    bic<-0
  }else{
    centroid_distribution.norm<-centroid_distribution/sum(centroid_distribution)
    reference<-rep(mean(centroid_distribution),length(centroid_distribution) )
    reference.norm<-reference/sum(reference)
    chisq<-sum((centroid_distribution.norm*1000-reference.norm*1000)^2/(reference.norm*1000))/length(centroid_distribution.norm)
    #high chisqr-> worse agreement with uniform distr
    #since we are selecting the maximum, let's invert the unif
    bic<-1/chisq
    
    #EXPLANATION OF THE CHI SQR CRITERION:
    #chi sqr probability calculation: for study purposes
    #the probability that the chisqr is lower than the calculcation has the inverse trend of the chisqr value
    #a small chisqr calculated means a higher probability of matching
    #a high chisqr calculated means a lower probability of matching
    #let's calculate the P(chisqr>chisqr_calculated), because the theoretical expected value of chisqr is 1
    #if this prob is high, chisqr_calculated is consistent with the expected value-> uniform distribution
    #if it is low then the chisqr_calculated is too far from the expected value-> non uniform distribution
    #uncomment for verification and testing
    #p_value <- pchisq(chisq, df = length(centroid_distribution.norm)-1, lower.tail = FALSE)
    #invert the criterion: high bic should be preferred because it corresponds to low p_value
    #bic<-p_value
    #cat("pvalue:",p_value,"\n")
    
    cat("Centroid distribution:",centroid_distribution.norm,"\n")
  }
  cat("Unif:",bic,"\n")
  bics<-c(bics,bic)
  cat("Done\n")
}

############################
# BEST K
############################

best_clusterisation <- k_values[which(bics == max(bics))]

cat("Best clustering: K=", best_clusterisation, "\n")


# SUMMARY 
results_summary <- data.frame(
  K = k_values,
  UNIF = bics
)

write.csv(
  results_summary,
  file = paste0(output_MKM, "/summary_UNIF.csv"),
  row.names = FALSE
)

# FILE BEST K 
best_clusterisation_file <- paste0(
  output_MKM,
  "/centroid_classification_assignment_",
  best_clusterisation,
  ".csv"
)

write.csv(
  data.frame(best_K = best_clusterisation),
  file = paste0(output_MKM, "/best_K.csv"),
  row.names = FALSE
)

cat("Best clustering file to take as result:", best_clusterisation_file, "\n")