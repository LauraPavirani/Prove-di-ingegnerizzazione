# CODICE CHE PRENDE IN IN PUT IL FILE RAW ORIGINALE
# OVVERO CON LE VARIABILI NON PROCESSATO MA ANCORA DA INVERTIRE E STANDARDIZZARE
# QUESTO CODICE ESPLICITA LE COORDINATE (longitude e latitude)


rm(list = ls())

library(tidyverse)
options(warn = -1)


############################
#   LETTURA ARGOMENTI
############################

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 8) {
  stop("Usage: RScript multikmeans_coords_raw_dataset.R <input_csv> <k_values> <max_iter> <coords> <features> [invert] [standardize] <year>")
}

############################
#   INPUT FILE
############################

variables_risk1_standardized_file <- args[1]

if (!file.exists(variables_risk1_standardized_file)) {
  stop("Input file does not exist")
}

############################
#   K VALUES (multi_centroidi)
############################

if (grepl(":", args[2])) {
  parts <- as.numeric(strsplit(args[2], ":")[[1]])
  multi_centroidi <- seq(
    from = parts[1],
    to = parts[2],
    by = ifelse(length(parts) == 3, parts[3], 1)
  )
} else {
  multi_centroidi <- as.numeric(strsplit(args[2], ",")[[1]])
}

############################
#   ITERAZIONI K-MEANS
############################

N <- as.numeric(args[3])

############################
#   FEATURES + COORDINATE
############################

coord_cols <- strsplit(args[4], ",")[[1]]  
selected_features <- strsplit(args[5], ",")[[1]] 

invert_vars <- if (length(args) >= 6) strsplit(args[6], ",")[[1]] else c()
standardize_vars <- if (length(args) >= 7) strsplit(args[7], ",")[[1]] else c()


############################
#   OUTPUT FOLDER
############################

output_std <- "output_std"

if (!dir.exists(output_std)) {
  dir.create(output_std, recursive = TRUE)
  cat("Created output folder:", output_std, "\n")
} else {
  cat("Output folder already exists:", output_std, "\n")
}
############################
#   YEAR EXTRACTION
############################

year <- args[8] 

cat("Year used:", year, "\n")

############################
#   LETTURA DATI
############################

variables_risk1_standardized <- read_csv(variables_risk1_standardized_file) %>% drop_na()

selection <- variables_risk1_standardized

############################
#   PREPROCESSING VARIABILI
############################

# Coordinate (esplicite)
coords <- selection[, coord_cols]

# SOLO le feature
data_vars <- selection[, selected_features]

processed_data <- data.frame(matrix(nrow = nrow(data_vars), ncol = 0))

for (var_name in colnames(data_vars)) {
  
  x <- data_vars[[var_name]]
  new_name <- var_name
  
  # INVERSIONE (con gestione dello 0)
  if (var_name %in% invert_vars) {
    x <- ifelse(x == 0, 0, 1 / x)
    new_name <- paste0(new_name, "_inv")
  }
  
  # STANDARDIZZAZIONE
  if (var_name %in% standardize_vars) {
    x <- scale(x)[,1]
    new_name <- paste0(new_name, "_std")
  }
  
  processed_data[[new_name]] <- x
}

# aggiorno dataset finale
selection <- cbind(coords, processed_data)


############################
#   UPDATE FEATURE NAMES
############################

updated_features <- selected_features

for (i in seq_along(selected_features)) {
  
  name <- selected_features[i]
  new_name <- name
  
  if (name %in% invert_vars) {
    new_name <- paste0(new_name, "_inv")
  }
  
  if (name %in% standardize_vars) {
    new_name <- paste0(new_name, "_std")
  }
  
  updated_features[i] <- new_name
}

selected_features <- updated_features

############################
#   SALVATAGGIO DATASET PREPROCESSATO
############################

# nome file: input + _processing.csv
input_basename <- tools::file_path_sans_ext(basename(variables_risk1_standardized_file))
processed_filename <- paste0(output_std, "/", input_basename, "_processing.csv")

# dataset con coordinate + variabili preprocessate
processed_output <- cbind(coords, processed_data)

write.csv(
  processed_output,
  file = processed_filename,
  row.names = FALSE
)

cat("Preprocessed dataset saved to:", processed_filename, "\n")

############################
#   CONTROLLO K
############################

v_check <- as.data.frame(selection[, selected_features])
max_k_allowed <- floor(nrow(v_check) / 2)

if (any(multi_centroidi > max_k_allowed)) {
  invalid_ks <- multi_centroidi[multi_centroidi > max_k_allowed]
  stop(paste0(
    "ERROR: K too large: ",
    paste(invalid_ks, collapse = ", ")
  ))
}

############################
#   LOOP SUI K
############################

bics <- c()

for (n_centroidi in multi_centroidi) {
  
  cat("####I'm analyzing ", n_centroidi, "centroids\n")
  
  selected_features_coords <- selection[, selected_features]
  
  # v è direttamente il dataset delle feature
  v <- as.data.frame(selected_features_coords)
  
  ############################
  # CENTROIDI
  ############################
  
  centroidi <- matrix(nrow = n_centroidi, ncol = ncol(v))
  
  for (centroide in 1:nrow(centroidi)) {
    centroidi[centroide, ] <- as.numeric(v[centroide, ])
  }
  
  km <- kmeans(as.matrix(v), centers = as.matrix(centroidi), iter.max = N)
  
  selected_features_coords$distance_class <- km$cluster
  v$distance_class <- km$cluster
  
  centroidi <- as.matrix(km$centers)
  
  ############################
  # DEVIAZIONE STANDARD
  ############################
  
  
  feature_names <- selected_features
  
  centroidi_sd <- matrix(nrow = n_centroidi, ncol = (ncol(v) - 1))
  
  for (centroide in 1:n_centroidi) {
    punti_cluster <- v[v$distance_class == centroide, 1:(ncol(v) - 1)]
    
    if (nrow(punti_cluster) > 1) {
      centroidi_sd[centroide, ] <- apply(punti_cluster, 2, sd)
    } else {
      centroidi_sd[centroide, ] <- 0
    }
  }
  
  centroidi_sd_df <- as.data.frame(centroidi_sd)
  names(centroidi_sd_df) <- paste0(feature_names, "_sd")
  
  ############################
  # QUANTILI
  ############################
  
  # nei codici usati finora si ha questo:
  # v_quantili <- apply(v, 2, quantile)
  # che segnala come bug.
  
  # possibile correzione
  v_quantili <- apply(v[, feature_names], 2, quantile)   #questo non considera la colonna distance_class nel calcolo dei quartili
  
  centroidi_labelled <- matrix("M", nrow = nrow(centroidi), ncol = length(feature_names),
                               dimnames = list(NULL, feature_names))
  

  for (centroide in 1:nrow(centroidi)) {
    for (feat in feature_names) {
      if (centroidi[centroide, feat] < v_quantili["50%", feat]) {
        centroidi_labelled[centroide, feat] <- "L"
      } else if (centroidi[centroide, feat] > v_quantili["75%", feat]) {
        centroidi_labelled[centroide, feat] <- "H"
      }
    }
  }
  
  ############################
  # INTERPRETAZIONE
  ############################
  
  c_H <- rowSums(centroidi_labelled == "H")
  c_M <- rowSums(centroidi_labelled == "M")
  c_L <- rowSums(centroidi_labelled == "L")
  
  centroide_interpretazione <- character(length(c_H))
  
  for (i in 1:length(c_H)) {
    if (c_H[i] > c_L[i] & c_H[i] > c_M[i]) {
      centroide_interpretazione[i] <- "high attention"
    } else if (c_L[i] >= c_H[i] & c_L[i] > c_M[i]) {
      centroide_interpretazione[i] <- "low attention"
    } else {
      centroide_interpretazione[i] <- "medium attention"
    }
  }
  
  ############################
  # OUTPUT CENTROIDI
  ############################
  
  centroidi_df <- as.data.frame(centroidi)
  centroidi_labelled_df <- as.data.frame(centroidi_labelled)
  
  names(centroidi_df) <- feature_names
  names(centroidi_labelled_df) <- paste0(feature_names, "_label")
  
  centroid_id <- data.frame(centroid_id = 1:nrow(centroidi_df))
  
  centroidi_annotated <- centroid_id
  
  for (i in seq_along(feature_names)) {
    centroidi_annotated[[feature_names[i]]] <- centroidi_df[[i]]
    centroidi_annotated[[paste0(feature_names[i], "_sd")]] <- centroidi_sd_df[[i]]
    centroidi_annotated[[paste0(feature_names[i], "_label")]] <- centroidi_labelled_df[[i]]
  }
  
  centroidi_annotated$attention_level <- centroide_interpretazione
  
  write.csv(
    centroidi_annotated,
    file = paste0(output_std, "/all_centroidi_annotated_", n_centroidi, "_", year, "_sd.csv"),
    row.names = FALSE
  )
  
  ############################
  # OUTPUT DATASET
  ############################
  
  v$distance_class_interpretation <- centroide_interpretazione[v$distance_class]
  
  v_no_coords <- v[, !(names(v) %in% names(coords)), drop = FALSE]
  nuovo_v <- cbind(coords, v_no_coords)
  
  write.csv(
    nuovo_v,
    file = paste0(output_std, "/all_centroid_classification_assignment_", n_centroidi, "_", year, "_five_spp.csv"),
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

best_clusterisation <- multi_centroidi[which(bics == max(bics))]

cat("Best clustering: K=", best_clusterisation, "\n")


# SUMMARY 
summary_df <- data.frame(
  K = multi_centroidi,
  UNIF = bics
)

write.csv(
  summary_df,
  file = paste0(output_std, "/summary_UNIF.csv"),
  row.names = FALSE
)

# FILE BEST K 
best_clusterisation_file <- paste0(
  output_std,
  "/centroid_classification_assignment_",
  best_clusterisation,
  ".csv"
)

write.csv(
  data.frame(best_K = best_clusterisation),
  file = paste0(output_std, "/best_K.csv"),
  row.names = FALSE
)

cat("Best clustering file to take as result:", best_clusterisation_file, "\n")