#versione che richiede dataset (csv) già pre processato ovvero invertito dove necessario e standardizzato

rm(list = ls())

library(tidyverse)
options(warn = -1)

main <- function() {
  
  ############################
  # 1. LETTURA ARGOMENTI
  ############################
  
  args <- commandArgs(trailingOnly = TRUE)
  
  if (length(args) < 5) {
    stop("Usage: RScript multikmeans.R <input_csv> <k_values> <max_iter> <columns> <output_folder>")
  }
  
  input_file <- args[1]
  
  # CONTROLLO ESISTENZA FILE
  if (!file.exists(input_file)) {
    stop("Input file does not exist")
  }
  
  if (grepl(":", args[2])) {
    parts <- as.numeric(strsplit(args[2], ":")[[1]])
    k_values <- seq(from = parts[1], to = parts[2], by = ifelse(length(parts) == 3, parts[3], 1))
  } else {
    k_values <- as.numeric(strsplit(args[2], ",")[[1]])
  }
  
  N <- as.numeric(args[3])
  
  selected_features <- strsplit(args[4], ",")[[1]]
  output_folder <- args[5]
  
  year <- gsub("\\D", "", input_file)
  
  cat("Input file:", input_file, "\n")
  cat("K values:", k_values, "\n")
  cat("Max iterations:", N, "\n")
  cat("Selected features:", selected_features, "\n")
  cat("Output folder:", output_folder, "\n")
  
  dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)
  
  ############################
  # 2. LETTURA DATI
  ############################
  
  data <- read_csv(input_file) %>% drop_na()
  
  selection <- data
  selected_features_coords <- selection[, selected_features]
  
  ############################
  # 2b. CONTROLLO K <= NROW/2 ovvero numero di vettori/2
  ############################
  
  v_check <- as.data.frame(selected_features_coords[, 3:ncol(selected_features_coords)])
  max_k_allowed <- floor(nrow(v_check) / 2)
  
  if (any(k_values > max_k_allowed)) {
    invalid_ks <- k_values[k_values > max_k_allowed]
    stop(paste0(
      "ERROR: The following K values exceed the maximum allowed (nrow/2 = ", max_k_allowed, "): ",
      paste(invalid_ks, collapse = ", "),
      "\nPlease retry with K values <= ", max_k_allowed
    ))
  }
  
  ############################
  # 3. LOOP SUI K
  ############################
  
  #N <- 3000
  bics <- c()
  
  set.seed(123)
  
  for (n_centroidi in k_values) {
    
    cat("\n--- Running K =", n_centroidi, "---\n")
    
    v <- as.data.frame(selected_features_coords[, 3:ncol(selected_features_coords)])
    
    centroidi <- matrix(nrow = n_centroidi, ncol = ncol(v))
    
    for (centroide in 1:nrow(centroidi)) {
      centroidi[centroide, ] <- as.numeric(v[centroide, ])
    }
    
    km <- kmeans(as.matrix(v), centers = as.matrix(centroidi), iter.max = N)
    
    selected_features_coords$distance_class <- km$cluster
    v$distance_class <- km$cluster
    
    centroidi <- as.matrix(km$centers)
    
    ############################
    # 4. DEVIAZIONE STANDARD
    ############################
    
    feature_names <- selected_features[3:length(selected_features)]
    
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
    # 5. QUANTILI E LABEL
    ############################
    
    v_quantili <- apply(v[, 1:(ncol(v)-1)], 2, quantile)   #evita che compernda anche la colonna distance class ora presente in v
    
    centroidi_labelled <- matrix("M", nrow = nrow(centroidi), ncol = ncol(centroidi))
    
    for (centroide in 1:nrow(centroidi)) {
      for (feat in 1:(ncol(v) - 1)) {
        if (centroidi[centroide, feat] < v_quantili[3, feat]) {
          centroidi_labelled[centroide, feat] <- "L"
        } else if (centroidi[centroide, feat] > v_quantili[4, feat]) {
          centroidi_labelled[centroide, feat] <- "H"
        }
      }
    }
    
    ############################
    # 6. INTERPRETAZIONE
    ############################
    
    c_H <- rowSums(centroidi_labelled == "H")
    c_M <- rowSums(centroidi_labelled == "M")
    c_L <- rowSums(centroidi_labelled == "L")
    
    ################   !!!!!  ################
    # ATTUALE -> rischio sovrascrittura     
    #centroide_interpretazione <- rep("medium attention", length(c_H))
    #centroide_interpretazione[c_H > c_L & c_H > c_M] <- "high attention"
    #centroide_interpretazione[c_L >= c_H & c_L > c_M] <- "low attention"
    
    # CORREZIONE PROPOSTA DA CHATGPT
    # logica precauzionale
    centroide_interpretazione <- case_when(
      c_H >= c_L & c_H >= c_M  ~ "high attention",
      c_L > c_H & c_L > c_M    ~ "low attention",
      TRUE                     ~ "medium attention"
    )
    
    ############################
    # 7. OUTPUT CENTROIDI
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
      file = paste0(output_folder, "/centroids_K", n_centroidi, ".csv"),
      row.names = FALSE
    )
    
    ############################
    # 8. OUTPUT DATASET
    ############################
    
    v$distance_class_interpretation <- centroide_interpretazione[v$distance_class]
    
    output_data <- cbind(selected_features_coords[, 1:2], v)
    
    write.csv(
      output_data,
      file = paste0(output_folder, "/classification_K", n_centroidi, ".csv"),
      row.names = FALSE
    )
    
    ############################
    # 9. UNIF
    ############################
    
    centroid_distribution <- km$size
    
    if (length(which(centroid_distribution <= 2)) > 0 ||
        ((min(centroid_distribution) / max(centroid_distribution)) < 0.007)) {
      bic <- 0
    } else {
      centroid_distribution.norm <- centroid_distribution / sum(centroid_distribution)
      reference <- rep(mean(centroid_distribution), length(centroid_distribution))
      reference.norm <- reference / sum(reference)
      
      chisq <- sum((centroid_distribution.norm * 1000 - reference.norm * 1000)^2 /
                     (reference.norm * 1000)) / length(centroid_distribution.norm)
      
      bic <- 1 / chisq
    }
    
    bics <- c(bics, bic)
    
    cat("UNIF:", bic, "\n")
  }
  
  ############################
  # 10. BEST K
  ############################
  
  best_k <- k_values[which.max(bics)]
  
  cat("\nBest K:", best_k, "\n")
  
  #  SALVATAGGIO SUMMARY UNIF
  summary_df <- data.frame(
    K = k_values,
    UNIF = bics
  )
  
  write.csv(
    summary_df,
    file = paste0(output_folder, "/summary_UNIF.csv"),
    row.names = FALSE
  )
  
  #  SALVATAGGIO BEST K
  write.csv(
    data.frame(best_K = best_k),
    file = paste0(output_folder, "/best_K.csv"),
    row.names = FALSE
  )
}

main()