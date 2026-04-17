################################################################################
#####                        VAE con Java                                  #####
################################################################################

################################################################################
#####                        Training / Test                               #####
################################################################################
#########PREREQUISITE: VAE-MODEL JAR FILE DOWNLOADABLE FROM#####################
#https://data.d4science.org/shub/E_RmlXSjJSbFVhZmVyT25YTFJJYlY1a3BJRWc0T0xueUVIOWNXamR3dStNV3RMZDl2WThJRE5rckY0b1cwWVU1Kw==
################################################################################

# =========================
# INPUT ARGUMENTS (BASH DRIVEN)
# =========================

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 9) {
  stop("Usage:
  Rscript vae_pipeline.R 
  <input_file_path>
  <coord_cols>
  <feature_cols>
  <hidden_nodes>
  <reconstruction_samples>
  <training_mode true/false>
  <output_folder>
  <trained_model_file>
  #<percentile>
       ")
}

input_file_path <- args[1]

coord_cols <- strsplit(args[2], ",")[[1]]

variable_names <- args[3]

number_of_hidden_nodes <- as.numeric(args[4])

number_of_reconstruction_samples <- as.numeric(args[5])

training_mode_active <- tolower(args[6]) == "true"     # == "true" ?? gestione booleano

output_folder <- args[7]     #come gestire la cartella di output? è da definire da bash. 
# La cartella dovrebbe essere creata per la fase di training. Cosa fa JAVA per sta cartella?

trained_model_file <- if (length(args) >= 8) args[8] else ""
# trained_model_file<-paste0(model_folder,"model_norm_1904X13_a46e53644752252350f55d39452053b60554958150c59855#13.bin")

#percentile <- as.numeric(args[9])    per classificazione del rischio


# =========================
# CONSTANTS ......???
# =========================

number_of_epochs <- 1000

################################################################################
#####                           TRAINING                                   #####
################################################################################

if(training_mode_active == TRUE){   # ?? if(training_mode_active=="true"){
  
  command_training <- paste0(
    "java -cp vae.jar it.cnr.anomaly.JavaVAE",
    " -i\"./", input_file_path, "\"",
    " -v\"", variable_names, "\"",
    " -o\"", output_folder, "\"",
    " -h", number_of_hidden_nodes,
    " -e", number_of_epochs,
    " -r", number_of_reconstruction_samples,
    " -t", training_mode_active
  )
  
  VAU_execution_train <- system(
    command_training,
    intern = TRUE,
    ignore.stdout = FALSE,
    ignore.stderr = FALSE,
    wait = TRUE,
    input = NULL,
    show.output.on.console = TRUE,
    minimized = FALSE,
    invisible = TRUE
  )
  
  execution_train_success <- length(
    which(grepl("OK VAU Training", VAU_execution_train))
  ) > 0
  
  log_file <- paste0(output_folder, "log_file_training.txt")
  
  writeLines(VAU_execution_train, log_file)
  
}else{
  
  dir.create(output_folder)   #, showWarnings = FALSE, recursive = TRUE
  
  ################################################################################
  #####                           TEST                                       #####
  ################################################################################
  
  command_test <- paste0(
    "java -cp vae.jar it.cnr.anomaly.JavaVAE",
    " -i\"./", input_file_path, "\"",
    " -v\"", variable_names, "\"",
    " -o\"", output_folder, "\"",
    " -r", number_of_reconstruction_samples,
    " -t", training_mode_active,
    " -m\"", trained_model_file, "\""
  )
  
  VAU_execution_test <- system(
    command_test,
    intern = TRUE,
    ignore.stdout = FALSE,
    ignore.stderr = FALSE,
    wait = TRUE,
    input = NULL,
    show.output.on.console = TRUE,
    minimized = FALSE,
    invisible = TRUE
  )
  
  execution_train_success <- length(
    which(grepl("OK VAU Test", VAU_execution_test))
  ) > 0
  
  log_file <- paste0(output_folder, "log_file_test.txt")
  
  writeLines(VAU_execution_test, log_file)
  
  ################################################################################
  #####                           EVALUATION                               #####
  ################################################################################
  
  file_pattern <- "classification_test_"
  files <- list.files(path = output_folder, pattern = paste0("^", file_pattern))
  
  if (length(files) == 1) {
    
    file_path <- file.path(output_folder, files[1])
    
    data_projected <- read.csv(file_path, header = TRUE)
    
  } else {
    stop("Zero or multiple classification_test files found.")
  }
  
  namelist <- unlist(strsplit(variable_names, split = ","))
  
  data_projected_rdx <- data_projected[, namelist]
  
  data_input<-read.csv(input_file_path,header = TRUE)
  data_input <- data_input[,namelist]
  
  vettore_differenza <- data_projected_rdx - data_input
  vettore_differenza_vector <- unlist(vettore_differenza)
  vettore_differenza_numeric <- as.numeric(vettore_differenza_vector)
  errore <- mean((as.numeric(vettore_differenza_numeric))^2)
  
  rec_prob_avg <- mean(data_projected$reconstruction_log_probability)
  
  cat(paste0(
    "error=", errore,
    ", average probability reconstruction=", rec_prob_avg
  ), "\n")
  
  data2 <- read.csv(paste0(output_folder, files), header = TRUE)   # questo è il file che mi genera il vae come output
  
  data3 <- read.csv(input_file_path, header = TRUE)     #questo è l'originale input multi k means
  
  # estrazione coordinate in modo generico
  coord_data <- data3[, coord_cols, drop = FALSE]
  
  data4 <- cbind(
    coord_data,
    data2$reconstruction_log_probability
  )
  
  colnames(data4) <- c(
    coord_cols,
    "reconstruction_log_probability"
  )
  
  write.csv(
    data4,
    paste0(output_folder, "output_VAE.csv"),
    row.names = FALSE
  )   
    
    
    
    
    
##############################################################  
feature_cols <- unlist(strsplit(variable_names, split = ","))
    
feature_data <- data3[, feature_cols, drop = FALSE]
    
data5 <- cbind(
  coord_data,
  feature_data,
  data2$reconstruction_log_probability
)
    
write.csv(
  data5,
  paste0(output_folder, "output_VAE_completo.csv"),
  row.names = FALSE
)
##############################################################




}