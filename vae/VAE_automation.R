################################################################################
#####                        VAE con Java                                  #####
################################################################################

################################################################################
#####                        Training / Test                               #####
################################################################################
#######  PREREQUISITE: VAE-MODEL JAR FILE DOWNLOADABLE FROM  ###################
#       https://github.com/cybprojects65/VariationalAutoencoder                #
################################################################################



#da bash:
# TRAINING:
# ..... "dataset_baltic_sea_2020_std.csv" "longitude,latitude" "five_spp_cpue_2020_std,bathymetry_2020_std,bottom_temperature_2020_std,surface_net_primary_production_2020_std,fishing_activity_hours_2020_std,ship_density_route_2020_std,bottom_salinity_2020_std,bottom_dissolved_oxygen_2020_inv_std,dumping_site_2020,military_area_2020,wind_farm_2020,bedrock_seabed_2020,hard_bottom_seabed_2020" 12 true "output_vae"

# TEST:
# ....  "dataset_baltic_sea_2020_std.csv" "longitude,latitude" "five_spp_cpue_2020_std,bathymetry_2020_std,bottom_temperature_2020_std,surface_net_primary_production_2020_std,fishing_activity_hours_2020_std,ship_density_route_2020_std,bottom_salinity_2020_std,bottom_dissolved_oxygen_2020_inv_std,dumping_site_2020,military_area_2020,wind_farm_2020,bedrock_seabed_2020,hard_bottom_seabed_2020" 12 false "output_vae/" "model.bin"





# =========================
# INPUT ARGUMENTS
# =========================

args <- commandArgs(trailingOnly = TRUE)


min_args <- if (length(args) >= 5 && tolower(args[5]) == "true") 6 else 7
if (length(args) < min_args) {
  stop("Usage:
  Rscript vae_pipeline.R 
  <input_file_path>
  <coord_cols>
  <feature_cols>
  <hidden_nodes>
  <training_mode true/false>
  <output_folder>
  <trained_model_file>  <- solo in modalita test
       ")
}





input_file_path <- args[1]

coord_cols <- strsplit(args[2], ",")[[1]]

variable_names <- args[3]

number_of_hidden_nodes <- as.numeric(args[4])


training_mode_active <- tolower(args[5]) == "true"     # == "true" or " false" 

output_folder <- args[6]     

trained_model_file <- if (length(args) >= 7) args[7] else ""
# model.bin in the output folder



# =========================
# CONSTANTS
# =========================

number_of_epochs <- 1000
number_of_reconstruction_samples <- 16


# =========================
# output folder
# =========================

dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)



################################################################################
#####                           TRAINING                                   #####
################################################################################

if(training_mode_active){                                   
  
  
  command_training<-paste0("java -cp vae.jar it.cnr.anomaly.JavaVAE -i\"./",input_file_path,"\" -v\"",variable_names,"\" -o\"",output_folder,"\" -h",number_of_hidden_nodes," -e",number_of_epochs," -r",number_of_reconstruction_samples," -t",training_mode_active)
  
  
  VAU_execution_train<-system(command_training, intern = TRUE,
                              ignore.stdout = FALSE, ignore.stderr = FALSE,
                              wait = TRUE, input = NULL, show.output.on.console = TRUE,
                              minimized = FALSE, invisible = TRUE)
  
  
  execution_train_success<-(length(which(grepl(pattern="OK VAU Training",x=VAU_execution_train)))>0)
  log_file <- paste0(output_folder,"log_file_training.txt")
  writeLines(VAU_execution_train, log_file)
}else{
  dir.create(output_folder)
  ################################################################################
  #####                           Test                                       #####
  ################################################################################
  
  
  
  command_test <- paste0("java -cp vae.jar it.cnr.anomaly.JavaVAE -i\"./",input_file_path,"\" -v\"",variable_names,"\" -o\"",output_folder,"\" -r",number_of_reconstruction_samples," -t",training_mode_active," -m\"",trained_model_file,"\"")
  
  
  VAU_execution_test<-system(command_test, intern = T,
                             ignore.stdout = FALSE, ignore.stderr = FALSE,
                             wait = TRUE, input = NULL, show.output.on.console = TRUE,
                             minimized = FALSE, invisible = TRUE)
  
  execution_train_success<-(length(which(grepl(pattern="OK VAU Test",x=VAU_execution_test)))>0)
  log_file <- paste0(output_folder,"log_file_test.txt")
  writeLines(VAU_execution_test, log_file)
  
  ################################################################################
  #####                           EVALUATION                               #####
  ################################################################################
  
  file_pattern <- "classification"         
  files <- list.files(path = output_folder, pattern = paste0("^", file_pattern))
  if (length(files) == 1) {
    # Build the full file path
    file_path <- file.path(output_folder, files[1])
    
    # Read CSV file
    data_projected <- read.csv(file_path,header = TRUE)
  } else {
    cat("More than one file found or no files found.")
  }
  namelist<- unlist(strsplit(variable_names, split = ","))
  data_projected_rdx <- data_projected[,namelist]
  
  data_input<-read.csv(input_file_path,header = TRUE)
  data_input <- data_input[,namelist]
  
  vettore_differenza <- data_projected_rdx - data_input
  vettore_differenza_vector <- unlist(vettore_differenza)
  vettore_differenza_numeric <- as.numeric(vettore_differenza_vector)
  errore <- mean((as.numeric(vettore_differenza_numeric))^2)
  
  
  rec_prob_avg<-mean(data_projected$reconstruction_log_probability)
  cat(paste0("error =",errore,", average probability recostruction =",rec_prob_avg),"\n")
  
  
  file_pattern <- "classification"      
  files <- list.files(path = output_folder, pattern = paste0("^", file_pattern))
  
  #data2 <- read.csv(paste0(output_folder, files), header = TRUE)   # this is the file that generates the VAE as output
  ####
  
  if (length(files) == 1) {
    data2 <- read.csv(file.path(output_folder, files[1]), header = TRUE)
  } else {
    stop("Error: Multiple or no files with pattern 'classification' found")
  }
  #####
  
  
  
  data3 <- read.csv(input_file_path, header = TRUE)     
  
  # extract coordinates
  coord_data <- data3[, coord_cols, drop = FALSE]
  
  
  ########### percentile ##################
  
  #Percentile calculation for reconstruction_log_probability
  prob_vals <- data2$reconstruction_log_probability
  q <- quantile(prob_vals, probs = c(0.25, 0.50, 0.75))
  
  percentile_class <- cut(
    prob_vals,
    breaks = c(-Inf, q[1], q[2], q[3], Inf),
    labels = c("0-25", "25-50", "50-75", "75-100"),
    include.lowest = TRUE
  )
  
  
  
  
  
  
  
  data4 <- cbind(
    coord_data,
    data2$reconstruction_log_probability,
    percentile_class
  )
  
  colnames(data4) <- c(
    coord_cols,
    "reconstruction_log_probability",
    "percentile"
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
    data2$reconstruction_log_probability,
    percentile_class
  )
  
  colnames(data5) <- c(
    coord_cols,
    feature_cols,
    "reconstruction_log_probability",
    "percentile"
  )
  
  write.csv(
    data5,
    paste0(output_folder, "output_VAE_complete.csv"),
    row.names = FALSE
  )
  ##############################################################
  
  
  
  
}