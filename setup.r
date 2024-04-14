source("data/dependencies.R")
source("data/dat-processing.r")

# Check for changes ---------------------------------------------------------------

get_file_hash <- function(file_path) {
  digest(file_path, algo = "md5", file = TRUE)
}

run_script_if_changed <- function(source_script, hash_store) {
  current_hash <- get_file_hash(source_script)
  
  if (!file.exists(hash_store)) {
    writeLines(current_hash, hash_store)
  } else {
    stored_hash <- readLines(hash_store)[1]
    if(stored_hash != current_hash) {
      source(source_script)
      writeLines(current_hash, hash_store)
      cat("Script has changed and was re-executed.\n")
    } else {
      cat("No changes in script. Not re-executing.\n")
    }
  }
}

run_script_if_changed("data/dat-processing.r", "hash_store.txt")

# Read data ---------------------------------------------------------------
emis_tbl <- read_csv("data/data.csv")

# Read model --------------------------------------------------------------

forecast_mod <- read_rds("data/forecast_mod.rds")
