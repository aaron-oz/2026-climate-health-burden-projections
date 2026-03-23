#BoD temperature

#extract names of the files 
  # Set your folder path
  folder_path <- "G:/Mi unidad/Projects/World Bank/bod shared/Info Burkart/TMRELs"  # Change this to your actual path
  
  # Get all file names
  file_names <- list.files(folder_path)
  
  # Keep only files that match the tmrel pattern
  tmrel_files <- file_names[grepl("^tmrel_\\d+", file_names)]
  
  # Extract the numbers
  numbers <- gsub("tmrel_(\\d+).*", "\\1", tmrel_files)
  
  # Get unique numbers and sort
  unique_numbers <- sort(unique(numbers))
  
  # Convert to dataframe
  df <- data.frame(UniqueNumber = unique_numbers)
  
  # Save to CSV
  write.csv(df, "unique_numbers.csv", row.names = FALSE)
