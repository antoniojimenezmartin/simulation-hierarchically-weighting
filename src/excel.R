# Determine the sheet name based on the conditions
get_sheet_name <- function(distribution, filter_condition) {
  if (distribution %in% c(0, 1) && is.null(filter_condition)) {
    sheet_name <- "Uniform without filter"
  } else if (distribution %in% c(0, 1) && !is.null(filter_condition)) {
    sheet_name <- "Uniform with filter"
  } else if (distribution == 2) {
    sheet_name <- "Normal"
  } else if (distribution == 3) {
    sheet_name <- "Exponential"
  } else {
    stop("Invalid distribution type")
  }

  sheet_name
}

# Create a output directory for charts and the output data frame
setup_results_directory <- function(dir_name, sheet_name) {
  # Create the output directory
  if (!dir.exists(dir_name)) {
      dir.create(dir_name)
  }

  # Create charts folder
  normalized_sheet_name <- gsub(pattern = " ", replacement = "_", tolower(sheet_name))
  chartsFolder <- file.path(dir_name, "charts", normalized_sheet_name)
  if (dir.exists(chartsFolder)) {
    unlink(chartsFolder, recursive = TRUE)
  }
  dir.create(chartsFolder, recursive = TRUE)
  dir.create(file.path(chartsFolder, "boxplots"))
  dir.create(file.path(chartsFolder, "violin charts"))

  chartsFolder
}

# Load or create excel file with the results
setup_results_excel_file <- function(dir_name, sheet_name) {
  # Create the results excel file
  results_file <- file.path(dir_name, "results.xlsx")

  if (file.exists(results_file)) {
    wb <- wb_load(results_file)

    # Clean worksheet if it already exists
    if (sheet_name %in% wb$sheet_names) {
        wb$clean_sheet(sheet_name)
    }
  } else {
    wb <- wb_workbook()
    wb$add_worksheet("Uniform without filter")
    wb$add_worksheet("Uniform with filter")
    wb$add_worksheet("Normal")
    wb$add_worksheet("Exponential")
  }

  wb
}

# Save the results in the excel file
save_excel <- function(x, dir_name) {
  wb_save(x, file.path(dir_name, "results.xlsx"), overwrite = TRUE)
}
