#1.Checks for spaces in the data frame columns, and if spaces are found, they are removed.
space_check <- function(df) {
  has_spaces <- any(sapply(df, function(col) any(grepl("\\s", col))))
  if (has_spaces) {
    warning("The data contains spaces, which have been removed!")
    df <- df %>% mutate(across(everything(), ~gsub("\\s", "", .)))
  }
  return(df)
}

# 2.For non-tidy data frames, remove duplicate elements in each row.
del_dup <- function(data){

  data2 <- as.data.frame(t(data))

  df_clean <- lapply(data2, function(x) unique(x[!duplicated(x) & !is.na(x)]))

  max_length <- max(sapply(df_clean, length))
  df_clean <- lapply(df_clean, `length<-`, max_length)

  df_clean <- as.data.frame(df_clean)
  df_clean <- as.data.frame(t(df_clean))

  colnames(df_clean)[1] <- colnames(data)[1]
  colnames(df_clean)[2:ncol(df_clean)] <- paste0("herb", 1:(ncol(df_clean) - 1))

  row.names(df_clean) <- NULL
  return(df_clean)
}

# 3.
# df should be a dataframe where the first column is the ID column, and the remaining columns are variable names.
#Each column's values must be numeric, representing the frequency of the variable for that ID.
#This function only supports numeric variables and does not support character variables.
numericCount_wide <- function(df) {

  df <- df[,-1]

  if (!is.data.frame(df) && !is.matrix(df)) {
    stop("Input data must be a data frame or matrix.")
  }


  if (!all(sapply(df, is.numeric))) {
    stop("All columns in the data frame or matrix must be numeric.")
  }

  if (any(sapply(df, function(x) any(is.null(x))))) {
    df[is.null(df)] <- NA
    warning("Data contains NULL values which have been replaced with NA.")
  }


  if (any(is.na(df))) {
    warning("Data contains NA values which have been ignored in the sum calculation.")
  }
  sum_wide <- colSums(df, na.rm = TRUE)
  sum_wide <- data.frame(item_name = names(sum_wide), frequency = as.numeric(sum_wide))
  sum_wide <- sum_wide[order(-sum_wide$frequency), ]
}

# 4.
# Place the row with the core ID at the first row and remove columns that have 0 in the first row
core_to_top <- function(df, core_id){
  # Arrange the dataframe so the row with 'core_id' is at the top
  # df is a dataframe ,core_id is the id of the core row, core_id is a string.
  df <- df %>%
    arrange(desc(.data$id == core_id))

  # Get the values of the first row
  first_row <- df[1, ]

  # Find column names where the value in the first row is 1
  columns_to_keep <- names(first_row)[which(first_row == 1)]

  # Add the 'id' column back to the list of columns to keep
  columns_to_keep <- c("id", columns_to_keep)

  # Return a dataframe containing only these columns
  df_retained <- df %>%
    select(all_of(columns_to_keep))
}

