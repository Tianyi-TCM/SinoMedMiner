#' Data Cleaning Function
#'
#' @description
#' This function is used to clean and standardize information fields in the data, such as herbal names, symptom names, and acupoint names. The function can identify and handle potential irregular symbols (e.g., newline characters, spaces) and supports fields with or without dosage information.
#'
#' @param df A data frame where the first column is the ID, and the remaining columns must contain only one element per cell. For example, "丹参" should occupy one cell, and "丹参,黄芪" in one cell is not allowed.
#' @param lookup_table A data frame containing the standardized names of herbs. The user needs to fill in the standard names.
#'
#' @importFrom dplyr group_by mutate filter ungroup summarise
#' @importFrom tidyr separate unite
#' @importFrom stringr str_extract str_trim
#'
#' @return A data frame with standardized field names.
#'
#' @details
#' The main functions of this function include:
#' 1. Removing newline characters and leading/trailing spaces from the data.
#' 2. Checking for duplicate herbs within the same prescription and issuing a warning if found.
#' 3. Determining if the data contains dosage information. If dosage is included, it splits the herbal name and dosage and performs dosage checks (e.g., missing values, non-numeric dosage).
#' 4. Calls the function to standardize herbal names.
#' 5. Reconstructs the data frame based on whether dosage information is included.
#'
#' @export

clean_data_optimized <- function(df, lookup_table) {

  df_long <- trans_long(df)
  colnames(df_long) <- c("id", "itemnames")

  ##### 清洗: 移除换行符与首尾空格 #####
  # Initialize flags
  has_newline <- FALSE
  herbs_with_spaces <- FALSE

  df_long[] <- lapply(df_long, function(x) {
    # 检测换行符并移除
    if (any(grepl("\n", x))) {
      has_newline <<- TRUE
      x <- gsub("\n", "", x)
    }
    # 检测首尾空格并移除
    if (any(grepl("^\\s|\\s$", x))) {
      herbs_with_spaces <<- TRUE
      x <- str_trim(x)
    }
    return(x)
  })

  # 如果检测到换行符或空格，将发出警告。
  if (has_newline) {
    warning("\u4e2d\u836f\u5b58\u5728\u6362\u884c\u7b26\uff0c\u5df2\u5c06\u6362\u884c\u7b26\u5220\u9664")
  }
  if (herbs_with_spaces) {
    warning("\u5b58\u5728\u7a7a\u683c\uff0c\u5df2\u5c06\u9996\u5c3e\u7a7a\u683c\u5220\u9664")
  }

  ##### 检查同一方剂内是否有重复中药 #####
  repeated_herbs <- df_long %>%
    # 使用正则表达式提取中药名称，忽略剂量部分
    mutate(herb_only = gsub("\\d+$", "", .data$itemnames)) %>%  # 只保留中药名称，删除末尾数字
    group_by(id, .data$herb_only) %>%  # 根据id和中药名称进行分组
    filter(n() > 1) %>%
    ungroup()

  if (nrow(repeated_herbs) > 0) {
    # 创建一条详细的消息，显示重复的草药名称及其对应的ID。
    warning_message <- paste0("\u5b58\u5728\u91cd\u590d\u4e2d\u836f\uff0c\u4ee5\u4e0b\u4e3a\u5bf9\u5e94\u7684\u0069\u0064\u548c\u91cd\u590d\u4e2d\u836f\u003a\n",
                              paste0("id: ", repeated_herbs$id,
                                     ", herb: ", repeated_herbs$herb_only, collapse = "\n"))
    warning(warning_message)
  }

  #### 判断数据是否包含剂量信息 ####
  contains_numbers <- any(grepl("\\d", df_long$itemnames))

  # 如果数据中包含剂量信息，执行以下代码
  if (contains_numbers) {
    problematic_rows <- df_long[!grepl(".*\\D+\\d+.*", df_long$itemnames), ]
    if (nrow(problematic_rows) > 0) {
      warning(paste0("Found rows that may not separate correctly:\n",
                     paste0(problematic_rows$itemnames, collapse = "\n")))
    }

    # 分割中药与剂量信息
    df_long <- df_long %>%
      mutate(
        items = stringr::str_extract(.data$itemnames, "[\\u4e00-\\u9fa5a-zA-Z]+"),  # 提取字母或汉字部分
        graw_weight = str_extract(.data$itemnames, "\\d+(?:\\.\\d*)?[a-zA-Z\\u4e00-\\u9fa5]*")  # 提取数字及单位
      )

    ##### Check dosage data. #####
    # Check for missing values.
    if (any(!is.na(df_long$items) & is.na(df_long$graw_weight))) {
      warning("\u5b58\u5728\u672a\u8f93\u5165\u5242\u91cf\u7684\u4e2d\u836f\uff0c\u8bf7\u6838\u6539\u539f\u6570\u636e")
    }

    # Check if the dosage is numeric.
    non_numeric_dosage <- df_long %>%
      filter(!grepl("^[0-9]+$", .data$graw_weight))

    if (nrow(non_numeric_dosage) > 0) {
      # 创建详细的警告信息，显示非数值剂量的行
      warning_message <- paste0("\u5b58\u5728\u975e\u6570\u503c\u7684\u5242\u91cf\uff0c\u8bf7\u6838\u6539\u539f\u6570\u636e\u3002\n",
                                paste0("id: ", non_numeric_dosage$id,
                                       ", dosage: ", non_numeric_dosage$graw_weight,
                                       collapse = "\n"))
      warning(warning_message)
    }

    df_long[[1]] <- as.character(df_long[[1]])

    # Call the function for standardized names.
    df_long <- regulate_name(df_long, lookup_table)
    df_long <- df_long %>%
      mutate(order = row_number())  # 为每个id创建一个顺序列


    # Reconstruct the data frame.
    df_reconstructed <- df_long %>%
      unite("composition", 'items','graw_weight', sep = "", na.rm = TRUE) %>%
      group_by(id) %>%
      summarise(itemnames = paste(.data$composition, collapse = ";"),
                order = min(order),  # 保留原始顺序
                .groups = 'drop') %>%
      arrange(order) %>%  # 按原始顺序排序
      select(-order)  # 移除顺序列

    max_columns <- max(sapply(strsplit(df_reconstructed$itemnames, ";"), length))
    df_reconstructed <- df_reconstructed %>%
      separate(.data$itemnames, into = paste0("\u4e2d\u836f", 1:max_columns), sep = ";", fill = "right")
    df <- df_reconstructed

  } else {
    # If there is no dosage, perform standardization directly.
    df <- regulate_name(df_long, lookup_table)
    colnames(df) <- c("id", "itemnames")
    df <- df %>%
      mutate(order = row_number())  # 为每个id创建一个顺序列

    df_reconstructed <- df %>%
      group_by(id) %>%
      summarise(itemnames = paste(.data$itemnames, collapse = ";"),
                order = min(order),  # 保留原始顺序
                .groups = 'drop') %>%
      arrange(order) %>%  # 按原始顺序排序
      select(-order)  # 移除顺序列

    max_columns <- max(sapply(strsplit(df_reconstructed$itemnames, ";"), length))
    df_reconstructed <- df_reconstructed %>%
      separate(.data$itemnames, into = paste0("\u4e2d\u836f", 1:max_columns), sep = ";", fill = "right")
    df <- df_reconstructed
  }

  return(df)
}
