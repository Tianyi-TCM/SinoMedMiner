#' Convert dimension data to long data format suitable for creating Sankey diagrams
#' @param dim1 A data frame containing the data for the first dimension
#' @param dim2 A data frame containing the data for the second dimension
#' @param dim3 A data frame containing the data for the third dimension
#' @param dim4 A data frame containing the data for the fourth dimension
#' @param dim1_n An integer specifying the top 'n' elements by frequency for the first dimension
#' @param dim2_n An integer specifying the top 'n' elements by frequency for the second dimension
#' @param dim3_n An integer specifying the top 'n' elements by frequency for the third dimension
#' @param dim4_n An integer specifying the top 'n' elements by frequency for the fourth dimension
#' @return A list containing the long data frame and frequency data frame
#' @importFrom dplyr group_by summarise arrange left_join filter
#' @export

tidy_sanky_screen <- function(dim1, dim2,
                              dim3 = NULL, dim4 = NULL,
                              dim1_n = 20, dim2_n= 5,
                              dim3_n = 20 , dim4_n = 20){
  # 构建处理维度的函数，返回长数据框和频数数据框
  process_dimension <- function(dim, dim_name) {
    if (is.null(dim)) {
      return(NULL)  # If the dimension is NULL, return NULL
    }

    dim_long <- dim %>% trans_long2()
    sum(is.na(dim_long))  # Check for missing values
    colnames(dim_long) <- c('id', dim_name)

    dim_freq <- dim_long %>% group_by(!!sym(dim_name)) %>%
      summarise(freq = n()) %>% arrange(desc(.data$freq))
    # 返回长数据框和频数数据框
    return(list(dim_long = dim_long, dim_freq = dim_freq))
  }


  # 处理维度，每个维度的列表数据命名为dimx_data
  # 1. Process Dimension 1
  dim1_data <- process_dimension(dim1, 'dim1')
  # 2. Process Dimension 2
  dim2_data <- process_dimension(dim2, 'dim2')
  # 3. Process Dimension 3
  dim3_data <- process_dimension(dim3, 'dim3')
  # 4. Process Dimension 4
  dim4_data <- process_dimension(dim4, 'dim4')
  # Prepare list for left join, only include non-null data frames
  dfs <- list(dim1_data$dim_long, dim2_data$dim_long)
  if (!is.null(dim3_data)) dfs <- append(dfs, list(dim3_data$dim_long))
  if (!is.null(dim4_data)) dfs <- append(dfs, list(dim4_data$dim_long))

  # 拼接维度数据
  all_data_long_t <- Reduce(function(x, y) left_join(x, y, by = "id"), dfs)

  #筛选数据
  # 提取各个维度高频前n个元素
  dim1_list <- as.character(dim1_data$dim_freq$dim1[1:dim1_n])
  dim2_list <- as.character(dim2_data$dim_freq$dim2[1:dim2_n])
  dim3_list <- if (!is.null(dim3_data)) as.character(dim3_data$dim_freq$dim3[1:dim3_n]) else NULL
  dim4_list <- if (!is.null(dim4_data)) as.character(dim4_data$dim_freq$dim4[1:dim4_n]) else NULL

  #筛选
  all_data_long_screened <- all_data_long_t %>%
    filter(dim1 %in% dim1_list & dim2 %in% dim2_list)

  if (!is.null(dim3_data)) all_data_long_screened <- all_data_long_screened %>%
    filter(dim3 %in% dim3_list)
  if (!is.null(dim4_data)) all_data_long_screened <- all_data_long_screened %>%
    filter(dim4 %in% dim4_list)

  # 返回筛选后的数据
  return(all_data_long_screened)

}
