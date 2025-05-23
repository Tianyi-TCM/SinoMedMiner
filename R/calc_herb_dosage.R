#' Statistics of Herb Dosage Distribution
#'
#' @param df A data frame where the first column is the ID, and the remaining cells contain herb names and their dosages. The dosages should be numeric values representing weight in grams, without any units attached. Correct example: "Huangqi10".
#' @param top Only the dosage distribution of the top-ranking herbs is calculated, where the number of herbs considered is determined by the `top` parameter.
#'
#' @return A data frame showing the herb names and their dosage distributions.
#' @importFrom dplyr left_join group_by summarise count arrange
#' @export

#'
#' @examples
#' df <- data.frame(prescription_id = c(1, 2,3),
#' herb1 = c("Huangqi10", "Gancao5","Dihuang12"),
#' herb2 = c("Shengdi", "Danggui12","Banxia15"),
#' herb3 = c("Baizhu15", "Danshen12", NA),
#' herb4 = c("Huangqin12", NA,NA ))
#'


calc_herb_dosage <- function(df, top = NULL){

  df_long <- trans_long2(df)

  dose_frequency <- df_long %>%
    group_by(.data$itemnames, .data$gram_weight) %>%   # 按中药和剂量分组
    summarise(gw_freq = n(), .groups = 'drop') %>%   # 统计每种剂量的频数
    arrange(.data$itemnames, desc(.data$gram_weight))  # 按中药升序，再按剂量降序排序

  # 统计中药的频数
  item_count <- df_long %>%
    count(.data$itemnames, name = "item_frequency") %>%
    arrange(desc(.data$item_frequency))  # 按频数降序排列

  # 合并中药频数和剂量频数
  dose_frequency <- left_join(dose_frequency, item_count, by = "itemnames")

  # 提取频数排名前25的中药
  if(!is.null(top)){

    #检测中药个数最大值
    item_class <- length(unique(dose_frequency$itemnames))

    if (top <= item_class) {
      top_items <- item_count$itemnames[1:top]
      dose_frequency <- dose_frequency[dose_frequency$itemnames %in% top_items, ]
    }
  }

  # 计算每个中药的剂量频数百分比
  dose_frequency <- dose_frequency %>%
    group_by(.data$itemnames) %>%
    mutate(percent = .data$gw_freq / sum(.data$gw_freq) * 100) %>%
    ungroup() # 去掉分组
  colnames(dose_frequency) <- c("\u4e2d\u836f", "\u5242\u91cf", "\u5242\u91cf\u9891\u6570", "\u4e2d\u836f\u9891\u6570", "\u767e\u5206\u6bd4")
  return(dose_frequency)

}













