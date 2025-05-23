#' Convert data to long format
#'
#' @param df A messy data frame where the first column is the prescription ID, each row is a prescription, and each cell contains one Chinese herb. If a herb has an associated dose, the dose will be extracted and converted into a numeric value representing the weight in grams, without including the unit (e.g., "Huangqi10").
#'
#' @return A long-format data frame containing three columns: prescription ID, herb, and the herb's dose (if available). Each row represents one herb.
#' @importFrom dplyr select distinct
#' @importFrom tidyr drop_na separate
#' @export

#'
#' @examples
#' df <- data.frame(prescription_id = c(1, 2,3),
#' herb1 = c("Huangqi10", "Gancao5","Dihuang12"),
#' herb2 = c("Shengdi", "Danggui12","Banxia15"),
#' herb3 = c("Baizhu15", "Danshen12", NA),
#' herb4 = c("Huangqin12", NA,NA )
#' )
trans_long2 <- function(df){
  #change the row sequence, zhuzshi yu 10.19
  df[[1]] <- as.character(df[[1]])
  id_col <- colnames(df)[1]
  contains_numbers <- any(grepl("\\d", df[[2]]))
  longdata <- df %>%
    pivot_longer(cols = -all_of(id_col),
                 names_to = "herb", values_to = "itemnames") %>%
    select(-2) %>% drop_na() %>% distinct() %>%
    mutate(id_order = factor(.data[[colnames(df)[1]]], levels = df[[1]])) %>%
    arrange(.data$id_order) %>%
    select(-.data$id_order)

  if(contains_numbers){
    longdata <- longdata %>%
      tidyr::separate("itemnames", into = c("itemnames", "gram_weight"), sep = "(?<=\\D)(?=\\d)")
    longdata[[3]] <- as.numeric(longdata[[3]])
    }

  return(longdata)
}







