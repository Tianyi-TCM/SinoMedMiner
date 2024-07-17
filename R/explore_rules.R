#' Exploring Association Rules
#'
#' @name explore_rules2
#'
#' @description
#' For different combinations of support and confidence, this function returns the number of association rules, support, confidence, lift, and other information. Due to the specific nature of clinical data, it is recommended to customize the support and confidence combinations. Additionally, only association rules with a lift greater than 1 are returned, as only these are meaningful.
#'
#' @param trans The transformed transactions object.
#' @param supp  Set of support values, defaulting to c(0.05, 0.07, 0.10, 0.12, 0.15, 0.2, 0.22, 0.25, 0.28, 0.3); customization is recommended.
#' @param conf  Set of confidence values, defaulting to c(0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9); customization is recommended.
#' @param maxlen The maximum length of association rules, default is 4. For clinical data, it is recommended not to exceed 4.
#' @return A data frame containing the number of association rules, the set support level, the set confidence level, the minimum support, the maximum support, the minimum confidence, the maximum confidence, and the minimum lift. Rows that resulted in Inf due to excessively high or low parameters have been removed.
#' @export
#' @importFrom arules apriori
#' @importFrom dplyr filter_all
#' @importFrom dplyr all_vars

explore_rules <- function(trans,
                          supp = c(0.05, 0.07, 0.10, 0.12, 0.15, 0.2, 0.22, 0.25, 0.28, 0.3),
                          conf = c(0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9),
                          maxlen = 4) {

  rules_list <- list()

  for (i in supp) {
    for (j in conf) {
      rules <- apriori(data = trans,
                       parameter = list(support = i, confidence = j, minlen = 1, maxlen = maxlen))

      # Explicitly refer to the lift column
      rules <- subset(rules, subset = rules@quality$lift > 1)

      if (length(rules) > 0) {
        quality <- rules@quality
        rules_sc <- data.frame(
          Number_association_rules = nrow(quality),
          Specified_support = i,
          Specified_confi = j,
          Min_supp = min(quality$support, na.rm = TRUE),
          Max_supp = max(quality$support, na.rm = TRUE),
          Min_confi = min(quality$confidence, na.rm = TRUE),
          Max_confi = max(quality$confidence, na.rm = TRUE),
          Min_lift = min(quality$lift, na.rm = TRUE)
        )
        rules_list[[length(rules_list) + 1]] <- rules_sc
      }
    }
  }

  if (length(rules_list) > 0) {
    rules_select <- do.call(rbind, rules_list)
  } else {
    rules_select <- data.frame(
      Number_association_rules = integer(),
      Specified_support = double(),
      Specified_confi = double(),
      Min_supp = double(),
      Max_supp = double(),
      Min_confi = double(),
      Max_confi = double(),
      Min_lift = double()
    )
  }

  return(rules_select)
}
