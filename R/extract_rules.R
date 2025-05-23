#' Extract Association Rules
#'
#' This function extracts association rules from a given data frame using the apriori algorithm.
#'
#' @param df A data frame containing the transaction data. The first colomn is ID and the rest are binary variables.
#' @param supp The minimum support threshold for the apriori algorithm. Default is 0.07.
#' @param conf The minimum confidence threshold for the apriori algorithm. Default is 0.7.
#' @param min_lift The minimum lift threshold for filtering rules. Default is 1.
#' @param minlen The minimum length of the rules. Default is 1.
#' @param maxlen The maximum length of the rules. Default is 4.
#'
#' @return A set of association rules that meet the specified thresholds.
#' @export
#' @importFrom arules apriori subset sort
#'
#' @examples
#' # Example usage:
#' # df <- data.frame(
#' #   item1 = c(1, 0, 1, 0, 1),
#' #   item2 = c(0, 1, 1, 1, 0),
#' #   item3 = c(1, 1, 0, 1, 1)
#' # )
#' # rules <- extract_rules(df, supp = 0.1, conf = 0.8)
#' # inspect(rules)
extract_rules <- function(df,
                          supp = 0.07,
                          conf = 0.7,
                          min_lift = 1,
                          minlen = 1, maxlen = 4){

  # Transform the data frame into transactions format
  trans <- trans_rules(df)
  rules <- apriori(data = trans,
                   parameter = list(support = supp, confidence = conf, minlen = minlen, maxlen = maxlen))

  # Filter rules based on the minimum lift threshold
  rules <- subset(rules, subset = rules@quality$lift > 1)
  rules <- sort(rules, by = "confidence", decreasing = TRUE)
  return(rules)
}






