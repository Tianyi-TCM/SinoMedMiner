#' Find Similar Prescriptions
#' @description
#' By comparing with classic famous formulas, find similar prescriptions in classic formulas that resemble the input prescription.
#'
#' @param rxs A vector of strings, such as: fangji <- c("Mahuang, Guizhi, Shaoyao, Gancao"), where each string represents a prescription and, following convention, Chinese herbs are separated by Chinese commas.
#' @param threshold A numeric value to set the similarity threshold, default is 0.3.
#'
#' @return Returns a string that represents the IDs and compositions of similar prescriptions, for example: "1：Mahuang，Guizhi，Shaoyao，Gancao".
#'
#' @importFrom dplyr arrange desc filter
#' @importFrom stringr str_trim
#' @export
#'
#' @examples
#' #
findSimRxs <- function(rxs, threshold = 0.3) {

  # Set similarity function
  jaccard_similarity <- function(set1, set2) {
    intersection <- length(intersect(set1, set2))
    union <- length(union(set1, set2))
    return(intersection / union)
  }
  # Function to split into lists
  split_formula <- function(formula) {
    return(str_trim(unlist(strsplit(formula, "\uFF0C"))))
  }

  formlae_prescription$formula_list <- lapply(formlae_prescription$formula, split_formula)
  df2_list <- split_formula(rxs)

  formlae_prescription$jaccard_similarity <- sapply(formlae_prescription$formula_list,
                                                    function(f) jaccard_similarity(f, df2_list))


  # Filter similar prescriptions according to the threshold
  df3 <- formlae_prescription %>% filter(jaccard_similarity >= threshold) %>% arrange(desc(jaccard_similarity))


  # Check if df3 is empty
  if (nrow(df3) == 0) {
    cat("No similar prescriptions under the current similarity threshold.\n")
  } else {
    # Concatenate id and formula columns
    df3$pasted <- paste(df3$id, df3$formula, sep=":")

    # Print results
    cat("Similar prescriptions are:\n")
    for (i in 1:nrow(df3)) {
      cat(df3$pasted[i], "\n")
    }

  }
}


