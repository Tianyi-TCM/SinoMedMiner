#' Create a Sunburst Plot for Efficacy Data
#'
#' This function calculates the efficacy of the given data and creates a sunburst plot
#' to visualize the main and sub efficacy categories. It allows customization of the
#' plot palette, label size, and an option to ignore categories below a certain threshold.
#'
#' @param df A data frame containing the input data. The data frame should have columns named `main_efficacy`, `sub_efficacy`, and `n`.
#' @param cal_pal A string representing the RColorBrewer palette to be used. Default is "Set3".
#' @param ignore A numeric value between 0 and 1 indicating the threshold below which categories will be ignored. Default is 0.03.
#' @param lab_size A numeric value specifying the label text size in the plot. Default is 2.5.
#' @param added A data frame that contains the added information of Chinese medicine properties.
#' @return A ggplot2 object representing the sunburst plot.
#' @importFrom dplyr mutate filter
#' @importFrom ggsunburst get_sunplot_dataset draw_sunburst_plot
#' @importFrom RColorBrewer brewer.pal
#' @importFrom rlang sym
#' @export
#'
#' @examples
#' \dontrun{
#'   df <- data.frame(
#'     main_efficacy = c("A", "B", "A", "C"),
#'     sub_efficacy = c("A1", "B1", "A2", "C1"),
#'     n = c(10, 20, 30, 40)
#'   )
#'   fig_efficacy(df, cal_pal = "Set2", ignore = 0.05, lab_size = 3)
#' }
fig_efficacy <- function(df, cal_pal = "Set3",
                         ignore = 0.03,
                         lab_size = 2.5,
                         added = NULL) {
  # Check if cal_pal is not NULL and if it is a valid RColorBrewer palette
  if (!is.null(cal_pal)) {
    valid_palettes <- c("Set1", "Set2", "Set3", "Pastel1",
                        "Pastel2", "Dark2", "Accent", "Paired")
    if (!cal_pal %in% valid_palettes) {
      stop("cal_pal must be a valid RColorBrewer palette")
    }
  }

  # Check if ignore is a numeric value between 0 and 1
  if (!is.numeric(ignore) || ignore <= 0 || ignore >= 1) {
    stop("ignore must be a numeric value greater than 0 and less than 1")
  }

  # Calculate efficacy
  if(is.null(added)){
    gongxiao <- calc_efficacy(df)
  } else {
    gongxiao <- calc_efficacy(df, added)
  }
  colnames(gongxiao) <- c("main_efficacy", "sub_efficacy", "n")

  # Check if gongxiao is an empty data frame
  if (nrow(gongxiao) == 0) {
    stop("The calculated gongxiao dataframe is empty")
  }

  # Data processing: calculate percentage and filter
  gongxiao2 <- gongxiao %>%
    mutate(perc = n / sum(n)) %>%
    filter(.data$perc >= ignore)

  # Check if gongxiao2 is an empty data frame
  if (nrow(gongxiao2) == 0) {
    stop("No data left after filtering based on the ignore threshold")
  }

  # Prepare plot data using rlang::sym to correctly handle column names
  main_efficacy_sym <- rlang::sym("main_efficacy")
  sub_efficacy_sym <- rlang::sym("sub_efficacy")
  count_var_sym <- rlang::sym("n")

  plot_dat <- ggsunburst::get_sunplot_dataset(
    .dat = gongxiao2,
    !!main_efficacy_sym,
    !!sub_efficacy_sym,
    .count_var = !!count_var_sym
  )
  fig_font()
  # Draw sunburst plot
  ### 自改函数 ### 在myfunc.R中
  p <- draw_sunburst_plot2(
    .dat = plot_dat,
    .label_txt_size = lab_size,
    palette = cal_pal,
    family = "Song")
  return(p)
}


