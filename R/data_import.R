#' Launch the Sinomedminer Shiny Application to import data
#'
#' This function launches the Sinomedminer Shiny application.
#' @importFrom shiny runApp
#' @export
#'
#'
data_import <- function() {
  # 获取 Shiny 应用目录的路径
  appDir <- system.file("shiny_import", package = "SinoMedminer")

  # 检查目录是否存在
  if (appDir == "") {
    stop("Could not find the Shiny app file. Try re-installing `Sinomedminer`.", call. = FALSE)
  }

  # 启动 Shiny 应用
  shiny::runApp(appDir, display.mode = "normal")
}
