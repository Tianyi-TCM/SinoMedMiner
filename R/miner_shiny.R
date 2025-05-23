#' Launch the Sinomedminer Shiny Application
#'
#' This function launches the Sinomedminer Shiny application.
#' @param port The port to run the Shiny application on.
#' @importFrom shiny runApp
#' @export
#'
#'
miner_shiny <- function(port = NULL) {
  # 获取 Shiny 应用目录的路径
  appDir <- system.file("shiny", package = "SinoMedminer")

  # 检查目录是否存在
  if (appDir == "") {
    stop("Could not find the Shiny app file. Try re-installing `Sinomedminer`.", call. = FALSE)
  }

  # 启动 Shiny 应用
  shiny::runApp(appDir, display.mode = "normal", port = port)
}


