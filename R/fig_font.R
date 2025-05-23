#' Set the Font for Plotting
#'
#' @param font The font name, either "Songti" or "Kaiti". Default is "Songti", used for plotting on Linux.
#'
#' @return No return value.
#' @export
#'
#' @examples
#' # fig_font("Songti")
#'
#' @import showtext
#' @import sysfonts
fig_font <- function(font = "Song") {
  if (font == "Song") {
    font_path = "/usr/share/fonts/windows/simsun.ttc"
    showtext::showtext_auto()
    sysfonts::font_add("Song", font_path)
    showtext::showtext_opts(dpi = 600)
  }
  if (font == "Kaiti") {
    font_path = "/usr/share/fonts/windows/simkai.ttf"
    showtext::showtext_auto()
    sysfonts::font_add("Kaiti", font_path)
    showtext::showtext_opts(dpi = 600)
  }
}

