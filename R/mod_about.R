#' about UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_about_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bs4Dash::box(
      width = 6,
      collapsible = FALSE,
      headerBorder = FALSE,
      shiny::HTML("&nbsp;&nbsp;&nbsp;"),
      tags$img(
        src = "www/tsu_logo_black_screen_transparent.png",
        style = "height: 120px;"
      )
    ),
    bs4Dash::box(
      title = "About",
      width = 6,
      collapsible = FALSE,
      md_file_to_html("app", "text", "about-explanation.md")
    ),
    bs4Dash::box(
      title = "Updates",
      width = 6,
      collapsible = FALSE,
      md_file_to_html("app", "text", "about-updates.md")
    ),
    bs4Dash::box(
      title = "License",
      width = 6,
      collapsible = FALSE,
      md_file_to_html("app", "text", "about-license.md")
    )
  )
}
