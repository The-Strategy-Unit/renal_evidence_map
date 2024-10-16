#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {

  nav_panel_about <- bslib::nav_panel(
    id = "nav_panel_about",
    title = "About",
    mod_about_ui("mod_about")
  )

  nav_panel_map <- bslib::nav_panel(
    id = "nav_panel_map",
    title = "Evidence map",
    mod_evidence_map_ui("mod_evidence_map")
  )

  nav_panel_search <- bslib::nav_panel(
    id = "nav_panel_search",
    title = "Search",
    mod_search_ui("mod_search")
  )

  nav_panel_taxonomy <- bslib::nav_panel(
    id = "nav_panel_taxonomy",
    title = "Taxonomy",
    mod_taxonomy_ui("mod_taxonomy")
  )

  shiny::tagList(
    golem_add_external_resources(),
    bslib::page_navbar(
      id = "page_navbar",
      title = "Evidence to Inform Renal Services Modelling",
      bg = "#F8F9FA",
      nav_panel_about,
      nav_panel_map,
      nav_panel_search,
      nav_panel_taxonomy
    )
  )

}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {

  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Renal Evidence Map"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )

}
