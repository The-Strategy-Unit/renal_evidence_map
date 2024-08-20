#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {

  header <- bs4Dash::dashboardHeader(title = "Renal Evidence Map")

  sidebar <- bs4Dash::dashboardSidebar(
    fixed = TRUE,
    skin = "light",
    status = "primary",
    bs4Dash::sidebarMenu(
      id = "sidebarMenu",
      bs4Dash::menuItem("About", tabName = "tab_about"),
      bs4Dash::menuItem("Summarise Evidence", tabName = "tab_summary"),
      bs4Dash::menuItem("Search Evidence", tabName = "tab_search")
      # bs4Dash::menuItem("Taxonomy", tabName = "tab_taxonomy")  # TODO: temporarily disabled
    )
  )

  body <- bs4Dash::dashboardBody(
    bs4Dash::tabItems(
      bs4Dash::tabItem(
        tabName = "tab_about",
        mod_about_ui("about")
      ),
      bs4Dash::tabItem(
        tabName = "tab_summary",
        mod_summary_table_ui("summary_table")
      ),
      bs4Dash::tabItem(
        tabName = "tab_search",
        mod_search_ui("search")
      )#,
      # bs4Dash::tabItem(  # TODO: temporarily disabled
      #   tabName = "tab_taxonomy",
      #   mod_taxonomy_ui("taxonomy")
      # )
    )
  )

  shiny::tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    shinyjs::useShinyjs(),
    bs4Dash::dashboardPage(
      help = NULL,
      dark = NULL,
      header,
      sidebar,
      body
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
      app_title = "evidence_maps"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
