#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  shiny::tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    bslib::page_navbar(
      id = "page_navbar",
      title = "Evidence to Inform Renal Services Modelling",
      bg = "#F8F9FA",

      bslib::nav_panel(
        id = "nav_panel_about",
        title = "About",
        mod_about_ui("mod_about")
      ),

      bslib::nav_panel(
        id = "nav_panel_map",
        title = "Evidence map",
        bslib::card(
          id = "card_evidence_map",
          bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              open = TRUE,
              shiny::selectInput(
                inputId = "select_row",
                label = bslib::tooltip(
                  trigger = list(
                    "Select row category",
                    bsicons::bs_icon("info-circle")
                  ),
                  "The grouping category to show in the rows of the evidence map."
                ),
                choices = NULL,
                selected = NULL,
                multiple = FALSE
              ),
              shiny::selectInput(
                inputId = "select_column",
                label = bslib::tooltip(
                  trigger = list(
                    "Select column category",
                    bsicons::bs_icon("info-circle")
                  ),
                  "The grouping category to show in the columns of the evidence map."
                ),
                choices = NULL,
                selected = NULL,
                multiple = FALSE
              ),
              shiny::selectInput(
                inputId = "select_years",
                label = bslib::tooltip(
                  trigger = list(
                    "Select years",
                    bsicons::bs_icon("info-circle")
                  ),
                  "Filter for evidence from selected years. Use the buttons below to select or clear all years."
                ),
                choices = NULL,
                selected = NULL,
                multiple = TRUE,
              ),
              actionButton(
                inputId = "button_years_all",
                label = "Select all"
              ),
              actionButton(
                inputId = "button_years_clear",
                label = "Clear"
              )
            ),
            bslib::layout_column_wrap(
              bslib::card(
                id = "card_evidence_map_table",
                full_screen = TRUE,
                bslib::card_header("Evidence map", class = "bg-light"),
                DT::DTOutput(outputId = "evidence_map_table")
              ),
              bslib::card(
                id = "card_waffle",
                full_screen = TRUE,
                bslib::card_header("Waffle chart", class = "bg-light"),
                shiny::plotOutput(outputId = "waffle")
              )
            ),
            bslib::card(
              id = "card_filtered_table",
              full_screen = TRUE,
              bslib::card_header(
                "Filtered publications",
                class = "bg-light"
              ),
              DT::DTOutput(outputId = "filtered_table")
            )
          )
        )
      ),
      bslib::nav_panel(
        id = "nav_panel_search",
        title = "Search",
        bslib::card(
          id = "card_search",
          full_screen = TRUE,
          DT::DTOutput("search_table")
        )
      ),
      bslib::nav_panel(
        id = "nav_panel_taxonomy",
        title = "Taxonomy",
        mod_taxonomy_ui("mod_taxonomy")
      )
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
      app_title = "renal_evidence_map"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
