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
      title = "Generic evidence map",
      bg = "#F8F9FA",
      bslib::nav_panel(
        id = "nav_panel_about",
        title = "About",
        bslib::card(
          id = "card_about",
          full_screen = TRUE,
          htmltools::p("Placeholder")
        )
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
                label = "Select row category",
                choices = paste0("category_", c("x", "y", "z")),
                selected = "category_x",
                multiple = FALSE
              ),
              shiny::selectInput(
                inputId = "select_column",
                label = "Select column category",
                choices = paste0("category_", c("x", "y", "z")),
                selected = "category_y",
                multiple = FALSE
              ),
              shiny::selectInput(
                inputId = "select_years",
                label = "Select years",
                choices = NULL,
                selected = "2010",
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
                bslib::card_header("Waffle", class = "bg-light"),
                shiny::plotOutput(outputId = "waffle")
              )
            ),
            bslib::card(
              id = "card_filtered_table",
              full_screen = TRUE,
              bslib::card_header("Filtered table", class = "bg-light"),
              DT::DTOutput(outputId = "filtered_table")
            )
          )
        )
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
      app_title = "evidence_maps"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
