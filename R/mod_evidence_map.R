#' evidence_map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_evidence_map_ui <- function(id) {

  ns <- shiny::NS(id)

  select_row <- shiny::selectInput(
    inputId = ns("select_row"),
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
  )

  select_column <- shiny::selectInput(
    inputId = ns("select_column"),
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
  )

  select_years <- shiny::selectInput(
    inputId = ns("select_years"),
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
  )

  button_years_all <- shiny::actionButton(
    inputId = ns("button_years_all"),
    label = "Select all"
  )

  button_years_clear <- shiny::actionButton(
    inputId = ns("button_years_clear"),
    label = "Clear"
  )

  card_evidence_map_table <- bslib::card(
    id = "card_evidence_map_table",
    full_screen = TRUE,
    bslib::card_header("Evidence map", class = "bg-light"),
    DT::DTOutput(ns("evidence_map_table"))
  )

  card_waffle <- bslib::card(
    id = "card_waffle",
    full_screen = TRUE,
    bslib::card_header("Waffle chart", class = "bg-light"),
    shiny::plotOutput(ns("waffle"))
  )

  card_filtered_table <- bslib::card(
    id = "card_filtered_table",
    full_screen = TRUE,
    bslib::card_header(
      "Filtered publications",
      class = "bg-light"
    ),
    DT::DTOutput(ns("filtered_table"))
  )

  shiny::tagList(
    bslib::card(
      id = "card_evidence_map",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          open = TRUE,
          select_row,
          select_column,
          select_years,
          button_years_all,
          button_years_clear
        ),
        bslib::layout_column_wrap(
          card_evidence_map_table,
          card_waffle
        ),
        card_filtered_table
      )
    )
  )

  # shiny::tagList(
  #   bslib::card(
  #     id = "card_evidence_map",
  #     bslib::layout_sidebar(
  #       sidebar = bslib::sidebar(
  #         open = TRUE,
  #         shiny::selectInput(
  #           inputId = ns("select_row"),
  #           label = bslib::tooltip(
  #             trigger = list(
  #               "Select row category",
  #               bsicons::bs_icon("info-circle")
  #             ),
  #             "The grouping category to show in the rows of the evidence map."
  #           ),
  #           choices = NULL,
  #           selected = NULL,
  #           multiple = FALSE
  #         ),
  #         shiny::selectInput(
  #           inputId = ns("select_column"),
  #           label = bslib::tooltip(
  #             trigger = list(
  #               "Select column category",
  #               bsicons::bs_icon("info-circle")
  #             ),
  #             "The grouping category to show in the columns of the evidence map."
  #           ),
  #           choices = NULL,
  #           selected = NULL,
  #           multiple = FALSE
  #         ),
  #         shiny::selectInput(
  #           inputId = ns("select_years"),
  #           label = bslib::tooltip(
  #             trigger = list(
  #               "Select years",
  #               bsicons::bs_icon("info-circle")
  #             ),
  #             "Filter for evidence from selected years. Use the buttons below to select or clear all years."
  #           ),
  #           choices = NULL,
  #           selected = NULL,
  #           multiple = TRUE,
  #         ),
  #         shiny::actionButton(
  #           inputId = ns("button_years_all"),
  #           label = "Select all"
  #         ),
  #         shiny::actionButton(
  #           inputId = ns("button_years_clear"),
  #           label = "Clear"
  #         )
  #       ),
  #       bslib::layout_column_wrap(
  #         bslib::card(
  #           id = "card_evidence_map_table",
  #           full_screen = TRUE,
  #           bslib::card_header("Evidence map", class = "bg-light"),
  #           DT::DTOutput(ns("evidence_map_table"))
  #         ),
  #         bslib::card(
  #           id = "card_waffle",
  #           full_screen = TRUE,
  #           bslib::card_header("Waffle chart", class = "bg-light"),
  #           shiny::plotOutput(ns("waffle"))
  #         )
  #       ),
  #       bslib::card(
  #         id = "card_filtered_table",
  #         full_screen = TRUE,
  #         bslib::card_header(
  #           "Filtered publications",
  #           class = "bg-light"
  #         ),
  #         DT::DTOutput(ns("filtered_table"))
  #       )
  #     )
  #   )
  # )


}

#' evidence_map Server Functions
#'
#' @noRd
mod_evidence_map_server <- function(id, dat) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # Selection reactives ----

    all_years <- shiny::reactive({
      dat$`Publication year` |>
        unique() |>
        sort(decreasing = TRUE)  # latest first
    })

    selected_years <- shiny::reactive({
      shiny::req(input$select_years)
      input$select_years
    })

    selected_column <- shiny::reactive({
      shiny::req(input$select_column)
      input$select_column
    })

    selected_row <- shiny::reactive({
      shiny::req(input$select_row)
      input$select_row
    })

    selected_cell_coords <- shiny::reactive({
      shiny::req(input$evidence_map_table_cells_selected)
      input$evidence_map_table_cells_selected
    })

    # Observers

    all_categories <- c(
      "Focus (simplified)" = "focus_simplified",  # row category default
      "Type of evidence" = "type_of_evidence",  # column category default
      "High level outcomes" = "high_level_outcomes"
    )

    shiny::observe({
      shiny::updateSelectInput(
        inputId = "select_row",
        choices = all_categories,
        selected = all_categories[1]
      )
    })

    shiny::observe({
      shiny::updateSelectInput(
        inputId = "select_column",
        choices = all_categories,
        selected = all_categories[2]
      )
    })

    shiny::observe({
      shiny::updateSelectInput(
        inputId = "select_row",
        choices = all_categories[all_categories != selected_column()],
        selected = selected_row()
      )
      shiny::updateSelectInput(
        inputId = "select_column",
        choices = all_categories[all_categories != selected_row()],
        selected = selected_column()
      )
    })

    shiny::observe({
      shiny::updateSelectInput(
        inputId = "select_years",
        choices = all_years(),
        selected = all_years()
      )
    })

    shiny::observeEvent(
      eventExpr = input$button_years_all,
      handlerExpr = {
        shiny::updateSelectInput(
          inputId = "select_years",
          choices = all_years(),
          selected = all_years()
        )
      }
    )

    shiny::observeEvent(
      eventExpr = input$button_years_clear,
      handlerExpr = {
        shiny::updateSelectInput(
          inputId = "select_years",
          choices = all_years(),
          selected = NULL
        )
      }
    )

    # Data prep ----

    filtered_data <- shiny::reactive({
      dat |> dplyr::filter(`Publication year` %in% selected_years())
    })

    counted_data <- shiny::reactive({
      filtered_data() |>
        dplyr::count(.data[[selected_row()]], .data[[selected_column()]])
    })

    evidence_map_data <- shiny::reactive({

      dat_prepared <- counted_data() |>
        tidyr::pivot_wider(
          id_cols = selected_row(),
          names_from = selected_column(),
          values_from = "n"
        ) |>
        dplyr::mutate(
          dplyr::across(
            tidyselect::everything(),
            \(x) tidyr::replace_na(x, 0)
          )
        ) |>
        dplyr::arrange(1) |>  # column name can change, so use index
        dplyr::rename("aaa" = 1)  # ensures row-category column will be ordered first

      dat_prepared |>
        dplyr::select(order(names(dat_prepared))) |>
        dplyr::rename(" " = "aaa")  # remove column name

    })

    row_category_value <- shiny::reactive({
      row_i <- selected_cell_coords()[1, 1]
      evidence_map_data()[[row_i, 1]]
    })

    column_category_value <- shiny::reactive({
      column_i <- selected_cell_coords()[1, 2] + 1  # zero-indexed
      names(evidence_map_data()[, column_i, drop = FALSE])
    })

    # Outputs ----

    output$evidence_map_table <- DT::renderDT({

      shiny::validate(
        shiny::need(
          selected_years(),
          message = "Select at least one publication year."
        )
      )

      evidence_map_data() |>
        DT::datatable(
          style = "default",
          class = "stripe",
          rownames = FALSE,
          selection = list(
            mode = "single",
            target = "cell"
          ),
          options = list(
            pageLength = -1,  # show all rows
            dom = "t",
            ordering = FALSE
          )
        )

    })

    output$waffle <- shiny::renderPlot({

      shiny::validate(
        shiny::need(
          selected_years(),
          message = "Select at least one publication year."
        )
      )

      shiny::validate(
        shiny::need(
          selected_cell_coords(),
          message = "Click a cell in the evidence map table."
        )
      )

      row_filtered_data <- filtered_data() |>
        dplyr::filter(.data[[selected_row()]] == row_category_value())

      row_filtered_data |>
        ggwaffle::waffle_iron(
          mapping = selected_column(),
          rows = floor(sqrt(nrow(row_filtered_data)))  # prevents error
        ) |>
        ggplot2::ggplot(ggplot2::aes(x, y, fill = group)) +
        ggwaffle::geom_waffle() +
        ggwaffle::geom_waffle(
          data = \(x) dplyr::filter(x, group == column_category_value()),
          colour = "black",
          show.legend = FALSE
        ) +
        ggplot2::coord_equal() +
        viridis::scale_fill_viridis(discrete = TRUE) +
        ggwaffle::theme_waffle() +
        ggplot2::theme(
          axis.title.x = ggplot2::element_blank(),
          axis.title.y = ggplot2::element_blank(),
          legend.position = "right",
          legend.title = ggplot2::element_blank(),
          legend.text = ggplot2::element_text(size = 14)
        ) +
        ggplot2::guides(fill = ggplot2::guide_legend(ncol = 1))

    })

    output$filtered_table <- DT::renderDT({

      shiny::validate(
        shiny::need(
          selected_years(),
          message = "Select at least one publication year."
        )
      )

      shiny::validate(
        shiny::need(
          selected_years(),
          message = "Click a cell in the evidence map table."
        )
      )

      filtered_data() |>
        dplyr::filter(
          .data[[selected_row()]] == row_category_value(),
          .data[[selected_column()]] == column_category_value()
        ) |>
        dplyr::select("Authors", "Title", "Publication year", "Link") |>
        dplyr::arrange("Publication year") |>
        DT::datatable(
          style = "default",
          class = "stripe",
          rownames = FALSE,
          selection = "none",
          escape = FALSE
        )

    })

  })
}

## To be copied in the UI
# mod_evidence_map_ui("mod_evidence_map")

## To be copied in the server
# mod_evidence_map_server("mod_evidence_map")
