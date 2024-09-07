#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # Demo data

  set.seed(123)
  n <- 500
  gen_cats <- \(cat, range = 1:5, n) sample(paste0(cat, range), n, TRUE)
  dat <- tibble::tibble(
    year = sample(paste0("20", 10:24), n, replace = TRUE),
    category_x = gen_cats("x", 1:5, n),
    category_y = gen_cats("y", 1:4, n),
    category_z = gen_cats("z", 1:9, n)
  )

  # Selection reactives

  all_categories <- shiny::reactive({
    dat[, names(dat) != "year"] |> names() |> sort()
  })

  all_years <- shiny::reactive({
    dat$year |>
      unique() |>
      sort(decreasing = TRUE)  # latest first
  })

  selected_row <- shiny::reactive({
    shiny::req(input$select_row)
    input$select_row
  })

  selected_column <- shiny::reactive({
    shiny::req(input$select_column)
    input$select_column
  } )

  selected_years <- shiny::reactive({
    shiny::req(input$select_year)
    input$select_years
  })

  # Table-cell reactives

  row_category_value <- shiny::reactive({
    cell_coords <- input$evidence_map_table_cells_selected
    row_i <- cell_coords[1, 1]
    evidence_map_data()[[row_i, 1]]
  })

  column_category_value <- shiny::reactive({
    cell_coords <- input$evidence_map_table_cells_selected
    column_i <- cell_coords[1, 2] + 1  # zero-indexed
    names(evidence_map_data()[, column_i, drop = FALSE])
  })

  # Observers

  shiny::observe({
    shiny::updateSelectInput(
      inputId = "select_row",
      choices = all_categories()[all_categories() != selected_column()],
      selected = selected_row()
    )
    shiny::updateSelectInput(
      inputId = "select_column",
      choices = all_categories()[all_categories() != selected_row()],
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

  observeEvent(
    eventExpr = input$button_years_all,
    handlerExpr = {
      shiny::updateSelectInput(
        inputId = "select_years",
        choices = all_years(),
        selected = all_years()
      )
    }
  )

  observeEvent(
    eventExpr = input$button_years_clear,
    handlerExpr = {
      shiny::updateSelectInput(
        inputId = "select_years",
        choices = all_years(),
        selected = NULL
      )
    }
  )

  # Data prep

  filtered_data <- reactive({
    dat |> dplyr::filter(year %in% input$select_years)
  })

  counted_data <- reactive({
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
          dplyr::everything(),
          \(x) tidyr::replace_na(x, 0)
        )
      ) |>
      dplyr::arrange(1) |>  # column name can change, so use index
      dplyr::rename("aaa" = 1)  # ensures row-category column will be ordered first

    dat_prepared |>
      dplyr::select(order(names(dat_prepared))) |>
      dplyr::rename(!!rlang::sym(selected_row()) := "aaa")  # revert column name

  })

  # Outputs

  output$evidence_map_table <- DT::renderDT({

    shiny::validate(
      shiny::need(
        input$select_years,
        message = "Select at least one year."
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
        input$select_years,
        message = "Select at least one year."
      )
    )

    shiny::validate(
      shiny::need(
        input$evidence_map_table_cells_selected,
        message = "Click a cell in the evidence map table."
      )
    )

    row_filtered_data <- filtered_data() |>
      dplyr::filter(.data[[selected_row()]] == row_category_value())

    row_filtered_data |>
      ggwaffle::waffle_iron(
        mapping = selected_column(),
        rows = floor(sqrt(nrow(row_filtered_data)))
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
        input$select_years,
        message = "Select at least one year."
      )
    )

    shiny::validate(
      shiny::need(
        input$evidence_map_table_cells_selected,
        message = "Click a cell in the evidence map table.")
    )

    filtered_data() |>
      dplyr::filter(
        .data[[selected_row()]] == row_category_value(),
        .data[[selected_column()]] == column_category_value()
      ) |>
      DT::datatable(
        style = "default",
        class = "stripe",
        rownames = FALSE,
        selection = "none"
      )
  })

}
