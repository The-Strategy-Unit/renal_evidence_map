#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # Data

  pinned_data <- get_pinned_data("matt.dray/renal_evidence_map_data")

  taxonomy <- pinned_data[["About this map"]]

  dat <- get_evidence_data(pinned_data) |>
    dplyr::rename(  # the waffle errors without this
      "type_of_evidence" = "Type of evidence",
      "high_level_outcomes" = "High level outcomes",
      "focus_simplified" = "Focus (simplified)"
    )

  taxonomy <- get_taxonomy_tables(pinned_data)

  all_categories <- c(
    "Focus (simplified)" = "focus_simplified",  # row category default
    "Type of evidence" = "type_of_evidence",  # column category default
    "High level outcomes" = "high_level_outcomes"
  )

  # Selection reactives

  all_years <- shiny::reactive({
    dat$`Publication year` |>
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
    dat |> dplyr::filter(`Publication year` %in% input$select_years)
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

  # Outputs

  output$intro <- shiny::renderText({
    get_intro(pinned_data)
  })

  output$evidence_map_table <- DT::renderDT({

    shiny::validate(
      shiny::need(
        input$select_years,
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
        input$select_years,
        message = "Select at least one publication year."
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

  output$search_table <- DT::renderDT({
    dat |>
      dplyr::rename(  # because the waffle errored unless dat names were changed
        "Focus (simplified)" = "focus_simplified",
        "Type of evidence" = "type_of_evidence",
        "High level outcomes" = "high_level_outcomes"
      ) |>
      dplyr::select(-"Abstract") |>
      dplyr::mutate(
        dplyr::across(
          c(
            "Publication year",
            "Journal",
            "Type of evidence",
            "High level outcomes",
            "Focus of the paper",
            "Focus (simplified)",
            "Setting",
          ),
          factor  # allows discrete dropdown in datatable
        )
      ) |>
      DT::datatable(
        style = "default",
        class = "stripe",
        rownames = FALSE,
        selection = "none",
        escape = FALSE,
        filter = list(position = "top"),
        options = list(search = list(regex = TRUE))
      )
  })

  output$taxonomy_theme <- shiny::renderTable({
    taxonomy[["Theme categories"]]
  })

  output$taxonomy_focus <- shiny::renderTable({
    taxonomy[["Focus categories"]]
  })

  output$taxonomy_setting <- shiny::renderTable({
    taxonomy[["Setting"]]
  })

  output$taxonomy_evidence_type <- shiny::renderTable({
    taxonomy[["Evidence type"]]
  })

  output$taxonomy_clinical <- shiny::renderTable({
    taxonomy[["Clinical conditions"]]
  })

  output$taxonomy_high_level_preamble <- shiny::renderText({
    taxonomy[["High-level outcome preamble"]]
  })

  output$taxonomy_high_level_outcome <- shiny::renderTable({
    taxonomy[["High-level outcome categories"]]
  })

}
