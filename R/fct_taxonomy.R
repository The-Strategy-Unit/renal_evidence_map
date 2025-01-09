# Consistent {DT} table style for the taxonomy tables
tabulate_taxonomy_table <- function(taxonomy_table) {
  taxonomy_table |>
    DT::datatable(
      options = list(
        dom = "t",        # show table only (no table control elements)
        ordering = FALSE  # remove interactive sort
      ),
      style = "default",
      class = "stripe",
      rownames = FALSE,
      selection = "none"
    )
}
