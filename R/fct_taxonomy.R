# Consistent {DT} table style for the taxonomy tables
tabulate_taxonomy_table <- function(taxonomy_table) {
  taxonomy_table |>
    DT::datatable(
      options = list(
        dom = "t",  # show table only (no table control elements)
        ordering = FALSE,  # remove interactive sort
        pageLength = nrow(taxonomy_table)  # show all rows
      ),
      style = "default",
      class = "stripe",
      rownames = FALSE,
      selection = "none",
      escape = FALSE  # respect HTML <br> linebreaks in text
    )
}
