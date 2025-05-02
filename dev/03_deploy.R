deploy <- function(server_name, app_id) {
  rsconnect::deployApp(
    server = server_name,
    appFiles = c(
      "R/",
      "inst/",
      "NAMESPACE",
      "DESCRIPTION",
      "app.R"
    ),
    appId = app_id,
    lint = FALSE,
    forceUpdate = TRUE
  )
}

deploy("connect.strategyunitwm.nhs.uk", 306)
deploy("connect.su.mlcsu.org", 92)
