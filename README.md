# renal_evidence_map

## About

Evidence maps are a way to visually display the body of literature in a specific field in order to show gaps in evidence or study characteristics. 

This repository contains a Shiny app that allows interactive exploration of evidence that supports renal-services modelling.
This work is part of the larger New Hospital Programme (NHP).

The user can select from different categories and years to present a table and chart of the relevant papers.
There is also a table of information about all papers that can be filtered and searched.

## Developer notes

### Development

This is [a {golem} app](https://thinkr-open.github.io/golem/).
Code is arranged into modules in `R/`, with one file per section of the app.
To run the app for development purposes, source the contents of `dev/run_dev.R`.

### Deploy

To re-deploy the app to Connect, run the `deployApp()` call in `dev/03_deploy.R`.
This step will read the unique `appId` from the `rsconnect/` folder in the project root.
You won't have an `rsconnect/` folder if you haven't deployed this app before, so you'll have to write in the app ID manually.
It can be found under 'Content ID' in the Settings > Info menu after you log in to Posit Connect and view the app.

### Update pinned data

The underlying data for this app is a spreadsheet, tabs of which are extracted and stored as a 'pin' on Posit Connect.
The app reads data from the pin using [the {pins} package](https://pins.rstudio.com/).
You can overwrite the existing pin and it will create a new version; you can see and revert to earlier versions of the pin if needed.

Below is some illustrative code to update the pinned data.
You must have access to and permissions for Posit Connect to be able to run this code.

``` r
# Connect to board
board <- pins::board_connect()

# Check existing pin
pin_name <- "matt.dray/renal_evidence_map_data"
board |> pins::pin_exists(pin_name)  # logical
board |> pins::pin_read(pin_name) |> str(1)  # list structure
board |> pins::pin_versions(pin_name)  # active and past versions

# Read new version of spreadsheet into list

file <- "spreadsheet.xlsx"  # path to local copy of evidence map spreadsheet
sheet_names <- readxl::excel_sheets(file)[1:2]  # 'About this map' and 'Datasheet' tabs

sheets_list <- purrr::map(
  sheet_names,
  \(x) suppressMessages(readxl::read_xlsx(file, sheet = x))
) |> 
  purrr::set_names(sheet_names)

# Write to pin with custom 'notes' metadata
board |> pins::pin_write(
  sheets_list,
  pin_name,
  metadata = list(
    # update the notes (version/date from the 'About this map' tab)
    notes = "Version X, Month YYYY, file 'spreadsheet.xlsx'"  # leave a note
  ),
  type = "rds"  # otherwise it may autodetect json
)

# Confirm upload
board |> pins::pin_versions(pin_name)  # should see new version
pins::pin_meta(board, pin_name)[["user"]][["notes"]]  # custom notes metadata
```
