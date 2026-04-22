
## Challenge:  #30DayChartChallenge 2026
## Prompt:     Day 22 · Timeseries | New Tool

## Topic:      Renewable share of electricity, 2000–2023 — 6 selected countries
## Author:     Steven Ponce
## Date:       2026-04-22

##             https://datawrapper.dwcdn.net/U969h/1/
##

## NOTE: Visualization handled entirely in Datawrapper (new tool).
##       This script handles reshaping and CSV export only.
##       Six countries chosen for maximum trajectory contrast:
##         Norway   — ceiling benchmark (~95% throughout)
##         Denmark  — dramatic rise, the transition star
##         China    — rising from a massive generation base
##         India    — modest rise, developing-world story
##         Poland   — low and slow, fossil fuel laggard
##         Australia — late mover, strong recent acceleration
##
##   Data source: Our World in Data — Energy Data
##   URL: https://raw.githubusercontent.com/owid/energy-data/master/owid-energy-data.csv


## 1. LOAD PACKAGES & SETUP ----

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,   # Easily Install and Load the 'Tidyverse'
  janitor,     # Simple Tools for Examining and Cleaning Dirty Data
  here         # A Simpler Way to Find Your Files
)


## 2. READ IN THE DATA ----

energy_raw <- read_csv(here("2026/data/owid-energy-data.csv"))


## 3. TIDY DATA ----

### |- define the 6 focal countries ----
focal_countries <- c(
  "Norway", "Denmark", "China", "India", "Poland", "Australia"
)

### |- filter: focal countries, 2000–2023, non-missing renewable share ----
renewables_long <- energy_raw |>
  filter(
    country %in% focal_countries,
    year >= 2000,
    year <= 2023,
    !is.na(renewables_share_elec)
  ) |>
  select(country, year, renewables_share_elec) |>
  mutate(
    renewables_share_elec = round(renewables_share_elec, 1)
  ) |>
  arrange(country, year)

### |- check coverage: should be 24 rows per country (2000–2023) ----
renewables_long |>
  count(country) |>
  print()

### |- inspect ----
renewables_long |>
  filter(year %in% c(2000, 2010, 2023)) |>
  pivot_wider(names_from = year, values_from = renewables_share_elec) |>
  arrange(desc(`2023`)) |>
  print()


## 4. RESHAPE WIDE FOR DATAWRAPPER ----

# Datawrapper multi-line format:
#   - One column per series (country)
#   - One row per x-axis value (year)
#   - First column = Year (x-axis)
#
# Shape:
#   Year | Norway | Denmark | China | India | Poland | Australia

renewables_dw <- renewables_long |>
  pivot_wider(
    names_from  = country,
    values_from = renewables_share_elec
  ) |>
  rename(Year = year) |>
  # Column order: Year first, then countries roughly by 2023 share descending
  select(Year, Norway, Denmark, Australia, China, India, Poland)

### |- inspect final wide table ----
print(renewables_dw, n = 25)


## 5. EXPORT FOR DATAWRAPPER ----

write_csv(
  renewables_dw,
  here("2026/data/dcc_2026_day22_renewables_long.csv")
)

message("Export complete: dcc_2026_day22_renewables_long.csv")
message(glue::glue("Rows exported: {nrow(renewables_dw)}"))


## ---- DATAWRAPPER SETUP NOTES ------------------------------------------------
##
## 1. Upload CSV
##    Dashboard > New Chart > Upload / paste CSV
##    File: dcc_2026_day22_renewables_long.csv
##
## 2. Chart type
##    Select: Lines (Multiple Lines if DW offers it)
##    X-axis = Year (auto-detected as numeric/date)
##    Each country column = one line
##
## 3. Refine tab — key settings
##    - Line labels: "Right" — shows country name at end of each line
##    - No legend needed if end-labels are on
##    - Y-axis: start at 0, max ~100 (or let DW auto-scale)
##    - X-axis: 2000 to 2023
##    - Consider: bold Denmark line to draw the eye to the transition star
##
## 4. Annotate tab
##    - Title: "Some countries surged. Others barely moved."
##      or: "The uneven race to renewables"
##    - Subtitle: "Renewable share of electricity, 2000–2023 (selected countries)"
##    - Source: Our World in Data / Ember
##    - Byline: Steven Ponce | #30DayChartChallenge 2026
##    - Consider: one annotation callout on Denmark's acceleration ~2008–2020
##
## 5. Publish > Export PNG for thumbnail
##    Save as: thumbnails/tt_30dcc_2026_22.png
##
## 6. Embed in .qmd
##    Copy the responsive iframe code from Publish tab
##    Paste inside a ```{=html} block in your Quarto file
##
## -----------------------------------------------------------------------------