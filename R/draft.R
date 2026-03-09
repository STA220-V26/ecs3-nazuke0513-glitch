library(tidyverse)
library(data.table)
library(janitor)
#patient data
if (!fs::file_exists("data.zip")) {
  curl::curl_download(
    "https://github.com/eribul/cs/raw/refs/heads/main/data.zip",
    "data.zip",
    quiet = FALSE
  )
}

patients <- readr::read_csv(unz("data.zip", "data-fixed/patients.csv"))
#Convert the data to data.table format for easier and faster subsequent processing.
setDT(patients)
#Setting 'id' as the primary key improves the speed of searching and merging data.
setkey(patients, id)
#Preliminary data cleaning
patients <- janitor::remove_empty(patients, quiet = FALSE)
patients <- janitor::remove_constant(patients, quiet = FALSE)
#3 Expectations
library(pointblank)
checks <- patients |>
create_agent(label = "Patient Data Validation Report") 
checks <- patients |>
  create_agent(label = "Patient Data Validation Report") |>
  
  # Check 1: The date must be between 1900 and today.
  col_vals_between(
    columns = where(is.Date),
    left = as.Date("1900-01-01"),
    right = as.Date(Sys.Date()),
    na_pass = TRUE,
    label = "Check if dates are within reasonable limits (1900 to today)"
  ) |>
  
  # Check 2: The date of death must be after the date of birth.
  col_vals_gte(
    columns = deathdate,
    value = vars(birthdate),
    na_pass = TRUE,
    label = "Death date must be after birth date"
  ) |>
  
  # Check 3：SSN format
  col_vals_regex(
    columns = ssn,
    regex = "^[0-9]{3}-[0-9]{2}-[0-9]{4}$",
    label = "SSN must match standard US format (XXX-XX-XXXX)"
  ) |>
  
  # Check 4: ID is an integer
  col_is_integer(
    columns = id,
    label = "ID column must be integer type"
  ) |>

  # Additional check 1: Marital status
 col_vals_in_set(
    columns = marital,
    set = c("S", "M", "D", "W"),
    label = "Marital status should be S, M, D, or W"
  ) |>
  
  # Additional check 2: Gender
 col_vals_in_set(
    columns = gender,
    set = c("M", "F"),
    label = "Gender should be M or F"
  ) |>
  
  # Additional check 3: ID uniqueness
col_vals_not_null(
    columns = id,
    label = "Patient IDs must not be missing"
  ) |>
  
 
  interrogate()

# HTML report
export_report(checks, "patient_validation.html")


# 3.1 (Factors)

# marital changes
patients[, marital := factor(
  marital,
  levels = c("S", "M", "D", "W"),
  labels = c("Single", "Married", "Divorced", "Widowed")
)]


# 4. Derived variables

# Age
patients[, age := as.integer((as.IDate(Sys.Date()) - birthdate) / 365.2425)]


# 5. Names

# NA change 
patients[, names(.SD) := lapply(.SD, \(x) replace_na(x, "")), .SDcols = c("prefix", "middle", "suffix")]

# full_name 
patients[, full_name := paste(
  prefix,
  first,
  middle,
  last,
  fifelse(suffix != "", paste0(", ", suffix), "")
)]

patients[, full_name := stringr::str_squish(full_name)]

patients[, c("prefix", "first", "middle", "last", "suffix", "maiden") := NULL]