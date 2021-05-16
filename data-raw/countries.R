## code to prepare `countries` dataset goes here
# https://gist.githubusercontent.com/marijn/396531/raw/188caa065e3cd319fed7913ee3eecf5eec541918/countries.txt
missing_countries <- tibble::tribble(
  ~country, "Vietnam", "Laos", "Iran", "Oman, Sultanate Of",
  "South Korea", "North Korea", "Taiwan", "Kazakhstan",
  "Syria", "Palestine", "Libya", "Tanzania"
) %>%
  dplyr::mutate(country = stringr::str_to_lower(country))

countries <- readr::read_delim("data-raw/countries.txt", "|",
                               escape_double = FALSE,
                               col_names = FALSE, trim_ws = TRUE) %>%
  dplyr::rename(code = X1, country = X2) %>%
  dplyr::select(country) %>%
  dplyr::mutate(country = stringr::str_to_lower(country)) %>%
  dplyr::bind_rows(missing_countries)


usethis::use_data(countries, overwrite = TRUE, internal = TRUE)
