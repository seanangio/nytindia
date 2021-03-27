## code to prepare `govt` dataset goes here
# how to remove nested
# how to get today's date to end it
# how to get first date
library(tibble)
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)

# originally from https://en.wikipedia.org/wiki/List_of_prime_ministers_of_India
# but may have changed
# copied into a spreadsheet and then copied with {datapasta}
wiki_governments <- tribble(
  ~pm,                            ~party,              ~from,                ~to,
  "Jawaharlal Nehru",        "Indian National Congress",   "15 August 1947",    "15 April 1952",
  "Jawaharlal Nehru",        "Indian National Congress",    "15 April 1952",    "17 April 1957",
  "Jawaharlal Nehru",        "Indian National Congress",    "17 April 1957",     "2 April 1962",
  "Jawaharlal Nehru",        "Indian National Congress",     "2 April 1962",      "27 May 1964",
  "Gulzarilal Nanda",        "Indian National Congress",      "27 May 1964",      "9 June 1964",
  "Lal Bahadur Shastri",        "Indian National Congress",      "9 June 1964",  "11 January 1966",
  "Gulzarilal Nanda",        "Indian National Congress",  "11 January 1966",  "24 January 1966",
  "Indira Gandhi",        "Indian National Congress",  "24 January 1966",     "4 March 1967",
  "Indira Gandhi",        "Indian National Congress",     "4 March 1967",    "15 March 1971",
  "Indira Gandhi",        "Indian National Congress",    "15 March 1971",    "24 March 1977",
  "Morarji Desai",                    "Janata Party",    "24 March 1977",     "28 July 1979",
  "Charan Singh", "Janata Party (Secular) with INC",     "28 July 1979",  "14 January 1980",
  "Indira Gandhi",    "Indian National Congress (I)",  "14 January 1980",  "31 October 1984",
  "Rajiv Gandhi",    "Indian National Congress (I)",  "31 October 1984", "31 December 1984",
  "Rajiv Gandhi",    "Indian National Congress (I)", "31 December 1984",  "2 December 1989",
  "Vishwanath Pratap Singh",     "Janata Dal (National Front)",  "2 December 1989", "10 November 1990",
  "Chandra Shekhar", "Samajwadi Janata Party with INC", "10 November 1990",     "21 June 1991",
  "P. V. Narasimha Rao",    "Indian National Congress (I)",     "21 June 1991",      "16 May 1996",
  "Atal Bihari Vajpayee",          "Bharatiya Janata Party",      "16 May 1996",      "1 June 1996",
  "H. D. Deve Gowda",       "Janata Dal (United Front)",      "1 June 1996",    "21 April 1997",
  "Inder Kumar Gujral",       "Janata Dal (United Front)",    "21 April 1997",    "19 March 1998",
  "Atal Bihari Vajpayee",    "Bharatiya Janata Party (NDA)",    "19 March 1998",  "10 October 1999",
  "Atal Bihari Vajpayee",    "Bharatiya Janata Party (NDA)",  "10 October 1999",      "22 May 2004",
  "Manmohan Singh",  "Indian National Congress (UPA)",      "22 May 2004",      "22 May 2009",
  "Manmohan Singh",  "Indian National Congress (UPA)",      "22 May 2009",      "26 May 2014",
  "Narendra Modi",    "Bharatiya Janata Party (NDA)",      "26 May 2014",      "30 May 2019",
  "Narendra Modi",    "Bharatiya Janata Party (NDA)",      "30 May 2019",      "Incumbent"
)

# works as long as current govt hasn't changed
latest_date <- format(Sys.Date(), format="%d %B %Y")

govt_table <- wiki_governments %>%
  mutate(
    to = if_else(to == "Incumbent", latest_date, to),
    from = dmy(from),
    # wikipedia convention is last day in one govt is first day in next
    # this overlapping creates a problem so subtract 1 day from last date
    to = dmy(to) - 1)

party_colors <- tribble(
  # mostly from wikipedia, then adapted to RColorBrewer's Paired
  ~color_party_abb,
  "#FF9933, Bharatiya_Janata_Party, BJP",
  "#FF9933, Bharatiya_Janata_Party_(NDA), BJP",
  "#a6cee3, Indian_National_Congress, INC",
  "#a6cee3, Indian_National_Congress_(I), INC (I)",
  "#a6cee3, Indian_National_Congress_(UPA), INC",
  "#33a02c, Janata_Dal, JD",
  "#33a02c, Janata_Dal_(United_Front), JD",
  "#33a02c, Janata_Dal_(National_Front), JD",
  "#138808, Janata_Dal_(Secular), JD (S)",
  "#1f78b4, Janata_Party, JP",
  "#CAB2D6, Janata_Party_(Secular) with INC, JP (S)",
  "#b2df8a, Samajwadi_Janata_Party_(Rashtriya), SJP (R)",
  "#b2df8a, Samajwadi Janata Party with INC, SJP (R)"
) %>%
  separate(color_party_abb,
           into = c("color", "party", "abb"),
           sep = ", ") %>%
  mutate(
    party = str_replace_all(party, "_", " ")
  )

govt <- govt_table %>%
  left_join(party_colors, by = "party") %>%
  # need a row to match start of nyt dataset
  add_row(pm = "Viceroy", party = "British Raj",
          from = as.Date("1855-01-01"), # earliest API date
          to = as.Date("1947-08-14"),
          color = "#fb9a99", abb = "GB") %>%
  mutate(govt_name = str_glue('{from} / {to}: {pm} ({party})')) %>%
  arrange(from)

usethis::use_data(govt, overwrite = TRUE)
