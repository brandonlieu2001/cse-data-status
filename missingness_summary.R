library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

# ----------------------------------------------------------Calculate number candidates and missingness per state spreadsheet ----------------------------------------------

# find pathways to local files
file_pathways <- list.files("/Users/brandonlieu/Desktop/sheriff_data_check", full.names = TRUE)

# read in list of state spreadsheets
state_sheets <- lapply(file_pathways, read_excel, sheet = 2) # assumes that data is on second tab; 
names(state_sheets) <- basename(file_pathways)               # Brandon combined TX: Angie/Kendall/Sophie data onto tab 2 of local files; also for CA between Brandon/Xuechao/Chris

# for each state spreadsheet, rename col given row 1 is col names
state_sheets <- lapply(state_sheets, function(state_data) {
  # grab first row and name columns given first row, then remove that first row (the col names)
  new_names <- as.character(unlist(state_data[1, ]))
  colnames(state_data) <- new_names
  state_data <- state_data[-1, ]
})

# for each state spreadsheet, calculate # of candidates and missing data for each descriptive variable
state_summaries <- lapply(state_sheets, function(state_data) {
  
  # Candidate-level filtering and missingness
  candidate_summary <- state_data %>%
    filter(
      !is.na(`Candidate(winner/runner-up)_Firstname`) & `Candidate(winner/runner-up)_Firstname` != "",
      !is.na(Candidate_Index)
    ) %>%
    summarise(
      n_candidates        = n(),
      n_missing_votes     = sum(is.na(Percent_Votes) | Percent_Votes == "", na.rm = TRUE),
      n_missing_gender    = sum(is.na(`Picture-identified Gender`) | `Picture-identified Gender` == "", na.rm = TRUE),
      n_missing_pronoun   = sum(is.na(Pronoun) | Pronoun == "", na.rm = TRUE),
      n_missing_ethnicity = sum(is.na(Ethnicity) | Ethnicity == "", na.rm = TRUE),
      n_missing_political = sum(is.na(Political_Affiliation) | Political_Affiliation == "", na.rm = TRUE),
      n_missing_race      = sum(!(as.logical(`Race: White`) |
                                    as.logical(`Race: Black or African American`) |
                                    as.logical(`Race: American Indian or Alaska Native`) |
                                    as.logical(`Race: Asian`) |
                                    as.logical(`Race: Native Hawaiian or Other Pacific Islander`)), na.rm = TRUE),
      n_missing_birthyear = sum(is.na(Birth_Year) | Birth_Year == "", na.rm = TRUE),
      
      # proportion of missing data (TRUE (1) == missing, FALSE (0) == found)
      prop_missing_votes       = mean(is.na(Percent_Votes) | Percent_Votes == "", na.rm = TRUE), # <- i.e. where n candidates identified, this fraction don't have votes associated w/ candidate
      prop_missing_gender      = mean(is.na(`Picture-identified Gender`) | `Picture-identified Gender` == "", na.rm = TRUE),
      prop_missing_pronoun     = mean(is.na(Pronoun) | Pronoun == "", na.rm = TRUE),
      prop_missing_ethnicity   = mean(is.na(Ethnicity) | Ethnicity == "", na.rm = TRUE),
      prop_missing_political   = mean((is.na(Political_Affiliation) | Political_Affiliation == ""), na.rm = TRUE),
      prop_missing_race        = mean(!(as.logical(`Race: White`) |
                                          as.logical(`Race: Black or African American`) |
                                          as.logical(`Race: American Indian or Alaska Native`) |
                                          as.logical(`Race: Asian`) |
                                          as.logical(`Race: Native Hawaiian or Other Pacific Islander`)), na.rm = TRUE),
      prop_missing_birthyear   = mean(is.na(Birth_Year) | Birth_Year == "", na.rm = TRUE)
    )
  
  # Election-level counts (using county-year as election ID)
  election_summary <- state_data %>%
    filter(
      !is.na(`Candidate(winner/runner-up)_Firstname`) & `Candidate(winner/runner-up)_Firstname` != "",
      !is.na(Candidate_Index)
    ) %>%
    group_by(County_Name, Election_Year) %>%
    summarise(n_candidates = n()) %>%
    ungroup() %>% # drop groups to sum at state-level, rather than county-level
    summarise(
      n_elections = n(),
      n_contested_elections = sum(n_candidates > 1),
      n_uncontested_elections = sum(n_candidates == 1)
    )
  
  # Combine both into one summary row
  bind_cols(candidate_summary, election_summary)
})

# ---------------------------------------------------------- Calculate total candidates/elections and missingness across ALL states using state_summaries -----------------------------------------------------------
desc_vars <- c("n_candidates", "n_elections", "n_contested_elections", "n_missing_votes", "n_missing_gender", "n_missing_pronoun", "n_missing_ethnicity", 
          "n_missing_political", "n_missing_race", "n_missing_birthyear")

grand_totals_df <- bind_rows(state_summaries) %>%
  summarise(across(all_of(desc_vars), sum, na.rm = TRUE)) # applies sum to all_of the descriptive vars described above in entire dataset (all states)

# Calculate proportions of missingness with grand totals across all states
vars_prop <- c("n_missing_votes", "n_missing_gender", "n_missing_pronoun", "n_missing_ethnicity",
               "n_missing_political", "n_missing_race", "n_missing_birthyear")
grand_total_prop_df <- grand_totals_df[vars_prop] / grand_totals_df[["n_candidates"]]

# ---------------------------------------------------------- Visualization of missing data for entire data set ----------------------------

grand_total_prop_df %>%
  pivot_longer(everything(), names_to = "variable", values_to = "prop") %>%
  ggplot(aes(x = prop*100, y = reorder(variable, prop))) +
  geom_col(fill = "black") +
  geom_text(aes(label = paste0(round(prop*100,1), "%")), hjust = -0.3) +
  labs(
    title = "Missing Data by Variable",
    subtitle = paste("n_candidates =", grand_totals_df[["n_candidates"]]),
    x = "% Missing", y = NULL
  ) +
  theme_minimal()

# ---------------------------------------------------------- Minority candidate calculation ----------------------------------------------------------
all_state_data <- bind_rows(state_sheets) %>% # combine all separate sheets onto one main sheet
  filter(!(is.na(Statefips))) %>% # remove blank rows (no state associated)
  filter(!is.na(`Candidate(winner/runner-up)_Firstname`) & `Candidate(winner/runner-up)_Firstname`!= "") %>%  # only include rows with candidates
  filter(!is.na(`Candidate_Index`))


# Add one new col that detects if the candidate is minority
all_state_data <- all_state_data %>% 
  mutate(minority_flag = as.numeric((`Race: White` == FALSE) &
    (`Race: Black or African American` == TRUE |
       `Race: American Indian or Alaska Native` == TRUE |
       `Race: Asian` == TRUE |
       `Race: Native Hawaiian or Other Pacific Islander` == TRUE))
    )

# Total number of contested elections with at least 1 minority candidate 
contest_elections_minority_summary <- all_state_data %>%
  mutate(Election_Year_num = as.numeric(Election_Year),
         contested_flag = if_else(as.numeric(Candidate_Index) == 2, 1, 0)) %>%
  group_by(State_Name, County_Name, Election_Year_num) %>%
  mutate(contested_flag = max(contested_flag), na.rm = TRUE) %>%   # Set all rows in county-year to 1 if any county-year has contested_flag == 1
  filter(contested_flag == 1) %>% # filter county-year only contested elections (n=413)
  summarise(
    n_minority = sum(minority_flag == 1, na.rm = TRUE), # given contested county-year, identify number of minorities involved
    n_contested_elections = 1,                             # each county-year = 1 contested election
    n_contested_elections_w_minority = as.integer(n_minority > 0) # given contested count-year, 1 = minority involved in contested, 0 otherwise
  ) %>%
  ungroup() %>% # disregard county-year grouping
  summarise( 
    total_minority = sum(n_minority), # total minorities involved in contested elections
    total_contested = sum(n_contested_elections), # total contested elections
    total_contested_w_minority = sum(n_contested_elections_w_minority) # total contested elections involving at least one minority
  )

