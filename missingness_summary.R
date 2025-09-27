library(readxl)
library(dplyr)
library(ggplot2)

### Calculate totals per state ###
# find pathways to local files
file_pathways <- list.files("/Users/brandonlieu/Desktop/sheriff_data_check", full.names = TRUE)

# read in list of state spreadsheets
state_sheets <- lapply(file_pathways, read_excel, sheet = 2) # assumes that data is on second tab; 
names(state_sheets) <- basename(file_pathways)               # Brandon combined Angie/Kendall/Sophie data onto tab 2 of local files

# for each state spreadsheet, rename col given row 1 is col names
state_sheets <- lapply(state_sheets, function(state_data) {
  # grab first row and name columns given first row, then remove that first row (the col names)
  new_names <- as.character(unlist(state_data[1, ]))
  colnames(state_data) <- new_names
  state_data <- state_data[-1, ]
})

# for each state spreadsheet, calculate # of candidates and missing data for each descriptive variable
state_summaries <- lapply(state_sheets, function(state_data) {
  
  state_summary <- state_data %>%
    filter(!is.na(`Candidate(winner/runner-up)_Firstname`) & `Candidate(winner/runner-up)_Firstname`!= "") %>% # assumes that all candidates have at least first name
    summarise(
      
      # raw counts
      n_candidates          = n(),
      n_missing_votes       = sum(is.na(Percent_Votes) | Percent_Votes == "", na.rm = TRUE),
      n_missing_gender      = sum(is.na(`Picture-identified Gender`) | `Picture-identified Gender` == "", na.rm = TRUE),
      n_missing_pronoun     = sum(is.na(Pronoun) | Pronoun == "" , na.rm = TRUE),
      n_missing_ethnicity   = sum(is.na(Ethnicity) | Ethnicity == "", na.rm = TRUE),
      n_missing_political   = sum(is.na(Political_Affiliation) | Political_Affiliation == "", na.rm = TRUE),
      n_missing_race        = sum(!(as.logical(`Race: White`) |
                                      as.logical(`Race: Black or African American`) |
                                      as.logical(`Race: American Indian or Alaska Native`) |
                                      as.logical(`Race: Asian`) |
                                      as.logical(`Race: Native Hawaiian or Other Pacific Islander`)), na.rm = TRUE),
      n_missing_birthyear   = sum(is.na(Birth_Year) | Birth_Year == "", na.rm = TRUE),
      
      # election counts
      n_elections           = sum(as.numeric(Candidate_Index) == 1, na.rm = TRUE),
      n_contested_elections = sum(as.numeric(Candidate_Index) == 2, na.rm = TRUE),
      
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
})

### Calculate totals across ALL states ###

desc_vars <- c("n_candidates", "n_elections", "n_contested_elections", "n_missing_votes", "n_missing_gender", "n_missing_pronoun", "n_missing_ethnicity", 
          "n_missing_political", "n_missing_race", "n_missing_birthyear")

grand_totals_df <- bind_rows(state_summaries) %>%
  summarise(across(all_of(desc_vars), sum, na.rm = TRUE))

# Calculate proportions of missingness with grand totals across all states
vars_prop <- c("n_missing_votes", "n_missing_gender", "n_missing_pronoun", "n_missing_ethnicity",
               "n_missing_political", "n_missing_race", "n_missing_birthyear")
grand_total_prop_df <- grand_totals_df[vars_prop] / grand_totals_df[["n_candidates"]]

### Visualization of missing data for entire data set ###
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

