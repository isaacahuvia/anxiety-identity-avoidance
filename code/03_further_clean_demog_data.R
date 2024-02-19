# ---------------------------------------------------------------------------- #
# Further Clean Demographic Data
# Author: Jeremy W. Eberle
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Notes ----
# ---------------------------------------------------------------------------- #

# Before running script, restart R (CTRL+SHIFT+F10 on Windows) and set working 
# directory to parent folder

# ---------------------------------------------------------------------------- #
# Store working directory, check correct R version, load packages ----
# ---------------------------------------------------------------------------- #

# Store working directory

wd_dir <- getwd()

# Load custom functions

source("./code/01a_define_functions.R")

# Check correct R version, load groundhog package, and specify groundhog_day

groundhog_day <- version_control()

# No packages loaded

# ---------------------------------------------------------------------------- #
# Import data ----
# ---------------------------------------------------------------------------- #

load("./data/intermediate_clean_further/dat.RData")

# ---------------------------------------------------------------------------- #
# Recode "Prefer not to answer" in categorical and ordinal variables ----
# ---------------------------------------------------------------------------- #

pna <- "Prefer not to answer"

# Recode 555 as "Prefer not to answer". 555 in "birth_year" (integer variable)
# is recoded after "age" is computed below.

target_vars <- c("education", "employment_stat", "ethnicity", "gender", "income",
                 "marital_stat")
dat$demographics[, target_vars][dat$demographics[, target_vars] == 555] <- pna

dat$demographics_race$race[dat$demographics_race$race == 555] <- pna

# Recode "NoAnswer" as "Prefer not to answer"

dat$demographics$country[dat$demographics$country == "NoAnswer"] <- pna

# ---------------------------------------------------------------------------- #
# Compute age ----
# ---------------------------------------------------------------------------- #

# Compute age (use NA where "birth_year" is 555 ["prefer not to answer"])

dat$demographics$age <- NA

for (i in 1:nrow(dat$demographics)) {
  if (dat$demographics$birth_year[i] != 555) {
    dat$demographics$age[i] <- 
      as.integer(format(dat$demographics$date_as_POSIXct[i], "%Y")) - 
      dat$demographics$birth_year[i]
  }
}

# Participants range from 18 to 75 years of age, which is reasonable

range(dat$demographics$age, na.rm = TRUE) == c(18, 75)

# All NAs in "age" are due to "birth_year" of 555

all(dat$demographics$birth_year[is.na(dat$demographics$age)] == 555)

# Recode 555 in "birth_year" to "Prefer not to answer"

dat$demographics$birth_year[dat$demographics$birth_year == 555] <- pna

# ---------------------------------------------------------------------------- #
# Clean gender ----
# ---------------------------------------------------------------------------- #

# Reorder levels and add "Transgender Female". Codebook indicates that on 8/5/2019, 
# "Transgender" option was replaced with "Transgender Male" and "Transgender Female".

dat$demographics$gender <- 
  factor(dat$demographics$gender,
         levels = c("Female", "Male", 
                    "Transgender", "Transgender Female", "Transgender Male",
                    "Other", "Prefer not to answer"))

# ---------------------------------------------------------------------------- #
# Clean ethnicity ----
# ---------------------------------------------------------------------------- #

# Reorder levels

dat$demographics$ethnicity <- 
  factor(dat$demographics$ethnicity,
         levels = c("Hispanic or Latino", "Not Hispanic or Latino", 
                    "Unknown", "Prefer not to answer"))

# ---------------------------------------------------------------------------- #
# Clean education ----
# ---------------------------------------------------------------------------- #

# Reorder levels

dat$demographics$education <-
  factor(dat$demographics$education,
         levels = c("Junior High", "Some High School", "High School Graduate", 
                    "Some College", "Associate's Degree", "Bachelor's Degree",
                    "Some Graduate School", "Master's Degree", "M.B.A.", "J.D.", 
                    "M.D.", "Ph.D.", "Other Advanced Degree",
                    "Prefer not to answer"))

# ---------------------------------------------------------------------------- #
# Clean employment status ----
# ---------------------------------------------------------------------------- #

# Recode "Homemaker"

dat$demographics$employment_stat[dat$demographics$employment_stat ==
                                   "Homemaker/keeping house or raising children full-time"] <-
  "Homemaker"

# Reorder levels

dat$demographics$employment_stat <- 
  factor(dat$demographics$employment_stat,
         levels = c("Student", "Homemaker", "Unemployed or laid off", "Looking for work",
                    "Working part-time", "Working full-time", "Retired", "Other",
                    "Unknown", "Prefer not to answer"))

# ---------------------------------------------------------------------------- #
# Clean income ----
# ---------------------------------------------------------------------------- #

# Recode "Don't know"

dat$demographics$income[dat$demographics$income == "Don't know"] <- "Unknown"

# Reorder levels

dat$demographics$income <-
  factor(dat$demographics$income,
         levels = c("Less than $5,000", "$5,000 through $11,999", 
                    "$12,000 through $15,999", "$16,000 through $24,999", 
                    "$25,000 through $34,999", "$35,000 through $49,999",
                    "$50,000 through $74,999", "$75,000 through $99,999",
                    "$100,000 through $149,999", "$150,000 through $199,999",
                    "$200,000 through $249,999", "$250,000 or greater",
                    "Unknown", "Prefer not to answer"))

# ---------------------------------------------------------------------------- #
# Clean marital status ----
# ---------------------------------------------------------------------------- #

# Reorder levels

dat$demographics$marital_stat <-
  factor(dat$demographics$marital_stat,
         levels = c("Single", "Single,dating", "Single,engaged", "Single,marriagelike",
                    "Married", "civilunion", "Separated", "Divorced", "Widow/widower",
                    "Other", "Prefer not to answer"))

# ---------------------------------------------------------------------------- #
# Clean race ----
# ---------------------------------------------------------------------------- #

# No duplicated responses exist

nrow(dat$demographics_race[duplicated(dat$demographics_race[, c("participant_id", 
                                                                "race")]), ]) == 0

# Compute number of responses per "participant_id"

race_ag <- aggregate(race ~ participant_id,
                     dat$demographics_race,
                     length)
names(race_ag)[names(race_ag) == "race"] <- "race_cnt"

dat$demographics_race <- merge(dat$demographics_race, race_ag,
                               by = "participant_id", all.x = TRUE)

# Define "race_col", collapsing cases of more than once race

dat$demographics_race$race_col <- NA

for (i in 1:nrow(dat$demographics_race)) {
  if (dat$demographics_race$race_cnt[i] == 1) {
    dat$demographics_race$race_col[i] <- dat$demographics_race$race[i]
  } else if (dat$demographics_race$race_cnt[i] > 1) {
    dat$demographics_race$race_col[i] <- "More than one race"
  }
}

# Per centralized Calm Thinking data cleaning, participant 1992 completed only 
# "dass21_as" before Calm Thinking enrollment closed on 2020-04-06 23:59 EDT but 
# re-engaged with the program after the TET study launched on 2020-04-07 00:00 EDT 
# and got assigned to a TET condition. Thus, centralized cleaning changed their 
# condition, current session, and last session date to what they were in Calm 
# Thinking and removed all their data after the launch of TET. This participant's 
# data remains in the "demographics_race" table because "system_date_time_earliest"
# there is NA (see https://github.com/TeachmanLab/MT-Data-CalmThinkingStudy/issues/10).
# Thus, exclude this participant from "demographics_race" table.

dat$demographics_race <- dat$demographics_race[dat$demographics_race$participant_id != 1992, ]

# Add "race_col" to "demographics" table

dat$demographics <- merge(dat$demographics,
                          unique(dat$demographics_race[, c("participant_id", 
                                                           "race_col")]),
                          by = "participant_id",
                          all.x = TRUE)

# Reorder levels

dat$demographics$race_col <-
  factor(dat$demographics$race_col,
         levels = c("American Indian/Alaska Native", "Black/African origin",
                    "East Asian", "Native Hawaiian/Pacific Islander", "South Asian",
                    "White/European origin", "More than one race", "Other or Unknown", 
                    "Prefer not to answer"))

# ---------------------------------------------------------------------------- #
# Clean country ----
# ---------------------------------------------------------------------------- #

# Recode "Ã…land Islands"

dat$demographics$country[dat$demographics$country == "Ã…land Islands"] <- "Åland Islands"

# Define desired levels order (decreasing frequency ending with "prefer not to answer")

country_levels <- names(sort(table(dat$demographics$country), decreasing = TRUE))
country_levels <- c(country_levels[country_levels != pna], pna)

# Reorder levels of "country"

dat$demographics$country <- factor(dat$demographics$country, levels = country_levels)

# Define "country_col", collapsing countries with fewer than 10 participants into "Other"

top_countries <- 
  names(table(dat$demographics$country)[as.numeric(table(dat$demographics$country)) > 10])

all(top_countries == c("United States", "Australia", "United Kingdom", "Canada"))

dat$demographics$country_col <- NA

for (i in 1:nrow(dat$demographics)) {
  if (as.character(dat$demographics$country)[i] %in% top_countries) {
    dat$demographics$country_col[i] <- as.character(dat$demographics$country)[i]
  } else if (as.character(dat$demographics$country)[i] == pna) {
    dat$demographics$country_col[i] <- pna
  } else {
    dat$demographics$country_col[i] <- "Other"
  }
}

# Reorder levels of "country_col"

dat$demographics$country_col <-
  factor(dat$demographics$country_col,
         levels = c(top_countries, "Other", pna))

# ---------------------------------------------------------------------------- #
# Save cleaned data ----
# ---------------------------------------------------------------------------- #

dat2 <- dat

save(dat2, file = "./data/intermediate_clean_further/dat2.RData")