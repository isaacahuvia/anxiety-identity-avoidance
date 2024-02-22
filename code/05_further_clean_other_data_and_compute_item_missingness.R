# ---------------------------------------------------------------------------- #
# Further Clean Other Data
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

# Load custom functions

source("./code/01a_define_functions.R")

# Check correct R version, load groundhog package, and specify groundhog_day

groundhog_day <- version_control()

# No packages loaded

# ---------------------------------------------------------------------------- #
# Import data ----
# ---------------------------------------------------------------------------- #

load("./data/intermediate_clean_further/dat2.RData")

# ---------------------------------------------------------------------------- #
# Restrict tables if needed ----
# ---------------------------------------------------------------------------- #

# Tables for substantive analysis:
#   - "anxiety_identity" ("anxiety_identity" column)
#   - "oa" ("axf", "axs", "avo" columns; multiple entries)
#   - "mechanisms" ("comp_act_willing" column)
#   - "mental_health_history" ("anxiety_duration" column)
# Tables for baseline characteristics:
#   - "dass21_as", "demographics", "demographics_race"
# Support tables
#   - "participant", "study", "task_log"

# No restriction needed

dat3 <- dat2

# ---------------------------------------------------------------------------- #
# Define scale items ----
# ---------------------------------------------------------------------------- #

# Define items for OASIS 

oa_items <- c("axf", "axs", "avo", "wrk", "soc")

length(oa_items) == 5

# Define items for anxiety symptom severity (mean of frequency and severity items)

oa_sx_sev_items <- c("axf", "axs")

length(oa_sx_sev_items) == 2

# Define items for DASS-21-AS

dass21_as_items <- c("bre", "dry", "hea", "pan", "sca", "tre", "wor")
dass21_as_mean_items <- paste0(dass21_as_items, "_mean")

length(dass21_as_items) == 7

# ---------------------------------------------------------------------------- #
# Handle repeated screenings ----
# ---------------------------------------------------------------------------- #

# Centralized Calm Thinking data cleaning revealed that "dass21_as" table has 
# repeated attempts at screening for some participants. Participants with more 
# than two unique sets of item responses will be excluded from analysis, and 
# "dass21_as_total_anal" (i.e., total score based on row mean of available column 
# means of available multiple item responses) will serve as basis for analysis.

# View(dat2$dass21_as[!is.na(dat2$dass21_as$n_eligibility_rows) &
#                              dat2$dass21_as$n_eligibility_rows > 1, ])

# Remove attempt-specific columns except "X", "id", and date-related columns

dass21_as_exclude_cols <- c("session_and_eligibility_status", "time_on_page",
                            "over18", "dass21_as_total", "dass21_as_total_interp",
                            "dass21_as_eligible")

dat3$dass21_as <- dat3$dass21_as[, !(names(dat3$dass21_as) %in% dass21_as_exclude_cols)]

# Temporarily remove original items

tmp_dass21_as <- dat3$dass21_as[, !(names(dat3$dass21_as) %in% dass21_as_items)]

# For duplicated rows on "participant_id" and "session_only" when excluding the
# original items above, keep only last row

nrow(tmp_dass21_as) == nrow(dat3$dass21_as)

dat3$dass21_as <- 
  dat3$dass21_as[!duplicated(tmp_dass21_as[, c("participant_id", "session_only")], 
                             fromLast = TRUE), ]

# Recode original items as NA at "Eligibility" given that items with "_mean"
# appended are the ones that comprise "dass21_as_total_anal" at that time point

dat3$dass21_as[dat3$dass21_as$session_only == "Eligibility", dass21_as_items] <- NA

# ---------------------------------------------------------------------------- #
# Handle unexpected multiple entries ----
# ---------------------------------------------------------------------------- #

# Centralized Calm Thinking data cleaning revealed that "oa" table has unexpected
# multiple entries but identical item responses (just different "time_on_page", so 
# use "time_on_page_mean" for analysis) and that "task_log" table has unexpected 
# multiple entries (some reflecting repeat screenings for "dass21_as" table; so use 
# "time_on_task_mean" for analysis). Centralized cleaning did not find unexpected
# multiple entries for the other tables analyzed in the present project.

# View(dat2$oa[dat2$oa$n_rows > 1, ])
# View(dat2$task_log[dat2$task_log$n_rows > 1, ])

# Remove attempt-specific columns except "X", "id", and date-related columns from
# "oa" table

dat3$oa$time_on_page <- NULL

# For duplicated rows on "participant_id" and "session_only" for "oa" table, keep 
# only last row. Leave "task_log" as it is.

dat3$oa <-    dat3$oa[!duplicated(dat3$oa[, c("participant_id", "session_only")], 
                                  fromLast = TRUE), ]

# ---------------------------------------------------------------------------- #
# Recode "prefer not to answer" values ----
# ---------------------------------------------------------------------------- #

# Check for substantive analysis variables. None already have NA values.

sum(is.na(dat3$oa[, oa_items])) == 0

sum(is.na(dat3$anxiety_identity$anxiety_identity)) == 0
sum(is.na(dat3$mechanisms$comp_act_willing)) == 0
sum(is.na(dat3$mental_health_history$anxiety_duration)) == 0

sum(dat3$oa[, oa_items] == 555, na.rm = TRUE)

sum(dat3$anxiety_identity$anxiety_identity == 555, na.rm = TRUE)
sum(dat3$mechanisms$comp_act_willing == 555, na.rm = TRUE)
sum(dat3$mental_health_history$anxiety_duration == 555, na.rm = TRUE)

dat3$oa[, oa_items][dat3$oa[, oa_items] == 555] <- NA

dat3$anxiety_identity$anxiety_identity[dat3$anxiety_identity$anxiety_identity == 555] <- NA
dat3$mechanisms$comp_act_willing[dat3$mechanisms$comp_act_willing == 555] <- NA
dat3$mental_health_history$anxiety_duration[dat3$mental_health_history$anxiety_duration == 555] <- NA

# Check for DASS-21-AS. "dass21_as" already has NA values. At "Eligibility", 555 
# ("prefer not to answer") was already recoded to NA for "_mean" items, and original 
# items were recoded as NA. For "dass21_as" at other time points, "_mean" items are 
# NA (as they were computed only at "Eligibility").

sum(is.na(dat3$dass21_as[dat3$dass21_as$session_only != "Eligibility", dass21_as_items]))
sum(is.na(dat3$dass21_as[dat3$dass21_as$session_only == "Eligibility", dass21_as_mean_items]))

sum(dat3$dass21_as[, dass21_as_items] == 555, na.rm = TRUE)
sum(dat3$dass21_as[, dass21_as_mean_items] == 555, na.rm = TRUE)

dat3$dass21_as[, dass21_as_items][dat3$dass21_as[, dass21_as_items] == 555] <- NA

# ---------------------------------------------------------------------------- #
# Check response ranges ----
# ---------------------------------------------------------------------------- #

# Check for substantive analysis variables

all(sort(unique(as.vector(as.matrix(dat3$oa[, oa_items])))) %in% 0:4)

all(sort(unique(dat3$anxiety_identity$anxiety_identity)) %in% 0:4)
all(sort(unique(dat3$mechanisms$comp_act_willing)) %in% 1:7)
all(sort(unique(dat3$mental_health_history$anxiety_duration)) %in% 0:8)

# Check for DASS-21-AS

all(sort(unique(as.vector(as.matrix(dat3$dass21_as[, dass21_as_items])))) %in% 0:3)

dass21_as_mean_range <- 
  range(sort(unique(as.vector(as.matrix(dat3$dass21_as[, dass21_as_mean_items])))))
all(dass21_as_mean_range >= 0 & dass21_as_mean_range <= 3)

# ---------------------------------------------------------------------------- #
# Reverse-code anxiety identity ----
# ---------------------------------------------------------------------------- #

dat3$anxiety_identity$anxiety_identity_rev <-
  abs(dat3$anxiety_identity$anxiety_identity - 4)

# ---------------------------------------------------------------------------- #
# Compute average item scores ----
# ---------------------------------------------------------------------------- #

# Compute anxiety symptom severity

dat3$oa$oa_sx_sev_m <- rowMeans(dat3$oa[, oa_sx_sev_items], na.rm = TRUE)

# Recode "dass21_as_total_anal" (total score) into average item score

length(dass21_as_items) == 7
dat3$dass21_as$dass21_as_m <- dat3$dass21_as$dass21_as_total_anal / 7

# Change NaN values (occur when all items are NA) to NA. This was already done
# for "dass21_as_total_anal" used to compute "dass21_as_m".

dat3$oa$oa_sx_sev_m[is.nan(dat3$oa$oa_sx_sev_m)] <- NA

sum(is.nan(dat3$dass21_as$dass21_as_m)) == 0

# ---------------------------------------------------------------------------- #
# Collapse "Eligibility" and "preTest" into "baseline" ----
# ---------------------------------------------------------------------------- #

# Do this for longitudinal tables (except "task_log")

target_longit_dfs <- c("anxiety_identity", "oa", "mechanisms", "dass21_as")

for (i in 1:length(dat3)) {
  if (names(dat3)[i] %in% target_longit_dfs) {
    dat3[[i]][, "session_only_col"] <- dat3[[i]][, "session_only"]
    dat3[[i]][, "session_only_col"][dat3[[i]][, "session_only_col"] %in% 
                                      c("Eligibility", "preTest")] <- "baseline"
  }
}

# ---------------------------------------------------------------------------- #
# Compute rates of item-level missingness at baseline for ITT participants ----
# ---------------------------------------------------------------------------- #

# Restrict to ITT participants

itt_ids <- dat3$participant$participant_id[dat3$participant$itt_anlys == 1]

dat3_itt <- lapply(dat3, function(x) x[x$participant_id %in% itt_ids, ])

# Define function to compute percentage of scale scores computed with at least 
# one item missing for given outcome at baseline

compute_some_item_missingness_bl <- function(dat, outcome, items) {
  dat <- dat[dat$session_only_col == "baseline", ]
  
  denom <- sum(!is.na(dat[, outcome]))
  
  rows_at_least_one_item_na <- rowSums(is.na(dat[, items])) > 0
  rows_all_items_na <- rowSums(!is.na(dat[, items])) == 0
  
  numer <- nrow(dat[rows_at_least_one_item_na & !rows_all_items_na, ])
  
  prop <- numer/denom
  percent <- prop*100
  
  cat(outcome, ": ", percent, "%", "\n", sep = "")
}

# Run function for ITT participants and write results

missing_rates_path <- "./results/missing_rates/"
dir.create(missing_rates_path)

sink(file = paste0(missing_rates_path, "some_item_missingness.txt"))

cat("Percentages of Scale Scores at Baseline Computed With At Least One Item Missing:", "\n\n")

compute_some_item_missingness_bl(dat3_itt$oa,        "oa_sx_sev_m", oa_sx_sev_items)
compute_some_item_missingness_bl(dat3_itt$dass21_as, "dass21_as_m", dass21_as_items)

sink()

# Define function to compute number of scale scores missing due to endorsements
# of "prefer not to answer" for all items at baseline

compute_all_item_missingness_bl <- function(dat, outcome, items) {
  dat <- dat[dat$session_only_col == "baseline", ]
  
  rows_all_items_na <- rowSums(!is.na(dat[, items])) == 0
  num_rows_all_items_na <- sum(rows_all_items_na)

  num_missing_outcome <- sum(is.na(dat[, outcome]))
  
  if (num_rows_all_items_na != num_missing_outcome) {
    warning(paste0("For ", outcome, ", ",
                   "number of rows with NA for all items does not equal ",
                   "number of scale scores with values of NA.", "\n"))
  }
  
  cat(outcome, ": ", num_rows_all_items_na, "\n", sep = "")
  
  print(table(dat[rows_all_items_na, "session_only"]))
  cat("\n", "-----", "\n\n")
}

# Run function for ITT participants and write results

sink(file = paste0(missing_rates_path, "all_item_missingness.txt"))

cat("Number of Scale Scores at Baseline Missing Due to 'Prefer Not to Answer' for All Items:", "\n\n")

compute_all_item_missingness_bl(dat3_itt$oa,        "oa_sx_sev_m", oa_sx_sev_items)
compute_all_item_missingness_bl(dat3_itt$dass21_as, "dass21_as_m", c(dass21_as_items, 
                                                                     dass21_as_mean_items))
sink()

# ---------------------------------------------------------------------------- #
# Restrict to baseline ----
# ---------------------------------------------------------------------------- #

# Do this for longitudinal tables above (except "task_log")

for (i in 1:length(dat3)) {
  if (names(dat3)[i] %in% target_longit_dfs) {
    dat3[[i]] <- dat3[[i]][dat3[[i]][, "session_only_col"] == "baseline", ]
  }
}

# ---------------------------------------------------------------------------- #
# Create analysis data frame ----
# ---------------------------------------------------------------------------- #

# Create template data frame

anlys_df <- dat3$participant[, c("participant_id", "exclude_analysis", "itt_anlys")]

# Add variables relevant to substantive analysis

index_var <- "participant_id"

anlys_df <- merge(anlys_df,
                  dat3$anxiety_identity[, c(index_var, "anxiety_identity_rev")],
                  by = index_var, all.x = TRUE)
anlys_df <- merge(anlys_df,
                  dat3$oa[, c(index_var, c("axf", "axs", "avo", "oa_sx_sev_m"))],
                  by = index_var, all.x = TRUE)
anlys_df <- merge(anlys_df,
                  dat3$mechanisms[, c(index_var, "comp_act_willing")],
                  by = index_var, all.x = TRUE)
anlys_df <- merge(anlys_df,
                  dat3$mental_health_history[, c(index_var, "anxiety_duration")],
                  by = index_var, all.x = TRUE)

# Restrict to ITT sample

anlys_df <- anlys_df[anlys_df$itt_anlys == 1, ]

# Sort table

anlys_df <- anlys_df[order(anlys_df$participant_id), ]

# ---------------------------------------------------------------------------- #
# Standardize continuous predictors ----
# ---------------------------------------------------------------------------- #

# Standardize anxiety symptom severity

anlys_df$oa_sx_sev_m_std <-
  (anlys_df$oa_sx_sev_m - mean(anlys_df$oa_sx_sev_m, na.rm = TRUE)) / 
  sd(anlys_df$oa_sx_sev_m, na.rm = TRUE)

# ---------------------------------------------------------------------------- #
# Export data ----
# ---------------------------------------------------------------------------- #

dir.create("./data/final_clean")

save(anlys_df, file = "./data/final_clean/anlys_df.RData")