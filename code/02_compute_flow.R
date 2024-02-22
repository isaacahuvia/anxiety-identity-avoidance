# ---------------------------------------------------------------------------- #
# Compute Participant Flow
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
# Import intermediate clean data ----
# ---------------------------------------------------------------------------- #

# Obtain file names of selected intermediate clean CSV data files and output a
# warning if they do not contain all those relevant to present manuscript

int_cln_data_dir <- paste0(wd_dir, "/data/intermediate_clean")
filenames <- list.files(int_cln_data_dir, pattern = "\\.csv$", full.names = FALSE)

check_relevant_files(filenames)

# Import tables into list and name tables

dat <- lapply(paste0(int_cln_data_dir, "/", filenames), read.csv)
names(dat) <- sub(".csv", "", filenames)

# Convert system-generated timestamps to POSIXct data types

dat <- convert_POSIXct(dat)

# ---------------------------------------------------------------------------- #
# Note on filtering data ----
# ---------------------------------------------------------------------------- #

# Note: As noted in README for centralized data cleaning, end of data collection
# for Calm Thinking study was defined as 12/3/2020, but the last system-generated
# timestamp for a Calm Thinking participant was "2020-11-13 22:13:27 EST". We will
# follow the main outcomes paper and analyze data collected through 11/27/2020;
# thus, no filtering of data is needed.

# ---------------------------------------------------------------------------- #
# Compute participant flow ----
# ---------------------------------------------------------------------------- #

# Note: Numbers screened (3519), ineligible (for various reasons, 774 + 111 + 23), 
# eligible but not enrolled (2611), and enrolled (1748) were computed as part of
# centralized data cleaning. For ineligible participants with multiple screening
# attempts, their reason for ineligibility is based on their most recent attempt.

# Confirm number enrolled

enrolled_ids <- dat$participant$participant_id[!is.na(dat$participant$participant_id)]
length(enrolled_ids) == 1748

# Compute number Stage 1 randomized

stg1_ids <- dat$study$participant_id[dat$study$conditioning != "NONE"]
length(stg1_ids) == 1614

# Compute number enrolled but not Stage 1 randomized. These participants did not
# complete "demographic" table, whose data are required for Stage 1 randomization,
# which was stratified by gender and baseline anxiety symptom severity

length(enrolled_ids) - length(stg1_ids) == 134

nrow(dat$demographics[!(dat$demographics$participant_id %in% stg1_ids), ]) == 0

# Compute number who started S1 training

start_s1_train_ids <- dat$task_log[dat$task_log$session_only == "firstSession" &
                                     dat$task_log$task_name == "Affect" &
                                     dat$task_log$tag == "pre", "participant_id"]
length(start_s1_train_ids) == 1239

# Compute number who did not start S1 training

no_start_s1_ids <- setdiff(stg1_ids, start_s1_train_ids)
length(no_start_s1_ids) == 375

# ---------------------------------------------------------------------------- #
# Identify analysis exclusions ----
# ---------------------------------------------------------------------------- #

# Identify analysis exclusions due to having more than two unique sets of
# DASS-21-AS screening responses by condition (for more information, see
# https://github.com/TeachmanLab/MT-Data-CalmThinkingStudy#participant-flow-and-analysis-exclusions)

exclude_repeat_screen_ids <- 
  dat$participant$participant_id[dat$participant$exclude_analysis == 1]
length(exclude_repeat_screen_ids) == 6

#   Note: Participants 1453 and 1893 did not start Session 1 training so are 
#   already not ITT participants

setdiff(exclude_repeat_screen_ids, start_s1_train_ids) == c(1453, 1893)

# Identify analysis exclusions due to switching conditions, by condition 
# (for more information, see 
# https://github.com/TeachmanLab/MT-Data-CalmThinkingStudy#condition-switching)

exclude_switch_condition_ids <- 382

nrow(dat$study[dat$study$participant_id %in% exclude_switch_condition_ids &
                 dat$study$conditioning == "CONTROL", ]) == 1     # 382

#   Note: Participant 382 would otherwise be part of ITT sample if not excluded   

exclude_switch_condition_ids %in% start_s1_train_ids

# Collect analysis exclusions

exclude_ids <- c(exclude_repeat_screen_ids, exclude_switch_condition_ids)

# ---------------------------------------------------------------------------- #
# Define ITT analysis sample ----
# ---------------------------------------------------------------------------- #

# Identify intent-to-treat (ITT) analysis sample

itt_anlys_ids <- setdiff(start_s1_train_ids, exclude_ids)
length(itt_anlys_ids) == 1234

# Add indicator for ITT sample to "participant" table

dat$participant$itt_anlys <- 0
dat$participant[dat$participant$participant_id %in% itt_anlys_ids,
                "itt_anlys"] <- 1

# ---------------------------------------------------------------------------- #
# Export data ----
# ---------------------------------------------------------------------------- #

dir.create("./data/intermediate_clean_further")

save(dat, file = "./data/intermediate_clean_further/dat.RData")