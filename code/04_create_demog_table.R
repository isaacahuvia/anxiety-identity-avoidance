# ---------------------------------------------------------------------------- #
# Create Demographics Table
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

# Load packages

pkgs <- c("flextable", "officer", "ftExtra")
groundhog.library(pkgs, groundhog_day)

# Set "flextable" package defaults and load "officer" package properties

source("./code/01b_set_flextable_defaults.R")
source("./code/01c_set_officer_properties.R")

# ---------------------------------------------------------------------------- #
# Import data ----
# ---------------------------------------------------------------------------- #

load("./data/intermediate_clean_further/dat2.RData")

# ---------------------------------------------------------------------------- #
# Create demographics table ----
# ---------------------------------------------------------------------------- #

dem_tbl <- dat2$demographics

# Add analysis sample indicators

dem_tbl <- merge(dem_tbl,
                 dat2$participant[, c("participant_id", "exclude_analysis",
                                      "itt_anlys")],
                 by = "participant_id", all.x = TRUE)

# Restrict to ITT sample

dem_tbl_itt <- dem_tbl[dem_tbl$itt_anlys == 1, ]

# Define function to compute descriptives

compute_desc <- function(df) {
  # Compute sample size
  
  n <- data.frame(label = "n",
                  value = length(df$participant_id))
  
  # Compute mean and standard deviation for numeric variables
  
  num_res <- rbind(data.frame(label = "Age",
                              value = NA),
                   data.frame(label = "Years: M (SD)",
                              value = paste0(format(round(mean(df$age, na.rm = TRUE), 2),
                                                    nsmall = 2, trim = TRUE), 
                                             " (",
                                             format(round(sd(df$age, na.rm = TRUE), 2),
                                                    nsmall = 2, trim = TRUE),
                                             ")")))
  
  # Compute count and percentage "prefer not to answer" for numeric variables
  
  num_res_pna <- data.frame(label = "Prefer not to answer: n (%)",
                            value = paste0(sum(is.na(df$age)),
                                           " (",
                                           format(round((sum(is.na(df$age))/length(df$age))*100, 1),
                                                  nsmall = 1, trim = TRUE),
                                           ")"))
  
  # Compute count and percentage for factor variables
  
  vars <- c("gender", "race_col", "ethnicity", "country_col", "education",
            "employment_stat", "income", "marital_stat")
  var_labels <- paste0(c("Gender", "Race", "Ethnicity", "Country", "Education",
                         "Employment Status", "Annual Income", "Marital Status"),
                       ": n (%)")
  
  fct_res <- data.frame()
  
  for (i in 1:length(vars)) {
    tbl <- table(df[, vars[i]])
    prop_tbl <- prop.table(tbl)*100
    
    tbl_res <- rbind(data.frame(label = var_labels[i],
                                value = NA),
                     data.frame(label = names(tbl),
                                value = paste0(as.numeric(tbl),
                                               " (", 
                                               format(round(as.numeric(prop_tbl), 1),
                                                      nsmall = 1, trim = TRUE),
                                               ")")))
    fct_res <- rbind(fct_res, tbl_res)
  }
  
  # Combine results
  
  res <- rbind(n, num_res, num_res_pna, fct_res)
  
  return(res)
}

# Compute descriptives across conditions for ITT sample

res_itt_across_cond <- compute_desc(dem_tbl_itt)

# Save table to CSV

dem_path <- "./results/demographics/"

dir.create(dem_path, recursive = TRUE)

write.csv(res_itt_across_cond,
          paste0(dem_path, "itt_across_cond.csv"), row.names = FALSE)

# ---------------------------------------------------------------------------- #
# Format demographics table ----
# ---------------------------------------------------------------------------- #

# "flextable" defaults are set in "set_flextable_defaults.R" above

# Section and text properties are sourced from "set_officer_properties.R" above

# Define function to format demographics table

format_dem_tbl <- function(dem_tbl, footnotes, title) {
  # Format "label" column using Markdown
  
  dem_tbl$label_md <- dem_tbl$label
  
  rows_no_indent <- dem_tbl$label_md == "n" | 
    grepl("\\b(Age|Gender|Race|Ethnicity|Country|Education|Employment|Annual|Marital)\\b",
          dem_tbl$label_md)
  rows_indent <- !rows_no_indent
  
  indent_spaces <- "\\ \\ \\ \\ \\ "
  
  dem_tbl$label_md[rows_indent] <- paste0(indent_spaces, dem_tbl$label_md[rows_indent])
  
  dem_tbl$label_md[dem_tbl$label_md == "n"] <- "*n*"
  dem_tbl$label_md <- gsub("n \\(%\\)", "*n* \\(%\\)", dem_tbl$label_md)
  
  dem_tbl$label_md <- gsub("M \\(SD\\)", "*M* \\(*SD*\\)", dem_tbl$label_md)
  
  dem_tbl <- dem_tbl[c("label_md", names(dem_tbl)[names(dem_tbl) != "label_md"])]
  
  # Identify rows for footnotes
  
  transgender_rows <- c("Transgender", "Transgender Female", "Transgender Male")
  transgender_rows_idx <- which(dem_tbl$label %in% transgender_rows)
  
  country_other_row_idx <- 33
  if (dem_tbl$label[country_other_row_idx] != "Other") {
    stop("Row index for 'country' value of 'Other' is incorrect. Update 'country_other_row_idx'.")
  }
  
  # Define columns
  
  left_align_body_cols <- "label_md"
  target_cols <- names(dem_tbl)[names(dem_tbl) != "label"]
  
  # Create flextable
  
  dem_tbl_ft <- flextable(dem_tbl[, target_cols]) |>
    set_table_properties(align = "left") |>
    
    set_caption(as_paragraph(as_i(title)), word_stylename = "heading 1",
                fp_p = fp_par(padding.left = 0, padding.right = 0),
                align_with_table = FALSE) |>
    
    align(align = "center", part = "header") |>
    align(align = "center", part = "body") |>
    align(j = left_align_body_cols, align = "left", part = "body") |>
    align(align = "left", part = "footer") |>
    
    valign(valign = "bottom", part = "header") |>
    
    set_header_labels(label_md = "Characteristic",
                      value    = "Value") |>

    colformat_md(j = "label_md", part = "body") |>
    
    # add_footer_lines(gen_note) |>     # General note not used
  
    footnote(i = transgender_rows_idx,
             j = 1,
             value = as_paragraph_md(footnotes$transgender),
             ref_symbols = " a",
             part = "body") |>
    footnote(i = country_other_row_idx,
             j = 1,
             value = as_paragraph_md(footnotes$country_other),
             ref_symbols = " b",
             part = "body") |>

    autofit()
}

# Define table notes

# gen_note <- as_paragraph_md("*Note.* ")     # General note not used

footnotes <- list(transgender = "\\ Partway through data collection (on 8/5/2019), Transgender was replaced by Transgender Female and Transgender Male.",
                  country_other = "\\ Countries with fewer than 10 participants in the Stage 1 randomized sample (*n* = 1,614) were collapsed into Other.")

# Run function

dem_tbl_itt_ft <-
  format_dem_tbl(res_itt_across_cond, footnotes, "Demographic Characteristics")

# ---------------------------------------------------------------------------- #
# Write demographics tables to MS Word ----
# ---------------------------------------------------------------------------- #

# Write demographics table (note: "flextable" seems to have a bug in which blank 
# page is at end of doc)

tbls <- list(dem_tbl_itt_ft)

tbl_orientations <- c("p")
tbl_numbers      <- c("S1")

doc <- read_docx()
doc <- body_set_default_section(doc, psect_prop)

for (i in 1:length(tbls)) {
  doc <- body_add_fpar(doc, fpar(ftext(paste0("Table ", tbl_numbers[[i]]),
                                       prop = text_prop_bold)))
  doc <- body_add_par(doc, "")
  
  doc <- body_add_flextable(doc, tbls[[i]], align = "left")
  
  if (tbl_orientations[[i]] == "p") {
    doc <- body_end_block_section(doc, block_section(psect_prop))
  } else if (tbl_orientations[[i]] == "l") {
    doc <- body_end_block_section(doc, block_section(lsect_prop))
  }
}

dem_tbl_path <- paste0(dem_path, "table/")

dir.create(dem_tbl_path)

print(doc, target = paste0(dem_tbl_path, "dem_tbl.docx"))