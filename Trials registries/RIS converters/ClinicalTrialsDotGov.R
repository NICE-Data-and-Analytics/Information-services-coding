library(tidyverse) 

# (1) Upload file to Posit Cloud in bottom-right corner
# (2) Change file name in speech marks on line 9 to match your file name
# (3) "Select all" (ctrl+A) in this window, click "run" (above)
# (4) Generated file is called clinicaltrialsris.txt. Select it in bottom-right window (check box on left)
# (5) Click More (cog icon) > Export. RIS file will land in your downloads folder.

df <- read_csv("menopause trials.csv")

# Basic method here is to combine relevant fields to make an abstract;
# create new columns for field tags then unite these with data from the CSV.

# Lines 16-31 are formatting data from CSV then combining fields to form an abstract. 

df <- df |>
  select(Title, Status, `Study Designs`, Conditions, Age, Enrollment, Interventions, `Outcome Measures`, URL, `First Posted`, `NCT Number`, `Primary Completion Date`, Locations) |>
  mutate(`Study Designs` = str_replace_all(`Study Designs`, "\\|", "\n\t"), 
         `Outcome Measures` = str_replace_all(`Outcome Measures`, "\\|", "\n\t"),
         Conditions = str_replace_all(Conditions, "\\|", "\n\t"),
         Interventions = str_replace_all(Interventions, "\\|", "\n\t"),
         UK = str_detect(Locations, "United Kingdom"),
         UK = case_when(UK == TRUE ~ "yes",
                        UK == FALSE ~ "no",
                        is.na(UK) ~ "not stated"),
         Age = str_replace_all(Age, "...\\(.*", ""),
         Status = paste0("THIS IS A TRIALS REGISTY RECORD. The trial status is currently recorded as ", str_to_lower(Status), ". This may not be reflective of the actual state of the trial. The completion date for the primary outcome is given as: ", `Primary Completion Date`, "\n"),
         `Study Designs` = paste0("STUDY DESIGN:\n\t", `Study Designs`, "\n\tNumber of participants: ", Enrollment, "\n\tAge: ", Age, "\n\tUK trial centre(s): ", UK, "\n"),
         Conditions = paste0("CONDITION(S):\n\t", Conditions, "\n"),
         Interventions = paste0("INTERVENTION(S):\n\t", Interventions, "\n"),
         `Outcome Measures` = paste0("OUTCOME(S):\n\t", `Outcome Measures`),
         
         # First Posted is a non-controlled string so have just
         # truncated to leave 4-digit year code rather than use lubridate. 
         # This field becomes the year in the EPPI record. Needs to be present and consistent for deduping purposes,
         # hence use of first posted date rather than completion date.
         
         `First Posted` = str_trunc(`First Posted`, 4, side = "left", ellipsis = ""),
         NCT2 = paste0(`NCT Number`, " - ")
  ) |>
  
  # Create new columns to represent RIS field tags
  
  mutate(TY = "TY  - DBASE", TI = "TI  - ", AB = "AB  - ", Y1 = "Y1  - ", 
         UR = "UR  - ", JF = "JF  - clinicaltrials.gov", SP = "SP  - 1", ER = "ER  - ", VL = "VL  - ") |>
  unite(TI, c(TI, NCT2, Title), sep = "") |>
  unite(AB, c(AB, Status, `Study Designs`, Conditions, Interventions, `Outcome Measures`), sep = "") |>
  unite(UR, c(UR, URL), sep = "") |>
  unite(Y1, c(Y1, `First Posted`), sep = "") |>
  unite(VL, c(VL, `NCT Number`), sep = "")

# Reordering columns for output. If I were reordering the rows that would go before the first comma.

df <- df[, c("TY", "TI", "Y1", "AB", "JF", "UR", "VL", "SP", "ER")]

# write.table used to create text files with new lines as separators and line ends.

write.table(df, file = "clinicaltrialsris.txt", quote = FALSE, sep = "\n", eol = "\n\n", col.names = FALSE, row.names = FALSE)
