library(tidyverse) 
library(readr)

df <- read_csv("ISRCTN_trials.csv")

#Basic method here is to combine relevant fields to make an abstract;
#create new columns for field tags then unite these with data from the CSV.

df2 <- df |>
  select(Title, `Overall study status`, `Primary study design`, Condition, 
         `Age group`, `Intervention type`, `Drug/device/biological/vaccine name(s)`, ISRCTN, Phase, 
         `Target number of participants`, `Total final enrolment`, `Country of recruitment`, `Overall study end date`, `ClinicalTrials.gov number`
) |>
  unite(`Intervention type`, `Intervention type`, `Drug/device/biological/vaccine name(s)`, sep = ": ", na.rm=TRUE) |>
  
  mutate(URL = ISRCTN)  |>
  mutate(URL = str_replace(URL,"^", "https://www.isrctn.com/"))|>
  filter(!grepl('NCT', `ClinicalTrials.gov number`))|>
  
    
  mutate(Status = paste0("THIS IS A TRIALS REGISTY RECORD", ". \n", 
        "STATUS: ", `Overall study status`, ". This may not be reflective of the actual state of the trial. \n"),
         `Primary study design` = paste0("STUDY DESIGN: ", `Primary study design`, ". \n"),
         Condition = paste0("CONDITION(S): ", Condition, ". \n"),
        `Age group` = paste0("AGE: ", `Age group`, ". \n"),
         `Intervention type` = paste0("INTERVENTION(S): ", `Intervention type`, ". \n"),
         Phase = paste0("PHASE: ", Phase, ". \n"),
         `Target number of participants`= paste0("EST PARTICIPANTS: ", `Target number of participants`, ". \n"),
         `Total final enrolment`= paste0("FINAL ENROLMENT: ", `Total final enrolment`, ". \n"),
         `Country of recruitment`= paste0("COUNTRY: ", `Country of recruitment`, ". \n"),
         `Overall study end date` = paste0("TRIAL END DATE: ", `Overall study end date`, ".")
  ) |>
  
  # Create new columns to represent RIS field tags
  
    mutate(TY = "TY  - DBASE", TI = "TI  - ", DO = "DO  - https://dx.doi.org/10.1186/", AB = "AB  - ", JF = "JF  - ISRCTN", UR = "UR  - ", ER = "ER  - ") |>
    unite(TI, c(TI, Title), sep = "") |>
    unite(AB, c(AB, Status, `Primary study design`, Condition, `Intervention type`, Phase, `Target number of participants`, `Total final enrolment`, `Country of recruitment`, `Overall study end date`), sep = "") |>
    unite(UR, c(UR, URL), sep = "")|>
    unite(DO, c(DO, ISRCTN), sep = "")
  
  #Reordering columns for output. If I were reordering the rows that would go before the comma.
  
  df2 <- df2[, c("TY", "TI", "DO", "AB", "JF", "UR", "ER")]
  
  #write.table can be used to create text files with new lines as separators and line ends.
  
  write.table(df2, file = "ISRCTNris.txt", quote = FALSE, sep = "\n", eol = "\n\n", col.names = FALSE, row.names = FALSE)
  
