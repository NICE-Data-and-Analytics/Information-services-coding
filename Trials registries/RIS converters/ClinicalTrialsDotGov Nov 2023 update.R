library(tidyverse) 

# (1) Upload file to Posit Cloud in bottom-right corner
# (2) Change file name in speech marks on line 9 to match your file name
# (3) "Select all" (ctrl+A) in this window, click "run" (above)
# (4) Generated file is called clinicaltrialsris.txt. Select it in bottom-right window (check box on left)
# (5) Click More (cog icon) > Export. RIS file will land in your downloads folder.

df <- read_csv("231114 clinical trials_gov MH child services CSV 1-9.csv", col_types = cols(`First Posted` = col_character()))


# Basic method here is to combine relevant fields to make an abstract; 

# create new columns for field tags then unite these with data from the CSV. 


df <- df |> 
  
  select(`Study Title`, `Study Status`, `Study Design`, Conditions, Age, Enrollment, Interventions, `Primary Outcome Measures`, `Study URL`, `First Posted`, `NCT Number`, `Primary Completion Date`, Locations) |> 
  
  mutate(`Study Design` = str_replace_all(`Study Design`, "\\|", "\n\t"),  
         
         `Primary Outcome Measures` = str_replace_all(`Primary Outcome Measures`, "\\|", "\n\t"), 
         
         Conditions = str_replace_all(Conditions, "\\|", "\n\t"), 
         
         Interventions = str_replace_all(Interventions, "\\|", "\n\t"), 
         
         UK = str_detect(Locations, "United Kingdom"), 
         
         UK = case_when(UK == TRUE ~ "yes", 
                        
                        UK == FALSE ~ "no", 
                        
                        is.na(UK) ~ "not stated"), 
         
         OECD = str_detect(Locations, "Australia|Austria|Belgium|Canada|Chile|Colombia|Costa Rica|Czech Republic|Czechia|Denmark|Estonia|Finland|France|Germany|Greece|Hungary|Iceland|Ireland|Israel|Italy|Japan|Korea|Latvia|Lithuania|Luxembourg|Mexico|Netherlands|New Zealand|Norway|Poland|Portugal|Slovak Republic|Slovakia|Slovenia|Spain|Sweden|Switzerland|Türkiye|Turkey|United Kingdom|United States"), 
         
         OECD = case_when(OECD == TRUE ~ "yes", 
                          
                          OECD == FALSE ~ "none", 
                          
                          is.na(OECD) ~ "not stated"), 
         
         Age = str_replace_all(Age, "...\\(.*", ""), 
         
         `Study Status` = paste0("THIS IS A TRIALS REGISTY RECORD. The trial status is currently recorded as ", str_to_lower(`Study Status`), ". This may not be reflective of the actual state of the trial. The completion date for the primary outcome is given as: ", `Primary Completion Date`, "\n"), 
         
         `Study Design` = paste0("STUDY DESIGN:\n\t", `Study Design`, "\n\tNumber of participants: ", Enrollment, "\n\tAge: ", Age, "\n\tUK trial centre(s): ", UK, "\n"), 
         
         Conditions = paste0("CONDITION(S):\n\t", Conditions, "\n"), 
         
         Interventions = paste0("INTERVENTION(S):\n\t", Interventions, "\n"), 
         
         `Primary Outcome Measures` = paste0("OUTCOME(S):\n\t", `Primary Outcome Measures`), 
         
         
         
         # First Posted is a non-controlled string so have just 
         
         # truncated to leave 4-digit year code rather than use lubridate.  
         
         # This field becomes the year in the EPPI record. Needs to be consistent for deduping, 
         
         # hence use of first posted date rather than completion date. 
         
         
         
         `First Posted` = str_trunc(`First Posted`, 4, side = "left", ellipsis = ""), 
         
         NCT2 = paste0(`NCT Number`, " - ") 
         
  ) |> 
  
  
  
  # Create new columns to represent RIS field tags 
  
  
  
  mutate(TY = "TY  - DBASE", TI = "TI  - ", AB = "AB  - ", Y1 = "Y1  - ",  
         
         UR = "UR  - ", JF = "JF  - clinicaltrials.gov", SP = "SP  - 1", ER = "ER  - ", VL = "VL  - ") |> 
  
  unite(TI, c(TI, NCT2, `Study Title`), sep = "") |> 
  
  unite(AB, c(AB, `Study Status`, `Study Design`, Conditions, Interventions, `Primary Outcome Measures`), sep = "") |> 
  
  unite(UR, c(UR, `Study URL`), sep = "") |> 
  
  unite(Y1, c(Y1, `First Posted`), sep = "") |> 
  
  unite(VL, c(VL, `NCT Number`), sep = "") 



# Reordering columns for output. If I were reordering the rows that would go before the first comma. 



df <- df[, c("TY", "TI", "Y1", "AB", "JF", "UR", "VL", "SP", "ER")] 



# write.table used to create text files with new lines as separators and line ends. 



write.table(df, file = "clinicaltrialsris.txt", quote = FALSE, sep = "\n", eol = "\n\n", col.names = FALSE, row.names = FALSE) 