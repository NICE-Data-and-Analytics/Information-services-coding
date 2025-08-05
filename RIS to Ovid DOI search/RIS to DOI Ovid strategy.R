library(tidyverse)
library(revtools)

# This script automatically creates a search strategy to find papers contained within a RIS file. 
# It does this by looking for a DOI in the RIS file and adding this to a query which runs in the DOI field (.do) in Ovid.
# Use it by uploading your RIS file into the "files" window (bottom right), then...
# change line 12 by putting your file name between the speech marks, 
# select all (ctrl+A) in this window and click Run (above). Your strategy will appear in the files window (bottom right).
# Click to open DOIStrategy.txt. This will open in another tab. You can copy and paste this directly into Ovid.
# Don't forget to ignore the last "OR" when you copy-and-paste. I will remove this if people start using this script frequently [Tom].

df <- read_bibliography("Embase air pollution refs.ris", return_df = TRUE)

doi_strategy <- df %>%
    select(doi) %>%
    filter(doi != "NA") %>%
    mutate(doi = str_replace(doi, "https://dx.doi.org/", "")) %>%
    mutate(doi = str_replace(doi, "$", ""))
      
write.table(doi_strategy, "DOIStrategy.txt", sep = ".do OR ", row.names = FALSE, col.names = FALSE, eol = ".do OR ", fileEncoding = "UTF-8")
