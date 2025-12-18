library(tidyverse)   # Load tidyverse (dplyr, stringr, readr, etc.)

# Find all .txt files in the current working directory
text_files <- list.files(path = ".", pattern = "\\.txt$")

# Read the first Cochrane export file
# - Tab-delimited
# - Skip the first 4 metadata lines
df <- read_delim(
  text_files[1],
  delim = "\t",
  quote = "\\\"",
  escape_double = FALSE,
  trim_ws = TRUE,
  skip = 4
)

df <- df |>
  # Keep only the Search column
  select(Search) |>
  
  # Convert search strings to lowercase
  mutate(Search = str_to_lower(Search)) |> 
  
  # Replace 'mesh descriptor:' and whole-word mh/mhe with a quote
  mutate(
    Search = str_replace_all(
      Search,
      regex("mesh descriptor: |\\bmh\\b|\\bmhe\\b"),
      "\""
    )
  ) |>
  
  # Remove square brackets and closing curly bracket
  mutate(Search = str_remove_all(Search, "\\[|\\]|\\}")) |>
  
  # Convert 'explode all trees' to INAHTA [mhe] syntax
  mutate(Search = str_replace_all(Search, " explode all trees", "\"\\[mhe\\]")) |>
  
  # Convert 'this term only' to INAHTA [mh] syntax
  mutate(Search = str_replace_all(Search, " this term only", "\"\\[mh\\]")) |>
  
  # Replace proximity operators (NEAR/x, NEXT) with AND
  mutate(Search = str_replace_all(Search, "near\\/[:digit:]+|next", "AND")) |>
  
  # Remove title/abstract/keyword field restrictions
  mutate(Search = str_remove_all(Search, ":ti,ab,kw|:ti,ab|:ti|:ab|:kw")) |>
  
  # Add a note where OR combinations need manual checking
  mutate(Search = str_replace_all(Search, "\\{or", "OR combination manually tick")) |>
  
  # Flag limits that should be deleted manually
  mutate(Search = str_replace_all(Search, "\"conference\":pt|\\(clinicaltrials", "DELETE THIS LIMIT"))

# Regex pattern to match hyphenated free-text terms (including multi-hyphens and *)
pattern <- "\\b([[:alnum:]]+(?:-[[:alnum:]]+)+\\*?)"

df <- df |>
  mutate(
    # Quote hyphenated terms only in non-MeSH lines
    Search =
      if_else(
        str_detect(Search, "\\[mh\\]|\\[mhe\\]"),
        Search,
        str_replace_all(Search, pattern, '"\\1"')
      )
  )

# Add general warning notes at the end of the output
output_lines <- c(
  df$Search,
  " ",
  "PLEASE CHECK free-text lines and hyphenated free-text words",
  "PLEASE REVIEW OR combinations manually",
  "PLEASE VERIFY delete limits"
)

# Write the final INAHTA-style search to a text file
write.table(
  output_lines,
  file = "INAHTA_translation.txt",
  row.names = FALSE,
  col.names = FALSE,
  quote = FALSE
)
