library(tidyverse)
library(readr)

# Code that creates list of text file names from working directory to input into our script later.
# If working on desktop R you may need to change the reference on line 13 to find the right file.

text_files <- list.files(path = ".", pattern = "\\.txt$")

# Cochrane files are TSV format so tabs are delimiters. 
# Leading quote marks get removed without specifying otherwise. Skip removes the first
# 4 rows and trim_ws gets rid of white space.

df <- read_delim(text_files[1], delim = "\t", 
                 quote = "\\\"", escape_double = FALSE, 
                 trim_ws = TRUE, skip = 4)

# Lines 23-37... 
# remove all columns bar search terms, 
# convert all text to lower case
# remove MeSH headings leaving an empty row
# change all 'NEAR's to 'AND's (no proximity operator in Epistemonikos) 

df <- df |>
  select(Search) |>
  mutate(Search = str_to_lower(Search)) |>
  filter(!str_starts(Search, "\\[mh |mesh descriptor:")) |>
  mutate(Search = str_replace_all(Search, "near\\/[:digit:]|next", "AND")) |>
  
  # Next lines (35-43) replace anythign starting with Boolean combos with two line breaks.
  # The idea is that the Boolean lines delineate separate 'chunks' of the strategy
  # and that the chunks will be pasted into Epistemonikos and AND-ed manually.
  
  # A basic assumption is that people won't mix Boolean combos with new terms.
  
  mutate(Search = case_when(
    str_starts(Search, 
               "\\{|\\#[:digit:][:digit:][:digit:] AND \\#|\\#[:digit:][:digit:] AND \\#|\\#[:digit:] AND \\#|\\#[:digit:][:digit:][:digit:] and \\#|\\#[:digit:][:digit:] and \\#|\\#[:digit:] and \\#|\\#[:digit:][:digit:][:digit:] OR \\#|\\#[:digit:][:digit:] OR \\#|\\#[:digit:] OR \\#\\{|\\#[:digit:][:digit:][:digit:] or \\#|\\#[:digit:][:digit:] or \\#|\\#[:digit:] or \\#|\\{|\\#[:digit:][:digit:][:digit:] NOT \\#|\\#[:digit:][:digit:] NOT \\#|\\#[:digit:] NOT \\#|\\#[:digit:][:digit:][:digit:] not \\#|\\#[:digit:][:digit:] not \\#|\\#[:digit:] not \\#"
    ) ~ "
    
", 
.default = Search
  )
  )  |>
  
  # 50-52 looks for lines that start with alphanumeric characters and have spaces in 
  # and surrounds them with brackets so the default Boolean AND applies   
  
  # check how this works with phrases deeper in lines - see Jenny's strategy
  
  mutate(Search = case_when(
    str_starts(Search, "[:alnum:]")&str_detect(Search, " ") ~ paste0("(", Search, ")"),
    .default = Search)) |> 
  
  # Change field codes for OR, then deal with case where field codes are within brackets
  # as a result of the above.
  
  mutate(Search = str_replace_all(Search, ":ti,ab,kw|:ti,ab|:ti|:ab", " OR ")) |>
  mutate(Search = str_replace_all(Search, " OR \\)", "\\)"))

# Create vector with locations of double blank lines which used to be ANDs
# and are now double-spaces. This will be used to strip training
# 'ORs' off the lines before. I have to make a small adjustment on 
# line 65 in case there are blank lines which used to be MeSH at the start of the strategy.

break_lines <- which(str_detect(df$Search, pattern = "
    
"))

trailing_ORs <- break_lines[ !break_lines == '1'] - 1

df[trailing_ORs, ] <- df[trailing_ORs, ] |> 
  mutate(Search = str_replace_all(Search, " OR", ""))

#Next line turns multi-row dataframe into single cell before export

search_string <- str_flatten(df$Search, collapse = "")

write.table(search_string, file = "epistemonikos_chunks.txt", 
            row.names = FALSE, col.names = FALSE, quote = FALSE)