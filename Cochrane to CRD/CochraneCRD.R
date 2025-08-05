library(tidyverse)
library(stringr)
library(kableExtra)

df <- read_delim("Cochrane.txt", delim = "\t", 
                 quote = "\\\"", escape_double = FALSE, 
                 trim_ws = TRUE, skip = 4)

df<- df |>
  select(ID, Search)|>
  mutate(ID = str_replace_all(ID, "#", "")) |>
  #mh for when copied from Medline, this might need to be changed as it uses phrases and mh might replace from free text
  mutate(Search = str_replace_all(Search, "MeSH descriptor: |mh", "MESH DESCRIPTOR "))|>
  #mutate(Search = case_when(grepl("\\[mh \\"", Search) ~ str_remove_all("\\"")),.default = Search)|>
  mutate(Search = str_replace_all (Search, "\\[|\\]|\\/|\\{or |\\{OR |\\}", ""))|>
  mutate(Search = str_remove_all(Search, "this term only"))|>
  #mutate(Search = str_replace (Search, "\\-#", ":"))|>
  #mutate(Search = casefold("explode all trees", upper = TRUE) Come back to this, also tried str to upper. If phrases can be changed, can combine with line 9
  mutate(Search = str_replace_all(Search, "explode all trees", "EXPLODE ALL TREES"))|>
  mutate(Search  = str_replace_all(Search, "\\:pt|\\:so", " LIMIT - DELETE LINE"))|>
  mutate(Search = str_replace_all (Search, "with Cochrane Library publication",
                                   "DELETE THIS TEXT AND APPLY"))|>
  mutate(Search = gsub("\\:.*", "", Search))|>
  mutate(Merged="")|>
  unite(Merged, Merged, ID, Search, sep=" ", na.rm=TRUE)

df <- select(df, Merged) 

write.table(df, file = "crd.txt", row.names = FALSE, col.names = FALSE, quote = FALSE)

#Need to figure out how to add text to condensed combination lines to say "expand these". Text output could include a chunk of numbers


