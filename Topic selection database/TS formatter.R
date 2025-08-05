library(readxl)
#library(Microsoft365R)
library(tidyverse)


df <- read_xlsx("C:/Users/DNicholls/Downloads/topics.xlsx")

df <- df %>%
  select(Title, ID, Status) %>%
  filter(ID > 7649) %>%
 
  filter(!grepl("C list|Duplicate|Non-Prioritisation|Database outcome|C listed|CCPHA checking|Checking with guidance programme|Considered at CP|Considered at ROG|Decision by Minister|Eliminated|Evaluation pathway|Final referral|IP considering|IS checking|Library of topics|Minded referral|New Medicines Evidence Summary|NHSC / NIHRIO completed briefing|NHSC / NIHRIO considering|NHSC / NIHRIO monitoring|NICE monitoring|Non-prioritisation|NTAH considering|Published|R&D checking|R&D Team Checking|Rejected under new process|Rejected under old process", Status)) %>%
  #use next line if things to remove in title e.g. non-Hodgkins when a Hodgkins topic)
  #filter(!grepl("non small|Non small|non-small|Non-small", Title)) %>%
  #filter(!grepl("HER2-positive|HER2 positive", Title)) %>%
  mutate(ID = as.character(ID)) %>%
  mutate(Title = str_replace(Title, pattern = "$", replacement = ". NICE technology appraisal. Publication date to be confirmed. Topic selection number ")) %>%
  mutate(ID = as.character(ID)) %>%
  mutate(ID = str_replace(ID, pattern = "$", replacement = ". Status: ")) %>%
  unite("Output", c(Title, ID, Status), sep = "", remove = TRUE, na.rm = FALSE)



writeClipboard(df$Output)






