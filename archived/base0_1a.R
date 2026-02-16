# base0_1a.R

# Check for the presence of technical or vocational education in project title

library(dplyr)

epdraw <- openxlsx::read.xlsx("D:/AdvancedR/knowbankedu/rawdata/Education Portfolio Dashboard Source File - 6 Feb 2024.xlsx")
# 19,597 rows - each row is a manual Education GP theme code


# list of components and add column of number of components
epdrawAPI_a <- epdraw %>%
  group_by(Project.ID) %>%                     # Group by Project.ID
  summarize(
    Components = list(
      unique(na.omit(Component.Name))          # Remove NAs and keep unique Component.Name values
    )
  ) %>% ungroup() %>% mutate(n_Components = lengths(Components))
# 690 projects

epdrawAPI_b <- epdraw %>%
  group_by(Project.ID) %>%      # Group by Project.ID
  filter(row_number() == 1) %>% # Keep only the first row in each group
  ungroup()    %>% select(-Component.Name) # Remove Component.Name column
# 690 projects

# Now merge the two parts of the data using dplyr::inner_join

epdrawAPI <- dplyr::inner_join(epdrawAPI_a, epdrawAPI_b, by = "Project.ID")

# I have component list but would like to check this with the LLM derived data on components.


# Tagging TVET mention in title

epdrawAPI <- epdrawAPI %>%
  mutate(
    TVET_Title = ifelse(
      grepl("\\b(Technical|Vocational|Skills|TVET)\\b", Project.Name, ignore.case = TRUE),
      TRUE,
      FALSE
    )
  )

sum(epdrawAPI$TVET_Title) 

# 2 projects

epdrawAPI %>%
  filter(grepl("\\bTechnical Assistance\\b", Project.Name, ignore.case = TRUE)) %>% select(Project.Name) 


# rephrase

# Tagging projects with specific keywords but excluding "Technical Assistance"
epdrawAPI <- epdrawAPI %>%
  mutate(
    TVET_Title = ifelse(
      grepl("\\b(Technical|Vocational|Skills|TVET)\\b", Project.Name, ignore.case = TRUE) &
        !grepl("\\bTechnical Assistance\\b", Project.Name, ignore.case = TRUE),
      TRUE,
      FALSE
    )
  )



# Frequency by country of 77 projects in 46 countries
epdrawAPI %>% filter(TVET_Title == TRUE) %>% select(Project.Name, Country.Name) %>% count(Country.Name) %>% arrange(desc(n)) %>%
  print(n = 46)


TTPIDs <- epdrawAPI %>% filter(TVET_Title == TRUE) %>% select(Project.ID) 
padcover1b <- read.csv("D:/AdvancedR/knowbankedu/working/padcover1b.csv")


TTPIDSinAI <- left_join(TTPIDs, padcover1b, by = "Project.ID") %>% filter(!is.na(Project.ID)) # All 77
TTPIDSinAI <- left_join(TTPIDs, padcover1b, by = "Project.ID") %>% filter(!is.na(File_Name)) # 59 - not bad



####### 
padcover2 <- read.csv("D:/AdvancedR/knowbankedu/working/class_based_padcover1.csv")

padcover2 %>% filter(report_no=="") %>% n_distinct() # 175 missing
padcover2 %>% filter(project_name!="") %>% n_distinct() # 337
padcover2 %>% filter(project_name=="") %>% n_distinct() # only 12 


padcover2PI <- padcover2 %>%
  mutate(
    TVET_Title = ifelse(
      grepl("\\b(Technical|Vocational|Skills|TVET)\\b", project_name, ignore.case = TRUE) &
        !grepl("\\bTechnical Assistance\\b", project_name, ignore.case = TRUE),
      TRUE,
      FALSE
    )
  )

# Frequency by country of 77 projects in 46 countries
epdrawAPI %>% filter(TVET_Title == TRUE) %>% select(Project.Name, Country.Name) %>% count(Country.Name) %>% arrange(desc(n)) %>%
  print(n = 46)

padcover2PI %>% filter(TVET_Title == TRUE) %>% select(project_name) # 57

TTPIDSinAI %>% filter(!is.na(Fin_Amount_USD)) %>% arrange(desc(Fin_Amount_USD)) %>% select(Project.ID, Country, Project_Name, Fin_Amount_USD) 
  

