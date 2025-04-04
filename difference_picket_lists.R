# abgleich

rm(list = ls())
gc()


# libraries
library(readxl)
library(dplyr)
library(tidyr)
library(writexl)

# Get the excel file from file path
Picket_List_f <- read_excel("/Users/s70cky/Desktop/Picket_List/Digitalization_Biobank_samples_picklists.xlsx", sheet = "vials_report_1736442245_122") 
Picket_List_r <- read_excel("/Users/s70cky/Desktop/df_final_data_filtered2.xlsx")

used_donors <- read_excel("/Users/s70cky/Desktop/Picket_List/Digitalization_Biobank_samples_picklists.xlsx", sheet = "overview used donors")

Picket_List_f <- Picket_List_f %>%
  select(-Owner, -Expiration, -`Blood withdrawl`, -`Processing start`, -`Processing finished`, 
         -`Freezed Date (outside of biobank)`, -`Blood Type`, -`Lab specialist`, 
         -`Quality control`, -`Documentation 2`, -Freezer, -Level1, -Level2, -Level3, -Level4, -Level5)

Picket_List_r <- Picket_List_r %>%
  select(-PositionNumber)

# (1) Rows with matching SwiSCI_ID
# (2) Rows from Picket_List_f that are NOT in Picket_List_r
# (3) Rows from Picket_List_r that are NOT in Picket_List_f
# (4) used donors Abgleich

# (1)
common <- inner_join(Picket_List_f, Picket_List_r, by = "SwiSCI_ID", relationship = "many-to-many")

# (2)
Picket_List_f_only <- Picket_List_f %>% 
  filter(!SwiSCI_ID %in% Picket_List_r$SwiSCI_ID)

# (3)
Picket_List_r_only <- Picket_List_r %>% 
  filter(!SwiSCI_ID %in% Picket_List_f$SwiSCI_ID)

# (4)
Picket_List_r_short <- Picket_List_r %>% 
  select(SwiSCI_ID, `Sample Group`)

matches <- inner_join(Picket_List_r_short, used_donors, by = "SwiSCI_ID", relationship = "many-to-many") %>%
  distinct(SwiSCI_ID) %>%
  mutate(origin = "match")

only_in_picket <- anti_join(Picket_List_r_short, used_donors, by = "SwiSCI_ID") %>%
  distinct(SwiSCI_ID) %>%
  mutate(origin = "only_in_picket")

only_in_used <- anti_join(used_donors, Picket_List_r_short, by = "SwiSCI_ID") %>%
  distinct(SwiSCI_ID) %>%
  mutate(origin = "only_in_used")

comparison_result <- bind_rows(only_in_picket, only_in_used, matches)

