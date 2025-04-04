# Picket list quering

rm(list = ls())
gc()

# install libraries
# install.packages("readxl")
# install.packages("dyplr")
# install.packages("tidyr")
# install.packages("writeexl")

# libraries
library(readxl)
library(dplyr)
library(tidyr)
library(writexl)

# (1) make main table to work with
#     cancel out columns (- owner, experation,  blood withdraw, process start, process finish, freeze date outside of biobank, blood type, lab specialist, quality control, documentation 2, freezer, level 1-5) 
# (2) keep SwiSCI
# (3) SwiSCI without QC
# (4) only the one that have T1 and T4 (NEED to have both) -> in condisderation of SAME SwiSCI ID
# (5) Vials > 1
# (6) Volume > 0
# (7) no QC
# (8) lowest position for manually taking sample out faster

# Get the excel file from file path
input_file <- "/Users/s70cky/Desktop/Picket_List/PicketList_main.xlsx"  

# make df
df <- read_excel(input_file)
# display (There were 50 or more warnings)
all_warnings <- capture.output(warnings())
cat(all_warnings, sep = "\n")


# (1)
df_selected_columns <- df %>%
  select(-Owner, -Expiration, -`Blood withdrawl`, -`Processing start`, -`Processing finished`, 
       -`Freezed Date (outside of biobank)`, -`Blood Type`, -`Lab specialist`, 
       -`Quality control`, -`Documentation 2`, -Freezer, -Level1, -Level2, -Level3, -Level4, -Level5)

# (2)
df_only_Name_SwiSCI <- df_selected_columns %>%
  filter(grepl("^SwiSCI", Name))

# (3)
df_group_ID <- df_only_Name_SwiSCI %>%
  arrange(SwiSCI_ID)
 
# (4)
#df_group_T1_T4 <- df_group_ID %>%
 # filter(any(`Sample Group` == "t1") & any(`Sample Group` == "t4"))
df_group_T1_T4 <- df_group_ID %>%
  group_by(SwiSCI_ID) %>%
  filter(any(`Sample Group` == "t1") & any(`Sample Group` == "t4")) %>%  
  ungroup() %>%  
  group_by(SwiSCI_ID) %>%
  filter(n_distinct(`Sample Group`) == 2) %>% 
  ungroup() 
# (5-6)
df_filter_vials_volume <- df_group_T1_T4 %>%
  filter(Vials > 0, Volume > 0)

# (7)
df_no_qc <- df_filter_vials_volume %>%
  filter(!grepl(" QC$", Name))

# (8)
df_position <- df_no_qc %>%
  mutate(PositionNumber = as.numeric(gsub("^(\\d+).*", "\\1", Position))) 

df_position_final <- df_position %>%
  group_by(SwiSCI_ID, `Sample Group`) %>%
  filter(PositionNumber == min(PositionNumber)) %>% 
  ungroup() 


# DAS FUNKTIONIERT NOCH NICHT GANZ MIT POSITION SO WEIT INNEN (1,2,3) ODER AUßEN (10,11,12)
df_final_data_filtered <- df_position %>%
  filter(PositionNumber %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)) %>%
  group_by(SwiSCI_ID, `Sample Group`) %>%
  slice_min(order_by = PositionNumber, n = 1, with_ties = FALSE) %>%
  ungroup()

write_xlsx(df_final_data_filtered, "~/Desktop/df_final_data_filtered2.xlsx")
