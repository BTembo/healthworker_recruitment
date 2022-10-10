
# Load the required libraries
library(tidyverse) # This has all other packages used in this script


#
data_folder_path <- './data/output_all/'

get_files <- list.files(path = data_folder_path, pattern = '.csv')
get_files

##



# Function for extracting different sized tables from a file
check_table_dimension <- function(FolderPathInput){
  
  start.time2 <- Sys.time()
  
  files <- list.files(path = FolderPathInput, pattern = '.csv')
  # print(files)
  
  for (file in files){
    
    # print(file)
    
    file_path <- paste0(FolderPathInput, file)
    
    DataVal <- readr::read_csv(file_path, col_names = F,
                               show_col_types = FALSE)
    
    print(dim(DataVal))
    
  }
  
}

check_table_dimension(data_folder_path)


# Function for extracting different sized tables from a file
collate_25cols_tables_files <- function(FolderPathInput){
  
  files <- list.files(path = FolderPathInput, pattern = '.csv')
  # print(files)
  
  df_col25 <- NULL
  
  
  for (file in files){
    
    print(file)
    
    file_path <- paste0(FolderPathInput, file)
    
    DataVal <- readr::read_csv(file_path, col_names = F,
                               show_col_types = FALSE)
    
        if(dim(DataVal)[2] == 25){
          
          print(dim(DataVal))

          DataVal <- DataVal %>%
            dplyr::as_tibble() %>%
            dplyr::mutate(file_name = file,
                          across(everything(), as.character),)

          df_col25 <- dplyr::bind_rows(df_col25, DataVal)
          # print(df_col25)

        }else{

          print("--- Starts here ---")

          # print(file)
          print(paste0("This ", file, " has has not be captured!"))
          print(DataVal)
          print(dim(DataVal))

          print("--- Ends here ---")

        }
    
  }
  
  # df_col25
  
  colnames(df_col25)
  
  dataVal_pt1 <- df_col25 %>% 
    dplyr::select(all_of(c('X2', 'X3', 'X4', 'X5', 'X6', 'X7', 'file_name'))) %>%  
    dplyr::rename(SERIE_NUMBER = 1, PROVINCE = 2, DISTRICT = 3,
                  POSITION = 4, NAME_OF_CANDIDATE = 5, NRC = 6)
  
  dataVal_pt2 <- df_col25 %>% 
    dplyr::select(all_of(c('X8', 'X9', 'X10', 'X11', 'X12', 'X13', 'file_name'))) %>% 
    dplyr::rename(SERIE_NUMBER = 1, PROVINCE = 2, DISTRICT = 3,
                  POSITION = 4, NAME_OF_CANDIDATE = 5, NRC = 6)
  
  dataVal_pt3 <- df_col25 %>% 
    dplyr::select(all_of(c('X14', 'X15',   'X16',   'X17',   'X18',   'X19', 'file_name'))) %>% 
    dplyr::rename(SERIE_NUMBER = 1, PROVINCE = 2, DISTRICT = 3,
                  POSITION = 4, NAME_OF_CANDIDATE = 5, NRC = 6)
  
  dataVal_pt4 <- df_col25 %>% 
    dplyr::select(all_of(c('X20', 'X21',   'X22',   'X23',   'X24',   'X25', 'file_name'))) %>% 
    dplyr::rename(SERIE_NUMBER = 1, PROVINCE = 2, DISTRICT = 3,
                  POSITION = 4, NAME_OF_CANDIDATE = 5, NRC = 6)
  
  df_col25 <- dplyr::bind_rows(dataVal_pt1,
                               dataVal_pt2,
                               dataVal_pt3,
                               dataVal_pt4)
  
  return(df_col25)
  
}


df_25_cols <- collate_25cols_tables_files(data_folder_path)
df_25_cols


# Function for extracting different sized tables from a file
collate_12cols_tables_files <- function(FolderPathInput){
  
  files <- list.files(path = FolderPathInput, pattern = '.csv')
  # print(files)
  
  df_col12 <- NULL
  
  for (file in files){
    
    file_path <- paste0(FolderPathInput, file)
    
    DataVal <- readr::read_csv(file_path, col_names = F,
                               show_col_types = FALSE)
    
    if(dim(DataVal)[2] == 12){
      
      print(file)
      
      print(dim(DataVal))
      
      DataVal <- DataVal %>%
        dplyr::as_tibble() %>%
        dplyr::mutate(file_name = file,
                      across(everything(), as.character),)
      
      df_col12 <- dplyr::bind_rows(df_col12, DataVal)
      # print(df_col12)
      
    }
    
  }
  
  # df_col12
  
  colnames(df_col12)
  
  dataVal_pt1 <- df_col12 %>%
    dplyr::select(all_of(c('X2', 'X3', 'X4', 'X5', 'X6', 'X7',
                           'file_name'))) %>%
    dplyr::rename(SERIE_NUMBER = 1, PROVINCE = 2, DISTRICT = 3,
                  POSITION = 4, NAME_OF_CANDIDATE = 5, NRC = 6)

  dataVal_pt2 <- df_col12 %>%
    dplyr::select(all_of(c('X8', 'X9', 'X10', 'X11', 'X12', #'X13',
                           'file_name')))
  
  X8_col <- as.character(dataVal_pt2$X8)
  df_split_X8 <- as.data.frame(stringr::str_split_fixed(X8_col, " ", 2)) %>% 
    dplyr::rename(SERIE_NUMBER = V1,
                  PROVINCE = V2)
  
  dataVal_pt2 <- dplyr::bind_cols(dataVal_pt2,
                                  df_split_X8)%>% 
    dplyr::rename(DISTRICT = X9, POSITION = X10, 
                  NAME_OF_CANDIDATE = X11, 
                  NRC = X12) %>% 
    dplyr::select(SERIE_NUMBER, PROVINCE, DISTRICT, POSITION,
                  NAME_OF_CANDIDATE, NRC, file_name)
  
  df_col12 <- dplyr::bind_rows(dataVal_pt1,
                               dataVal_pt2)
  
}

df_12_cols <- collate_12cols_tables_files(data_folder_path)
df_12_cols


## Get the other data -- manually collated
other_data_file <- "health_workers_other_pages.csv"

df_other_data <- readr::read_csv(paste0(data_folder_path, 
                                        other_data_file),
                                 col_names = TRUE,
                                 show_col_types = FALSE) %>% 
  dplyr::mutate(file_name = other_data_file)

df_other_data


df_merged <- dplyr::bind_rows(df_25_cols,
                              df_12_cols,
                              df_other_data) %>% 
  dplyr::filter(SERIE_NUMBER != "S/N")

df_merged



unique(df_merged$PROVINCE)
sort(unique(tolower(df_merged$POSITION)))

# # # export the position data to a csv file
# 
# position_lst <- sort(unique(df_merged$POSITION))
# df_positions <- dplyr::bind_rows(POSITION = position_lst)
# 
# save_to_file <- paste0("./data/", 'df_positions.csv')
# readr::write_csv(df_positions, save_to_file)
# 
# 
# distr_lst <- sort(unique(df_merged$DISTRICT))
# df_distr <- dplyr::bind_rows(DISTRICT = distr_lst)
# 
# save_to_file2 <- paste0("./data/", 'df_distr.csv')
# readr::write_csv(df_distr, save_to_file2)


df_map_positions <-  readxl::read_excel(paste0('./data/', 'mappinq_variables_hwr.xlsx'),
                                        sheet = "positions")

df_map_positions

df_map_distr <-  readxl::read_excel(paste0('./data/', 'mappinq_variables_hwr.xlsx'),
                                    sheet = "districts")

df_map_distr

## map positions
df_merged_cleaned <- df_merged %>% 
  mutate(district_recode = plyr::mapvalues(DISTRICT,
                                           from = df_map_distr$DISTRICT,
                                           to = df_map_distr$new_distr_name),
         
         province_recode = plyr::mapvalues(DISTRICT,
                                           from = df_map_distr$DISTRICT,
                                           to = df_map_distr$new_prov_name),
         province_recode = ifelse(is.na(province_recode), PROVINCE, 
                                  province_recode),
         position_recode = plyr::mapvalues(POSITION,
                                           from = df_map_positions$POSITION,
                                           to = df_map_positions$new_name),
         
         NRC = ifelse(NRC %in% c("Unnamed: 0", "Unnamed: 1"),
                      NA, NRC),
         nrc_recode = ifelse(is.na(NRC),
                             str_replace_all(NAME_OF_CANDIDATE, 
                                             pattern = "[[:alpha:]]", ""),
                             NRC),
         nrc_recode = str_replace_all(nrc_recode, pattern = "[[:space:]]", ""),
         nrc_recode = str_replace_all(nrc_recode, pattern = "\\.", ""),
         nrc_recode = str_replace_all(nrc_recode, pattern = "\\’", ""),
         nrc_recode = str_replace_all(nrc_recode, pattern = "\\-", ""),
         
         name_recode = ifelse(is.na(NRC),
                             str_replace_all(NAME_OF_CANDIDATE, 
                                             pattern = "\\.", ""),
                             NAME_OF_CANDIDATE),
         
         # name_recode = str_replace_all(NAME_OF_CANDIDATE, pattern = "\\.", ""),
         name_recode = str_replace_all(name_recode, pattern = "\\/", ""),
         # name_recode = str_replace_all(NAME_OF_CANDIDATE, pattern = "[[:punct:]]", ""),
         name_recode = str_replace_all(name_recode, pattern = "[[:digit:]]", ""),
         name_recode = str_trim(name_recode),
         name_recode = str_replace_all(name_recode, pattern = "  ", " "),
         name_recode = str_replace_all(name_recode, pattern = "\\.", ""),
         name_recode = ifelse(name_recode == "�eresa Chanda",
                              "Theresa Chanda",
                              name_recode),
         
         name_recode = ifelse(name_recode == "Gi� Bwile",
                              "Gift Bwile",
                              name_recode),
         name_recode = ifelse(name_recode == "Kamboyi Gi�",
                              "Kamboyi Gift",
                              name_recode),
         name_recode = ifelse(name_recode == "Siwakwi Sa�c",
                              "Siwakwi Saffic",
                              name_recode),
         
         name_recode = str_replace_all(name_recode, pattern = "  ", " "),
         SERIE_NUMBER = as.numeric(SERIE_NUMBER),
         position_recode = stringi::stri_trans_totitle(position_recode),
         district_recode = stringi::stri_trans_totitle(district_recode),
         province_recode = stringi::stri_trans_totitle(province_recode),
         name_recode = stringi::stri_trans_totitle(name_recode),
         test_special_char = stringr::str_detect(name_recode, "�"),
         ) %>% 
  dplyr::select(-file_name) %>% 
  dplyr::filter(POSITION != "POSITION",
                !is.na(position_recode),
                # is.na(DISTRICT),
                # PROVINCE %in% c("Itezhi Tezhi", "Kabwe", "Kapiri",  "Mkushi",
                #                 "Luano",  "Serenje",  "Shibuyunji", "Ngabwe"),
                # is.na(NRC),
                # nrc_recode %in% c("Unnamed:0", "Unnamed:1"),
                # name_recode == "�eresa Chanda",
                # test_special_char == TRUE,
                # is.na(PROVINCE)
                ) %>% 
  dplyr::rename(series = SERIE_NUMBER) %>% 
  dplyr::arrange(series) %>% 
  dplyr::distinct()
  
df_merged_cleaned


table(df_merged_cleaned$district_recode)
table(df_merged_cleaned$province_recode)
table(df_merged_cleaned$position_recode)


### --- The script ends here --- ###
