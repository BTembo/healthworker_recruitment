
# Load the required libraries
# library(dplyr)
# library(readr)
# library(stringr)
library(tidyverse)


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

df_tbl_dim <- check_table_dimension(data_folder_path)

## Run the loop based on all the available files ##
for (fileList in filesList){
  # print(fileList)
  df_tbl_dim <- check_table_dimension(data_folder_path, fileList)
  
}




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


sort(unique(tolower(df_25_cols$PROVINCE)))
sort(unique(tolower(df_25_cols$DISTRICT)))
sort(unique(tolower(df_25_cols$POSITION)))
sort(unique(tolower(df_25_cols$NAME_OF_CANDIDATE)))
sort(unique(tolower(df_25_cols$NRC)))





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
      
    }#else{
    #   
    #   print("--- Starts here ---")
    #   
    #   # print(file)
    #   print(paste0("This ", file, " has has not be captured!"))
    #   print(DataVal)
    #   print(dim(DataVal))
    #   
    #   print("--- Ends here ---")
    #   
    # }
    
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
                           'file_name')))#%>%
    # dplyr::rename(SERIE_NUMBER = 1, PROVINCE = 2, DISTRICT = 3,
    #               POSITION = 4, NAME_OF_CANDIDATE = 5, NRC = 6)
  
  X8_col <- as.character(dataVal_pt2$X8)
  df_split_X8 <- as.data.frame(stringr::str_split_fixed(X8_col, " ", 2)) %>% 
    dplyr::rename(SERIE_NUMBER = V1,
                  PROVINCE = V2)

  print(dataVal_pt2)
  print(df_split_X8)
  
  
  # dataVal_pt3 <- df_col12 %>% 
  #   dplyr::select(all_of(c('X14', 'X15',   'X16',   'X17',   'X18',   'X19', 'file_name'))) %>% 
  #   dplyr::rename(SERIE_NUMBER = 1, PROVINCE = 2, DISTRICT = 3,
  #                 POSITION = 4, NAME_OF_CANDIDATE = 5, NRC = 6)
  # 
  # dataVal_pt4 <- df_col12 %>% 
  #   dplyr::select(all_of(c('X20', 'X21',   'X22',   'X23',   'X24',   'X25', 'file_name'))) %>% 
  #   dplyr::rename(SERIE_NUMBER = 1, PROVINCE = 2, DISTRICT = 3,
  #                 POSITION = 4, NAME_OF_CANDIDATE = 5, NRC = 6)
  # 
  # df_col12 <- dplyr::bind_rows(dataVal_pt1,
  #                              dataVal_pt2,
  #                              dataVal_pt3,
  #                              dataVal_pt4)
  
  dataVal_pt2
  
  dataVal_pt2 <- dplyr::bind_cols(dataVal_pt2,
                                  df_split_X8)%>% 
    dplyr::rename(DISTRICT = X9, POSITION = X10, 
                  NAME_OF_CANDIDATE = X11, 
                  NRC = X12) %>% 
    dplyr::select(SERIE_NUMBER, PROVINCE, DISTRICT, POSITION,
                  NAME_OF_CANDIDATE, NRC, file_name)
  
  df_col12 <- dplyr::bind_rows(dataVal_pt1,
                               dataVal_pt2)
  
  # return(df_col12)
  
}


df_12_cols <- collate_12cols_tables_files(data_folder_path)
df_12_cols



unique(df_12_cols$SERIE_NUMBER)
unique(df_12_cols$NAME_OF_CANDIDATE)
unique(df_12_cols$NRC)



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

# # export the position data to a csv file
# 
# position_lst <- sort(unique(df_merged$POSITION))
# df_positions <- dplyr::bind_rows(POSITION = position_lst)
# 
# save_to_file <- paste0("./data/", 'df_positions.csv')
# readr::write_csv(df_positions, save_to_file)


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
df_merged_pos <- df_merged %>% 
  mutate(position_recode = plyr::mapvalues(POSITION,
                                           from = df_map_positions$POSITION,
                                           to = df_map_positions$new_name),
         district_recode = plyr::mapvalues(DISTRICT,
                                           from = df_map_distr$DISTRICT,
                                           to = df_map_distr$new_distr_name),
         
         province_recode = plyr::mapvalues(DISTRICT,
                                           from = df_map_distr$DISTRICT,
                                           to = df_map_distr$new_prov_name),
         province_recode = ifelse(is.na(province_recode), PROVINCE, 
                                  province_recode),
         
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
         name_recode = str_replace_all(name_recode, pattern = "  ", " "),
         SERIE_NUMBER = as.numeric(SERIE_NUMBER), 
         position_recode = stringi::stri_trans_totitle(position_recode),
         district_recode = stringi::stri_trans_totitle(district_recode),
         province_recode = stringi::stri_trans_totitle(province_recode),
         name_recode = stringi::stri_trans_totitle(name_recode)
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
                # is.na(PROVINCE)
                ) %>% 
  dplyr::arrange(SERIE_NUMBER) %>% 
  dplyr::distinct()
  
df_merged_pos

nrc_lst = as.character(df_merged_pos$NAME_OF_CANDIDATE)
nrc_lst2 = str_replace_all(nrc_lst, pattern = "[[:alpha:]]", "")
nrc_lst2


table(df_merged_pos$district_recode)
table(df_merged_pos$province_recode)
table(df_merged_pos$position_recode)


unique(df_merged_pos$PROVINCE)

unique(df_merged_pos$DISTRICT)
sort(unique(df_merged_pos$nrc_recode))
tail(sort(unique(df_merged_pos$nrc_recode)), 100)

sort(unique(df_merged_pos$name_recode))
tail(sort(unique(df_merged_pos$name_recode)), 100)

sort(unique(df_merged_pos$SERIE_NUMBER))
tail(sort(unique(df_merged_pos$SERIE_NUMBER)), 100)



# files_xxx <- as.character(df_12_cols$SERIE_NUMBER)
# df_files_xxx <- as.data.frame(stringr::str_split_fixed(files_xxx, " ", 2)) %>% 
#   dplyr::rename(SERIE_NUMBER = V1,
#                 PROVINCE = V2)
# 
# 
# ## Run the loop based on all the available files ##
# for (fileList in filesList){
#   
#   # print(fileList)
#   df_25_cols <- collate_25cols_tables_files(data_folder_path, fileList)
#   print(dim(df_25_cols))
#   
# }
# 
# 
# df_25_cols
# 
# 
# 
# 
# 
# ###############################
# 
# df_file0 <- readr::read_csv(paste0(data_folder_path,
#                                    "publishednames-civilservicecommission_tbl_0.csv"),
#                             col_names = FALSE,
#                             show_col_types = FALSE) %>% 
#   dplyr::select(-X1, -X2, -X3)
# 
# df_file0
# 
# 
# df_file1 <- readr::read_csv(paste0(data_folder_path,
#                                    "publishednames-civilservicecommission_tbl_1.csv"),
#                             col_names = FALSE,
#                             show_col_types = FALSE) #%>% 
#   # dplyr::select(-X1, -X2, -X3)
# 
# df_file1
# 
# 
# df_file0_part1 <- df_file0 %>% 
#   dplyr::select(all_of(c('X4', 'X5', 'X6', 'X7', 'X8', 'X9'))) %>% 
#   dplyr::mutate(POSITION = paste0(X6, 'ffi', X7)) %>% 
#   dplyr::rename(NRC = X9)
# 
# df_file0_part1
# 
# 
# 
# df_file2 <- readr::read_csv(paste0(data_folder_path,
#                                    "publishednames-civilservicecommission_tbl_2.csv"),
#                             col_names = FALSE,
#                             show_col_types = FALSE) %>% 
#   dplyr::select(-1)
# 
# df_file2
# 
# 
# df_file2_pt1 <- df_file2 %>% 
#   dplyr::select(all_of(c('X2', 'X3', 'X4', 'X5', 'X6', 'X7'))) %>%  
#   dplyr::rename(SERIE_NUMBER = 1, PROVINCE = 2, DISTRICT = 3,
#                 POSITION = 4, NAME_OF_CANDIDATE = 5, NRC = 6)
#   # # dplyr::mutate(POSITION = paste0(X6, 'ffi', X7)) %>% 
#   # dplyr::rename(SERIE_NUMBER = X2, PROVINCE = X3, DISTRICT = X4,
#   #               POSITION = X5, NAME_OF_CANDIDATE = X6, NRC = X7)
# 
# df_file2_pt1
# 
# 
# df_file2_pt2 <- df_file2 %>% 
#   dplyr::select(all_of(c('X8', 'X9', 'X10', 'X11', 'X12', 'X13'))) %>% 
#   dplyr::rename(SERIE_NUMBER = 1, PROVINCE = 2, DISTRICT = 3,
#                 POSITION = 4, NAME_OF_CANDIDATE = 5, NRC = 6)
#   # # dplyr::mutate(POSITION = paste0(X6, 'ffi', X7)) %>% 
#   # dplyr::rename(SERIE_NUMBER = X8, PROVINCE = X9, DISTRICT = X10,
#   #               POSITION = X11, NAME_OF_CANDIDATE = X12, NRC = X13)
# 
# df_file2_pt2
# 
# 
# df_file2_pt3 <- df_file2 %>% 
#   dplyr::select(all_of(c('X14', 'X15',   'X16',   'X17',   'X18',   'X19'))) %>% 
#   dplyr::rename(SERIE_NUMBER = 1, PROVINCE = 2, DISTRICT = 3,
#                 POSITION = 4, NAME_OF_CANDIDATE = 5, NRC = 6)
#   # dplyr::rename(SERIE_NUMBER = X14, PROVINCE = X15, DISTRICT = X16,
#   #               POSITION = X17, NAME_OF_CANDIDATE = X18, NRC = X19)
# 
# df_file2_pt3
# 
# 
# df_file2_pt4 <- df_file2 %>% 
#   dplyr::select(all_of(c('X20', 'X21',   'X22',   'X23',   'X24',   'X25'))) %>% 
#   dplyr::rename(SERIE_NUMBER = 1, PROVINCE = 2, DISTRICT = 3,
#                 POSITION = 4, NAME_OF_CANDIDATE = 5, NRC = 6)
# 
# df_file2_pt4
# 
# 
# df_file3 <- readr::read_csv(paste0(data_folder_path,
#                                    "publishednames-civilservicecommission_tbl_3.csv"),
#                             col_names = FALSE,
#                             show_col_types = FALSE) %>% 
#   dplyr::select(-1)
# 
# df_file3
# 
# df_file3_pt1 <- df_file3 %>% 
#   dplyr::select(all_of(c('X2', 'X3', 'X4', 'X5', 'X6', 'X7'))) %>% 
#   dplyr::rename(SERIE_NUMBER = 1, PROVINCE = 2, DISTRICT = 3,
#                 POSITION = 4, NAME_OF_CANDIDATE = 5, NRC = 6)
# 
# df_file3_pt1
# 
# 
# 
# 
# 
# test_file <- "publishednames-civilservicecommission_tbl_0.csv"
# test_file_20 <- "publishednames-civilservicecommission_tbl_20.csv"
# 
# df_test_file <- readr::read_csv(paste0(data_folder_path, 
#                                        test_file),
#                                 col_names = FALSE,
#                                 show_col_types = FALSE)
# 
# df_test_file
# 
# 
# 
# df_test_file1 <- readr::read_csv(paste0(data_folder_path, 
#                                         test_file_20),
#                                 col_names = FALSE,
#                                 show_col_types = FALSE)
# 
# df_test_file1
# 
# # Function for collating all the files names in the folders
# get_main_file_name <- function(FolderPathInput){
#   
#   start.time2 <- Sys.time()
#   
#   files <- list.files(path = FolderPathInput, pattern = '.csv')
#   
#   # files <- as.data.frame(stringr::str_split_fixed(files, "\\_", 2))
#   # files <- unique(as.character(files$V1))
#   # # # files <- tolower(files)
#   # # print(files)
#   # 
#   # return(files)
#   
# }
# 
# 
# filesList <- get_main_file_name(data_folder_path)
# filesList
# 
# 
# 
# # Function for collating all the files names in the folders
# get_file_names_table <- function(FolderPathInput){
#   
#   start.time2 <- Sys.time()
#   
#   files <- list.files(path = FolderPathInput, pattern = '.csv')
#   
#   files_fullname <- as.character(files)
#   
#   df_files <- as.data.frame(stringr::str_split_fixed(files, "\\_", 3)) %>% 
#     dplyr::rename(corename = V1,
#                   V2x = V2)
#   files_csv <- as.character(df_files$V3)
#   df_files_csv <- as.data.frame(stringr::str_split_fixed(files_csv, "\\.", 2)) %>% 
#     dplyr::rename(tbl_number = V1)
#   
#   # print(dim(df_files))
#   # print(dim(df_files_csv))
#   
#   files_table <- dplyr::bind_cols(file =  files_fullname,
#                                   df_files,
#                                   df_files_csv) %>% 
#     dplyr::select(file, corename, tbl_number) %>%
#     dplyr::as_tibble()
# 
#   # return(files_table)
#   
# }
# 
# 
# filesTable <- get_file_names_table(data_folder_path)
# # filesTable
# 
# 
# # all_directoriesX <- list.dirs()
# 
# # Function for extracting different sized tables from a file
# get_tables_files <- function(FolderPathInput, FilesInput){
#   
#   start.time2 <- Sys.time()
#   
#   files <- list.files(path = FolderPathInput, pattern = '.csv')
#   # print(files)
#   
#   
#   for (file in fileList){
#     
#     print(file)
#     
#     file_path <- paste0(FolderPathInput, file)
#     
#     DataVal <- readr::read_csv(file_path, col_names = F,
#                                show_col_types = FALSE)
#     
#     print(dim(DataVal))
#     
#     }
#   
#   
#   # num_of_files <- 0
#   # 
#   # ##
#   # df_col8 <- NULL
#   # df_col9 <- NULL
#   # ##
#   # 
#   # for (FileInput in FilesInput){
#   #   
#   #   df_filesTable <- get_file_names_table(FolderPathInput) %>% 
#   #     dplyr::filter(corename == FileInput,
#   #                   # tbl_number %in% as.character(seq(1:24))
#   #                   )
#   #   
#   #   fileList <- as.character(df_filesTable$file)
#   #   
#   #   print("--- Starts here ---")
#   #   print(FileInput)
#   #   
#   #   for (file in fileList){
#   #     
#   #     file_path <- paste(FolderPathInput, file, sep = '/')
#   # 
#   #     DataVal <- readr::read_csv(file_path, col_names = F,
#   #                                show_col_types = FALSE)
#   #     
#   #     if(dim(DataVal)[2] == 8){
#   # 
#   #       DataVal <- DataVal %>%
#   #         dplyr::as_tibble() %>%
#   #         dplyr::mutate(across(everything(), as.character),
#   #                       core_name = FilesInput,
#   #                       file_name = file)
#   #       
#   #       df_col8 <- dplyr::bind_rows(DataVal, df_col8)
#   #       # print(df_col8)
#   #       
#   #     }else if(dim(DataVal)[2] == 9){
#   # 
#   #       DataVal <- DataVal %>%
#   #         dplyr::as_tibble() %>%
#   #         dplyr::mutate(across(everything(), as.character),
#   #                       core_name = FilesInput,
#   #                       file_name = file)
#   # 
#   #       df_col9 <- dplyr::bind_rows(DataVal, df_col9) %>%
#   #         dplyr::rename(series = X3,
#   #                       name = X4,
#   #                       nrc = X5,
#   #                       sex = X6,
#   #                       category = X7,
#   #                       district = X8,
#   #                       province = X9) %>%
#   #         dplyr::select(series, name, nrc, sex, category,
#   #                       district, province, core_name, file_name)
#   #       # print(df_col9)
#   #       
#   #     }else{
#   # 
#   #       print("--- Starts here ---")
#   #       
#   #       # print(file)
#   #       print(paste0("This ", file, " has has not be captured!"))
#   #       print(DataVal)
#   #       print(dim(DataVal))
#   # 
#   #       print("--- Ends here ---")
#   # 
#   #     }
#   # 
#   #     num_of_files <- num_of_files + 1
#   #     
#   #   }
#   # 
#   # }
#   # 
#   # print("--- Ends here ---")
#   # 
#   # print(num_of_files)
#   # 
#   # df_col8 =  df_col8 %>%
#   # dplyr::rename(series = X2,
#   #               name = X3,
#   #               nrc = X4,
#   #               sex = X5,
#   #               category = X6,
#   #               district = X7,
#   #               province = X8) %>%
#   # dplyr::select(series, name, nrc, sex, category,
#   #               district, province, core_name, file_name)
#   # 
#   # df_combinedcols_8_9 <- dplyr::bind_rows(df_col9, df_col8)
#   # 
#   # end.time2 <- Sys.time()
#   # time.taken2 <- end.time2 - start.time2
#   # print(time.taken2)
#   # 
#   # return(df_combinedcols_8_9)
#   
# }
# 
# 
# # dfXX <- get_tables_files(data_folder_path, "COPPERBELTPROVINCE")
# # dfXX
# 
# ## Run the loop based on all the available files ##
# for (fileList in filesList){
#   
#   # print(fileList)
#   DataVal <- get_tables_files(data_folder_path, fileList)
#   print(dim(DataVal))
#   
# }
# 
# 
# # # Function for idebtifying files that have not be captured
# # get_tables_not_captured <- function(FolderPathInput, FilesInput){
# #   
# #   files <- list.files(path = FolderPathInput, pattern = '.csv')
# #   # print(files)
# #   
# #   num_of_files <- 0
# #   
# #   ##
# #   lst_notcaptured <- NULL
# #   df_col9 <- NULL
# #   ##
# #   
# #   for (FileInput in FilesInput){
# #     
# #     df_filesTable <- get_file_names_table(FolderPathInput) %>% 
# #       dplyr::filter(corename == FileInput)
# #     
# #     fileList <- as.character(df_filesTable$file)
# #     
# #     for (file in fileList){
# #       
# #       file_path <- paste(FolderPathInput, file, sep = '/')
# #       
# #       DataVal <- readr::read_csv(file_path, col_names = F,
# #                                  show_col_types = FALSE)
# #       
# #       
# #       if(!dim(DataVal)[2] %in% c(8, 9)){
# #         
# #         # print(paste0("This ", file, " has has not be captured!"))
# #         
# #         lst_notcaptured <- append(lst_notcaptured, file)
# #         
# #       }
# #       
# #       num_of_files <- num_of_files + 1
# #       
# #     }
# #     
# #   }
# #   
# #   return(lst_notcaptured)
# # 
# # }
# # 
# # 
# # list_not_captured <- NULL
# # 
# # for (fileList in filesList){
# # 
# #   # print(fileList)
# #   DataVal <- get_tables_not_captured(data_folder_path, fileList)
# #   # print(dim(DataVal))
# #   print(DataVal)
# #   
# #   list_not_captured <- append(list_not_captured, DataVal)
# # 
# # }
# # 
# # list_not_captured
# 
# 
# 
# get_tables_data <- function(FolderPathInput, FilesInput){
#   
#   start.time2 <- Sys.time()
#   
#   # Extract data from pages with 8 and 9 columns
#   get_tables_files <- function(FolderPathInput, FilesInput){
#     
#     # start.time2 <- Sys.time()
#     
#     files <- list.files(path = FolderPathInput, pattern = '.csv')
#     # print(files)
#     
#     num_of_files <- 0
#     
#     ##
#     df_col8 <- NULL
#     df_col9 <- NULL
#     ##
#     
#     for (FileInput in FilesInput){
#       
#       df_filesTable <- get_file_names_table(FolderPathInput) %>% 
#         dplyr::filter(corename == FileInput)
#       
#       fileList <- as.character(df_filesTable$file)
#       
#       # print("--- Starts here ---")
#       # print(FileInput)
#       
#       for (file in fileList){
#         
#         file_path <- paste(FolderPathInput, file, sep = '/')
#         
#         DataVal <- readr::read_csv(file_path, col_names = F,
#                                    show_col_types = FALSE)
#         
#         if(dim(DataVal)[2] == 8){
#           
#           DataVal <- DataVal %>%
#             dplyr::as_tibble() %>%
#             dplyr::mutate(across(everything(), as.character),
#                           core_name = FilesInput,
#                           file_name = file)
#           
#           df_col8 <- dplyr::bind_rows(DataVal, df_col8)
#           # print(df_col8)
#           
#         }else if(dim(DataVal)[2] == 9){
#           
#           DataVal <- DataVal %>%
#             dplyr::as_tibble() %>%
#             dplyr::mutate(across(everything(), as.character),
#                           core_name = FilesInput,
#                           file_name = file)
#           
#           df_col9 <- dplyr::bind_rows(DataVal, df_col9) %>%
#             dplyr::rename(series = X3,
#                           name = X4,
#                           nrc = X5,
#                           sex = X6,
#                           category = X7,
#                           district = X8,
#                           province = X9) %>%
#             dplyr::select(series, name, nrc, sex, category,
#                           district, province, core_name, file_name)
#           # print(df_col9)
#           
#         }else{
#           
#           # print("--- Starts here ---")
#           
#           # # print(file)
#           # print(paste0("This ", file, " has has not be captured!"))
#           # print(DataVal)
#           # print(dim(DataVal))
#           
#           # print("--- Ends here ---")
#           
#         }
#         
#         num_of_files <- num_of_files + 1
#         
#       }
#       
#     }
#     
#     # print("--- Ends here ---")
#     # 
#     # print(num_of_files)
#     
#     df_col8 =  df_col8 %>%
#       dplyr::rename(series = X2,
#                     name = X3,
#                     nrc = X4,
#                     sex = X5,
#                     category = X6,
#                     district = X7,
#                     province = X8) %>%
#       dplyr::select(series, name, nrc, sex, category,
#                     district, province, core_name, file_name)
#     
#     df_combinedcols_8_9 <- dplyr::bind_rows(df_col9, df_col8)
#     
#     # end.time2 <- Sys.time()
#     # time.taken2 <- end.time2 - start.time2
#     # print(time.taken2)
#     
#     return(df_combinedcols_8_9)
#     
#   }
#   
#   ## Run the loop based on all the available files ##
#   df_val <- NULL
#   
#   for (fileList in filesList){
#     
#     # print(fileList)
#     DataVal <- get_tables_files(data_folder_path, fileList)
#     # print(dim(DataVal))
#     df_val <- dplyr::bind_rows(DataVal, df_val)
#     
#   }
#   
#   end.time2 <- Sys.time()
#   time.taken2 <- end.time2 - start.time2
#   print(time.taken2)
#   
#   return(df_val)
#   
# }
# 
# df_combined_1 <- get_tables_data(data_folder_path, fileList)
# 
# # save_to_file <- paste("./data/combined_output", 'combined_provincial_p1.csv', sep = '/')
# # readr::write_csv(df_combined_1, save_to_file)
# 
# sort(unique(tolower(df_combined_1$district)))
# 
# 
# 
# 
# get_list_of_tables_not_captured <- function(FolderPathInput, FilesInput){
#   
#   # Function for idebtifying files that have not be captured
#   get_tables_not_capturedX <- function(FolderPathInput, FilesInput){
#     
#     files <- list.files(path = FolderPathInput, pattern = '.csv')
#     # print(files)
#     
#     num_of_files <- 0
#     
#     ##
#     lst_notcaptured <- NULL
#     df_col9 <- NULL
#     ##
#     
#     for (FileInput in FilesInput){
#       
#       df_filesTable <- get_file_names_table(FolderPathInput) %>% 
#         dplyr::filter(corename == FileInput)
#       
#       fileList <- as.character(df_filesTable$file)
#       
#       for (file in fileList){
#         
#         file_path <- paste(FolderPathInput, file, sep = '/')
#         
#         DataVal <- readr::read_csv(file_path, col_names = F,
#                                    show_col_types = FALSE)
#         
#         
#         if(!dim(DataVal)[2] %in% c(8, 9)){
#           
#           # print(paste0("This ", file, " has has not be captured!"))
#           
#           lst_notcaptured <- append(lst_notcaptured, file)
#           
#         }
#         
#         num_of_files <- num_of_files + 1
#         
#       }
#       
#     }
#     
#     return(lst_notcaptured)
#     
#   }
#   
#   
#   list_not_capturedX <- NULL
#   
#   for (fileList in filesList){
#     
#     # print(fileList)
#     DataVal <- get_tables_not_capturedX(data_folder_path, fileList)
#     # print(dim(DataVal))
#     # print(DataVal)
#     
#     list_not_capturedX <- append(list_not_capturedX, DataVal)
#     
#   }
#   
#   # print(list_not_capturedX)
#   list_not_capturedX
#   
# }
# 
# 
# lst_1 <- get_list_of_tables_not_captured(data_folder_path, fileList)
# lst_1
# 
# 
# 
# df_centp_tbl_77 <- readr::read_csv(paste0(data_folder_path, "/", "CENTRALPROVINCE_tbl_77.csv"),
#                                    col_names = F, show_col_types = FALSE) %>% 
#   dplyr::mutate(nrc = stringr::str_replace_all(tolower(X3), "[[:alpha:]]", ""),
#                 nrc = stringr::str_replace_all(nrc, "[[:space:]]", ""),
#                 nrc = stringr::str_trim(nrc, side = "both"),
#                 name = stringr::str_replace_all(X3, "[[:digit:]]", ""),
#                 name = stringr::str_replace_all(name, "[[:punct:]]", ""),
#                 name = stringr::str_trim(name, side = "both"),
#                 
#                 core_name = "CENTRALPROVINCE",
#                 file_name = "CENTRALPROVINCE_tbl_77.csv"
#                 
#                 ) %>%
#   dplyr::rename(series = X2,
#                 # name = X3,
#                 # nrc = X4,
#                 sex = X4,
#                 category = X5,
#                 district = X6,
#                 province = X7) %>%
#   dplyr::select(series, name, nrc, sex, category,
#                 district, province, core_name, file_name)
# 
# df_centp_tbl_77
# 
# 
# 
# 
# df_muchp_tbl_26 <- readr::read_csv(paste0(data_folder_path, "/", "MUCHINGAPROVINCE_tbl_26.csv"),
#                                    col_names = F, show_col_types = FALSE) %>% 
#   dplyr::mutate(district = stringr::str_replace_all((X7), "MUCHINGA", ""),
#                 # nrc = stringr::str_replace_all(nrc, "[[:space:]]", ""),
#                 district = stringr::str_trim(district, side = "both"),
#                 province = stringr::str_replace_all(X7, "LAVUSHIMAN", ""),
#                 province = stringr::str_replace_all(province, "DA", ""),
#                 province = stringr::str_trim(province, side = "both"),
# 
#                 core_name = "MUCHINGAPROVINCE",
#                 file_name = "MUCHINGAPROVINCE_tbl_26.csv"
# 
#   ) %>%
#   dplyr::rename(series = X2,
#                 name = X3,
#                 nrc = X4,
#                 sex = X5,
#                 category = X6,
#                 # district = X6,
#                 # province = X7
#                 ) %>%
#   dplyr::select(series, name, nrc, sex, category,
#                 district, province, core_name, file_name)
# 
# df_muchp_tbl_26
# 
# 
# 
# df_muchp_tbl_27 <- readr::read_csv(paste0(data_folder_path, "/", "MUCHINGAPROVINCE_tbl_27.csv"),
#                                    col_names = F, show_col_types = FALSE) %>% 
#   dplyr::mutate(district = stringr::str_replace_all((X7), "MUCHINGA", ""),
#                 # district = stringr::str_replace_all(district, "MAFINGA", ""),
#                 district = stringr::str_trim(district, side = "both"),
#                 province = stringr::str_replace_all(X7, "LAVUSHIMANDA", ""),
#                 province = stringr::str_replace_all(province, "MAFINGA", ""),
#                 province = stringr::str_trim(province, side = "both"),
#                 
#                 core_name = "MUCHINGAPROVINCE",
#                 file_name = "MUCHINGAPROVINCE_tbl_27.csv"
#                 
#   ) %>%
#   dplyr::rename(series = X2,
#                 name = X3,
#                 nrc = X4,
#                 sex = X5,
#                 category = X6,
#                 # district = X6,
#                 # province = X7
#   ) %>%
#   dplyr::select(series, name, nrc, sex, category,
#                 district, province, core_name, file_name)
# 
# df_muchp_tbl_27
# 
# 
# 
# 
# df_west_tbl_130 <- readr::read_csv(paste0(data_folder_path, "/", "WESTERNPROVINCE_tbl_130.csv"),
#                                    col_names = F, show_col_types = FALSE) %>% 
#   dplyr::mutate(district = stringr::str_replace_all((X7), "WESTERN", ""),
#                 # district = stringr::str_replace_all(district, "MAFINGA", ""),
#                 district = stringr::str_trim(district, side = "both"),
#                 province = stringr::str_replace_all(X7, "SESHEKE", ""),
#                 province = stringr::str_replace_all(province, "SHANGOMBO", ""),
#                 province = stringr::str_trim(province, side = "both"),
#                 
#                 core_name = "WESTERNPROVINCE",
#                 file_name = "WESTERNPROVINCE_tbl_130.csv"
#                 
#   ) %>%
#   dplyr::rename(series = X2,
#                 name = X3,
#                 nrc = X4,
#                 sex = X5,
#                 category = X6,
#                 # district = X6,
#                 # province = X7
#   ) %>%
#   dplyr::select(series, name, nrc, sex, category,
#                 district, province, core_name, file_name)
# 
# df_west_tbl_130
# 
# 
# df_west_tbl_131 <- readr::read_csv(paste0(data_folder_path, "/", "WESTERNPROVINCE_tbl_131.csv"),
#                                    col_names = F, show_col_types = FALSE) %>% 
#   dplyr::mutate(district = stringr::str_replace_all((X7), "WESTERN", ""),
#                 # district = stringr::str_replace_all(district, "MAFINGA", ""),
#                 district = stringr::str_trim(district, side = "both"),
#                 province = stringr::str_replace_all(X7, "SHANGOMBO", ""),
#                 # province = stringr::str_replace_all(province, "SHANGOMBO", ""),
#                 province = stringr::str_trim(province, side = "both"),
#                 
#                 core_name = "WESTERNPROVINCE",
#                 file_name = "WESTERNPROVINCE_tbl_131.csv"
#                 
#   ) %>%
#   dplyr::rename(series = X2,
#                 name = X3,
#                 nrc = X4,
#                 sex = X5,
#                 category = X6,
#                 # district = X6,
#                 # province = X7
#   ) %>%
#   dplyr::select(series, name, nrc, sex, category,
#                 district, province, core_name, file_name)
# 
# df_west_tbl_131
# 
# 
# 
# 
# df_west_tbl_132 <- readr::read_csv(paste0(data_folder_path, "/", "WESTERNPROVINCE_tbl_132.csv"),
#                                    col_names = F, show_col_types = FALSE) %>% 
#   dplyr::mutate(district = stringr::str_replace_all((X7), "WESTERN", ""),
#                 # district = stringr::str_replace_all(district, "MAFINGA", ""),
#                 district = stringr::str_trim(district, side = "both"),
#                 province = stringr::str_replace_all(X7, "SHANGOMBO", ""),
#                 # province = stringr::str_replace_all(province, "SHANGOMBO", ""),
#                 province = stringr::str_trim(province, side = "both"),
#                 
#                 core_name = "WESTERNPROVINCE",
#                 file_name = "WESTERNPROVINCE_tbl_132.csv"
#                 
#   ) %>%
#   dplyr::rename(series = X2,
#                 name = X3,
#                 nrc = X4,
#                 sex = X5,
#                 category = X6,
#                 # district = X6,
#                 # province = X7
#   ) %>%
#   dplyr::select(series, name, nrc, sex, category,
#                 district, province, core_name, file_name)
# 
# df_west_tbl_132
# 
# 
# 
# 
# 
# df_west_tbl_133 <- readr::read_csv(paste0(data_folder_path, "/", "WESTERNPROVINCE_tbl_133.csv"),
#                                    col_names = F, show_col_types = FALSE) %>% 
#   dplyr::mutate(district = stringr::str_replace_all((X7), "WESTERN", ""),
#                 # district = stringr::str_replace_all(district, "MAFINGA", ""),
#                 district = stringr::str_trim(district, side = "both"),
#                 province = stringr::str_replace_all(X7, "SHANGOMBO", ""),
#                 # province = stringr::str_replace_all(province, "SHANGOMBO", ""),
#                 province = stringr::str_trim(province, side = "both"),
#                 
#                 core_name = "WESTERNPROVINCE",
#                 file_name = "WESTERNPROVINCE_tbl_133.csv"
#                 
#   ) %>%
#   dplyr::rename(series = X2,
#                 name = X3,
#                 nrc = X4,
#                 sex = X5,
#                 category = X6,
#                 # district = X6,
#                 # province = X7
#   ) %>%
#   dplyr::select(series, name, nrc, sex, category,
#                 district, province, core_name, file_name)
# 
# df_west_tbl_133
# 
# 
# 
# 
# df_west_tbl_134 <- readr::read_csv(paste0(data_folder_path, "/", "WESTERNPROVINCE_tbl_134.csv"),
#                                    col_names = F, show_col_types = FALSE) %>% 
#   dplyr::mutate(district = stringr::str_replace_all((X7), "WESTERN", ""),
#                 # district = stringr::str_replace_all(district, "MAFINGA", ""),
#                 district = stringr::str_trim(district, side = "both"),
#                 province = stringr::str_replace_all(X7, "SHANGOMBO", ""),
#                 province = stringr::str_replace_all(province, "SIKONGO", ""),
#                 province = stringr::str_trim(province, side = "both"),
#                 
#                 core_name = "WESTERNPROVINCE",
#                 file_name = "WESTERNPROVINCE_tbl_134.csv"
#                 
#   ) %>%
#   dplyr::rename(series = X2,
#                 name = X3,
#                 nrc = X4,
#                 sex = X5,
#                 category = X6,
#                 # district = X6,
#                 # province = X7
#   ) %>%
#   dplyr::select(series, name, nrc, sex, category,
#                 district, province, core_name, file_name)
# 
# df_west_tbl_134
# 
# 
# 
# df_combined_2 <- dplyr::bind_rows(df_centp_tbl_77, df_muchp_tbl_26) %>% 
#   dplyr::bind_rows(df_muchp_tbl_27) %>% 
#   dplyr::bind_rows(df_west_tbl_130) %>%
#   dplyr::bind_rows(df_west_tbl_131) %>%
#   dplyr::mutate(series = as.character(series)) %>% 
#   dplyr::bind_rows(df_west_tbl_132) %>%
#   dplyr::bind_rows(df_west_tbl_133) %>%
#   dplyr::mutate(series = as.numeric(series)) %>% 
#   dplyr::bind_rows(df_west_tbl_134) %>% 
#   dplyr::mutate(series = as.character(series))
# 
# df_combined_2
# 
# lst_1 == sort(unique((df_combined_2$file_name)))
# 
# save_to_file_2 <- paste("./data/combined_output", 'combined_provincial_p2.csv', sep = '/')
# readr::write_csv(df_combined_2, save_to_file_2)
# 
# sort(unique(tolower(df_combined_2$district)))
# 
# 
# ##
# 
# df_combined <- dplyr::bind_rows(df_combined_1, df_combined_2)
# save_to_file_combined <- paste("./data/combined_output", 'combined_provincial.csv', sep = '/')
# readr::write_csv(df_combined, save_to_file_combined)
# 
# 
# 
# # df11 <- df_centp_tbl_77 %>% 
# #   dplyr::mutate(nrc = stringr::str_replace_all(tolower(X3), "[[:alpha:]]", ""),
# #                 nrc = stringr::str_replace_all(nrc, "[[:space:]]", ""),
# #                 nrc = stringr::str_trim(nrc, side = "both"),
# #                 name = stringr::str_replace_all(X3, "[[:digit:]]", ""),
# #                 name = stringr::str_replace_all(name, "[[:punct:]]", ""),
# #                 name = stringr::str_trim(name, side = "both"))
# # 
# # df11
# 
# # get_list_of_tables_not_captured <- function(FolderPathInput, FilesInput){
# #   
# #   lst_notcaptured <- NULL
# #   
# #   for (fileList in filesList){
# #     
# #     DataVal <- get_tables_not_captured(FolderPathInput, FilesInput)
# #     # print(dim(DataVal))
# #     # print(DataVal)
# #     
# #     lst_notcaptured <- append(lst_notcaptured, DataVal)
# #     
# #   }
# #   
# #   print(lst_notcaptured)
# #   return(lst_notcaptured)
# #   
# # }
# # 
# # get_list_of_tables_not_captured(data_folder_path, fileList)

