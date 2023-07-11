# clean_svy_tbl <- function(df_svy, ...) {
#   
#   nss_items_pce <- df_svy |> 
#     summarise(across(c(...) | starts_with("pce"), 
#                      ~ survey_mean(.x , na.rm = T))) |> 
#     pivot_longer(everything(),
#                  names_to = "item_variable",
#                  values_to = "all_india") |> 
#     filter(!str_detect(item_variable, "_se"))
#   
#   ### by urbanicity
#   nss_items_urb_pce <- df_svy |> 
#     group_by(urbanicity) |> 
#     summarise(across(c(...) | starts_with("pce"), 
#                      ~ survey_mean(.x , na.rm = T))) |> 
#     pivot_longer(-urbanicity,
#                  names_to = "item_variable",
#                  values_to = "value") |> 
#     filter(!str_detect(item_variable, "_se")) |> 
#     mutate(urbanicity = case_when(urbanicity == "1" ~ "rural",
#                                   urbanicity == "2" ~ "urban")) |> 
#     pivot_wider(names_from = "urbanicity",
#                 values_from = "value") 
#   
#   nss_items_pce |> 
#     full_join(nss_items_urb_pce,
#               by = "item_variable")  |> 
#     mutate(itemcode = str_extract(item_variable, "\\d+"))
#   
#   
# }


clean_svy_tbl <- function(.df, .df_itemcodes = NULL, nss_round) {
  
  df <- .df |>
    # nss04_hh_cons |>
    summarise(n = n(),
              wtst = sum(wt),
              across(starts_with("mpce"), ~ weighted.mean(.x, w = wt, na.rm = T)),
              across(starts_with("pce"), ~ weighted.mean(.x, w = wt, na.rm = T))
    ) |>
    ungroup() |>
    mutate(nss_round = nss_round) |>
    pivot_longer(-c("nss_round", "n", "wtst", contains("mpce")),
                 names_to = "item_variable",
                 values_to = "mpce_item") |>
    mutate(itemcode = if_else(str_detect(item_variable, "\\d"),
                              str_remove(item_variable, "pce_"),
                              NA_character_)) |>
    select(nss_round, sample_size = n, wtst, everything()) 
  
  df_k <- .df |> 
    # nss04_hh_cons |>
    summarise(across(starts_with("kmpce"), ~ weighted.mean(.x, w = wt, na.rm = T)),
              across(starts_with("kpce"), ~ weighted.mean(.x, w = wt, na.rm = T))
    ) |>
    ungroup() |>
    pivot_longer(-c(contains("kmpce")),
                 names_to = "item_variable",
                 values_to = "kmpce_item") |>
    mutate(item_variable = str_replace(item_variable, "kpce_", "pce_")) |> 
    select(item_variable, everything()) 
  
  df <- df |> full_join(df_k, by = "item_variable")
  
  df_urb <- .df |>
    # nss04_hh_cons |>
    group_by(urbanicity) |>
    summarise(n = n(),
              wtst = sum(wt),
              across(starts_with("pce"), ~ weighted.mean(.x, w = wt, na.rm = T)),
              across(starts_with("mpce"), ~ weighted.mean(.x, w = wt, na.rm = T))
              ) |>
    ungroup() |>
    mutate(nss_round = nss_round) |>
    mutate(urbanicity = as.character(urbanicity)) |>
    pivot_longer(-c("urbanicity", "nss_round", "n", "wtst", contains("mpce")),
                 names_to = "item_variable",
                 values_to = "mpce_item") |>
    mutate(itemcode = if_else(str_detect(item_variable, "\\d"),
                              str_remove(item_variable, "pce_"),
                              NA_character_)) |>
    select(nss_round, urbanicity, sample_size = n, wtst, everything()) 
  
  df_k_urb <- .df |>
    # nss04_hh_cons |>
    group_by(urbanicity) |>
    summarise(across(starts_with("kmpce"), ~ weighted.mean(.x, w = wt, na.rm = T)),
              across(starts_with("kpce"), ~ weighted.mean(.x, w = wt, na.rm = T))
    ) |>
    ungroup() |>
    pivot_longer(-c("urbanicity", contains("kmpce")),
                 names_to = "item_variable",
                 values_to = "kmpce_item") |>
    mutate(item_variable = str_replace(item_variable, "kpce_", "pce_")) |> 
    select(item_variable, urbanicity, everything()) 

  df_urb <- df_urb |> full_join(df_k_urb, by = c("urbanicity", "item_variable"))
  
  dff <- bind_rows(df_urb,
                   df) |> 
    select(nss_round, urbanicity, sample_size, wtst, item_variable, itemcode, everything()) 
  
  if(!is.null(.df_itemcodes)) {
    
    dff |> 
      left_join(.df_itemcodes,
                by = "itemcode") |> 
      select(nss_round, urbanicity, sample_size, wtst, everything()) 
  }
  else {
    dff
  }
  
}

clean_svy_tbl_st <- function(.df, .df_itemcodes = NULL, nss_round) {
  
  df_st <- .df |>
    # nss04_hh_cons |>
    group_by(state) |>
    summarise(n = n(),
              wtst = sum(wt),
              across(starts_with("mpce"), ~ weighted.mean(.x, w = wt, na.rm = T)),
              across(starts_with("pce"), ~ weighted.mean(.x, w = wt, na.rm = T))
              ) |>
    ungroup() |>
    #mutate(nss_round = str_remove(.df, "") |> 
    mutate(nss_round = nss_round) |>
    pivot_longer(-c("state", "nss_round", "n", "wtst", contains("mpce")),
                 names_to = "item_variable",
                 values_to = "mpce_item") |>
    mutate(itemcode = if_else(str_detect(item_variable, "\\d"),
                              str_remove(item_variable, "pce_"),
                              NA_character_)) |>
    select(nss_round, state, sample_size = n, wtst, everything()) 
  
  df_k_st <- .df |>
    # nss04_hh_cons |> 
    group_by(state) |>
    summarise(n = n(),
              wtst = sum(wt),
              across(starts_with("kmpce"), ~ weighted.mean(.x, w = wt, na.rm = T)),
              across(starts_with("kpce"), ~ weighted.mean(.x, w = wt, na.rm = T))
    ) |>
    ungroup() |>
    #mutate(nss_round = str_remove(.df, "") |> 
    mutate(nss_round = nss_round) |>
    pivot_longer(-c("state", "nss_round", "n", "wtst", contains("mpce")),
                 names_to = "item_variable",
                 values_to = "kmpce_item") |>
    mutate(itemcode = if_else(str_detect(item_variable, "\\d"),
                              str_remove(item_variable, "pce_"),
                              NA_character_)) |>
    mutate(item_variable = str_replace(item_variable, "kpce_", "pce_")) |> 
    select(nss_round, state, sample_size = n, wtst, everything()) 
  
  df_states <- df_st |> 
    full_join(df_k_st |> 
                select(state, item_variable, contains("kmpce")), by = c("state", "item_variable"))
  
  df_st_urb <- .df |>
    # nss04_hh_cons |> 
    group_by(state, urbanicity) |>
    summarise(n = n(),
              wtst = sum(wt),
              across(starts_with("mpce"), ~ weighted.mean(.x, w = wt, na.rm = T)),
              across(starts_with("pce"), ~ weighted.mean(.x, w = wt, na.rm = T))
              ) |>
    ungroup() |>
    #mutate(nss_round = str_remove(.df, "") |> 
    mutate(nss_round = nss_round) |>
    mutate(urbanicity = as.character(urbanicity)) |>
    pivot_longer(-c("state", "urbanicity", "nss_round", "n", "wtst", contains("mpce")),
                 names_to = "item_variable",
                 values_to = "mpce_item") |>
    mutate(itemcode = if_else(str_detect(item_variable, "\\d"),
                              str_remove(item_variable, "pce_"),
                              NA_character_)) |>
    select(nss_round, state, urbanicity, sample_size = n, wtst, everything()) 
  
  df_k_st_urb <- .df |>
    # nss04_hh_cons |>
    group_by(state, urbanicity) |>
    summarise(n = n(),
              wtst = sum(wt),
              across(starts_with("kmpce"), ~ weighted.mean(.x, w = wt, na.rm = T)),
              across(starts_with("kpce"), ~ weighted.mean(.x, w = wt, na.rm = T))
    ) |>
    ungroup() |>
    #mutate(nss_round = str_remove(.df, "") |> 
    mutate(nss_round = nss_round) |>
    mutate(urbanicity = as.character(urbanicity)) |>
    pivot_longer(-c("state", "urbanicity", "nss_round", "n", "wtst", contains("mpce")),
                 names_to = "item_variable",
                 values_to = "kmpce_item") |>
    mutate(itemcode = if_else(str_detect(item_variable, "\\d"),
                              str_remove(item_variable, "pce_"),
                              NA_character_)) |>
    mutate(item_variable = str_replace(item_variable, "kpce_", "pce_")) |> 
    select(nss_round, state, urbanicity, sample_size = n, wtst, everything()) 
  
  df_states_urb <- df_st_urb |> 
    full_join(df_k_st_urb |> 
                select(state, urbanicity, item_variable, contains("kmpce")), by = c("state", "urbanicity", "item_variable"))
  
  dff_st <- bind_rows(df_states_urb,
                      df_states) |>
    select(nss_round, state, urbanicity, sample_size, wtst, everything()) 
  
  if(!is.null(.df_itemcodes)) {
    dff_st |> 
      left_join(.df_itemcodes,
                by = "itemcode") |> 
      select(nss_round, urbanicity, sample_size, wtst, everything()) 
  }
  
  else {
    dff_st
  }
  
}

clean_nss_stateid_to_state <- function(.df) {
  
  .df |> 
    mutate(state = case_when(state_id == "01" ~ "Jammu and Kashmir",
                             state_id == "02" ~ "Himachal Pradesh",
                             state_id == "03" ~ "Punjab",
                             state_id == "04" ~ "Chandigarh",
                             state_id == "05" ~ "Uttrakhand",
                             state_id == "06" ~ "Haryana",
                             state_id == "07" ~ "NCT of Delhi",
                             state_id == "08" ~ "Rajasthan",
                             state_id == "09" ~ "Uttar Pradesh",
                             state_id == "10" ~ "Bihar",
                             state_id == "11" ~ "Sikkim",
                             state_id == "12" ~ "Arunachal Pradesh",
                             state_id == "13" ~ "Nagaland",
                             state_id == "14" ~ "Manipur",
                             state_id == "15" ~ "Mizoram",
                             state_id == "16" ~ "Tripura",
                             state_id == "17" ~ "Meghalaya",
                             state_id == "18" ~ "Assam",
                             state_id == "19" ~ "West Bengal",
                             state_id == "20" ~ "Jharkhand",
                             state_id == "21" ~ "Odisha",
                             state_id == "22" ~ "Chattisgarh",
                             state_id == "23" ~ "Madhya Pradesh",
                             state_id == "24" ~ "Gujarat",
                             state_id == "25" ~ "Daman and Diu",
                             state_id == "26" ~ "Dadra and Nagar Haveli",
                             state_id == "27" ~ "Maharashtra",
                             state_id == "28" ~ "Andhra Pradesh",
                             state_id == "29" ~ "Karnataka",
                             state_id == "30" ~ "Goa",
                             state_id == "31" ~ "Lakshadweep",
                             state_id == "32" ~ "Kerala",
                             state_id == "33" ~ "Tamil Nadu",
                             state_id == "34" ~ "Puducherry",
                             state_id == "35" ~ "Andaman and Nicobar Islands",
                             state_id == "36" ~ "Telengana"
    ))
  
}

clean_nss_hh_cons_2011_prices <- function(.df, cpist_agyear, year) {
  
  nss_hh_cons <- .df |> 
    #nss14_hh_cons |> 
    mutate(year = as.double(year)) |> 
    select(year, hhid, wt, state, urbanicity, starts_with("mpce"), starts_with("pce")) |> 
    left_join(cpist_agyear, 
              by = c("state", "year")) |> 
    mutate(urbanicity = as.double(urbanicity)) |> 
    mutate(across(starts_with("pce") | starts_with("mpce"), 
                  ~ case_when(urbanicity == 1 ~ .x/deflator_u,
                              urbanicity == 2 ~ .x/deflator_r), 
                  .names = "k{.col}")) |> 
    mutate(urbanicity = as.character(urbanicity)) |> 
    select(year, hhid, wt, state, urbanicity, pline19r, pline19u, deflator_r, deflator_u, contains("mpce"), contains("pce"))
  
  return(nss_hh_cons)
  
}

########################### Item Codes ###########################

clean_nss04_itemcodes <- function(nss_folder = "nss/",
                                  itemcodes_file = "/nss2004ce/Schedule_61_1.xlsx") {
  
  #nss04_itemcodes <- 
    read_excel(paste0(nss_folder, itemcodes_file)#,
                                #col_names = c("itemcode", "item")
                                ) |> 
    clean_names() |> 
    mutate(subcat = if_else(str_ends(itemcode, "9"), "1", "0")) |> 
    mutate(itemcode_2d = str_sub(itemcode, 1, 2)) |> 
    mutate(itemcode = str_remove_all(itemcode, "\\*")) |>
    mutate(item = str_to_title(item)) |> 
    mutate(item = str_replace_all(item, "\\r", " "))
  
  #return(nss04_itemcodes)
  
}

clean_nss09_itemcodes <- function(nss_folder = "nss/", 
                                  itemcodes_file = "/nss2009ce_type1/Schedule 1.0 66 Type 1.xlsx") {
  
  nss09_itemcodes <- read_excel(paste0(nss_folder, itemcodes_file)#,
                                #col_names = c("itemcode", "item")
  ) |> 
    clean_names() |> 
    filter(!is.na(itemcode)) |> 
    mutate(subcat = if_else(str_ends(itemcode, "9"), "1", "0")) |> 
    mutate(itemcode_2d = str_sub(itemcode, 1, 2)) |> 
    mutate(itemcode = str_remove_all(itemcode, "\\*")) |>
    mutate(item = str_to_title(item))
  
  return(nss09_itemcodes)
  
}

clean_nss11_itemcodes <- function(nss_folder = "nss/", itemcodes_file = "/nss2011ce_type1/nss68_itemcodes.xlsx") {
  
  nss11_itemcodes <- read_excel(paste0(nss_folder, itemcodes_file)) |> 
    clean_names() |> 
    mutate(subcat = if_else(str_ends(itemcode, "9"), "1", "0")) |> 
    mutate(itemcode_2d = str_sub(itemcode, 1, 2)) |> 
    mutate(itemcode = str_remove_all(itemcode, "\\*")) |>
    mutate(item = str_to_title(item))
  
  return(nss11_itemcodes)
  
}

clean_nss14_itemcodes <- function(nss_folder = "nss/", 
                                  itemcodes_file = "/nss2014_services_durables/nss_item_codes.xlsx") {
  
  sheets <- c("transport_expenditure",
              "transport_expenditure_x",
              "misc_consumer_services",
              "repairs_maintenance",
              "food_hotels",
              "durable_goods"
  )
  
  freq <- c("monthly",
            "monthly",
            "monthly",
            "yearly",
            "weekly",
            "yearly"
  )
  
  nss14_itemcodes <- 
    map2_dfr(sheets, 
             freq,
             ~ read_excel(paste0(nss_folder, itemcodes_file),
                          .x,
                          skip = 1) |> 
               clean_names() |> 
               select(item, itemcode = code) |> 
               mutate(subcat = if_else(str_ends(itemcode, "9"), "1", "0")) |> 
               mutate(itemcode_2d = str_sub(itemcode, 1, 2)) |> 
               mutate(freq = .y)
    ) |> 
    mutate(item = case_when(itemcode == 689 ~ "expenditure on repairs and maintenance: sub-total(680-687)",
                            itemcode == 699 ~ "food expenditure in hotels and restaurants: sub-total (690-694)",
                            TRUE ~ item)) |> 
    mutate(itemcode = as.character(itemcode)) |>
    mutate(item = str_to_title(item))
  
  return(nss14_itemcodes)
  
}

########################### Common Items ###########################

create_common_items_14 <- function(nss14_itemcodes) {
  
  common_items_14 <- 
    tribble(
      ~ item_11_14, ~itemcode_14,
      "bus/tram fare",                               "502",     
      "bus/tram fare",                               "513",     
      "barber, beautician, etc.",                    "540",     
      "telephone charges: mobile ",                  "580",     
      "grinding charges",                            "651",     
      "tailor",                                      "669",     
      "bus/tram fare",                               "514",     
      "bus/tram fare",                               "512",     
      "cable TV",                                    "550",     
      "photography",                                 "600",     
      "bus/tram fare",                               "517",     
      "barber, beautician, etc.",                    "541",     
      "newspapers, periodicals",                     "608",     
      "bus/tram fare",                               "515",     
      "barber, beautician, etc.",                    "542",     
      "taxi, auto-rickshaw fare",                    "504",     
      "repair charges for non-durables",             "570",     
      "taxi, auto-rickshaw fare",                    "503",     
      "repair charges for non-durables",             "572",     
      "other conveyance expenses",                   "524",     
      "cinema, theatre",                             "603",     
      "priest",                                      "590",     
      "taxi, auto-rickshaw fare",                    "520",     
      "domestic services",                           "530",     
      "repair charges for non-durables",             "573",     
      "internet expenses",                           "583",     
      "pets",                                        "607",     
      "postage & telegram",                          "582",     
      "railway fare",                                "501",     
      "steamer, boat fare",                          "505",     
      "legal expenses",                              "687",     
      "other conveyance expenses",                   "507",     
      "railway fare",                                "511",     
      "domestic services",                           "531",     
      "taxi, auto-rickshaw fare",                    "518",     
      "club fees",                                   "602",     
      "horse cart fare",                             "523",     
      "washerman, laundry, ironing",                 "560",     
      "washerman, laundry, ironing",                 "561",     
      "domestic services",                           "532",     
      "bus/tram fare",                               "516",     
      "library charges",                             "605",     
      "air fare",                                    "500",     
      "steamer, boat fare",                          "521",     
      "air fare",                                    "510",     
      "domestic services",                           "533",     
      "VCD/ DVD hire (incl. instrument)",            "601",     
      "horse cart fare",                             "506",     
      "pressure cooker",                             "735",     
      "stainless steel utensils",                    "750",     
      "other furniture & fixtures",                  "768",     
      "other crockery & utensils",                   "753",     
      "mobile handset",                              "780",     
      "motor car, jeep",                             "703",     
      "air cooler",                                  "712",     
      "refrigerator",                                "715",     
      "casseroles, thermos, thermoware",             "752",     
      "plugs, switches & other electrical fittings", "792",     
      "electric fan",                                "710",     
      "motor cycle, scooter",                        "702",     
      "lantern, lamp, electric lampshade",           "790",     
      "bedstead",                                    "760",     
      "chair, stool, bench, table",                  "762",     
      "gold ornaments",                              "830",     
      "other metal utensils",                        "751",     
      "bathroom and sanitary equipment",             "771",     
      "other ornaments",                             "833",     
      "floor mattings",                              "766",     
      "paintings",                                   "767",     
      "sewing machine",                              "800",     
      "television",                                  "741",     
      "silver ornaments",                            "831",     
      "travel goods",                                "764",     
      "washing machine",                             "770",     
      "jewels, pearls",                              "832",     
      "bicycle",                                     "700",     
      "radio",                                       "740",     
      "almirah, dressing table",                     "761",     
      "other goods for recreation",                  "747",     
      "vcr",                                         "742",     
      "inverter",                                    "718",     
      "travel goods",                                "763",     
      "water purifier",                              "732",     
      "musical instruments",                         "745",     
      "camera & photographic equipment",             "743",     
      "other transport equipment",                   "704",     
      "telephone instrument (landline)",             "781",     
      "CD, DVD, etc",                                "744",     
      "PC/ laptop systems",                          "782"
    ) |> 
    mutate(item_11_14 = str_squish(item_11_14)) |> 
    left_join(nss14_itemcodes |> 
                select(itemcode, item_14 = item),
              by = c("itemcode_14" = "itemcode")) |> 
    mutate(year = "2014")
  
}

create_common_items_11 <- function(nss11_itemcodes) {
  
  tribble(
    ~item_11_14, ~itemcode_11,
    ####
    "newspapers, periodicals",                     "402",
    "library charges",                             "403",
    ####
    "cinema, theatre",                             "430",     
    "club fees",                                   "433",
    "photography",                                 "435",
    "VCD/ DVD hire (incl. instrument)",            "436",     
    "cable TV",                                    "437",
    ####
    "domestic services",                           "480",     
    "domestic services",                           "481",    
    "domestic services",                           "482",     
    "barber, beautician, etc.",                    "483",    
    "washerman, laundry, ironing",                 "484",     
    "tailor",                                      "485",    
    "grinding charges",                            "486",     
    "telephone charges: landline",                 "487",    
    "telephone charges: mobile",                   "488",     
    "postage & telegram",                          "490",    
    "priest",                                      "492",     
    "legal expenses",                              "493",    
    "repair charges for non-durables",             "494",     
    "pets",                                        "495",    
    "internet expenses",                           "496",     
    "air fare",                                    "500",    
    "railway fare",                                "501",     
    "bus/tram fare",                               "502",    
    "taxi, auto-rickshaw fare",                    "503",     
    "steamer, boat fare",                          "504",    
    "taxi, auto-rickshaw fare",                    "505",     
    "horse cart fare",                             "506",    
    "other conveyance expenses",                   "513",     
    "bedstead",                                    "550",    
    "almirah, dressing table",                     "551",     
    "chair, stool, bench, table",                  "552",    
    "travel goods",                                "553",     
    "floor mattings",                              "555",    
    "paintings",                                   "556",     
    "other furniture & fixtures",                  "557",    
    "radio",                                       "560",     
    "television",                                  "561",    
    "vcr",                                         "562",     
    "camera & photographic equipment",             "563",    
    "CD, DVD, etc",                                "564",     
    "musical instruments",                         "565",    
    "other goods for recreation",                  "566",     
    "stainless steel utensils",                    "570",    
    "other metal utensils",                        "571",     
    "casseroles, thermos, thermoware",             "572",    
    "other crockery & utensils",                   "573",     
    "electric fan",                                "580",    
    "air cooler",                                  "581",     
    "inverter",                                    "582",    
    "lantern, lamp, electric lampshade",           "583",     
    "sewing machine",                              "584",    
    "washing machine",                             "585",     
    "pressure cooker",                             "587",    
    "refrigerator",                                "588",     
    "water purifier",                              "590",    
    "bicycle",                                     "600",     
    "motor cycle, scooter",                        "601",    
    "motor car, jeep",                             "602",     
    "other transport equipment",                   "604",    
    "PC/ laptop systems",                          "622",     
    "mobile handset",                              "623",    
    "telephone instrument (landline)",             "624",     
    "bathroom and sanitary equipment",             "630",    
    "plugs, switches & other electrical fittings", "631",     
    "gold ornaments",                              "640",    
    "silver ornaments",                            "641",     
    "jewels, pearls",                              "642",    
    "other ornaments",                             "643"
  ) |> 
    mutate(item_11_14 = str_squish(item_11_14)) |> 
    left_join(nss11_itemcodes |> 
                select(itemcode, item_11 = item),
              by = c("itemcode_11" = "itemcode")) |> 
    mutate(year = "2011")
  
}

create_common_items_04 <- function(nss04_itemcodes) {
  
  tribble(
    ~item_11_14, ~itemcode_04,
    "newspapers, periodicals",                     "401",    
    "library charges",                             "402",     
    "cinema, theatre",                             "430",     
    "club fees",                                   "433",
    "photography",                                 "435",
    "VCD/ DVD hire (incl. instrument)",            "436",     
    "cable TV",                                    "437",    
    "domestic services",                           "480",     
    "domestic services",                           "481",    
    "barber, beautician, etc.",                    "482",    
    "washerman, laundry, ironing",                 "483",     
    "tailor",                                      "484",    
    "grinding charges",                            "491",     
    #"telephone charges: landline",                 "487",
    # note - no landline, mobile sepratation in 2004
    # assigning to mobile
    "telephone charges: mobile",                   "488",     
    "postage & telegram",                          "487",    
    "priest",                                      "485",     
    "legal expenses",                              "486",    
    "repair charges for non-durables",             "490",     
    "pets",                                        "493",    
    # No internet expenses item in 2004.
    #"internet expenses",                           "496",
    #################
    "air fare",                                    "500",    
    "railway fare",                                "501",     
    "bus/tram fare",                               "502",    
    "taxi, auto-rickshaw fare",                    "503",     
    "steamer, boat fare",                          "504",    
    "taxi, auto-rickshaw fare",                    "505",     
    "horse cart fare",                             "506",    
    "other conveyance expenses",                   "513",
    #################
    "bedstead",                                    "550",    
    "almirah, dressing table",                     "551",     
    "chair, stool, bench, table",                  "552",    
    "travel goods",                                "553",     
    "floor mattings",                              "555",    
    "paintings",                                   "556",     
    "other furniture & fixtures",                  "557",
    #################
    "radio",                                       "561",     
    "television",                                  "562",    
    "vcr",                                         "563",     
    "camera & photographic equipment",             "564",    
    "CD, DVD, etc",                                "565",     
    "musical instruments",                         "567",    
    "other goods for recreation",                  "568", 
    #################
    "stainless steel utensils",                    "580",    
    "other metal utensils",                        "581",     
    "casseroles, thermos, thermoware",             "582",    
    "other crockery & utensils",                   "583",     
    "electric fan",                                "590",    
    "air cooler",                                  "591",     
    #"inverter",                                    "582",    
    "lantern, lamp, electric lampshade",           "592",     
    "sewing machine",                              "593",    
    "washing machine",                             "594",     
    "pressure cooker",                             "596",    
    "refrigerator",                                "597",     
    #"water purifier",                              "590", 
    #################
    "bicycle",                                     "610",     
    "motor cycle, scooter",                        "611",    
    "motor car, jeep",                             "612",     
    "other transport equipment",                   "614", 
    #################
    "PC/ laptop systems",                          "632",     
    "mobile handset",                              "633",    
    "telephone instrument (landline)",             "634",
    #################
    "bathroom and sanitary equipment",             "640",    
    "plugs, switches & other electrical fittings", "641",
    #################
    "gold ornaments",                              "570",    
    "silver ornaments",                            "571",     
    "jewels, pearls",                              "572",    
    "other ornaments",                             "573"
  ) |> 
    mutate(item_11_14 = str_squish(item_11_14)) |> 
    left_join(nss04_itemcodes |> 
                select(itemcode, item_04 = item),
              by = c("itemcode_04" = "itemcode")) |> 
    mutate(year = "2004")
  
}

create_common_items_09 <- function(nss09_itemcodes) {
  
  common_items_09 <- 
    tribble(
      ~item_11_14, ~itemcode_09,
      ####
      "newspapers, periodicals",                     "402",
      "library charges",                             "403",
      ####
      "cinema, theatre",                             "430",     
      "club fees",                                   "433",
      "photography",                                 "435",
      "VCD/ DVD hire (incl. instrument)",            "436",     
      "cable TV",                                    "437",
      ####
      "domestic services",                           "480",     
      "domestic services",                           "481",    
      "domestic services",                           "482",     
      "barber, beautician, etc.",                    "483",    
      "washerman, laundry, ironing",                 "484",     
      "tailor",                                      "485",    
      "grinding charges",                            "486",     
      "telephone charges: landline",                 "487",    
      "telephone charges: mobile",                   "488",     
      "postage & telegram",                          "490",    
      "priest",                                      "492",     
      "legal expenses",                              "493",    
      "repair charges for non-durables",             "494",     
      "pets",                                        "495",    
      #"internet expenses",                           "496",     
      "air fare",                                    "500",    
      "railway fare",                                "501",     
      "bus/tram fare",                               "502",    
      "taxi, auto-rickshaw fare",                    "503",     
      "steamer, boat fare",                          "504",    
      "taxi, auto-rickshaw fare",                    "505",     
      "horse cart fare",                             "506",    
      "other conveyance expenses",                   "513",     
      "bedstead",                                    "550",    
      "almirah, dressing table",                     "551",     
      "chair, stool, bench, table",                  "552",    
      "travel goods",                                "553",     
      "floor mattings",                              "555",    
      "paintings",                                   "556",     
      "other furniture & fixtures",                  "557",    
      "radio",                                       "560",     
      "television",                                  "561",    
      "vcr",                                         "562",     
      "camera & photographic equipment",             "563",    
      "CD, DVD, etc",                                "564",     
      "musical instruments",                         "565",    
      "other goods for recreation",                  "566",     
      "stainless steel utensils",                    "570",    
      "other metal utensils",                        "571",     
      "casseroles, thermos, thermoware",             "572",    
      "other crockery & utensils",                   "573",     
      "electric fan",                                "580",    
      "air cooler",                                  "581",     
      #"inverter",                                    "582",    
      "lantern, lamp, electric lampshade",           "582",     
      "sewing machine",                              "583",    
      "washing machine",                             "584",     
      "pressure cooker",                             "586",    
      "refrigerator",                                "587",     
      "water purifier",                              "588",    
      "bicycle",                                     "600",     
      "motor cycle, scooter",                        "601",    
      "motor car, jeep",                             "602",     
      "other transport equipment",                   "604",    
      "PC/ laptop systems",                          "622",     
      "mobile handset",                              "623",    
      "telephone instrument (landline)",             "624",     
      "bathroom and sanitary equipment",             "630",    
      "plugs, switches & other electrical fittings", "631",     
      "gold ornaments",                              "640",    
      "silver ornaments",                            "641",     
      "jewels, pearls",                              "642",    
      "other ornaments",                             "643"
    ) |> 
    mutate(item_11_14 = str_squish(item_11_14)) |> 
    left_join(nss09_itemcodes |> 
                select(itemcode, item_09 = item),
              by = c("itemcode_09" = "itemcode"))
  
  return(common_items_09)
  
}


########################### MICRO DATA ###########################

clean_nss04_hh_cons <- function(nss_folder = "nss/", cpist_agyear) {
  
  nss04_block3_p1_hh <- read_dta(paste0(nss_folder, "/nss2004ce/data/Block 3 Part 1_Household Characteristics.dta")) |>  
    clean_names() |>
    rename(
      urbanicity = sector,
      hhsize = b3_q1,
      nic2004 = b3_q2,
      nco2008 = b3_q3,
      hhtype = b3_q4,
      religion = b3_q5,
      caste = b3_q6
    ) |> 
    mutate(weight = if_else(nss != nsc, mlt/200, mlt/100),
           wt = weight*hhsize) 
  
  nss04_block3_p2_hh <- read_dta(paste0(nss_folder, "/nss2004ce/data/Block 3 Part 2_Household Characteristics.dta")) |>  
    clean_names() |>
    rename(
      urbanicity = sector,
      mpce_mrp = b3_q28,
      mpce_urp = b3_q29
    ) |>  
    select(hhid, mpce_mrp, mpce_urp)
  
  nss04_block3_hh <- nss04_block3_p1_hh |> 
    left_join(nss04_block3_p2_hh, by = "hhid")
  
  nss04_block5 <- read_dta(paste0(nss_folder, "/nss2004ce/data/Block 5_Monthly consumption of food, pan, tobacco and intoxicants.dta")) |> 
    clean_names() |> 
    rename(
      hhid = hhid,
      itemcode = b5_q1,
      hp_quantity = b5_q3,
      hp_hce = b5_q4,
      quantity = b5_q5,
      hce = b5_q6
    )
  
  nss04_block5_hh <- nss04_block5 |> 
    select(hhid, itemcode, hce = hce) |> 
    pivot_wider(names_from = "itemcode",
                values_from = c("hce"),
                names_glue = "{.value}_{itemcode}",
                values_fill = 0) |> 
    mutate(across(starts_with("hce"), ~ if_else(is.na(.x), 0, .x)))
  
  
  nss04_block6 <- read_dta(paste0(nss_folder, "/nss2004ce/data/Block 6_Monthly consumption of fuel & light.dta")) |> 
    clean_names() |> 
    rename(
      hhid = hhid,
      itemcode = b6_q1,
      hp_quantity = b6_q3,
      hp_hce = b6_q4,
      quantity = b6_q5,
      hce = b6_q6
    )
  
  nss04_block6_hh <- nss04_block6 |> 
    select(hhid, itemcode, hce = hce) |> 
    pivot_wider(names_from = "itemcode",
                values_from = c("hce"),
                names_glue = "{.value}_{itemcode}",
                values_fill = 0) |> 
    mutate(across(starts_with("hce"), ~ if_else(is.na(.x), 0, .x)))
  
  
  nss04_block7 <- read_dta(paste0(nss_folder, "nss2004ce/data/Block 7_Consumption of clothing.dta")) |> 
    clean_names() |> 
    rename(
      hhid = hhid,
      itemcode = b7_q1,
      quantity = b7_q3,
      hce = b7_q4,
      quantity365 = b7_q5,
      hce365 = b7_q6
    )
  
  nss04_block7_hh <- nss04_block7 |> 
    select(hhid, itemcode, hce = hce, hce365) |> 
    pivot_wider(names_from = "itemcode",
                values_from = c("hce", "hce365"),
                names_glue = "{.value}_{itemcode}",
                values_fill = 0) |> 
    mutate(across(starts_with("hce"), ~ if_else(is.na(.x), 0, .x)))
  
  nss04_block8 <- read_dta(paste0(nss_folder, "nss2004ce/data/Block 8_Consumption of footwear.dta")) |> 
    clean_names() |> 
    rename(
      hhid = hhid,
      itemcode = b8_q1,
      quantity = b8_q3,
      hce = b8_q4,
      quantity365 = b8_q5,
      hce365 = b8_q6
    )
  
  nss04_block8_hh <- nss04_block8 |> 
    select(hhid, itemcode, hce = hce, hce365) |> 
    pivot_wider(names_from = "itemcode",
                values_from = c("hce", "hce365"),
                names_glue = "{.value}_{itemcode}",
                values_fill = 0) |> 
    mutate(across(starts_with("hce"), ~ if_else(is.na(.x), 0, .x)))
  
  
  nss04_block9 <- read_dta(paste0(nss_folder, "nss2004ce/data/Block 9_Expenditure on education and medical (institutional) goods and services.dta")) |> 
    clean_names() |> 
    rename(
      hhid = hhid,
      itemcode = b9_q1,
      hce = b9_q3,
      hce365 = b9_q4
    )
  
  nss04_block9_hh <- nss04_block9 |> 
    select(hhid, itemcode, hce = hce, hce365) |> 
    pivot_wider(names_from = "itemcode",
                values_from = c("hce", "hce365"),
                names_glue = "{.value}_{itemcode}",
                values_fill = 0) |> 
    mutate(across(starts_with("hce"), ~ if_else(is.na(.x), 0, .x)))
  
  
  nss04_block10 <- read_dta(paste0(nss_folder, "nss2004ce/data/Block 10_Monthly expenditure on miscellaneous goods and services including medical (non-institutional), rents and taxes.dta")) |> 
    clean_names() |> 
    rename(
      hhid = hhid,
      itemcode = b10_q1,
      hce = b10_q4
    )
  
  nss04_block10_hh <- nss04_block10 |> 
    select(hhid, itemcode, hce = hce) |> 
    pivot_wider(names_from = "itemcode",
                values_from = c("hce"),
                names_glue = "{.value}_{itemcode}",
                values_fill = 0) |> 
    mutate(across(starts_with("hce"), ~ if_else(is.na(.x), 0, .x)))
  
  
  nss04_block11 <- read_dta(paste0(nss_folder, "nss2004ce/data/Block 11_Expenditure for purchase and construction (including repair and maintenance) of durable goods for domestic use.dta")) |> 
    clean_names() |> 
    rename(
      hhid = hhid,
      itemcode = b11_q1,
      hce = b11_q7,
      hce365 = b11_q14
    )
  
  nss04_block11_hh <- nss04_block11 |> 
    select(hhid, itemcode, hce = hce, hce365) |> 
    pivot_wider(names_from = "itemcode",
                values_from = c("hce", "hce365"),
                names_glue = "{.value}_{itemcode}",
                values_fill = 0) |> 
    mutate(across(starts_with("hce"), ~ if_else(is.na(.x), 0, .x)))
  
  nss04_hh_cons <- 
    nss04_block3_hh |> 
    left_join(nss04_block5_hh,
              by = "hhid") |> 
    left_join(nss04_block6_hh,
              by = "hhid") |> 
    left_join(nss04_block7_hh,
              by = "hhid") |> 
    left_join(nss04_block8_hh,
              by = "hhid") |> 
    left_join(nss04_block9_hh,
              by = "hhid") |> 
    left_join(nss04_block10_hh,
              by = "hhid") |> 
    left_join(nss04_block11_hh,
              by = "hhid") |> 
    mutate(across(starts_with("hce_"), 
                  ~ if_else(is.na(.x), 0, .x))) |> 
    mutate(across(starts_with("hce_"), 
                  ~ .x/hhsize, 
                  .names = "pce_{.col}")) |> 
    rename(state_id = state) |>  
    clean_nss_stateid_to_state()
  
  names(nss04_hh_cons) <- names(nss04_hh_cons) |> str_replace("pce_hce", "pce")
  
  nss04_hh_cons <- nss04_hh_cons |> 
    clean_nss_hh_cons_2011_prices(cpist_agyear, year = "2004")

  return(nss04_hh_cons)
  
}

clean_nss09t1_hh_cons <- function(nss_folder = "nss/", cpist_agyear) {
  
  nss09t1_block3_hh <- read_dta(paste0(nss_folder, "nss2009ce_type1/data/Household Characteristics.dta")) |>  
    clean_names() |>
    rename(
      urbanicity = sector,
      hhid = hh_id,
      hhsize = hh_size,
      nic2004 = nic_2004,
      nco2008 = nco_2004,
      hhtype = hh_type,
      religion = religion,
      caste = social_group#,
      # hce_purchased = b3_q14,
      # hce_home_produce = b3_q15,
      # hce_receipts = b3_q16,
      # hce_gifts = b3_q17,
      # hce_free_collection = b3_q18,
      # hce_total = b3_q19,
      # mpce = b3_q20,
      # hce_ration = b3_q23
    ) |> 
    mutate(weight = if_else(nss != nsc, mlt/200, mlt/100),
           wt = weight*hhsize) |> 
    mutate(across(starts_with("hce"), as.double)) #|> 
  #mutate(across(c("mpce_mrp", "mpce_urp"), ~ .x/1e2)) 
  
  nss09t1_block5 <- read_dta(paste0(nss_folder, "nss2009ce_type1/data/Consumption of cereals, pulses, milk and milk products, sugar and salt during the last 30 days.dta")) |> 
    clean_names() |> 
    rename(
      hhid = hh_id,
      itemcode = item_code,
      quantity = total_quantity,
      hce = total_value,
      source = source_code
    )
  
  nss09t1_block5_hh <- nss09t1_block5 |> 
    select(hhid, itemcode, hce = hce) |> 
    pivot_wider(names_from = "itemcode",
                values_from = c("hce"),
                names_glue = "{.value}_{itemcode}",
                values_fill = 0) |> 
    mutate(across(starts_with("hce"), ~ if_else(is.na(.x), 0, .x)))
  
  
  #TODO - Block 6
  
  nss09t1_block7_8 <- read_dta(paste0(nss_folder, "nss2009ce_type1/data/Consumption of clothing, bedding and footwear during last 30  and 365 days.dta")) |> 
    clean_names() |> 
    rename(
      hhid = hh_id,
      itemcode = item_code,
      quantity = last_30days_quantity,
      hce = last_30days_value,
      quantity365 = last_365days_quantity,
      hce365 = last_365days_value
    )
  
  nss09t1_block7_8_hh <- nss09t1_block7_8 |> 
    select(hhid, itemcode, hce, hce365) |> 
    mutate(hce365 = hce365*(30/365)) |> 
    pivot_wider(names_from = "itemcode",
                values_from = c("hce", "hce365"),
                names_glue = "{.value}_{itemcode}",
                values_fill = 0) |> 
    mutate(across(starts_with("hce"), ~ if_else(is.na(.x), 0, .x)))
  
  
  nss09t1_block9 <- read_dta(paste0(nss_folder, "nss2009ce_type1/data/Expenditure on education and medical (institutional) goods and services.dta")) |> 
    clean_names() |> 
    rename(
      hhid = hh_id,
      itemcode = item_code,
      hce = last_30days_value,
      hce365 = last_365days_value
    )
  
  nss09t1_block9_hh <- nss09t1_block9 |> 
    select(hhid, itemcode, hce, hce365) |> 
    mutate(hce365 = hce365*(30/365)) |> 
    pivot_wider(names_from = "itemcode",
                values_from = c("hce", "hce365"),
                names_glue = "{.value}_{itemcode}",
                values_fill = 0) |> 
    mutate(across(starts_with("hce"), ~ if_else(is.na(.x), 0, .x)))
  
  
  nss09t1_block10 <- read_dta(paste0(nss_folder, "nss2009ce_type1/data/Expenditure on miscellaneous goods and services including medical (non-institutional), rents and taxes during the last 30 days.dta")) |> 
    clean_names() |> 
    rename(
      hhid = hh_id,
      #psu = vill_blk_slno,
      itemcode = item_code,
      hce = value
    ) |> 
    mutate(item_subcat = if_else(str_ends(itemcode, "9"), "1", "0")) 
  
  nss09t1_block10_hh <- nss09t1_block10 |> 
    select(hhid, itemcode, hce) |> 
    pivot_wider(names_from = "itemcode",
                values_from = "hce",
                names_glue = "{.value}_{itemcode}",
                values_fill = 0) |> 
    mutate(across(starts_with("hce_"), ~ if_else(is.na(.x), 0, .x)))
  
  
  nss09t1_block11 <- read_dta(paste0(nss_folder, "nss2009ce_type1/data/Expenditure for purchase and construction (including repair and maintenance) of durable goods for domestic use.dta")) |> 
    clean_names() |> 
    rename(
      hhid = hh_id,
      #psu = vill_blk_slno,
      itemcode = item_code,
      hce_dur_365 = total_expenditure_365days,
      hce_dur_30 = total_expenditure_30days
    ) |> 
    mutate(item_subcat = if_else(str_ends(itemcode, "9"), "1", "0")) 
  
  nss09t1_block11_hh <- nss09t1_block11 |> 
    mutate(hce = hce_dur_365*(30/365)) |> 
    select(hhid, itemcode, hce) |> 
    pivot_wider(names_from = c("itemcode"),
                values_from = "hce",
                names_glue = "{.value}_{itemcode}",
                values_fill = 0) |> 
    mutate(across(starts_with("hce_"), ~ if_else(is.na(.x), 0, .x)))
  
  nss09t1_hh_cons <- nss09t1_block3_hh |> 
    full_join(
      nss09t1_block5_hh,
      by = "hhid"
    ) |> 
    # full_join(
    #   nss09t1_block6_hh,
    #   by = "hhid"
    # ) |> 
    full_join(
      nss09t1_block7_8_hh,
      by = "hhid"
    ) |> 
    full_join(
      nss09t1_block9_hh,
      by = "hhid"
    ) |> 
    full_join(
      nss09t1_block10_hh,
      by = "hhid"
    ) |> 
    full_join(
      nss09t1_block11_hh,
      by = "hhid"
    ) |> 
    # full_join(
    #   nss09t1_block12_hh,
    #   by = "hhid"
    # ) |> 
    mutate(across(starts_with("hce_"), 
                  ~ if_else(is.na(.x), 0, .x))) |> 
    mutate(across(starts_with("hce_"), 
                  ~ .x/hhsize, 
                  .names = "pce_{.col}")) |> 
    rename(state_id = state) |> 
    clean_nss_stateid_to_state()
  
  names(nss09t1_hh_cons) <- names(nss09t1_hh_cons) |> str_replace("pce_hce", "pce")
  
  nss09t1_hh_cons <- nss09t1_hh_cons |> 
    clean_nss_hh_cons_2011_prices(cpist_agyear, year = "2009")
  
  return(nss09t1_hh_cons)
  
}

clean_nss09t2_hh_cons <- function(nss_folder = "nss/", cpist_agyear) {
  
  nss09t2_block3_hh <- read_dta(paste0(nss_folder, "nss2009ce_type2/data/Household Characteristics.dta")) |>  
    clean_names() |>
    rename(
      urbanicity = sector,
      hhid = hh_id,
      hhsize = hh_size,
      nic2004 = nic_2004,
      nco2008 = nco_2004,
      hhtype = hh_type,
      religion = religion,
      caste = social_group#,
      # hce_purchased = b3_q14,
      # hce_home_produce = b3_q15,
      # hce_receipts = b3_q16,
      # hce_gifts = b3_q17,
      # hce_free_collection = b3_q18,
      # hce_total = b3_q19,
      # mpce = b3_q20,
      # hce_ration = b3_q23
    ) |> 
    mutate(weight = if_else(nss != nsc, mlt/200, mlt/100),
           wt = weight*hhsize) |> 
    mutate(across(starts_with("hce"), as.double)) #|> 
  #mutate(across(c("mpce_mrp", "mpce_urp"), ~ .x/1e2)) 
  
  #Block 5 - Monthly household expenditure on cereals, pulses, milk, sugar and salt
  
  #Itemcodes - 101 to 359
  
  #TODO - Itemcodes 190-339 are possibly 7 day reference period ?
  
  nss09t2_block5 <- read_dta(paste0(nss_folder, "nss2009ce_type2/data/Consumption of cereals, pulses, milk and milk products, sugar and salt during the last 30 days or 7 days.dta")) |> 
    clean_names() |> 
    rename(
      hhid = hh_id,
      itemcode = item_code,
      quantity = total_quantity,
      hce = total_value,
      source = source_code
    )
  
  nss09t2_block5_hh <- nss09t2_block5 |> 
    select(hhid, itemcode, hce = hce) |> 
    pivot_wider(names_from = "itemcode",
                values_from = c("hce"),
                names_glue = "{.value}_{itemcode}",
                values_fill = 0) |> 
    mutate(across(starts_with("hce"), ~ if_else(is.na(.x), 0, .x)))
  
  # TODO - Block 6 - NA
  
  # consumption of energy (fuel, light & household appliances) during the last 30 days ended on
  
  # nss11t2_block6 <- read_dta(paste0(nss_folder, "/nss2011ce_type1/data/.dta")) |> 
  #   clean_names() |> 
  #   rename(
  #     itemcode = item_code,
  #     quantity = last_30days_quantity,
  #     hce = last_30days_value,
  #     quantity365 = last_365days_quantity,
  #     hce365 = last_365days_value#,
  #     #source = source_code
  #     )
  # 
  # nss11t2_block6_hh <- nss11t2_block6 |> 
  #   select(hhid, itemcode, hce = hce) |> 
  #   pivot_wider(names_from = "itemcode",
  #               values_from = c("hce"),
  #               names_glue = "{.value}_{itemcode}",
  #               values_fill = 0) |> 
  #   mutate(across(starts_with("hce"), ~ if_else(is.na(.x), 0, .x)))
  
  #Block 7, 8 - Consumption of clothing, bedding and footwear during last 30 and 365 days
  
  #Itemcodes - 360 to 399
  
  nss09t2_block7_8 <- read_dta(paste0(nss_folder, "nss2009ce_type2/data/Consumption of clothing, bedding and footwear during last  365 days.dta")) |> 
    clean_names() |> 
    rename(
      hhid = hh_id,
      itemcode = item_code,
      quantity365 = last_365days_quantity,
      hce365 = last_365days_value
    )
  
  nss09t2_block7_8_hh <- nss09t2_block7_8 |> 
    select(hhid, itemcode, hce = hce365) |> 
    mutate(hce = hce*(30/365)) |> 
    pivot_wider(names_from = "itemcode",
                values_from = "hce",
                names_glue = "{.value}_{itemcode}",
                values_fill = 0) |> 
    mutate(across(starts_with("hce"), ~ if_else(is.na(.x), 0, .x)))
  
  #Block 9 - Expenditure on education and medical (institutional) goods and services
  
  #Itemcodes - 400 to 419
  
  nss09t2_block9 <- read_dta(paste0(nss_folder, "nss2009ce_type2/data/Expenditure on education and medical (institutional) goods and services.dta")) |> 
    clean_names() |> 
    rename(
      hhid = hh_id,
      itemcode = item_code,
      hce365 = t_value
    )
  
  nss09t2_block9_hh <- nss09t2_block9 |> 
    select(hhid, itemcode, hce = hce365) |> 
    mutate(hce = hce*(30/365)) |> 
    pivot_wider(names_from = "itemcode",
                values_from = "hce",
                names_glue = "{.value}_{itemcode}",
                values_fill = 0) |> 
    mutate(across(starts_with("hce"), ~ if_else(is.na(.x), 0, .x)))
  
  # Block 10 - Expenditure on miscellaneous goods and services including medical (non-institutional), rents and taxes during the last 30 days
  
  # Itemcodes - 420 to 549
  
  nss09t2_block10 <- read_dta(paste0(nss_folder, "nss2009ce_type2/data/Expenditure on miscellaneous goods and services including medical (non-institutional), rents and taxes during the last 30 days.dta")) |> 
    clean_names() |> 
    rename(
      hhid = hh_id,
      #psu = vill_blk_slno,
      itemcode = item_code,
      hce = value
    ) |> 
    mutate(item_subcat = if_else(str_ends(itemcode, "9"), "1", "0")) 
  
  nss09t2_block10_hh <- nss09t2_block10 |> 
    select(hhid, itemcode, hce) |> 
    pivot_wider(names_from = "itemcode",
                values_from = "hce",
                names_glue = "{.value}_{itemcode}",
                values_fill = 0) |> 
    mutate(across(starts_with("hce_"), ~ if_else(is.na(.x), 0, .x)))
  
  #Block 11 - Expenditure on miscellaneous goods and services including medical (non-institutional), rents and taxes during the last 30 days
  
  #Itemcodes - 550 to 659
  
  nss09t2_block11 <- read_dta(paste0(nss_folder, "nss2009ce_type1/data/Expenditure for purchase and construction (including repair and maintenance) of durable goods for domestic use.dta")) |> 
    clean_names() |> 
    rename(
      hhid = hh_id,
      #psu = vill_blk_slno,
      itemcode = item_code,
      hce_dur_365 = total_expenditure_365days,
      hce_dur_30 = total_expenditure_30days
    ) |> 
    mutate(item_subcat = if_else(str_ends(itemcode, "9"), "1", "0")) 
  
  nss09t2_block11_hh <- nss09t2_block11 |> 
    mutate(hce = hce_dur_365*(30/365)) |> 
    select(hhid, itemcode, hce) |> 
    pivot_wider(names_from = c("itemcode"),
                values_from = "hce",
                names_glue = "{.value}_{itemcode}",
                values_fill = 0) |> 
    mutate(across(starts_with("hce_"), ~ if_else(is.na(.x), 0, .x)))
  
  #Block 12 - Summary of Consumer Expenditure
  
  # HH Cons
  
  nss09t2_hh_cons <- nss09t2_block3_hh |> 
    full_join(
      nss09t2_block5_hh,
      by = "hhid"
    ) |> 
    # full_join(
    #   nss09t2_block6_hh,
    #   by = "hhid"
    # ) |> 
    full_join(
      nss09t2_block7_8_hh,
      by = "hhid"
    ) |> 
    full_join(
      nss09t2_block9_hh,
      by = "hhid"
    ) |> 
    full_join(
      nss09t2_block10_hh,
      by = "hhid"
    ) |> 
    full_join(
      nss09t2_block11_hh,
      by = "hhid"
    ) |> 
    # full_join(
    #   nss09t2_block12_hh,
    #   by = "hhid"
    # ) |> 
    mutate(across(starts_with("hce_"), 
                  ~ if_else(is.na(.x), 0, .x))) |> 
    mutate(across(starts_with("hce_"), 
                  ~ .x/hhsize, 
                  .names = "pce_{.col}")) |> 
    filter(!is.na(wt)) |> 
    rename(state_id = state) |> 
    clean_nss_stateid_to_state()
  
  names(nss09t2_hh_cons) <- names(nss09t2_hh_cons) |> 
    str_replace("pce_hce", "pce")
  
  nss09t2_hh_cons <- nss09t2_hh_cons |> 
    clean_nss_hh_cons_2011_prices(cpist_agyear, year = "2009") 
  
  return(nss09t2_hh_cons)
  
}

clean_nss11t1_hh_cons <- function(nss_folder = "nss/", cpist_agyear) {
  
  ### Block 3
  
  nss11t1_block3_l3 <- read_dta(paste0(nss_folder, 
                                       "nss2011ce_type1/data/Household characteristics - Block 3 - Level 3.dta")) |> 
    clean_names() |> 
    mutate(across(c("mpce_mrp", "mpce_urp"), ~ .x/1e2)) 
  
  nss11t1_block3_l2 <- read_dta(paste0(nss_folder, "nss2011ce_type1/data/Household Characteristics - Block 3 -  Level 2 -  68.dta")) |> 
    clean_names() |> 
    #mutate(fs_strata = str_c(state, sector, stratum, sub_stratum_no)) |> 
    rename(
      psu = fsu_serial_no,
      hhsize = hh_size,
      urbanicity = sector
    ) |> 
    mutate(across(c("hhsize"), as.numeric)) |> 
    mutate(weight = if_else(nss != nsc, mlt/200, mlt/100),
           wt = weight*hhsize)
  
  nss11t1_hh <- nss11t1_block3_l2 |> 
    full_join(nss11t1_block3_l3 |> 
                select(hhid, mpce_urp, mpce_mrp),
              by = "hhid")
  
  ### Block 5
  nss11t1_block5 <- read_dta(paste0(nss_folder, "/nss2011ce_type1/data/Consumption of cereals-pulses- milk and milk products  during the last 30 days  - Block 5.1- 5.dta")) |> 
    clean_names() |> 
    rename(
      itemcode = item_code,
      quantity = total_consumption_quantity,
      hce = total_consumption_value,
      source = source_code
    )
  
  nss11t1_block5_hh <- nss11t1_block5 |> 
    select(hhid, itemcode, hce = hce) |> 
    pivot_wider(names_from = "itemcode",
                values_from = c("hce"),
                names_glue = "{.value}_{itemcode}",
                values_fill = 0) |> 
    mutate(across(starts_with("hce"), ~ if_else(is.na(.x), 0, .x)))
  
  ### Block 6 - NA
  
  # consumption of energy (fuel, light & household appliances) during the last 30 days ended on
  
  # nss11t1_block6 <- read_dta(paste0(nss_folder, "/nss2011ce_type1/data/.dta")) |> 
  #   clean_names() |> 
  #   rename(
  #     itemcode = item_code,
  #     quantity = last_30days_quantity,
  #     hce = last_30days_value,
  #     quantity365 = last_365days_quantity,
  #     hce365 = last_365days_value#,
  #     #source = source_code
  #     )
  # 
  # nss11t1_block6_hh <- nss11t1_block6 |> 
  #   select(hhid, itemcode, hce = hce) |> 
  #   pivot_wider(names_from = "itemcode",
  #               values_from = c("hce"),
  #               names_glue = "{.value}_{itemcode}",
  #               values_fill = 0) |> 
  #   mutate(across(starts_with("hce"), ~ if_else(is.na(.x), 0, .x)))
  
  ### Block 7,8
  
  # Consumption of clothing, bedding and footwear during last 30 and 365 days - Block 7 and 8  - Level 6 -  68
  
  nss11t1_block7_8 <- read_dta(paste0(nss_folder, "/nss2011ce_type1/data/Consumption of clothing, bedding and footwear during last 30 and 365 days - Block 7 and 8  - Level 6 -  68.dta")) |> 
    clean_names() |> 
    rename(
      itemcode = item_code,
      quantity = last_30days_quantity,
      hce = last_30days_value,
      quantity365 = last_365days_quantity,
      hce365 = last_365days_value#,
      #source = source_code
    )
  
  nss11t1_block7_8_hh <- nss11t1_block7_8 |> 
    select(hhid, itemcode, hce = hce) |> 
    pivot_wider(names_from = "itemcode",
                values_from = c("hce"),
                names_glue = "{.value}_{itemcode}",
                values_fill = 0) |> 
    mutate(across(starts_with("hce"), ~ if_else(is.na(.x), 0, .x)))
  
  ### Block 9
  
  # Block 9 - Level 7 - 68
  
  # Expenditure on Education and Medical (institutional) goods and services
  
  # **REF PERIOD** -
  
  #  1.  last 30 days
  
  #  2.  last 365 days
  
  nss11t1_block9 <- read_dta(paste0(nss_folder, "nss2011ce_type1/data/Expenditure on Education and Medical (institutional) goods and services -  Block 9 - Level 7 -  68.dta")) |> 
    clean_names() |> 
    rename(
      #psu = vill_blk_slno,
      itemcode = item_code,
      hce = expenditure_in_rs_last_30_days,
      hce365 = expenditure_in_rs_last_365_days
    ) |> 
    mutate(item_subcat = if_else(str_ends(itemcode, "9"), "1", "0")) 
  
  nss11t1_block9_hh <- nss11t1_block9 |> 
    select(hhid, itemcode, hce, hce365) |> 
    mutate(hce365 = hce365*(30/365)) |> 
    pivot_wider(names_from = "itemcode",
                values_from = c("hce", "hce365"),
                names_glue = "{.value}_{itemcode}",
                values_fill = 0) |> 
    mutate(across(starts_with("hce"), ~ if_else(is.na(.x), 0, .x)))
  
  ### Block 10
  
  # Expenditure on miscellaneous goods and services including medical(non-institutional), rents and taxes **during the last 30 days**
  
  nss11t1_block10 <- read_dta(paste0(nss_folder, "nss2011ce_type1/data/Expenditure on miscellaneous goods and services including medical(non-institutional), rents and taxes during the last 30 days.dta")) |> 
    clean_names() |> 
    rename(
      #psu = vill_blk_slno,
      itemcode = item_code,
      hce = value
    ) |> 
    mutate(item_subcat = if_else(str_ends(itemcode, "9"), "1", "0")) 
  
  nss11t1_block10_hh <- nss11t1_block10 |> 
    select(hhid, itemcode, hce) |> 
    pivot_wider(names_from = "itemcode",
                values_from = "hce",
                names_glue = "{.value}_{itemcode}",
                values_fill = 0) |> 
    mutate(across(starts_with("hce_"), ~ if_else(is.na(.x), 0, .x)))
  
  ### Block 11
  
  nss11t1_block11 <- read_dta(paste0(nss_folder, "nss2011ce_type1/data/Expenditure for purchase and construction (including repair and maintenance) of durable goods for domestic use-  Block 11 - Level 9 -  68.dta")) |> 
    clean_names() |> 
    rename(
      #psu = vill_blk_slno,
      itemcode = item_code,
      hce_dur_365 = total_expenditure_365_days,
      hce_dur_30 = total_expenditure_30_days
    ) |> 
    mutate(item_subcat = if_else(str_ends(itemcode, "9"), "1", "0")) 
  
  nss11t1_block11_hh <- nss11t1_block11 |> 
    mutate(hce = hce_dur_365*(30/365)) |> 
    select(hhid, itemcode, hce) |> 
    pivot_wider(names_from = c("itemcode"),
                values_from = "hce",
                names_glue = "{.value}_{itemcode}",
                values_fill = 0) |> 
    mutate(across(starts_with("hce_"), ~ if_else(is.na(.x), 0, .x)))
  
  ### Block 12
  
  # Summary of Consumer Expenditure - Block 12 - Level 11 - 68
  
  # nss11t1_block12 <- read_dta(paste0(nss_folder, "nss2011ce_type1/data/Summary of Consumer Expenditure - Block 12 - Level 11 - 68.dta")) |> 
  #   clean_names() |> 
  #   mutate(across(c("srl_no"), as.double)) |> 
  #   left_join(nss11_schedule_block5 |> 
  #               select(item, srl_no, itemcode),
  #             by = "srl_no") |> 
  #   rename(
  #     #psu = vill_blk_slno,
  #     #itemcode = item_code,
  #     hce = value
  #   ) |> 
  #   mutate(item_subcat = if_else(str_ends(itemcode, "9"), "1", "0")) 
  # 
  # nss11t1_block12_hh <- nss11t1_block12 |> 
  #   select(hhid, itemcode, hce) |> 
  #   pivot_wider(names_from = "itemcode",
  #               values_from = "hce",
  #               names_glue = "bl12_{.value}_{itemcode}",
  #               values_fill = 0
  #   ) |> 
  #   mutate(across(starts_with("hce_"), ~ if_else(is.na(.x), 0, .x)))
  
  ### HH Cons
  
  nss11t1_hh_cons <- nss11t1_hh |> 
    full_join(
      nss11t1_block5_hh |> 
        select(-starts_with("hce365")),
      by = "hhid"
    ) |> 
    # full_join(
    #   nss11t1_block6_hh,
    #   by = "hhid"
    # ) |> 
    full_join(
      nss11t1_block7_8_hh |> 
        select(-starts_with("hce365")),
      by = "hhid"
    ) |> 
    full_join(
      nss11t1_block9_hh |> 
        select(-starts_with("hce365")),
      by = "hhid"
    ) |> 
    full_join(
      nss11t1_block10_hh,
      by = "hhid"
    ) |> 
    full_join(
      nss11t1_block11_hh,
      by = "hhid"
    ) |> 
    # full_join(
    #   nss11_block12_hh,
    #   by = "hhid"
    # ) |> 
    mutate(across(starts_with("hce_"), 
                  ~ if_else(is.na(.x), 0, .x))) |> 
    mutate(across(starts_with("hce_"), 
                  ~ .x/hhsize, 
                  .names = "pce_{.col}")) |> 
    rename(state_id = state_code) |> 
    clean_nss_stateid_to_state()
  
  names(nss11t1_hh_cons) <- names(nss11t1_hh_cons) |> 
    str_replace("pce_hce", "pce")
  
  nss11t1_hh_cons <- nss11t1_hh_cons |> 
    clean_nss_hh_cons_2011_prices(cpist_agyear, year = "2011")
  
  return(nss11t1_hh_cons)
  
}

clean_nss11t2_hh_cons <- function(nss_folder = "nss/", cpist_agyear) {
  
  ### Block 3 - Household characteristics
  
  # -   Decimal in `MPCE` variable in NSS 11 Household File is wrongly coded. Dividing by `100` to get the correct MPCEs.
  
  nss11t2_block3_l3 <- read_dta(paste0(nss_folder, "nss2011ce_type2/data/Household characteristics - Block 3 -  level3 - type2 - 68.dta")) |> 
    clean_names() |> 
    mutate(across(c("mpce"), ~ .x/1e2)) 
  
  nss11t2_block3_l2 <- read_dta(paste0(nss_folder, "nss2011ce_type2/data/Household characteristics - Block 3 - Level2 - type2 - 68.dta")) |> 
    clean_names() |> 
    #mutate(fs_strata = str_c(state, sector, stratum, sub_stratum_no)) |> 
    rename(
      psu = fsu_serial_no,
      hhsize = hh_size,
      urbanicity = sector
    ) |> 
    mutate(across(c("hhsize"), as.numeric)) |> 
    mutate(weight = if_else(nss != nsc, mlt/200, mlt/100),
           wt = weight*hhsize)
  
  nss11t2_hh <- nss11t2_block3_l2 |> 
    select(hhid, wt, hhsize, urbanicity, state = state_code) |> 
    full_join(nss11t2_block3_l3 |> 
                select(hhid, mpce),
              by = "hhid")
  
  ### Block 5
  
  nss11t2_block5 <- read_dta(paste0(nss_folder, "/nss2011ce_type2/data/Consumption of cereals-pulses- milk and milk products  during the last 30 days   - Block 5.1- 5.dta")) |> 
    clean_names() |> 
    rename(
      itemcode = item_code,
      quantity = total_consumption_quantity,
      hce = total_consumption_value,
      source = source_code
    )
  
  nss11t2_block5_hh <- nss11t2_block5 |> 
    select(hhid, itemcode, hce = hce) |> 
    pivot_wider(names_from = "itemcode",
                values_from = c("hce"),
                names_glue = "{.value}_{itemcode}",
                values_fill = 0) |> 
    mutate(across(starts_with("hce"), ~ if_else(is.na(.x), 0, .x)))
  
  ### Block 6 - NA
  
  # consumption of energy (fuel, light & household appliances) during the last 30 days ended on
  
  # nss11t2_block6 <- read_dta(paste0(nss_folder, "/nss2011ce_type2/data/.dta")) |> 
  #   clean_names() |> 
  #   rename(
  #     itemcode = item_code,
  #     quantity = last_30days_quantity,
  #     hce = last_30days_value,
  #     quantity365 = last_365days_quantity,
  #     hce365 = last_365days_value#,
  #     #source = source_code
  #     )
  # 
  # nss11t2_block6_hh <- nss11t2_block6 |> 
  #   select(hhid, itemcode, hce = hce) |> 
  #   pivot_wider(names_from = "itemcode",
  #               values_from = c("hce"),
  #               names_glue = "{.value}_{itemcode}",
  #               values_fill = 0) |> 
  #   mutate(across(starts_with("hce"), ~ if_else(is.na(.x), 0, .x)))
  
  ### Block 7,8
  
  # Consumption of clothing, bedding and footwear during last 365 days - Block 7 and 8  - Level 6 -   Type 2 - 68
  
  nss11t2_block7_8 <- read_dta(paste0(nss_folder, "/nss2011ce_type2/data/Consumption of clothing, bedding and footwear during last 365 days - Block 7 and 8  - Level 6 -   Type 2 - 68.dta")) |> 
    clean_names() |> 
    rename(
      itemcode = item_code,
      quantity365 = last_365_days_quantity_or_no,
      hce365 = last_365_days_value#,
      #source = source_code
    )
  
  nss11t2_block7_8_hh <- nss11t2_block7_8 |> 
    select(hhid, itemcode, hce = hce365) |> 
    mutate(hce = hce*(30/365)) |> 
    pivot_wider(names_from = "itemcode",
                values_from = c("hce"),
                names_glue = "{.value}_{itemcode}",
                values_fill = 0) |> 
    mutate(across(starts_with("hce"), ~ if_else(is.na(.x), 0, .x)))
  
  ### Block 9
  
  # Expenditure on Education and Medical (institutional) goods and services during the last 365 days - Block 9 - Level 7
  
  nss11t2_block9 <- read_dta(paste0(nss_folder, "nss2011ce_type2/data/Expenditure on Education and Medical (institutional) goods and services during the last 365 days -  Block 9 - Level 7 - type 2 - 68.dta")) |> 
    clean_names() |> 
    rename(
      #psu = vill_blk_slno,
      itemcode = item_code,
      hce_365 = value
    ) #|> 
  #mutate(item_subcat = if_else(str_ends(itemcode, "9"), "1", "0")) 
  
  nss11t2_block9_hh <- nss11t2_block9 |> 
    mutate(hce = hce_365*(30/365)) |> 
    select(hhid, itemcode, hce) |> 
    pivot_wider(names_from = "itemcode",
                values_from = "hce",
                names_glue = "{.value}_{itemcode}",
                values_fill = 0) |> 
    mutate(across(starts_with("hce"), 
                  ~ if_else(is.na(.x), 0, .x)))
  
  ### Block 10
  
  # Expenditure on miscellaneous goods and services including medical(non-institutional), rents and taxes during the last 30 days
  
  nss11t2_block10 <- read_dta(paste0(nss_folder, "nss2011ce_type2/data/Expenditure on miscellaneous goods and services including medical(non-institutional), rents and taxes during the last 30 days.dta")) |> 
    clean_names() |> 
    rename(
      #psu = vill_blk_slno,
      itemcode = item_code,
      hce = value
    ) # |> 
  #mutate(item_subcat = if_else(str_ends(itemcode, "9"), "1", "0")) 
  
  nss11t2_block10_hh <- nss11t2_block10 |> 
    select(hhid, itemcode, hce) |> 
    pivot_wider(names_from = "itemcode",
                values_from = "hce",
                names_glue = "{.value}_{itemcode}",
                values_fill = 0) |> 
    mutate(across(starts_with("hce_"), ~ if_else(is.na(.x), 0, .x)))
  
  ### Block 11
  
  # Expenditure for purchase and construction (including repair and maintenance) of durable goods for domestic use- Block 11 - Level 9 - Type 2
  # 
  # -   Itemwise HCE reference period in raw data is 365 days.
  # 
  # -   Converting HCE to monthly in the HH wide file.
  
  nss11t2_block11 <- read_dta(paste0(nss_folder, "nss2011ce_type2/data/Expenditure for purchase and construction (including repair and maintenance) of durable goods for domestic use-  Block 11 - Level 9 -  Type 2 - 68.dta")) |> 
    clean_names() |> 
    rename(
      #psu = vill_blk_slno,
      itemcode = item_code,
      hce_dur_365 = total_expenditure_365_days,
    ) #|> 
  #mutate(item_subcat = if_else(str_ends(itemcode, "9"), "1", "0")) 
  
  nss11t2_block11_hh <- nss11t2_block11 |> 
    mutate(hce = hce_dur_365*(30/365)) |> 
    select(hhid, itemcode, hce) |> 
    pivot_wider(names_from = c("itemcode"),
                values_from = "hce",
                names_glue = "{.value}_{itemcode}",
                values_fill = 0) |> 
    mutate(across(starts_with("hce_"), ~ if_else(is.na(.x), 0, .x)))
  
  ### Block 12
  
  # Summary of Consumer Expenditure - Block 12 - Level 11 - 68
  # 
  # -   This file has the summary of CE which can be used to match with official estimates in the report.
  # 
  # -   TODO - Convert t1 item codes to t2.
  
  # nss11t2_block12 <- read_dta(paste0(nss_folder, "nss2011ce_type2/data/Summary of Consumer Expenditure - Block 12 - Level 11 - type 2 - 68.dta")) |> 
  #   clean_names() |> 
  #   mutate(across(c("srl_no"), as.double)) |> 
  #   left_join(nss11_schedule_block5 |> 
  #               select(item, srl_no, itemcode),
  #             by = "srl_no") |> 
  #   rename(hce = value) 
  # 
  # nss11t2_block12_hh <- nss11t2_block12 |> 
  #   select(hhid, itemcode, hce) |> 
  #   pivot_wider(names_from = "itemcode",
  #               values_from = "hce",
  #               names_glue = "bl12_{.value}_{itemcode}",
  #               values_fill = 0
  #   ) |> 
  #   mutate(across(starts_with("hce_"), 
  #                 ~ if_else(is.na(.x), 0, .x)))
  
  ### NSS 11 T2 HH Cons
  
  #   **HH CONS FILE**
  #     
  #     -   Merging HH file with block 9, block 10 and block 11 wide format HH files.
  #   
  #   -   Generating `pce_` variables by dividing itemwise HCEs by household size.
  #   
  #   -   Replacing missing values with 0's for all CE items.
  # 
  # -   `nss11t2_hh_cons` is the final processed file for NSS 2011 CE T2 Items in wide format with one observation for each household.
  
  nss11t2_hh_cons <- nss11t2_hh |> 
    full_join(
      nss11t2_block5_hh |> 
        select(-starts_with("hce365")),
      by = "hhid"
    ) |> 
    # full_join(
    #   nss11t2_block6_hh,
    #   by = "hhid"
    # ) |> 
    full_join(
      nss11t2_block7_8_hh |> 
        select(-starts_with("hce365")),
      by = "hhid"
    ) |> 
    full_join(
      nss11t2_block9_hh |> 
        select(-starts_with("hce365")),
      by = "hhid"
    ) |> 
    full_join(
      nss11t2_block10_hh,
      by = "hhid"
    ) |> 
    full_join(
      nss11t2_block11_hh,
      by = "hhid"
    ) |> 
    mutate(across(starts_with("hce_"), 
                  ~ if_else(is.na(.x), 0, .x))) |> 
    mutate(across(starts_with("hce_"), 
                  ~ .x/hhsize, 
                  .names = "pce_{.col}")) |> 
    rename(state_id = state) |> 
    clean_nss_stateid_to_state()
  
  names(nss11t2_hh_cons) <- names(nss11t2_hh_cons) |>
    str_replace("pce_hce", "pce")
  
  nss11t2_hh_cons <- nss11t2_hh_cons |> 
    clean_nss_hh_cons_2011_prices(cpist_agyear, year = "2011")
  
  return(nss11t2_hh_cons)
  
}

clean_nss14_hh_cons <- function(nss_folder = "nss/", cpist_agyear) {
  
  ### Block 3
  
  # Block 3 - Household characteristics
  
  # -   Decimal in `MPCE` variable in NSS 11 Household File is wrongly coded. Dividing by `100` to get the correct MPCEs.
  
  nss14_hh <- read_dta(paste0(nss_folder, "nss2014_services_durables/data/Block 3 - Household characteristics.dta")) |> 
    clean_names() |> 
    mutate(fs_strata = str_c(state, sector, stratum, sub_stratum_no)) |> 
    rename(
      psu = vill_blk_slno,
      hhsize = b3q1,
      nic2008 = b3q2,
      nco2004 = b3q3,
      religion = b3q5,
      caste = b3q6,
      is_nonag_ent = b3q7,
      is_majdurables = b3q8,
      hfce = b3q9,
      urbanicity = sector
    ) |> 
    mutate(across(c("hhsize"), as.numeric)) |> 
    mutate(mpce = hfce/hhsize,
           weight = if_else(nss != nsc, mlt/200, mlt/100),
           wt = weight*hhsize) |> 
    select(hhid, wt, hhsize, urbanicity, hfce, mpce, is_nonag_ent, is_majdurables, everything())
  
  ### Block 5 - 9
  
  # Block 5, 6, 7, 8, 9 - transport expenditure, misc
  # 
  # -   Block 5 - Transport expenditure incurred during overnight "round journeys"completed **during the last 30 days**
  #   
  #   -   Block 6 - Transport expenses incurred for movements **during the last 30 days** that were not part of overnight "round journeys".
  # 
  # -   Block 7 - Expenditure on miscellaneous consumer services **during the last 30 days**
  #   
  #   -   Block 8 - Expenditure on repairs and maintenance of selected items, Annual Maintenance Contract payments, hotel lodging charges, and other selected services **during the last 365 days**
  #   
  #   -   Block 9 - Food expenditure in hotels and restaurants **during the last 7 days**
  #   
  #   -   Converting Block 8 and Block 9 to match 30 day reference period.
  
  
  nss14_block5_9 <- read_dta(paste0(nss_folder, "nss2014_services_durables/data/Block 5, 6, 7, 8, 9 - transport expenditure, misc.dta")) |> 
    clean_names() |> 
    rename(
      psu = vill_blk_slno,
      itemcode = b56789q2,
      hce = b56789q3
    ) |> 
    mutate(item_subcat = if_else(str_ends(itemcode, "9"), "1", "0")) 
  
  # block5_9_items_365 <- paste0("hce_", 680:689)
  # block5_9_items_7 <- paste0("hce_", 690:699)
  
  nss14_block5_9_items_365 <- 680:689
  nss14_block5_9_items_7 <- 690:699
  
  nss14_block5_9_hh <- nss14_block5_9 |> 
    select(hhid, itemcode, hce) |> 
    mutate(hce = case_when(itemcode %in% nss14_block5_9_items_365 ~ hce*(30/365),
                           itemcode %in% nss14_block5_9_items_7 ~ hce*(30/7),
                           TRUE ~ hce)) |> 
    pivot_wider(names_from = "itemcode",
                values_from = "hce",
                names_glue = "{.value}_{itemcode}",
                values_fill = 0) |> 
    #mutate(across(any_of(block5_9_items_365), ~ .x/12)) |> 
    #mutate(across(any_of(block5_9_items_7), ~ .x*30/7)) |> 
    mutate(across(starts_with("hce_"), ~ if_else(is.na(.x), 0, .x)))
  
  ### Block 10
  
  # Block 10 - Expenditure on durable goods acquired **during the last 365 days** other those used exclusively for entrepreneurial activity
  
  # -   Converting Block 10 to match 30 day reference period.
  
  nss14_block10 <- read_dta(paste0(nss_folder, "nss2014_services_durables/data/Block 10 - Expenditure on durable goods acquired during the last 365 days other those used exclusively for entrepreneurial activity.dta")) |> 
    clean_names() |> 
    rename(
      psu = vill_blk_slno,
      itemcode = b10q2,
      dur_purchased_no = b10q3,
      dur_purchased_value = b10q4,
      dur_1_hand = b10q5,
      dur_component_value = b10q6,
      dur_main_use = b10q7
    ) |> 
    #mutate(item_subcat = if_else(str_ends(itemcode, "9"), "1", "0")) |> 
    mutate(across(c("dur_main_use", "dur_purchased_no"), as.numeric))
  
  nss14_block10_hh <- nss14_block10 |> 
    mutate(hce = dur_purchased_value*(30/365)) |> 
    select(hhid, itemcode, dur_main_use, hce) |> 
    mutate(dur_main_use = case_when(dur_main_use == 1 ~ "d",
                                    dur_main_use == 2 ~ "e",
                                    is.na(dur_main_use) ~ "t"
    )) |> 
    pivot_wider(names_from = c("dur_main_use", "itemcode"),
                values_from = "hce",
                names_glue = "{.value}_{dur_main_use}_{itemcode}",
                values_fill = 0) |> 
    select(hhid, starts_with("hce_d")) |> 
    #mutate(across(starts_with("hce_d"), ~ .x/12)) |> 
    mutate(across(starts_with("hce_"), 
                  ~ if_else(is.na(.x), 0, .x)))
  
  names(nss14_block10_hh) <- names(nss14_block10_hh) |> 
    str_replace("hce_d_", "hce_")
  
  ### HH Cons
  
  #   -   Merging HH file with block 5-9, block 10 wide format HH files.
  #   
  #   -   Generating `pce_` variables by dividing itemwise HCEs by household size.
  #   
  #   -   Replacing missing values with 0's for all CE items.
  # 
  # -   `nss14_hh_cons` is the final processed file for NSS 2014 Services & Durables items in wide format with one observation for each household.
  
  nss14_hh_cons_df <- nss14_hh |> 
    full_join(
      nss14_block5_9_hh,
      by = "hhid"
    ) |> 
    full_join(
      nss14_block10_hh,
      by = "hhid"
    ) |> 
    mutate(across(starts_with("hce_"), 
                  ~ if_else(is.na(.x), 0, .x))) |> 
    mutate(across(starts_with("hce"),
                  ~ .x/hhsize, 
                  .names = "pce_{.col}")) |> 
    rename(state_id = state) |> 
    clean_nss_stateid_to_state()
  
  names(nss14_hh_cons) <- names(nss14_hh_cons) |> 
    str_replace("pce_hce", "pce")
  
  nss14_hh_cons <- nss14_hh_cons |> 
    clean_nss_hh_cons_2011_prices(cpist_agyear, year = "2014")
  
  return(nss14_hh_cons)
  
}

clean_nss14sc_hh_cons <- function(nss_folder = "nss/", cpist_agyear) {
  
  # -   survey period for the current round is of six months (January-June 2014)
  # 
  # -   Information on usual household consumer expenditure (\`) in a month was collected through a single question, in place of the five questions in earlier round.
  
  ### Block 3
  
  nss14sc_hh <- read_dta(paste0(nss_folder, "nss2014sc_edu/data/Block-3-Level-02 Household Characteristics.dta")) |> 
    clean_names() |> 
    #mutate(fs_strata = str_c(state, sector, stratum, sub_stratum_no)) |> 
    rename(
      hhid = hh_id,
      urbanicity = sector,
      hfce = hh_cons_exp
    ) |> 
    mutate(across(c("hhsize", "hfce"), as.double)) |> 
    mutate(mpce = hfce/hhsize,
           weight = if_else(nss != nsc, mlt/200, mlt/100),
           wt = weight*hhsize) |> 
    select(hhid, wt, hhsize, state = state_cd, urbanicity, hfce, mpce, everything())
  
  ### Block 4
  
  # **Block-4 - Level -03 Demographic and other particulars of Household members**
    
  nss14sc_block4 <- read_dta(paste0(nss_folder, "nss2014sc_edu/data/Block-4 - Level -03 Demographic and other particulars of Household members.dta")) |> 
    clean_names() |>
    rename(urbanicity = sector)
  
  # From a total of 310827 members in block 4, 93513 are `[currently attending in primary ( classI to V ) & above]` as per `edu_attend` variable.
  # 
  # These 93513 are asked further questions in block 6.
  # 
  # Note however this (edu_attend and block 6..?) is only applicable for members aged 5-29 years.
  
  ### Block 5
  
  # **Block-5 - Level-05 education particulars on basic course**

  nss14sc_block5 <- read_dta(paste0(nss_folder, "nss2014sc_edu/data/Block-5 - Level-05 education particulars on basic course.dta")) |> 
    clean_names() #|> 
  
  # nss14sc_block5 |> 
  #   select(hhid = hh_id, sector, psrl_no, age, levl_curr_attend, inst_type, edu_free)
  
  ### Block 6
  
  # **Block-6 - Level-04 Particulars of expenditure**
    
  nss14sc_block6 <- read_dta(paste0(nss_folder, "nss2014sc_edu/data/Block-6 - Level-04 Particulars of expenditure.dta")) |>
    clean_names() |> 
    rename(
      hhid = hh_id
    )
  
  nss14sc_block6_hh <- nss14sc_block6 |> 
    select(hhid, psrl_no, age, course_fee, books, transport, priv_coaching, other_exp, tot_exp) |>
    group_by(hhid) |> 
    summarise(npers_bl6 = n(),
              hce_course_fee = sum(course_fee, na.rm = T),
              hce_books = sum(books, na.rm = T),
              hce_transport = sum(transport, na.rm = T),
              hce_priv_coaching = sum(priv_coaching, na.rm = T),
              hce_other_exp = sum(other_exp, na.rm = T),
              hce_tot_exp = sum(tot_exp, na.rm = T)
    ) |> 
    mutate(across(starts_with("hce"), ~ .x*(30/365)))
  
  ### HH Cons
  
  hce_variables <- c("hce_course_fee", "hce_books", "hce_transport", "hce_priv_coaching", "hce_other_exp", "hce_tot_exp")
  
  nss14sc_hh_cons <- nss14sc_hh |> 
    select(hhid, state, urbanicity, hhsize, wt, hfce, mpce) |> 
    left_join(nss14sc_block6_hh,
              by = "hhid") |> 
    mutate(across(any_of(hce_variables), 
                  ~ if_else(is.na(.x), 0, .x))) |> 
    mutate(npers_bl6 = if_else(is.na(npers_bl6), 0, as.double(npers_bl6))) |> 
    mutate(across(any_of(hce_variables), 
                  ~ if_else(is.na(.x), 0, .x))) |> 
    mutate(across(any_of(hce_variables), 
                  ~ .x/hhsize,
                  .names = "pce_{.col}")) |> 
    rename(state_id = state) |> 
    clean_nss_stateid_to_state()
  
  names(nss14sc_hh_cons) <- names(nss14sc_hh_cons) |> str_replace("pce_hce", "pce")
  
  nss14sc_hh_cons <- nss14sc_hh_cons |>
    clean_nss_hh_cons_2011_prices(cpist_agyear, year = "2014")
  
  return(nss14sc_hh_cons)
  
}

clean_nss14sch_hh_cons <- function(nss_folder = "nss/", cpist_agyear) {
  
  ### Block 3
  nss14sch_hh <- read_dta(paste0(nss_folder, "nss2014sc_health/data/Block 3 - Household Characteristics.dta")) |> 
    clean_names() |> 
    #mutate(fs_strata = str_c(state, sector, stratum, sub_stratum_no)) |> 
    rename(
      urbanicity = sector,
      hfce = b3_q12,
      hhsize = b3_q1,
      nic2008 = b3_q2,
      nco2004 = b3_q3,
      religion = b3_q5,
      caste = b3_q6
    ) |> 
    mutate(across(c("hhsize", "hfce"), as.double)) |> 
    mutate(mpce = hfce/hhsize,
           weight = if_else(nss != nsc, mlt/200, mlt/100),
           wt = weight*hhsize) |> 
    select(hhid, wt, hhsize, urbanicity, hfce, mpce, everything())

  ### Block 7
  
  # Expenses incurred during the last 365 days for treatment of members as in-patient of medical institution
  
  nss14sch_block7 <- read_dta(paste0(nss_folder, "nss2014sc_health/data/Block 7 - expenses incurred during the last 365 days for treatment of members as in-patient of medical institution.dta")) |> 
    clean_names() |> 
    rename(
      pce_package_component = b7_q5,
      pce_doctor_surgeon = b7_q6,
      pce_medicines = b7_q7,
      pce_diagnostic = b7_q8,
      pce_bed = b7_q9,
      pce_other_medical = b7_q10,
      pce_medical = b7_q11,
      pce_transport = b7_q12,
      pce_other_non_medical = b7_q13,
      pce_total_expenditure = b7_q14,
      pce_reimbursed = b7_q15
    ) |> 
    mutate(across(starts_with("pce_"), ~.x*(30/365)))
  
  nss14sch_block7_hh <- nss14sch_block7 |> 
    group_by(hhid) |> 
    summarise(across(starts_with("pce"), 
                     ~ sum(.x, na.rm = T), 
                     .names = "hce_{.col}"))
  
  names(nss14sch_block7_hh) <- names(nss14sch_block7_hh) |> 
    str_replace("hce_pce", "hce_inst")
  
  ### Block 9
  
  # Expenses incurred during the last 15 days for treatment of members (not as in-patient of medical institution).
  
  nss14sch_block9 <- read_dta(paste0(nss_folder, "nss2014sc_health/data/Block 9 -  expenses incurred during the last 15 days for treatment of members (not as in-patient of medical institution).dta")) |> 
    clean_names() |> 
    rename(
      pce_doctor_surgeon = b9_q9,
      pce_medicines_ayush = b9_q10,
      pce_medicines_non_ayush = b9_q11,
      pce_diagnostic = b9_q12,
      pce_other_medical = b9_q13,
      pce_medical = b9_q14,
      pce_transport = b9_q15,
      pce_other_non_medical = b9_q16,
      pce_total_expenditure = b9_q17,
      pce_reimbursed = b9_q18
    ) |> 
    mutate(across(starts_with("pce_"), ~.x*2))
  
  nss14sch_block9_hh <- nss14sch_block9 |> 
    group_by(hhid) |> 
    summarise(across(starts_with("pce"), 
                     ~ sum(.x, na.rm = T), 
                     .names = "hce_{.col}"))
  
  names(nss14sch_block9_hh) <- names(nss14sch_block9_hh) |> 
    str_replace("hce_pce", "hce_ninst")
  
  ### HH Cons
  
  nss14sch_hh_cons <- nss14sch_hh |> 
    full_join(nss14sch_block7_hh,
              by = "hhid") |> 
    full_join(nss14sch_block9_hh,
              by = "hhid") |> 
    mutate(across(starts_with("hce_"), 
                  ~ if_else(is.na(.x), 0, .x))) |> 
    mutate(across(starts_with("hce_"), 
                  ~ .x/hhsize,
                  .names = "pce_{.col}")) |> 
    rename(state_id = state) |> 
    clean_nss_stateid_to_state()
  
  names(nss14sch_hh_cons) <- names(nss14sch_hh_cons) |> 
    str_replace("pce_hce", "pce")
  
  nss14sch_hh_cons <- nss14sch_hh_cons |>
    clean_nss_hh_cons_2011_prices(cpist_agyear, year = "2014")
  
  return(nss14sch_hh_cons)
  
}

clean_nss18sc_hh_cons <- function(nss_folder = "nss/", cpist_agyear) {
  
  
  ### Block 3

  nss18sc_hh <- read_dta(paste0(nss_folder, "nss2018sc_edu/data/R75252L02 (Block 3)-household characteristics.dta")) |> 
    clean_names() |> 
    #mutate(fs_strata = str_c(state, sector, stratum, sub_stratum_no)) |> 
    rename(
      #psu = vill_blk_slno,
      hhsize = household_size,
      nic2008 = nic_2008_5d_code,
      nco2004 = nco_2004_3d_code,
      hfce = hh_con_exp_rs,
      urbanicity = sector,
      mlt = mult
    ) |> 
    mutate(across(c("hhsize", "hfce"), as.double)) |> 
    mutate(mpce = hfce/hhsize,
           weight = if_else(nss != nsc, mlt/200, mlt/100),
           wt = weight*hhsize) |> 
    select(hhid, wt, hhsize, urbanicity, hfce, mpce, everything())
  
  ### Block 5
  
  
  nss18sc_block5 <- read_dta(paste0(nss_folder, "nss2018sc_edu/data/R75252L05 (Block-5)-Dducation particulars on basic course of the persons of age 3 to 35 years who are currently attending education.dta")) |> 
    clean_names() 
  
  ### Block 6
  
  # (Block 6)-Particulars of expenditure (Rs)
  
  nss18sc_block6 <- read_dta(paste0(nss_folder, "nss2018sc_edu/data/R75252L06 (Block 6)-Particulars of expenditure (Rs.dta")) |> 
    clean_names() 
  
  nss18sc_block6_hh <- nss18sc_block6 |> 
    select(hhid, psrl_no = per_serialno, age, 
           course_fee = course_fee_amt, 
           books = books_stationery_uniform_amt, 
           transport = transport_amt, 
           priv_coaching = private_coaching_amt, 
           other_exp = other_expenditure_amt, 
           tot_exp = total_expenditure_amt,
           exp_other_course = exp_other_course_amt,
           exp_prep_higher_studies = exp_prep_higher_studies_amt
    ) |> 
    mutate(across(c("course_fee", "books", "transport", "priv_coaching", "other_exp", "tot_exp", "exp_other_course", "exp_prep_higher_studies"),
                  as.double)) |>  
    group_by(hhid) |> 
    summarise(npers_bl6 = n(),
              hce_course_fee = sum(course_fee, na.rm = T),
              hce_books = sum(books, na.rm = T),
              hce_transport = sum(transport, na.rm = T),
              hce_priv_coaching = sum(priv_coaching, na.rm = T),
              hce_other_exp = sum(other_exp, na.rm = T),
              hce_tot_exp = sum(tot_exp, na.rm = T),
              hce_other_course = sum(exp_other_course, na.rm = T),
              hce_prep_higher_studies = sum(exp_prep_higher_studies, na.rm = T)
    ) |> 
    mutate(across(starts_with("hce"), ~ .x*(30/365)))
  
  ### HH Cons
  
  nss18sc_hce_variables <- c("hce_course_fee", "hce_books", "hce_transport", "hce_priv_coaching", "hce_other_exp", "hce_tot_exp")
  
  nss18sc_hh_cons <- nss18sc_hh |> 
    select(hhid, state, urbanicity, hhsize, wt, hfce, mpce) |> 
    left_join(nss18sc_block6_hh,
              by = "hhid") |> 
    mutate(across(any_of(nss18sc_hce_variables), 
                  ~ if_else(is.na(.x), 0, .x))) |> 
    mutate(npers_bl6 = if_else(is.na(npers_bl6), 0, as.double(npers_bl6))) |> 
    mutate(across(any_of(nss18sc_hce_variables), 
                  ~ if_else(is.na(.x), 0, .x))) |> 
    mutate(across(any_of(nss18sc_hce_variables), 
                  ~ .x/hhsize,
                  .names = "pce_{.col}")) |> 
    rename(state_id = state) |> 
    clean_nss_stateid_to_state()
  
  names(nss18sc_hh_cons) <- names(nss18sc_hh_cons) |> str_replace("pce_hce", "pce")
  
  nss18sc_hh_cons <- nss18sc_hh_cons |>
    clean_nss_hh_cons_2011_prices(cpist_agyear, year = "2017")
  
  return(nss18sc_hh_cons)
  
}

clean_nss18sch_hh_cons <- function(nss_folder = "nss/", cpist_agyear) {
  
  ### Block 3
  
  # - NOTE - NSS 18 does not have a state/state_id variable.
  # 
  # - Creating state_id from first 2 characters of `nss_region` variable.
  
  nss18sch_hh <- read_dta(paste0(nss_folder, "nss2018sc_health/data/Household Characteristics Block 3 Level 2; 75 Health; R75250L02.dta")) |> 
    clean_names() |> 
    #mutate(fs_strata = str_c(state, sector, stratum, sub_stratum_no)) |> 
    mutate(hhid = paste0(fsu, hamlet_sub_block, second_stage_stratum, sample_hhld)) |> 
    rename(
      urbanicity = sector,
      hfce = household_usual_consumer_expendi,
      hhsize = household_size,
      nic2008 = nic_2008_five_digit_code,
      nco2004 = nco_2004_three_digit_code,
      caste = social_group,
      mlt = mult
    ) |> 
    mutate(across(c("hhsize", "hfce"), as.double)) |> 
    mutate(mpce = hfce/hhsize,
           weight = if_else(nss != nsc, mlt/200, mlt/100),
           wt = weight*hhsize) |> 
    mutate(state_id = str_sub(nss_region, 1, 2)) |> 
    select(hhid, wt, hhsize, state_id, urbanicity, hfce, mpce, everything())

  ### Block 7
  
  # Expenses incurred during the last 365 days for treatment of members as in-patient of medical institution
  
  nss18sch_block7_l6_1_14 <- read_dta(paste0(nss_folder, "nss2018sc_health/data/Expenses incurred during the last 365 days for treatment of members as in-patient of medical institution; Item 1-14 Block 7 Level 6; 75 Health; R75250L06.dta")) |> 
    clean_names() |>
    mutate(hhid = paste0(fsu, hamlet_sub_block, second_stage_stratum, sample_hhld),
           psrlid = paste0(hhid, srl_no_of_member_hospitalised),
           caseid = paste0(hhid, srl_no_of_hospitalisation_case, srl_no_of_member_hospitalised)) |> 
    rename(
      pce_package_component = package_component_rs,
      pce_doctor_surgeon = doctors_surgeons_fee_rs,
      pce_medicines = medicines_rs,
      pce_diagnostic = diagonistic_tests_rs,
      pce_bed = bed_charges_rs,
      pce_other_medical = other_medical_expenses_rs,
      pce_medical = medical_expenditure_totalitem_rs,
      pce_transport = transport_for_patient_rs,
      pce_other_non_medical = other_non_medical_expenses_rs,
      pce_total_expenditure = expenditure_total_items_rs
    ) 
  
  nss18sch_block7_l6_15_20 <- read_dta(paste0(nss_folder, "nss2018sc_health/data/Expenses incurred during the last 365 days for treatment of members as in-patient of medical institution; Item 15-20 Block 7 Level 7; 75 Health; R75250L07.dta")) |>
    clean_names() |>
    mutate(hhid = paste0(fsu, hamlet_sub_block, second_stage_stratum, sample_hhld),
           psrlid = paste0(hhid, srl_no_of_member_hospitalised),
           caseid = paste0(hhid, srl_no_of_hospitalisation_case, srl_no_of_member_hospitalised)) |>
    rename(
      pce_reimbursed = total_amount_reimbursed_by_medic
    )
  
  nss18sch_block7_l6 <- nss18sch_block7_l6_1_14 |>
    full_join(nss18sch_block7_l6_15_20 |>
                select(caseid, pce_reimbursed),
              by = "caseid") |> 
    mutate(across(starts_with("pce_"), as.double)) |> 
    mutate(across(starts_with("pce_"), ~.x/12))
  
  nss18sch_block7_l6_hh <- nss18sch_block7_l6 |> 
    group_by(hhid) |> 
    summarise(across(starts_with("pce_"), 
                     ~ sum(.x, na.rm = T), 
                     .names = "hce_{.col}"))
  
  names(nss18sch_block7_l6_hh) <- names(nss18sch_block7_l6_hh) |> 
    str_replace("hce_pce", "hce_inst")

  ### Block 9
  
  # Expenses incurred during the last 15 days for treatment of members (not as in-patient of medical institution)
  
  nss18sch_block9_l9_1_18 <- read_dta(paste0(nss_folder, "nss2018sc_health/data/Expenses incurred during the last 15 days for treatment of members (not as in-patient of medical institution); Item 1-18 of Block 9 Level 9; 75 Health; R75250L09.dta")) |> 
    clean_names() |> 
    mutate(hhid = paste0(fsu, hamlet_sub_block, second_stage_stratum, sample_hhld),
           psrlid = paste0(hhid, srl_no_of_spell_of_ailment),
           caseid = paste0(hhid, srl_no_of_spell_of_ailment, srl_no_of_member_reporting_ailme)) |> 
    rename(
      itemcode = srl_no_of_spell_of_ailment,
      pce_doctor_surgeon = doctors_surgeons_fee_rs,
      pce_medicines_ayush = medicines_ayush_rs,
      pce_medicines_non_ayush = medicines_other_than_ayush_rs,
      pce_diagnostic = diagonistic_tests_rs,
      pce_other_medical = other_medical_expenses_rs,
      pce_medical = medical_expenditure_totalitem_rs,
      pce_transport = transport_for_patient_rs,
      pce_other_non_medical = other_expenses_rs,
      pce_total_expenditure = expenditure_total_items_rs
    ) #|> 
  #mutate(across(starts_with("pce_"), ~ as.double(.x)*2))
  
  nss18sch_block9_l9_19_23 <- read_dta(paste0(nss_folder, "nss2018sc_health/data/Expenses incurred during the last 15 days for treatment of members (not as in-patient of medical institution); Item 19-23 of Block 9 Level 10; 75 Health; R75250L10.dta ")) |> 
    clean_names() |> 
    mutate(hhid = paste0(fsu, hamlet_sub_block, second_stage_stratum, sample_hhld),
           psrlid = paste0(hhid, srl_no_of_member_reporting_ailme),
           caseid = paste0(hhid, srl_no_of_spell_of_ailmentas_ite, srl_no_of_member_reporting_ailme)) |>
    rename(
      pce_reimbursed = total_amount_reimbursed_by_medic
    ) #|> 
  #mutate(across(starts_with("pce_"), ~ as.double(.x)*2))
  
  nss18sch_block9_l9 <- nss18sch_block9_l9_1_18 |>
    full_join(nss18sch_block9_l9_19_23 |>
                select(caseid, pce_reimbursed),
              by = "caseid") |> 
    mutate(across(starts_with("pce_"), ~ as.double(.x)*2))
  
  nss18sch_block9_l9_hh <- nss18sch_block9_l9 |> 
    group_by(hhid) |> 
    summarise(across(starts_with("pce"), 
                     ~ sum(.x, na.rm = T), 
                     .names = "hce_{.col}"))
  
  names(nss18sch_block9_l9_hh) <- names(nss18sch_block9_l9_hh) |> 
    str_replace("hce_pce", "hce_ninst")

  # Expenditure on immunisation, if any, during the last 365 days and status of immunisation of children as on date of survey (age 0-5 years)
  
  ### Block 10
  
  nss14sch_block10_l12 <- read_dta(paste0(nss_folder, "nss2018sc_health/data/Expenditure on immunisation, if any, during the last 365 days and status of immunisation of children as on date of survey (age 0-5 years); Block 10 b Level 12; 75 Health; R75250L12.dta")) |> 
    clean_names() |>
    mutate(hhid = paste0(fsu, hamlet_sub_block, second_stage_stratum, sample_hhld),
           psrlid = paste0(hhid, srl_no_of_member)) |>
    rename(
      pce_immunisation = expenditure_on_immunisation_duri
    )  |> 
    mutate(across(starts_with("pce_"), ~ (as.double(.x)/12)))
  
  nss14sch_block10_l12_hh <- nss14sch_block10_l12 |> 
    group_by(hhid) |> 
    summarise(across(starts_with("pce"), 
                     ~ sum(.x, na.rm = T), 
                     .names = "hce_{.col}"))
  
  names(nss14sch_block10_l12_hh) <- names(nss14sch_block10_l12_hh) |> 
    str_replace("hce_pce", "hce")
  
  ### HH Cons
  
  nss18sch_hh_cons <- nss18sch_hh |> 
    full_join(nss18sch_block7_l6_hh,
              by = "hhid") |> 
    full_join(nss18sch_block9_l9_hh,
              by = "hhid") |> 
    full_join(nss14sch_block10_l12_hh,
              by = "hhid") |> 
    mutate(across(starts_with("hce_"), 
                  ~ if_else(is.na(.x), 0, .x))) |> 
    mutate(across(starts_with("hce_"), 
                  ~ .x/hhsize,
                  .names = "pce_{.col}")) |> 
    clean_nss_stateid_to_state()
  
  names(nss18sch_hh_cons) <- names(nss18sch_hh_cons) |> str_replace("pce_hce", "pce")
  
  nss18sch_hh_cons <- nss18sch_hh_cons |>
    clean_nss_hh_cons_2011_prices(cpist_agyear, year = "2017")
  
  return(nss18sch_hh_cons)
  
}

########################### Estimates ###########################

# clean_nss04_items_pce_estimates <- function(df, itemcodes_df) {
#   
#   nss04_items_pce_estimates <- df |> 
#     as_survey_design(weights = wt) |> 
#     clean_svy_tbl("mpce", "hhsize") |> 
#     left_join(itemcodes_df |> 
#                 select(item, itemcode),
#               by = "itemcode") |> 
#     mutate(item_variable_type = case_when(str_starts(item_variable, "pce") ~ "pce",
#                                           str_starts(item_variable, "hce") ~ "hce",
#                                           TRUE ~ item_variable
#     ))
#   
#   return(nss04_items_pce_estimates)
# }
# 
# clean_nss09t1_items_pce_estimates <- function(df, itemcodes_df) {
#   
#   nss09t1_items_pce_estimates <- df |> 
#     as_survey_design(weights = wt) |> 
#     clean_svy_tbl("mpce", "hhsize") |> 
#     left_join(itemcodes_df |> 
#                 select(item, itemcode),
#               by = "itemcode") |> 
#     mutate(item_variable_type = case_when(str_starts(item_variable, "pce") ~ "pce",
#                                           str_starts(item_variable, "hce") ~ "hce",
#                                           TRUE ~ item_variable
#     ))
#   
#   return(nss09t1_items_pce_estimates)
# }








