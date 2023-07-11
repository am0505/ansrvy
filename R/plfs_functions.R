
parse_hh <- function(file, yr, layout){
  
  hh_widths <- vroom::fwf_widths(widths = layout$field_length,
                                 col_names = layout$vars)
  
  hh_types <- vroom::cols(
    .default=vroom::col_character(),
    age=vroom::col_integer(),
    ss_multiplier=vroom::col_double())
  
  hh_types <- cols(.default=col_character(),
                   hh_size=col_integer(),
                   ss_multiplier=col_double())
  read_fwf(file,
           hh_widths,
           col_types=hh_types,
           na="-")%>%
    clean_names() %>%
    mutate(year=yr,
           adj_multiplier = if_else(ns_count_4s==ns_count_3s,
                                    ss_multiplier/100,
                                    ss_multiplier/200),
           uhid=paste(quarter, visit, fsu,sg_sb_no,stage2_stratum_no,hh_no,sep="")#,
           #uhid=paste(fsu,sg_sb_no,stage2_stratum_no,hh_no,sep="|")
           )
}

parse_prv <- function(file, yr, layout){
  
  prv_widths <- vroom::fwf_widths(widths = layout$field_length,
                                  col_names = layout$vars)
  
  prv_types <- vroom::cols(
    .default=vroom::col_character(),
    age=vroom::col_integer(),
    ss_multiplier=vroom::col_double())
  
  prv_types <- cols(.default=col_character(),
                    age=col_integer(),
                    ss_multiplier=col_double())
  read_fwf(file,
           prv_widths,
           col_types=prv_types,
           na="-") %>%
    clean_names() %>%
    mutate(year=yr,
           adj_multiplier = if_else(ns_count_4s==ns_count_3s,
                                    ss_multiplier,
                                    ss_multiplier/2),
           #uhid=paste(fsu,sg_sb_no,stage2_stratum_no,hh_no,sep="|"),
           uhid=paste(quarter, visit, fsu,sg_sb_no,stage2_stratum_no,hh_no,sep=""),
           upid=paste(uhid,person_no,sep="|"))  |> 
    mutate(status_cws_brd=category_status_coarse(status_cws),
           status_cws_nrw = category_status(status_cws)) |> 
    mutate(yq = paste(year, quarter, sep = ""))
}

parse_pfv <- function(file, yr, layout){
  
  pfv_widths <- vroom::fwf_widths(widths = layout$field_length,
                                  col_names = layout$vars)
  
  pfv_types <- vroom::cols(
    .default=vroom::col_character(),
    age=vroom::col_integer(),
    ss_multiplier=vroom::col_double())
  
  read_fwf(file,
           pfv_widths,
           col_types=pfv_types,
           na="-") |> 
    clean_names() |> 
    mutate(year=yr,
           adj_multiplier = if_else(ns_count_4s==ns_count_3s,
                                    ss_multiplier/100,
                                    ss_multiplier/200),
           uhid=paste(quarter, visit, fsu,sg_sb_no,stage2_stratum_no,hh_no,sep=""),
           upid=paste(uhid,person_no,sep="")) |> 
    mutate(status_cws_brd=category_status_coarse(status_cws),
           status_cws_nrw = category_status(status_cws)) |> 
    mutate(yq = paste(year, quarter, sep = ""))
  
}

category_status <- function(status.code){
  
  coarse.category.lvls <- c("slf-emp","csl-emp","sal-emp",
                            "unemp","nopart","sck-emp","nwrk")
  
  factor(case_when(
    status.code %in% c("11","12","21") ~ "slf-emp",
    status.code == "31" ~ "sal-emp",
    status.code %in% c("41","42","51") ~ "csl-emp",
    status.code %in% c("61","71") ~ "sck-emp",
    status.code %in% c("62","72") ~ "nwrk",
    status.code %in% c("81","82") ~"unemp",
    status.code %in% c("91","92","93","94","95","97","98","99")~"nopart"
  ),
  levels = coarse.category.lvls)
}

category_status_coarse <- function(status.code){
  
  case_when(
    status.code %in% c("81","82") ~"unemp",
    status.code %in% c("91","92","93","94","95","97","98","99")~"nopart",
    TRUE ~ "work"
  )
}

plfs_gen_panel <- function() {
  
  panel_sched <- tribble(
    
    ~year,~quarter,~visit,~panel,
    1,"Q1","V1","P11",
    1,"Q2","V2","P11",
    1,"Q3","V3","P11",
    1,"Q4","V4","P11",
    
    1,"Q2","V1","P12",
    1,"Q3","V2","P12",
    1,"Q4","V3","P12",
    2,"Q5","V4","P12",
    
    1,"Q3","V1","P13",
    1,"Q4","V2","P13",
    2,"Q5","V3","P13",
    2,"Q6","V4","P13",
    
    1,"Q4","V1","P14",
    2,"Q5","V2","P14",
    2,"Q6","V3","P14",
    2,"Q7","V4","P14",
    
    2,"Q5","V1","P15",
    2,"Q6","V2","P15",
    2,"Q7","V3","P15",
    2,"Q8","V4","P15",
    
    2,"Q6","V1","P16",
    2,"Q7","V2","P16",
    2,"Q8","V3","P16",
    3,"Q1","V4","P16",
    
    2,"Q7","V1","P17",
    2,"Q8","V2","P17",
    3,"Q1","V3","P17",
    3,"Q2","V4","P17",
    
    2,"Q8","V1","P18",
    3,"Q1","V2","P18",
    3,"Q2","V3","P18",
    3,"Q3","V4","P18",
    
    3,"Q1","V1","P21",
    3,"Q2","V2","P21",
    3,"Q3","V3","P21",
    3,"Q4","V4","P21",
    
    3,"Q2","V1","P22",
    3,"Q3","V2","P22",
    3,"Q4","V3","P22",
    
    3,"Q3","V1","P23",
    3,"Q4","V2","P23",
    
    3,"Q4","V1","P24"
    
  )
  
}

plfs_gen_status_cus <- function() {
  
  status_cus_df <- tribble(
    ~status, ~status_code,
    "worked in h.h. enterprise (self-employed): own account worker", "11", 
    "worked in h.h. enterprise (self-employed): employer", "12", 
    "worked as helper in h.h. enterprise (unpaid family worker)", "21",
    "worked as regular salaried/ wage employee", "31", 
    "worked as casual wage labour: in public works", "41", 
    "worked as casual wage labour: in other types of work", "51", 
    "did not work but was seeking and/or available for work", "81", 
    "attended educational institution", "91", 
    "attended domestic duties only", "92",
    "attended domestic duties and was also engaged in free collection of goods (vegetables, roots, firewood, cattle feed, etc.), sewing, tailoring, weaving, etc. for household use", "93", 
    "rentiers, pensioners , remittance recipients, etc.", "94", 
    "not able to work due to disability", "95", 
    "others (including begging, prostitution, etc.)", "97"
  )
  
  return(status_cus_df)
}

plfs_gen_status_cws <- function() {
  
  status_cws_df <- tribble(
    ~status_cws_label, ~status_cws_code,
    "worked in h.h. enterprise (self-employed): own account worker", "11", 
    "worked in h.h. enterprise (self-employed): employer", "12", 
    "worked as helper in h.h. enterprise (unpaid family worker)", "21",
    "worked as regular salaried/ wage employee", "31", 
    "worked as casual wage labour in public works other than MGNREG works", "41", 
    "worked as casual wage labour in MGNREG works", "42", 
    "worked as casual wage labour: in other types of work", "51", 
    "had work in h.h. enterprise but did not work due to: sickness", "61", 
    "had work in h.h. enterprise but did not work due to: other reasons", "62", 
    "had regular salaried/wage employment but did not work due to: sickness", "71", 
    "had regular salaried/wage employment but did not work due to: other reasons", "72",
    "sought work", "81", 
    "did not seek but was available for work", "82", 
    "attended educational institution", "91", 
    "attended domestic duties only", "92",
    "attended domestic duties and was also engaged in free collection of goods (vegetables, roots, firewood, cattle feed, etc.), sewing, tailoring, weaving, etc. for household use", "93", 
    "rentiers, pensioners , remittance recipients, etc.", "94", 
    "not able to work due to disability", "95", 
    "others (including begging, prostitution, etc.)", "97",
    "did not work due to temporary sickness (for casual workers only)", "98"
  )
  
  return(status_cws_df)
}

plfs_read_layout <- function(file, n_skip, n_max) {
  
  read_excel(here(file),
             skip = n_skip,
             n_max = n_max) |> 
    clean_names() |> 
    rename(byte_position_start = byte_position,
           byte_position_end = x7) 
  
}

plfs_get_cws_estimates <- function(df, yr_qtr) {
  
  plfs_per_fv_nest_yr_cy_ay <- df |>
    #plfs_perfv |> 
    mutate(adj_multiplier_yr = adj_multiplier/4) |> 
    nest(data = -yr_qtr) |>
    mutate(datasvy = map(data, 
                         ~ as_survey_design(.x, 
                                            weights = adj_multiplier_yr)
    ))
  
  plfs_yearly_lf_estimates_cy_ay <- plfs_per_fv_nest_yr_cy_ay |>
    mutate(cws = map(datasvy, 
                     ~ .x |> 
                       filter(age >= 15) |> 
                       group_by(status_cws_brd) |>
                       summarise(proportion = survey_mean(),
                                 total = survey_total())
    )) |> 
    select(-data, -datasvy) |> 
    unnest(cws)
  
  plfs_yearly_lf_estimates_cy_ay |> 
    #mutate(yq = paste0(year, quarter)) |>
    mutate(status_cws_brd = factor(status_cws_brd,
                                   levels = c("nopart", "work", "unemp"),
                                   labels = c("Out of Workforce", "In Workforce", "Unemployed"))) |>
    select(yr_qtr, status_cws_brd, total) |> 
    pivot_wider(names_from = "status_cws_brd",
                values_from = "total") |>
    mutate(`Population` = (`Out of Workforce` + `In Workforce` + Unemployed),
           `Labor Force` = Unemployed+`In Workforce`,
           lfpr = `Labor Force`/(`Labor Force`+`Out of Workforce`),
           wpr = `In Workforce`/`Population`,
           unemp_rate = Unemployed/`Labor Force`) |> 
    select(yr_qtr, `Population`, `Labor Force`, `In Workforce`, everything())
  
}

plfs_get_cws_summary <- function(.df) {
  
  .df |> 
    summarise(n = n(), 
              wt = sum(adj_multiplier)/100,
              earnings_self_emp = weighted.mean(earnings_self_emp[status_cws %in% c("11", "12")],
                                                adj_multiplier[status_cws %in% c("11", "12")]),
              earnings_sal_emp = weighted.mean(earnings_salaried[earnings_salaried > 0 & status_cws %in% c("31")],
                                               adj_multiplier[earnings_salaried > 0 & status_cws %in% c("31")]),
              earnings_casual_nonnrega = weighted.mean(earnings_casual[status_cws %in% c("41")],
                                                       adj_multiplier[status_cws %in% c("41")]),
              earnings_casual_nrega = weighted.mean(earnings_casual[status_cws %in% c("42")],
                                                    adj_multiplier[status_cws %in% c("42")]),
              earnings_casual_othernonpub = weighted.mean(earnings_casual[status_cws %in% c("51")],
                                                          adj_multiplier[status_cws %in% c("51")]),
              earnings_casual_nonnrega_hr = weighted.mean(earnings_casual_hourly[status_cws %in% c("41")],
                                                          adj_multiplier[status_cws %in% c("41")]),
              earnings_casual_nrega_hr = weighted.mean(earnings_casual_hourly[status_cws %in% c("42")],
                                                       adj_multiplier[status_cws %in% c("42")]),
              earnings_casual_othernonpub_hr = weighted.mean(earnings_casual_hourly[status_cws %in% c("51")],
                                                             adj_multiplier[status_cws %in% c("51")]),
              casual_nonnrega_total_days = weighted.mean(earnings_total_days[status_cws %in% c("41")],
                                                         adj_multiplier[status_cws %in% c("41")]),
              casual_nrega_total_days = weighted.mean(earnings_total_days[status_cws %in% c("42")],
                                                      adj_multiplier[status_cws %in% c("42")]),
              casual_othernonpub_total_days = weighted.mean(earnings_total_days[status_cws %in% c("51")],
                                                            adj_multiplier[status_cws %in% c("51")]),
              casual_nonnrega_total_hr = weighted.mean(earnings_total_hours[status_cws %in% c("41")],
                                                       adj_multiplier[status_cws %in% c("41")]),
              casual_nrega_total_hr = weighted.mean(earnings_total_hours[status_cws %in% c("42")],
                                                    adj_multiplier[status_cws %in% c("42")]),
              casual_othernonpub_total_hr = weighted.mean(earnings_total_hours[status_cws %in% c("51")],
                                                          adj_multiplier[status_cws %in% c("51")])) 
  
}

plfs_get_cws_earnings_estimates <- function(plfs_perfv) {
  
  plfs_earnings_est_urb <- plfs_perfv |> 
    select(file_id, year, quarter, uhid, upid, adj_multiplier, sector, age, status_cws, status_cws_label, status_cws_brd, status_cws_nrw, status_cws:earnings_self_emp, earnings_casual, earnings_casual_hourly, earnings_total_days, earnings_total_hours, a1_total_wage, a2_total_wage, a1_total_hours, a2_total_hours) |> 
    group_by(file_id, year, quarter, sector) |>
    plfs_get_cws_summary() |> 
    ungroup()
  
  plfs_earnings_est_india <- plfs_perfv |> 
    select(file_id, year, quarter, uhid, upid, adj_multiplier, sector, age, status_cws, status_cws_label, status_cws_brd, status_cws_nrw, status_cws:earnings_self_emp, earnings_casual, earnings_casual_hourly, earnings_total_days, earnings_total_hours, a1_total_wage, a2_total_wage, a1_total_hours, a2_total_hours) |> 
    group_by(file_id, year, quarter) |>
    plfs_get_cws_summary() |> 
    ungroup()
  
  plfs_earnings_est <- plfs_earnings_est_india |> 
    bind_rows(plfs_earnings_est_urb) |> 
    mutate(urbanicity = case_when(sector == "1" ~ "Rural",
                                  sector == "2" ~ "Urban",
                                  is.na(sector) ~ "All India")) |> 
    select(-sector) |> 
    select(file_id, year, quarter, urbanicity, sample_size = n, pop_size = wt, everything())
  
  return(plfs_earnings_est)
}

plfs_get_cws_earnings_estimates_by <- function(plfs_perfv, ...) {
  
  plfs_earnings_est_urb <- plfs_perfv |> 
    select(file_id, year, quarter, uhid, upid, adj_multiplier, sector, age, status_cws, status_cws_label, status_cws_brd, status_cws_nrw, status_cws:earnings_self_emp, earnings_casual, earnings_casual_hourly, earnings_total_days, earnings_total_hours, a1_total_wage, a2_total_wage, a1_total_hours, a2_total_hours, ...) |> 
    group_by(file_id, year, quarter, sector,...) |>
    plfs_get_cws_summary() |> 
    ungroup()
  
  plfs_earnings_est_india <- plfs_perfv |> 
    select(file_id, year, quarter, uhid, upid, adj_multiplier, sector, age, status_cws, status_cws_label, status_cws_brd, status_cws_nrw, status_cws:earnings_self_emp, earnings_casual, earnings_casual_hourly, earnings_total_days, earnings_total_hours, a1_total_wage, a2_total_wage, a1_total_hours, a2_total_hours, ...) |> 
    group_by(file_id, year, quarter, ...) |>
    plfs_get_cws_summary() |> 
    ungroup()
  
  plfs_earnings_est <- plfs_earnings_est_india |> 
    bind_rows(plfs_earnings_est_urb) |> 
    mutate(urbanicity = case_when(sector == "1" ~ "Rural",
                                  sector == "2" ~ "Urban",
                                  is.na(sector) ~ "All India")) |> 
    select(-sector) |> 
    select(file_id, year, quarter, urbanicity, sample_size = n, pop_size = wt, everything())
  
  return(plfs_earnings_est)
}
