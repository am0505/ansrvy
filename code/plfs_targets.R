
plfs_targets <- list(
 
  tar_target(
    plfs_panel,
    plfs_gen_panel(),
    format = "fst_tbl"
  ),
  
  tar_target(
    plfs_status_cus,
    plfs_gen_status_cus(),
    format = "fst_tbl"
  ),
  
  tar_target(
    plfs_status_cws,
    plfs_gen_status_cws(),
    format = "fst_tbl"
  ),
  
  tar_target(
    plfs_hh_layout_2017_18,
    plfs_read_layout(file = "data/plfs/2017-18/Data_LayoutPLFS_2017-18.xlsx",
                     n_skip = 2,
                     n_max = 32),
    format = "fst_tbl"
  ),
  
  tar_target(
    plfs_hh_layout_2018_19,
    plfs_read_layout(file = "data/plfs/2018-19/Data_LayoutPLFS_2018-19.xlsx",
                     n_skip = 2,
                     n_max = 32),
    format = "fst_tbl"
  ),
  
  tar_target(
    plfs_hh_layout_2019_20,
    plfs_read_layout(file = "data/plfs/2019-20/Data_LayoutPLFS_2019-20.xlsx",
                     n_skip = 2,
                     n_max = 32),
    format = "fst_tbl"
  ),
  
  tar_target(
    plfs_hh_layout_2020_21_fv,
    plfs_read_layout(file = "data/plfs/2020-21/Data_LayoutPLFS_2020-21.xlsx",
                     n_skip = 2,
                     n_max = 39),
    format = "fst_tbl"
  ),
  
  tar_target(
    plfs_hh_layout_2020_21_rv,
    plfs_read_layout(file = "data/plfs/2020-21/Data_LayoutPLFS_2020-21.xlsx",
                     n_skip = 44,
                     n_max = 32),
    format = "fst_tbl"
  ),
  
  tar_target(
    plfs_hh_layout_2021_22_fv,
    plfs_read_layout(file = "data/plfs/2021-22/Data_LayoutPLFS_2021-22.xlsx",
                     n_skip = 2,
                     n_max = 39),
    format = "fst_tbl"
  ),
  
  tar_target(
    plfs_hh_layout_2021_22_rv,
    plfs_read_layout(file = "data/plfs/2021-22/Data_LayoutPLFS_2021-22.xlsx",
                     n_skip = 44,
                     n_max = 32),
    format = "fst_tbl"
  ),
  
  ###  Person Files Layout
  
  #### FV
  tar_target(
    plfs_per_layout_2017_18_fv,
    plfs_read_layout(file = "data/plfs/2017-18/Data_LayoutPLFS_2017-18.xlsx",
                     n_skip = 38,
                     n_max = 129),
    format = "fst_tbl"
  ),
  
  tar_target(
    plfs_per_layout_2018_19_fv,
    plfs_read_layout(file = "data/plfs/2018-19/Data_LayoutPLFS_2018-19.xlsx",
                     n_skip = 38,
                     n_max = 129),
    format = "fst_tbl"
  ),
  
  tar_target(
    plfs_per_layout_2019_20_fv,
    plfs_read_layout(file = "data/plfs/2019-20/Data_LayoutPLFS_2019-20.xlsx",
                     n_skip = 38,
                     n_max = 129),
    format = "fst_tbl"
  ),
  
  tar_target(
    plfs_per_layout_2020_21_fv,
    plfs_read_layout(file = "data/plfs/2020-21/Data_LayoutPLFS_2020-21.xlsx",
                     n_skip = 81,
                     n_max = 159),
    format = "fst_tbl"
  ),
  
  tar_target(
    plfs_per_layout_2021_22_fv,
    plfs_read_layout(file = "data/plfs/2021-22/Data_LayoutPLFS_2021-22.xlsx",
                     n_skip = 81,
                     n_max = 145),
    format = "fst_tbl"
  ),
   
  tar_target(
    plfs_per_layout_cy2021_fv,
    plfs_read_layout(file = "data/plfs/2021/Data_LayoutPLFS_2021_without DAS.xlsx",
                     n_skip = 46,
                     n_max = 74),
    format = "fst_tbl"
  ),
  
  tar_target(
    plfs_per_layout_cy2022_fv,
    plfs_read_layout(file = "data/plfs/2022/Data_LayoutPLFS_2022.xlsx",
                     n_skip = 46,
                     n_max = 74),
    format = "fst_tbl"
  ),
  
  ### RV
  tar_target(
    plfs_per_layout_2017_18_rv,
    plfs_read_layout(file = "data/plfs/2017-18/Data_LayoutPLFS_2017-18.xlsx",
                     n_skip = 171,
                     n_max = 105),
    format = "fst_tbl"
  ),
  tar_target(
    plfs_per_layout_2018_19_rv,
    plfs_read_layout(file = "data/plfs/2018-19/Data_LayoutPLFS_2018-19.xlsx",
                     n_skip = 171,
                     n_max = 105),
    format = "fst_tbl"
  ),
  tar_target(
    plfs_per_layout_2019_20_rv,
    plfs_read_layout(file = "data/plfs/2019-20/Data_LayoutPLFS_2019-20.xlsx",
                     n_skip = 171,
                     n_max = 105),
    format = "fst_tbl"
  ),
  tar_target(
    plfs_per_layout_2020_21_rv,
    plfs_read_layout(file = "data/plfs/2020-21/Data_LayoutPLFS_2020-21.xlsx",
                     n_skip = 245,
                     n_max = 105),
    format = "fst_tbl"
  ),
  tar_target(
    plfs_per_layout_2021_22_rv,
    plfs_read_layout(file = "data/plfs/2021-22/Data_LayoutPLFS_2021-22.xlsx",
                     n_skip = 229,
                     n_max = 105),
    format = "fst_tbl"
  ),
  
  ############# Household Targets #####################
  
  tar_target(
    plfs_hhfv17_18,
    parse_hh(file = "data/plfs/2017-18/FHH_FV.TXT",
             yr = 1,
             layout = plfs_hh_layout_2017_18),
    format = "fst_tbl"
  ),
  
  tar_target(
    plfs_hhrv17_18,
    parse_hh(file = "data/plfs/2017-18/FHH_RV.TXT",
             yr = 1,
             layout = plfs_hh_layout_2017_18),
    format = "fst_tbl"
  ),
  
  tar_target(
    plfs_hhfv18_19,
    parse_hh(file = "data/plfs/2018-19/hh104_fv_final.txt",
             yr = 2,
             layout = plfs_hh_layout_2018_19),
    format = "fst_tbl"
  ),
  
  tar_target(
    plfs_hhrv18_19,
    parse_hh(file = "data/plfs/2018-19/hh104_rv_final.txt",
             yr = 2,
             layout = plfs_hh_layout_2018_19),
    format = "fst_tbl"
  ),
  
  tar_target(
    plfs_hhfv19_20,
    parse_hh(file = "data/plfs/2019-20/HHV1.TXT",
             yr = 3,
             layout = plfs_hh_layout_2019_20),
    format = "fst_tbl"
  ),
  
  tar_target(
    plfs_hhrv19_20,
    parse_hh(file = "data/plfs/2019-20/HHRV.TXT",
             yr = 3,
             layout = plfs_hh_layout_2019_20),
    format = "fst_tbl"
  ),
  
  tar_target(
    plfs_hhfv20_21,
    parse_hh(file = "data/plfs/2020-21/HHV1.TXT",
             yr = 4,
             layout = plfs_hh_layout_2020_21_fv),
    format = "fst_tbl"
  ),
  
  tar_target(
    plfs_hhrv20_21,
    parse_hh(file = "data/plfs/2020-21/HHRV.TXT",
             yr = 4,
             layout = plfs_hh_layout_2020_21_rv),
    format = "fst_tbl"
  ),
  
  tar_target(
    plfs_hhfv21_22,
    parse_hh(file = "data/plfs/2021-22/HHV1.TXT",
             yr = 5,
             layout = plfs_hh_layout_2021_22_fv),
    format = "fst_tbl"
  ),
  
  tar_target(
    plfs_hhrv21_22,
    parse_hh(file = "data/plfs/2021-22/HHRV.TXT",
             yr = 5,
             layout = plfs_hh_layout_2021_22_rv),
    format = "fst_tbl"
  ),
  
  tar_target(
    plfs_hh,
    bind_rows(plfs_hhfv17_18, plfs_hhrv17_18,
              plfs_hhfv18_19, plfs_hhrv18_19,
              plfs_hhfv19_20, plfs_hhrv19_20,
              plfs_hhfv20_21, plfs_hhrv20_21,
              plfs_hhfv21_22, plfs_hhrv21_22),
    format = "fst_tbl"
  ),
  
  ############# Person Targets #####################
  
  ## RV
  tar_target(
    plfs_perrv17_18,
    parse_prv(file = "data/plfs/2017-18/FPER_RV.TXT",
              yr = 1,
              layout = plfs_per_layout_2017_18_rv) |>
      as_tibble(),
    format = "fst_tbl"
  ),
  tar_target(
    plfs_perrv18_19,
    parse_prv(file = "data/plfs/2018-19/FPER_RV.TXT",
              yr = 2,
              layout = plfs_per_layout_2018_19_rv) |>
      as_tibble(),
    format = "fst_tbl"
  ),
  tar_target(
    plfs_perrv19_20,
    parse_prv(file = "data/plfs/2019-20/PERRV.TXT",
              yr = 3,
              layout = plfs_per_layout_2019_20_rv) |>
      as_tibble() |> 
      filter(file_id != ""),
    format = "fst_tbl"
  ),
  tar_target(
    plfs_perrv20_21,
    parse_prv(file = "data/plfs/2020-21/PERRV.txt",
              yr = 4,
              layout = plfs_per_layout_2020_21_rv) |>
      as_tibble(),
    format = "fst_tbl"
  ),
  tar_target(
    plfs_perrv21_22,
    parse_prv(file = "data/plfs/2021-22/PERRV.TXT",
              yr = 5,
              layout = plfs_per_layout_2021_22_rv) |>
      as_tibble(),
    format = "fst_tbl"
  ),
  
  ## FV
  tar_target(
    plfs_perfv17_18,
    parse_pfv(file = "data/plfs/2017-18/FPER_FV.TXT",
              yr = 1,
              layout = plfs_per_layout_2017_18_fv) |>
      as_tibble(),
    format = "fst_tbl"
  ),
  tar_target(
    plfs_perfv18_19,
    parse_pfv(file = "data/plfs/2018-19/per104_fv_final.txt",
              yr = 2,
              layout = plfs_per_layout_2018_19_fv) |>
      as_tibble(),
    format = "fst_tbl"
  ),
  tar_target(
    plfs_perfv19_20,
    parse_pfv(file = "data/plfs/2019-20/PERV1.TXT",
              yr = 3,
              layout = plfs_per_layout_2019_20_fv) |>
      as_tibble(),
    format = "fst_tbl"
  ),
  tar_target(
    plfs_perfv20_21,
    parse_pfv(file = "data/plfs/2020-21/PERV1.TXT",
              yr = 4,
              layout = plfs_per_layout_2020_21_fv) |>
      as_tibble(),
    format = "fst_tbl"
  ),
  tar_target(
    plfs_perfv21_22,
    parse_pfv(file = "data/plfs/2021-22/PERV1.TXT",
              yr = 5,
              layout = plfs_per_layout_2021_22_fv) |>
      as_tibble(),
    format = "fst_tbl"
  ),
  
  tar_target(
    plfs_perfv,
    {
      bind_rows(plfs_perfv17_18, #plfs_perrv17_18,
                plfs_perfv18_19, #plfs_perrv18_19,
                plfs_perfv19_20, #plfs_perrv19_20,
                plfs_perfv20_21, #plfs_perrv20_21,
                plfs_perfv21_22#, #plfs_perrv21_22
      ) |> 
        mutate(across(c("earnings_salaried", "earnings_self_emp", starts_with("hours_"), starts_with("tot_hours_"), starts_with("additional_hours_"), starts_with("wage_")), as.double)) |> 
        mutate(across(c("earnings_salaried", "earnings_self_emp"), ~ if_else(.x < 0, 0, .x))) |> 
        clean_names() |> 
        mutate(worked_a1_d1 = if_else(wage_a1_d1 > 0, 1, 0),
               worked_a1_d2 = if_else(wage_a1_d2 > 0, 1, 0),
               worked_a1_d3 = if_else(wage_a1_d3 > 0, 1, 0),
               worked_a1_d4 = if_else(wage_a1_d4 > 0, 1, 0),
               worked_a1_d5 = if_else(wage_a1_d5 > 0, 1, 0),
               worked_a1_d6 = if_else(wage_a1_d6 > 0, 1, 0),
               worked_a1_d7 = if_else(wage_a1_d7 > 0, 1, 0),
               worked_a2_d1 = if_else(wage_a2_d1 > 0, 1, 0),
               worked_a2_d2 = if_else(wage_a2_d2 > 0, 1, 0),
               worked_a2_d3 = if_else(wage_a2_d3 > 0, 1, 0),
               worked_a2_d4 = if_else(wage_a2_d4 > 0, 1, 0),
               worked_a2_d5 = if_else(wage_a2_d5 > 0, 1, 0),
               worked_a2_d6 = if_else(wage_a2_d6 > 0, 1, 0),
               worked_a2_d7 = if_else(wage_a2_d7 > 0, 1, 0)
        ) |> 
        mutate(tot_hours_weekly = tot_hours_d1 + tot_hours_d2 + tot_hours_d3 + tot_hours_d4 + tot_hours_d5 + tot_hours_d6 + tot_hours_d7,
               additional_hours_weekly = additional_hours_d1 + additional_hours_d2 + additional_hours_d3 + additional_hours_d4 + additional_hours_d5 + additional_hours_d6 + additional_hours_d7
               ) |> 
        mutate(a1_total_days = worked_a1_d1 + worked_a1_d2 + worked_a1_d3 + worked_a1_d4 + worked_a1_d5 + worked_a1_d6 + worked_a1_d7,
               a2_total_days = worked_a2_d1 + worked_a2_d2 + worked_a2_d3 + worked_a2_d4 + worked_a2_d5 + worked_a2_d6 + worked_a2_d7) |> 
        mutate(a1_total_wage = wage_a1_d1 + wage_a1_d2 + wage_a1_d3 + wage_a1_d4 + wage_a1_d5 + wage_a1_d6 + wage_a1_d7,
               a2_total_wage = wage_a2_d1 + wage_a2_d2 + wage_a2_d3 + wage_a2_d4 + wage_a2_d5 + wage_a2_d6 + wage_a2_d7,
               a1_total_hours = hours_a1_d1 + hours_a1_d2 + hours_a1_d3 + hours_a1_d4 + hours_a1_d5 + hours_a1_d6 + hours_a1_d7,
               a2_total_hours = hours_a2_d1 + hours_a2_d2 + hours_a2_d3 + hours_a2_d4 + hours_a2_d5 + hours_a2_d6 + hours_a2_d7,
               a1_wage_per_hour = if_else(a1_total_hours > 0, a1_total_wage/a1_total_hours, 0),
               a2_wage_per_hour = if_else(a2_total_wage > 0, a2_total_wage/a2_total_hours, 0)
        ) |> 
        mutate(a1_wage_per_day = if_else(a1_total_days > 0, a1_total_wage/a1_total_days, 0),
               a2_wage_per_day = if_else(a2_total_days > 0, a2_total_wage/a2_total_days, 0)) |> 
        mutate(earnings_casual = a1_wage_per_day,
               earnings_casual_hourly = a1_wage_per_hour,
               earnings_total_days = a1_total_days,
               earnings_total_hours = a1_total_hours
        ) |> 
        left_join(plfs_status_cws, by = c("status_cws" = "status_cws_code"))
      }
    ,
    format = "fst_tbl"
  ),
  
  tar_target(
    plfs_perrv,
    {
      bind_rows(
        #plfs_perfv17_18, 
        plfs_perrv17_18,
        #plfs_perfv18_19, 
        plfs_perrv18_19,
        #plfs_perfv19_20, 
        plfs_perrv19_20,
        #plfs_perfv20_21, 
        plfs_perrv20_21,
        #plfs_perfv21_22#, 
        plfs_perrv21_22
      ) |> 
        clean_names() |> 
        mutate(across(c("earnings_salaried", "earnings_self_emp", starts_with("hours_"), starts_with("tot_hours_"), starts_with("additional_hours_"), starts_with("wage_")), as.double)) |> 
        mutate(across(c("earnings_salaried", "earnings_self_emp"), ~ if_else(.x < 0, 0, .x))) |> 
        clean_names() |> 
        mutate(worked_a1_d1 = if_else(wage_a1_d1 > 0, 1, 0),
               worked_a1_d2 = if_else(wage_a1_d2 > 0, 1, 0),
               worked_a1_d3 = if_else(wage_a1_d3 > 0, 1, 0),
               worked_a1_d4 = if_else(wage_a1_d4 > 0, 1, 0),
               worked_a1_d5 = if_else(wage_a1_d5 > 0, 1, 0),
               worked_a1_d6 = if_else(wage_a1_d6 > 0, 1, 0),
               worked_a1_d7 = if_else(wage_a1_d7 > 0, 1, 0),
               worked_a2_d1 = if_else(wage_a2_d1 > 0, 1, 0),
               worked_a2_d2 = if_else(wage_a2_d2 > 0, 1, 0),
               worked_a2_d3 = if_else(wage_a2_d3 > 0, 1, 0),
               worked_a2_d4 = if_else(wage_a2_d4 > 0, 1, 0),
               worked_a2_d5 = if_else(wage_a2_d5 > 0, 1, 0),
               worked_a2_d6 = if_else(wage_a2_d6 > 0, 1, 0),
               worked_a2_d7 = if_else(wage_a2_d7 > 0, 1, 0)
        ) |> 
        mutate(tot_hours_weekly = tot_hours_d1 + tot_hours_d2 + tot_hours_d3 + tot_hours_d4 + tot_hours_d5 + tot_hours_d6 + tot_hours_d7,
               additional_hours_weekly = additional_hours_d1 + additional_hours_d2 + additional_hours_d3 + additional_hours_d4 + additional_hours_d5 + additional_hours_d6 + additional_hours_d7) |> 
        mutate(a1_total_days = worked_a1_d1 + worked_a1_d2 + worked_a1_d3 + worked_a1_d4 + worked_a1_d5 + worked_a1_d6 + worked_a1_d7,
               a2_total_days = worked_a2_d1 + worked_a2_d2 + worked_a2_d3 + worked_a2_d4 + worked_a2_d5 + worked_a2_d6 + worked_a2_d7) |> 
        mutate(a1_total_wage = wage_a1_d1 + wage_a1_d2 + wage_a1_d3 + wage_a1_d4 + wage_a1_d5 + wage_a1_d6 + wage_a1_d7,
               a2_total_wage = wage_a2_d1 + wage_a2_d2 + wage_a2_d3 + wage_a2_d4 + wage_a2_d5 + wage_a2_d6 + wage_a2_d7,
               a1_total_hours = hours_a1_d1 + hours_a1_d2 + hours_a1_d3 + hours_a1_d4 + hours_a1_d5 + hours_a1_d6 + hours_a1_d7,
               a2_total_hours = hours_a2_d1 + hours_a2_d2 + hours_a2_d3 + hours_a2_d4 + hours_a2_d5 + hours_a2_d6 + hours_a2_d7,
               a1_wage_per_hour = if_else(a1_total_hours > 0, a1_total_wage/a1_total_hours, 0),
               a2_wage_per_hour = if_else(a2_total_wage > 0, a2_total_wage/a2_total_hours, 0)
        ) |> 
        mutate(a1_wage_per_day = if_else(a1_total_days > 0, a1_total_wage/a1_total_days, 0),
               a2_wage_per_day = if_else(a2_total_days > 0, a2_total_wage/a2_total_days, 0)) |> 
        mutate(earnings_casual = a1_wage_per_day,
               earnings_casual_hourly = a1_wage_per_hour,
               earnings_total_days = a1_total_days,
               earnings_total_hours = a1_total_hours) |> 
        left_join(plfs_status_cws, by = c("status_cws" = "status_cws_code"))
      
    },
    format = "fst_tbl"
  ),
  
  
  ############ Calendar Year ##########
  
  tar_target(
    plfs_perfvcy21,
    parse_pfv(file = "data/plfs/2021/CPERV1.txt",
              yr = "2021",
              layout = plfs_per_layout_cy2021_fv) |>
      as_tibble(),
    format = "fst_tbl"
  ),
  
  tar_target(
    plfs_perfvcy22,
    parse_pfv(file = "data/plfs/2022/CPERV1.txt",
              yr = "2022",
              layout = plfs_per_layout_cy2022_fv) |>
      as_tibble(),
    format = "fst_tbl"
  ),
  tar_target(
    plfs_perfvcy,
    {
      bind_rows(plfs_perfvcy21, #plfs_perrv20_21,
                plfs_perfvcy22#, #plfs_perrv21_22
      ) #|> 
        # mutate(across(c("earnings_salaried", "earnings_self_emp", starts_with("hours"), starts_with("wage")), as.double)) |> 
        # mutate(across(c("earnings_salaried", "earnings_self_emp"), ~ if_else(.x < 0, 0, .x))) |> 
        # clean_names() |> 
        # mutate(a1_total_wage = wage_a1_d1 + wage_a1_d2 + wage_a1_d3 + wage_a1_d4 + wage_a1_d5 + wage_a1_d6 + wage_a1_d7,
        #        a2_total_wage = wage_a2_d1 + wage_a2_d2 + wage_a2_d3 + wage_a2_d4 + wage_a2_d5 + wage_a2_d6 + wage_a2_d7,
        #        a1_total_hours = hours_a1_d1 + hours_a1_d2 + hours_a1_d3 + hours_a1_d4 + hours_a1_d5 + hours_a1_d6 + hours_a1_d7,
        #        a2_total_hours = hours_a2_d1 + hours_a2_d2 + hours_a2_d3 + hours_a2_d4 + hours_a2_d5 + hours_a2_d6 + hours_a2_d7,
        #        a1_wage_per_hour = a1_total_wage/a1_total_hours,
        #        a2_wage_per_hour = a2_total_wage/a2_total_hours
        # ) |> 
        # #mutate(a1_employed_days_weekly = is.na(wage_a1_d1) + is.na(wage_a1_d2)) |> 
        # left_join(plfs_status_cws, by = c("status_cws" = "status_cws_code"))
    }
    ,
    format = "fst_tbl"
  )
)

plfs_estimates_targets <- list(
  
  tar_target(
    plfs_yearly_lf_cws_estimates,
    plfs_get_cws_estimates(plfs_perfv, "year"),
    format = "fst_tbl"
  ),
  tar_target(
    plfs_quarterly_lf_cws_estimates,
    plfs_get_cws_estimates(plfs_perfv, yr_qtr = c("year", "quarter")),
    format = "fst_tbl"
  ),
  
  tar_target(
    plfs_yearly_lf_cws_estimates_cy,
    plfs_get_cws_estimates(plfs_perfvcy, yr_qtr = c("year")),
    format = "fst_tbl"
  ),
  tar_target(
    plfs_quarterly_lf_cws_estimates_cy,
    plfs_get_cws_estimates(plfs_perfvcy, yr_qtr = c("year", "quarter")),
    format = "fst_tbl"
  ),
  
  #### Earnings Estimates ######
  
  tar_target(
    plfs_earnings_est,
    plfs_get_cws_earnings_estimates(plfs_perfv),
    format = "fst_tbl"
  ),
  tar_target(
    plfs_earnings_est_rv,
    plfs_get_cws_earnings_estimates(plfs_perrv),
    format = "fst_tbl"
  ),
  
  tar_target(
    plfs_output_stata,
    {
    plfs_earnings_est_dta <- plfs_earnings_est |> 
      mutate(year = case_when(year == "1" ~ "2017-18",
                              year == "2" ~ "2018-19",
                              year == "3" ~ "2019-20",
                              year == "4" ~ "2020-21",
                              year == "5" ~ "2021-22"
      ))
    
    writexl::write_xlsx(plfs_earnings_est_dta,
                        "output/plfs_earnings_est.xlsx")
    
    haven::write_dta(plfs_earnings_est_dta, 
                     path = "output/plfs_earnings_est.dta",
                     label = "PLFS Earnings Esimates - Quarterly, All India by Urbanicity")
    
    plfs_earnings_est_rv_dta <- plfs_earnings_est_rv |> 
      mutate(year = case_when(year == "1" ~ "2017-18",
                              year == "2" ~ "2018-19",
                              year == "3" ~ "2019-20",
                              year == "4" ~ "2020-21",
                              year == "5" ~ "2021-22"
      ))
    
    writexl::write_xlsx(plfs_earnings_est_rv_dta,
                        "output/plfs_earnings_est_rv.xlsx")
    
    haven::write_dta(plfs_earnings_est_rv_dta, 
                     path = "output/plfs_earnings_est_rv.dta",
                     label = "PLFS Earnings Esimates RV - Quarterly, All India by Urbanicity")
    
    plfs_perfv %>%
      set_names(str_sub(names(.), 1, 32)) %>%
      haven::write_dta(path = "output/plfs_perfv.dta",
                       label = "PLFS Microdata Person Level, First Visit - Quarterly")
    
    plfs_perrv %>%
      set_names(str_sub(names(.), 1, 32)) %>%
      haven::write_dta(path = "output/plfs_perrv.dta",
                       label = "PLFS Microdata Person Level, Repeat Visit - Quarterly")
    
    },
    format = "fst_tbl"
  )
  
)


