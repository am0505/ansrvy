
nss_hh_itemcodes_targets <- list(
  
  # 
  # tar_target(
  #   cpist_agyear,
  #   read_dta(here("cpi_agyear.dta")) |>
  #     clean_names() |> 
  #     select(state, year, cpial, cpiiw, cpial11, cpiiw11) |> 
  #     filter(year > 1949) |> 
  #     mutate(state = str_replace(state, "&", "and")),
  #   format = "fst_tbl"
  # ),
  
  tar_target(
    sbfile,
    here("data/sb/cpifinal_2.dta"),
    format = "file"
  ),
  
  tar_target(
    cpist_agyear,
    # read_dta(here("cpifinal_2.dta")) |>
    #   clean_names() |> 
    #   select(state, staten17, year, pline19r, pline19u) |>
    #   pivot_longer(cols = c("pline19r", "pline19u"), 
    #                names_to = "urban", 
    #                values_to = "pline19") |> 
    #   mutate(deflator_sb = pline19/789.4) |> 
    #   mutate(urban = str_remove(urban, "pline19"), 
    #          urban = if_else(urban == "r", 0, 1)) |> 
    #   mutate(state = str_replace(state, "&", "and")),
    read_dta(sbfile) |>
      clean_names() |> 
      select(state, staten17, year, pline19r, pline19u) |> 
      mutate(deflator_u = pline19u/789.4, 
             deflator_r = pline19r/789.4) |> 
      mutate(state = str_replace(state, "&", "and"))
      , 
    format = "fst_tbl"
  ),
  
  
  ## 
  
  tar_target(
    nss_folder_path,
    paste0(here("data/nss/"), "/")
  ),
  
  tar_target(
    nss04_itemcodes,
    clean_nss04_itemcodes(nss_folder = nss_folder_path),
    format = "fst_tbl"
  ),
  
  tar_target(
    nss09_itemcodes,
    clean_nss09_itemcodes(nss_folder = nss_folder_path),
    format = "fst_tbl"
  ),
  
  tar_target(
    nss11_itemcodes,
    clean_nss11_itemcodes(nss_folder = nss_folder_path),
    format = "fst_tbl"
  ),
  
  tar_target(
    nss14_itemcodes,
    clean_nss14_itemcodes(nss_folder = nss_folder_path),
    format = "fst_tbl"
  )
  
)

nss_common_items_targets <- list(
  
  tar_target(
    common_items_04,
    create_common_items_04(nss04_itemcodes),
    format = "fst_tbl"
  ),
  
  tar_target(
    common_items_09,
    create_common_items_09(nss09_itemcodes),
    format = "fst_tbl"
  ),
  
  tar_target(
    common_items_11,
    create_common_items_11(nss11_itemcodes),
    format = "fst_tbl"
  ),
  
  tar_target(
    common_items_14,
    create_common_items_14(nss14_itemcodes),
    format = "fst_tbl"
  )
)

nss_hh_cons_targets <- list(
  
  # 
  ## 
  tar_target(
    nss04_hh_cons,
    clean_nss04_hh_cons(nss_folder = nss_folder_path, cpist_agyear),
    format = "fst_tbl"
  ),
  tar_target(
    nss09t1_hh_cons,
    clean_nss09t1_hh_cons(nss_folder = nss_folder_path, cpist_agyear),
    format = "fst_tbl"
  ),
  tar_target(
    nss09t2_hh_cons,
    clean_nss09t2_hh_cons(nss_folder = nss_folder_path, cpist_agyear),
    format = "fst_tbl"
  ),
  tar_target(
    nss11t1_hh_cons,
    clean_nss11t1_hh_cons(nss_folder = nss_folder_path, cpist_agyear),
    format = "fst_tbl"
  ),
  tar_target(
    nss11t2_hh_cons,
    clean_nss11t2_hh_cons(nss_folder = nss_folder_path, cpist_agyear),
    format = "fst_tbl"
  ),
  tar_target(
    nss14_hh_cons,
    clean_nss14_hh_cons(nss_folder = nss_folder_path, cpist_agyear),
    format = "fst_tbl"
  ),
  tar_target(
    nss14sc_hh_cons,
    clean_nss14sc_hh_cons(nss_folder = nss_folder_path, cpist_agyear),
    format = "fst_tbl"
  ),
  tar_target(
    nss18sc_hh_cons,
    clean_nss18sc_hh_cons(nss_folder = nss_folder_path, cpist_agyear),
    format = "fst_tbl"
  ),
  tar_target(
    nss14sch_hh_cons,
    clean_nss14sch_hh_cons(nss_folder = nss_folder_path, cpist_agyear),
    format = "fst_tbl"
  ),
  tar_target(
    nss18sch_hh_cons,
    clean_nss18sch_hh_cons(nss_folder = nss_folder_path, cpist_agyear),
    format = "fst_tbl"
  )
)

nss_items_pce_estimates_targets <- list(
  
  tar_target(
    nss04_items_pce_estimates,
    clean_svy_tbl(nss04_hh_cons, nss04_itemcodes, "2004"),
    format = "fst_tbl"
  ),
  tar_target(
    nss09t1_items_pce_estimates,
    clean_svy_tbl(nss09t1_hh_cons, nss09_itemcodes, "2009t1"),
    format = "fst_tbl"
  ),
  tar_target(
    nss09t2_items_pce_estimates,
    clean_svy_tbl(nss09t2_hh_cons, nss09_itemcodes, "2009t2"),
    format = "fst_tbl"
  ),
  tar_target(
    nss11t1_items_pce_estimates,
    clean_svy_tbl(nss11t1_hh_cons, nss11_itemcodes, "2011t1"),
    format = "fst_tbl"
  ),
  tar_target(
    nss11t2_items_pce_estimates,
    clean_svy_tbl(nss11t2_hh_cons, nss11_itemcodes, "2011t2"),
    format = "fst_tbl"
  ),
  tar_target(
    nss14_items_pce_estimates,
    clean_svy_tbl(nss14_hh_cons, nss14_itemcodes, "2014"),
    format = "fst_tbl"
  ),
  tar_target(
    nss14sc_items_pce_estimates,
    clean_svy_tbl(nss14sc_hh_cons, nss_round = "2014sc"),
    format = "fst_tbl"
  ),
  tar_target(
    nss18sc_items_pce_estimates,
    clean_svy_tbl(nss18sc_hh_cons, nss_round = "2018sc"),
    format = "fst_tbl"
  ),
  tar_target(
    nss14sch_items_pce_estimates,
    clean_svy_tbl(nss14sch_hh_cons, nss_round = "2014sch"),
    format = "fst_tbl"
  ),
  tar_target(
    nss18sch_items_pce_estimates,
    clean_svy_tbl(nss18sch_hh_cons, nss_round = "2018sch"),
    format = "fst_tbl"
  ),
  tar_target(
    nss_items_pce_estimates,
    {
      bind_rows(nss04_items_pce_estimates |> 
                  left_join(common_items_04, by = c("itemcode" = "itemcode_04")),
                nss09t1_items_pce_estimates |> 
                  left_join(common_items_09, by = c("itemcode" = "itemcode_09")),
                nss09t2_items_pce_estimates |> 
                  left_join(common_items_09, by = c("itemcode" = "itemcode_09")),
                nss11t1_items_pce_estimates |> 
                  left_join(common_items_11, by = c("itemcode" = "itemcode_11")),
                nss11t2_items_pce_estimates |> 
                  left_join(common_items_11, by = c("itemcode" = "itemcode_11")),
                nss14_items_pce_estimates |> 
                  left_join(common_items_14, by = c("itemcode" = "itemcode_14")),
                nss14sc_items_pce_estimates,
                nss14sch_items_pce_estimates,
                nss18sc_items_pce_estimates,
                nss18sch_items_pce_estimates
      ) |>
        mutate(mpce_hh = case_when(nss_round %in% c("2004", "2009t1", "2011t1") ~ mpce_urp,
                                   TRUE ~ mpce)) |>
        mutate(kmpce_hh = case_when(nss_round %in% c("2004", "2009t1", "2011t1") ~ kmpce_urp,
                                    TRUE ~ kmpce)) #|> 
      #mutate(year = str_sub(nss_round, 1, 4))  
    },
    format = "fst_tbl"
  )
)

nss_items_pce_estimates_st_targets <- list(
  
  tar_target(
    nss04_items_pce_estimates_st,
    clean_svy_tbl_st(nss04_hh_cons, nss04_itemcodes, "2004"),
    format = "fst_tbl"
  ),
  tar_target(
    nss09t1_items_pce_estimates_st,
    clean_svy_tbl_st(nss09t1_hh_cons, nss09_itemcodes, "2009t1"),
    format = "fst_tbl"
  ),
  tar_target(
    nss09t2_items_pce_estimates_st,
    clean_svy_tbl_st(nss09t2_hh_cons, nss09_itemcodes, "2009t2"),
    format = "fst_tbl"
  ),
  tar_target(
    nss11t1_items_pce_estimates_st,
    clean_svy_tbl_st(nss11t1_hh_cons, nss11_itemcodes, "2011t1"),
    format = "fst_tbl"
  ),
  tar_target(
    nss11t2_items_pce_estimates_st,
    clean_svy_tbl_st(nss11t2_hh_cons, nss11_itemcodes, "2011t2"),
    format = "fst_tbl"
  ),
  tar_target(
    nss14_items_pce_estimates_st,
    clean_svy_tbl_st(nss14_hh_cons, nss14_itemcodes, "2014"),
    format = "fst_tbl"
  ),
  tar_target(
    nss14sc_items_pce_estimates_st,
    clean_svy_tbl_st(nss14sc_hh_cons, nss_round = "2014sc"),
    format = "fst_tbl"
  ),
  tar_target(
    nss18sc_items_pce_estimates_st,
    clean_svy_tbl_st(nss18sc_hh_cons, nss_round = "2018sc"),
    format = "fst_tbl"
  ),
  tar_target(
    nss14sch_items_pce_estimates_st,
    clean_svy_tbl_st(nss14sch_hh_cons, nss_round = "2014sch"),
    format = "fst_tbl"
  ),
  tar_target(
    nss18sch_items_pce_estimates_st,
    clean_svy_tbl_st(nss18sch_hh_cons, nss_round = "2018sch"),
    format = "fst_tbl"
  ),
  tar_target(
    nss_items_pce_estimates_st,
    {
      bind_rows(nss04_items_pce_estimates_st |> 
                  left_join(common_items_04, by = c("itemcode" = "itemcode_04")),
                nss09t1_items_pce_estimates_st |> 
                  left_join(common_items_09, by = c("itemcode" = "itemcode_09")),
                nss09t2_items_pce_estimates_st |> 
                  left_join(common_items_09, by = c("itemcode" = "itemcode_09")),
                nss11t1_items_pce_estimates_st |> 
                  left_join(common_items_11, by = c("itemcode" = "itemcode_11")),
                nss11t2_items_pce_estimates_st |> 
                  left_join(common_items_11, by = c("itemcode" = "itemcode_11")),
                nss14_items_pce_estimates_st |> 
                  left_join(common_items_14, by = c("itemcode" = "itemcode_14")) ,
                nss14sc_items_pce_estimates_st,
                nss14sch_items_pce_estimates_st,
                nss18sc_items_pce_estimates_st,
                nss18sch_items_pce_estimates_st
      ) |>
        mutate(mpce_hh = case_when(nss_round %in% c("2004", "2009t1", "2011t1") ~ mpce_urp,
                                   TRUE ~ mpce)) |>
        mutate(kmpce_hh = case_when(nss_round %in% c("2004", "2009t1", "2011t1") ~ kmpce_urp,
                                 TRUE ~ kmpce))
    },
    format = "fst_tbl"
  )
)



