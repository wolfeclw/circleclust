

###
### grp_clusters() identifies the groupings of clusters within a certain distance threshold

grp_clusters <- function(df, distance_threshold = 100) {

  d_places <- df %>%
    dplyr::filter(!is.na(cluster_grp)) %>%
    dplyr::group_split(cluster_grp) %>%
    purrr::map(., ~dplyr::mutate(., p_lat = median(lat, na.rm = TRUE),
                                 p_lon = median(lon, na.rm = TRUE))) %>%
    purrr::map_df(., ~dplyr::group_nest(., .key = 'p_data', cluster_grp, p_lat, p_lon)) %>%
    dplyr::select(-p_data) %>%
    sf::st_as_sf(., coords = c('p_lon', 'p_lat'), crs = 4326)


  ## distance matrix between places
  p_dist <- sf::st_distance(d_places)
  p_dist <- units::drop_units(p_dist)

  d_pos <- which(p_dist < distance_threshold & p_dist > 0, arr.ind = T)

  if (nrow(d_pos) < 1) {
    stop(paste0('All clusters are located more than ', distance_threshold, ' meters apart. ',
                'Select a larger distance threshold to aggregate clusters.'),
         call. = FALSE)
  }

  g_list <- tibble::as_tibble(d_pos) %>%
    dplyr::mutate(rsum = row + col,
                  rdiff = abs(row - col)) %>%
    dplyr::arrange(row) %>%
    dplyr::group_split(rsum)

  # identify unique combos of rows and columns (clusters)
  combos <- purrr::map(g_list, ~dplyr::group_by(., rsum, rdiff)) %>%
    purrr::map(., ~dplyr::mutate(., rcombo = dplyr::row_number())) %>%
    purrr::map_df(., ~dplyr::filter(., rcombo == 1)) %>%
    dplyr::arrange(row)

  urows_f <- duplicated(combos$row) %>% tibble::enframe(value = 'urows_f', name = NULL)
  urows_b <- duplicated(combos$row, fromLast = TRUE) %>% tibble::enframe(value = 'urows_b', name = NULL)
  d_urows <- dplyr::bind_cols(urows_f, urows_b)
  d_urows <- d_urows %>%
    dplyr::transmute(urow = ifelse(urows_f + urows_b > 0, 0, 1))

  ucols_f <- duplicated(combos$col) %>% tibble::enframe(value = 'ucols_f', name = NULL)
  ucols_b <- duplicated(combos$col, fromLast = TRUE) %>% tibble::enframe(value = 'ucols_b', name = NULL)
  d_ucols <- dplyr::bind_cols(ucols_f, ucols_b)
  d_ucols <- d_ucols %>%
    dplyr::transmute(ucol = ifelse(ucols_f + ucols_b > 0, 0, 1))

  urowcol <- dplyr::bind_cols(d_urows, d_ucols)

  ucombo_cols <- dplyr::bind_cols(combos, urowcol) %>% dplyr::ungroup()

  u_list <- ucombo_cols %>%
    dplyr::filter(!urow == 1) %>%
    dplyr::mutate(ugrp = cumsum(ucol)) %>%
    dplyr::group_split(ugrp)

  uvector <- function(d) {
    unique(c(d$row, d$col))
  }

  purrr::map(u_list, uvector)

}


###
### reorders aggregated clusters--new clusters are assigned the minimum cluster
### value (i.e. first cluster visited within the grouping)

reorder_grp_clusters <- function(df, dt_field = 'NULL', l_cluster_grps, cluster_list) {

  grped_clusters <- purrr::map(1:length(l_cluster_grps),
                               ~dplyr::filter(df, cluster_grp %in% l_cluster_grps[[.]])) %>%
    purrr::map(., ~dplyr::mutate(., spatiotemp_cluster_grp = cluster_grp,
                                 merge_id = dplyr::first(cluster_grp)))

  ungrped_clusters <- purrr::map(cluster_list,
                                 ~dplyr::filter(., !cluster_grp %in% c(unlist(l_cluster_grps), NA))) %>%
    purrr::discard(~nrow(.) == 0) %>%
    purrr::map(., ~dplyr::mutate(., spatiotemp_cluster_grp = cluster_grp,
                                 merge_id = dplyr::first(cluster_grp)))

  na_clusters <- purrr::map(cluster_list, ~dplyr::filter(., is.na(cluster_grp))) %>%
    purrr::discard(~nrow(.) == 0) %>%
    purrr::map(., ~dplyr::mutate(., spatiotemp_cluster_grp = cluster_grp))

  cluster_bind <- c(grped_clusters, ungrped_clusters) %>%
    purrr::reduce(., dplyr::bind_rows) %>%
    dplyr::arrange(.[[dt_field]])

  l_bind <- dplyr::group_split(cluster_bind, merge_id)

  c_reorder <- map(l_bind, ~dplyr::select(., -cluster_grp)) %>%
    map2(., 1:length(.), ~dplyr::mutate(.x, cluster_grp = .y))

  c_clust_mobile <- c(c_reorder, na_clusters)%>%
    purrr::reduce(., bind_rows) %>%
    dplyr::arrange(.[[dt_field]])

  c_clust_mobile

}
