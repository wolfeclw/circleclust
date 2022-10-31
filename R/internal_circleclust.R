
### INTERNAL CIRCULARIZE FUNCTIONS

# `rspeed_mintute` calculates the speed within a rolling 1 minute window
# Parameter used to in conjunction with circular variance to detect clustering if
# 'rspeed_threshold' is defined.

rspeed_minute <- function(x, rs_window) {
  if (sum(is.na(x)) > 0) {
    zoo::rollmedian(x, rs_window, na.rm = TRUE, fill = NA, align = "center")
  } else {
    zoo::rollmedian(x, rs_window, fill = NA, align = "center")
  }
}

# `places` defines/assigns numeric category to places identified by the
# circular variance algorithm

places <- function(df) {
  d_places <- df %>%
    dplyr::filter(move_break == 1) %>%
    dplyr::mutate(
      lag_rownum = dplyr::lag(r),
      rw_diff = r - lag_rownum,
      place_break = ifelse(rw_diff > 1 | is.na(lag_rownum), 1, 0),
      place_grp = cumsum(place_break)
    ) %>%
    dplyr::select(-c(move_break, lag_rownum, rw_diff, place_break))

  d_places <- suppressMessages(dplyr::full_join(df, d_places))
}

# `place_lapse` identifies lapses between places

place_lapse <- function(df) {
  rm_open_na_cluster <- df %>%
    dplyr::filter(
      cumsum(clustered_coord) != 0,
      rev(cumsum(rev(clustered_coord)) != 0)
    )

  if (nrow(rm_open_na_cluster) > 0) {
    d_place_lapse <- rm_open_na_cluster %>%
      dplyr::mutate(place_lapse = ifelse(clustered_coord == 0, 1, 0)) %>%
      dplyr::filter(place_lapse == 1)

    d_place_lapse <- d_place_lapse %>%
      dplyr::mutate(
        lag_rownum = dplyr::lag(r),
        rw_diff = r - lag_rownum,
        break_yn = ifelse(rw_diff > 1 | is.na(lag_rownum), 1, 0),
        place_lapse_grp = cumsum(break_yn)
      ) %>%
      dplyr::select(-c(place_lapse, lag_rownum, rw_diff, break_yn))

    c_lapse_join <- suppressMessages(dplyr::full_join(df, d_place_lapse))

    p_laps_grp <- c_lapse_join %>%
      dplyr::group_by(place_lapse_grp) %>%
      dplyr::mutate(n_pl_lapse_grp = ifelse(!is.na(place_lapse_grp), dplyr::n(), NA)) %>%
      dplyr::ungroup()
  }
}

# `place_lapse_dist` calculates distances between the centroids of sequential
# places

place_lapse_dist <- function(df) {
  p_lapse_grps <- df %>%
    dplyr::filter(!is.na(place_grp))

  p_lapse_grps <- p_lapse_grps %>%
    dplyr::select(place_grp, lat, lon) %>%
    dplyr::group_by(place_grp) %>%
    dplyr::summarise(
      mlat = median(lat),
      mlon = median(lon)
    )

  if (nrow(p_lapse_grps) > 1) {
    p_lapse1 <- p_lapse_grps[-nrow(p_lapse_grps), ] %>%
      sf::st_as_sf(coords = c("mlon", "mlat"), crs = 4326)

    p_lapse2 <- p_lapse_grps[2:nrow(p_lapse_grps), ] %>%
      sf::st_as_sf(coords = c("mlon", "mlat"), crs = 4326)

    p_dist <- sf::st_distance(p_lapse1, p_lapse2, by_element = TRUE) %>%
      tibble::enframe(name = NULL) %>%
      dplyr::mutate(
        place_lapse_grp = 1:nrow(.),
        pl_distance = as.numeric(round(value, digits = 3))
      ) %>%
      dplyr::select(
        place_lapse_grp,
        pl_distance
      )

    p_dist_join <- suppressMessages(dplyr::full_join(df, p_dist))

  } else {
    df %>% dplyr::mutate(pl_distance = NA)
  }
}

# `cluster` aggregates places identified by the circular variance algorithm
# into larger clusters. If the number of obs in a cluster is below the
# 'cluster_threshold', those observations are retained, but unclustered.
# Clusters are reordered if any observations are unclustered.

cluster <- function(df, cluster_threshold = NULL) {
  d_clust_breaks <- df %>%
    dplyr::filter(!is.na(place_grp)) %>%
    dplyr::mutate(
      lag_rownum = dplyr::lag(r),
      rw_diff = r - lag_rownum,
      clust_break = ifelse(rw_diff > 1 | is.na(lag_rownum), 1, 0),
      cluster_grp = cumsum(clust_break)
    ) %>%
    dplyr::select(-c(clust_break, lag_rownum, rw_diff, clust_break))

  clust_join <- suppressMessages(dplyr::full_join(df, d_clust_breaks))

  if (!is.null(cluster_threshold)) {
    if (!is.numeric(cluster_threshold)) {
      stop("Invalid 'type' of argument assigned to 'cluster_threshold.' Expecting a numeric value.", call. = FALSE)
    }

    dc <- clust_join %>%
      dplyr::group_by(cluster_grp) %>%
      dplyr::mutate(cluster_nrow = ifelse(is.na(cluster_grp), NA, dplyr::n())) %>%
      dplyr::ungroup()

    clust_n <- dc %>%
      dplyr::group_by(cluster_nrow) %>%
      dplyr::summarise(n_max = max(cluster_nrow)) %>%
      tidyr::drop_na() %>%
      .$n_max

    rm_clust <- sum(clust_n < cluster_threshold)

    d_clust <- dc %>% dplyr::select(-c(
      move_break, r, place_grp,
      place_lapse_grp, pl_distance, cluster_nrow
    ))

    if (rm_clust > 0) {
      dc[!is.na(dc$cluster_nrow) & dc$cluster_nrow < cluster_threshold, "place_grp"] <- NA

      dc_rm <- dc[, !grepl("cluster_grp", colnames(dc))]

      reorder_clust <- dc_rm %>%
        dplyr::filter(!is.na(place_grp)) %>%
        dplyr::mutate(
          lag_rownum = dplyr::lag(r),
          rw_diff = r - lag_rownum,
          clust_break = ifelse(rw_diff > 1 | is.na(lag_rownum), 1, 0),
          cluster_grp = cumsum(clust_break)
        ) %>%
        dplyr::select(-c(clust_break, lag_rownum, rw_diff, clust_break))

      d_clust <- suppressMessages(dplyr::full_join(dc_rm, reorder_clust))
      d_clust <- d_clust %>%
        dplyr::select(-c(move_break, r, place_lapse_grp, place_grp, n_pl_lapse_grp, clustered_coord, pl_distance, cluster_nrow))

      message(paste(
        "A total of", rm_clust,
        "identified clusters had observations fewer than the 'cluster_threshold.'",
        "\n These observations were retained, but unclustered."
      ))
    }
  } else {
    d_clust <- clust_join %>%
      dplyr::select(-c(move_break, r, place_lapse_grp, place_grp, n_pl_lapse_grp, clustered_coord, pl_distance))
  }
  d_clust
}
