recursive_sorting <- function(.data, n_groups = 1, self_sorting = FALSE) {
  sorting <- function(.data, n_groups = 2) {
    if (nrow(.data) > 0) {

      df <- .data %>%
        select(1:2) %>%
        set_names("name", "value") %>%
        mutate(
          cum_value = cumsum(value),
          g = c(first(name), rep(NA, times = n() - 1)),
          max_before = first(value),
          cum_value = cum_value - max_before
        )

      while(sum(is.na(df$g)) != 0) {
        df <- df %>%
          fill(g) %>%
          mutate(
            g = ifelse(cum_value > max_before, NA, g),
            g = ifelse(!is.na(lag(g)) & is.na(g), name, g),
            value = ifelse(is.na(g), value, 0),
            cum_value = cumsum(value)
          )
      }

      df %>%
        select(g) %>%
        {bind_cols(.data, .)} %>%
        rename(group = g)
    } else {
      .data %>%
        mutate(group = as.character(NA))
    }
  }

  current_data <- .data %>%
    sorting()

  if (n_groups > 1) {

    current_data <- current_data %>%
      rename(group1 = group)

    for (i in 2:n_groups) {
      current_data <- current_data %>%
        select(last_col(), 1:2) %>%
        rename(g = 1) %>%
        mutate(g = fct_inorder(g)) %>%
        group_by(g) %>%
        group_map(~., .keep = TRUE) %>%
        map(filter, var != g) %>%
        map(select, -g) %>%
        map(sorting) %>%
        map(select, 3) %>%
        map(~ bind_rows(tibble(group = as.character(NA)), .)) %>%
        bind_rows() %>%
        rename_all(str_c, i) %>%
        {bind_cols(current_data, .)}
    }
  }

  if (!self_sorting) {
    current_data %>%
      rename(self_sorting_check = 1) %>%
      mutate_at(-c(1:2), ~ ifelse(. == self_sorting_check, NA, .)) %>%
      select(-1) %>%
      {bind_cols(.data[1], .)}
  } else {
    current_data
  }
}
