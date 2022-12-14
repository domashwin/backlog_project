or (i in seq_along(df)) {            # 2. sequence
  output[[i]] <- median(df[[i]])      # 3. body
}
output

or (i in seq_along(df)) {            # 2. sequence
  output[[i]] <- median(df[[i]])      # 3. body
}
outputff

example_twins %>% 
  pivot_wider(
    names_from = "n", 
    names_prefix = "twin_", 
    values_from = "name"
  )