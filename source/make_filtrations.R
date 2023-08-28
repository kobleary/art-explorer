# Make filtration 

pacman::p_load(
  here,
  simplextree,
  rdist,
  ggthemes,
  Matrix,
  tidyverse,
  harrypotter, 
  TDA
)


create_dist_matrix <- function(data){
  # data must be matrix
  dist_matrix <- rdist(data, metric = "euclidean") %>% as.matrix()
  dist_matrix[upper.tri(dist_matrix)] <- NA
  diag(dist_matrix) <- NA
  
  return(dist_matrix)
  
}


make_filtration <- function(infile, vars, max_scale = NULL, max_dim = 3, quantile = "75%", dist_func = "euclidean", suffix = ""){
  
  df <- read_csv(infile) %>% 
    dplyr::select(-hex_code, all_of(vars)) %>% 
    scale() %>% 
    as.data.frame() %>% 
    glimpse()
  
  mat <- as.matrix(df)
  
  # # Individual folders for each piece
  out_folder <- str_replace(infile, "\\.csv", "") %>%
    str_replace("/data/", "/filtrations/")
  
  dir.create(out_folder, showWarnings = FALSE)

  dists <- create_dist_matrix(mat)
  vertices <- as.list(1:nrow(mat))
  
  if(is.null(max_scale)){
    quantiles <- quantile(sort(dists))
    max_scale <- quantiles[quantile]
  }
  
  filtration <- ripsDiag(
    mat, maxscale = max_scale, maxdimension = max_dim, dist = dist_func,
    location = TRUE, library = "Dionysus", printProgress = TRUE
  )
  
  saveRDS(filtration, here(out_folder, "filt.Rds"))
  
  filtration_df <-  tibble(
    dim = filtration$diagram[,1],
    birth = filtration$diagram[,2],
    death = filtration$diagram[,3],
    max_scale = max_scale,
    max_dim = max_dim
  )
  
  write_csv(filtration_df, here(out_folder, str_glue("filt_df{suffix}.csv")))
  
  return(filtration_df)
  
}

data_files <- list.files(here("data"), recursive = TRUE, full.names = TRUE, pattern = "\\.csv")
vars <- c("entropy", "loc_x", "loc_y", "avg_H", "avg_S", "avg_V", "contrast", "var_H", "var_S", "var_V")

walk(data_files, make_filtration, vars = vars, suffix = "_full")



