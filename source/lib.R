# Lib
pacman::p_load(
  tidyverse,
  here,
  rdist,
  simplextree,
  ggrepel,
  harrypotter
)

theme_set(  
  theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      panel.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
      plot.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
      panel.grid.minor = element_blank()
    )
)

# Data reading/viewing functions ------

read_in_data <- function(file, folder_depth = 0){
  folders <- str_split(file, "/")[[1]]
  painting <- str_replace(folders[length(folders) - folder_depth], "\\.csv", "")
  movement_label <- folders[length(folders) - 1 - folder_depth]
  
  read_csv(file) %>% 
    mutate(painting = painting,
           movement_label = movement_label)
}

get_middle_patches <- function(data){
  data %>% 
    group_by(painting) %>% 
    mutate(
      n = n(),
      middle_patch = floor(n/2),
      id = 1:n()
    ) %>% 
    filter(id == middle_patch) %>% 
    ungroup() %>% 
    mutate(n = 1:n()) %>% 
    glimpse() 
}

plot_image <- function(image){
  df <- as.data.frame(image, wide="c") %>% 
    mutate(rgb.val=rgb(c.1,c.2,c.3)) 
  
  ggplot(df, aes(x,y)) +
    geom_raster(aes(fill=rgb.val)) +
    scale_y_continuous(trans=scales::reverse_trans()) +
    scale_fill_identity() +
    theme_void()
}


# Simplicial complex functions --------
create_dist_matrix <- function(data){
  # data must be matrix
  dist_matrix <- rdist(data, metric = "euclidean") %>% as.matrix()
  dist_matrix[upper.tri(dist_matrix)] <- NA
  diag(dist_matrix) <- NA
  
  return(dist_matrix)
  
}

compute_edges <- function(dist_matrix, threshold){
  
  edges <- which(dist_matrix <= threshold, arr.ind = TRUE) %>%
    as_tibble() %>% 
    filter(row != col) %>% 
    as.matrix()
  
  return(edges)
  
}

construct_simplex <- function(edges, vertices){
  # Initialize simplex
  simplex <- simplex_tree()
  
  simplex <- simplex %>% 
    simplextree::insert(vertices) %>% 
    simplextree::insert(as.list(data.frame(t(edges)))) %>% # Add edges
    simplextree::expand(k = 2) %>% # Expand out from the edges the 2 and 3 simplexes
    simplextree::expand(k = 3)
  
  return(simplex)
}


# PCA functions -----

run_pca <- function(data){
  pc <- data %>% 
    prcomp(scale = TRUE)
  
  unclass(pc)$x %>% as_tibble() 
}
