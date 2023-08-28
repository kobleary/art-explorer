# create simplicial complex network
pacman::p_load(
  tidyverse,
  gifski, 
  gtools,
  here,
  rdist,
  simplextree,
  ggraph,
  tidygraph,
  magick,
  imager,
  ggpubr,
  harrypotter,
  ggrepel
)
source(here("lib.R"))

set.seed(19106)

# Define functions -------------------

plot_skeleton <- function(nodes, coords, tree){
  edges <- tree$as_edge_list() %>% as_tibble() %>% set_names(c("from", "to")) 
  g <- tbl_graph(nodes = nodes, edges = edges)
  
  skeleton_plot <- ggraph(g, coords) +
    geom_edge_link(alpha = 0.6, color = "maroon") + 
    geom_node_point(size = 1, color = "maroon") +
    theme_void() +
    labs(x = "", y = "", color = "", shape = "", fill = "") 
}

create_skeleton_animation <- function(data, frames, gif_name = "complex.gif", out_folder = here("output", "plots", "examples")){
  
  nodes <- data[["nodes"]]
  coords <- data[["coords"]]
  trees <- data[["trees"]][frames]
  
  h <- coords %>% count(x) %>% pull(n) %>% unique() 
  w <- coords %>% count(y) %>% pull(n) %>% unique() 
  
  i = 1
  for(tree in trees){
    skelly <- plot_skeleton(nodes, coords, tree)
    ggsave(here(out_folder, str_glue("skelly_{i}.png")), plot = skelly, height = h, w = w)
    i = i + 1
  }
  
  # Create animation from skeleton plots
  png_files <- list.files(here(out_folder), pattern = 'skelly', full.names = TRUE, recursive = TRUE) %>% mixedsort()
  gifski(png_files, gif_file = here(out_folder, gif_name), delay = 0.01, width = w*100, height = h*100)
  file.remove(png_files)
  
}

get_simplices <- function(infile, vars, thresholds = NULL){
  
  df <- read_in_data(infile, folder_depth = 0) %>% 
    mutate(n = 1:n())
  
  filt_df <- df %>% 
    ungroup() %>%
    dplyr::select(all_of(vars)) %>%
    scale() %>%
    as.data.frame() %>%
    glimpse()
  
  mat <- as.matrix(filt_df)
  
  # Create complex for each threshold ------
  dist_mat <- create_dist_matrix(mat)
  
  if(is.null(thresholds)){ thresholds <- sort(dist_mat) 
  } else if(thresholds == "auto") {thresholds <- sort(sample(dist_mat[!is.na(dist_mat) & dist_mat <= 4], 100)) }
  
  print("Making filtration...")
  trees <- map(thresholds, ~ compute_edges(dist_mat, .x) %>% 
                 construct_simplex(., as.list(1:nrow(mat))))
  
  # Nodes and edges for plotting skeletons -----
  coords <- df %>% dplyr::select(x = loc_x, y = loc_y) %>% mutate(y = -y)
  nodes <- df %>% dplyr::select(painting, movement_label, n)
  return(lst(trees, coords, nodes))
}

# Main -----

data_files <- list.files(here("output", "data"), recursive = TRUE, full.names = TRUE, pattern = "\\.csv")
vars <- c("entropy", "loc_x", "loc_y", "avg_H", "avg_S", "avg_V", "contrast", "var_H", "var_S", "var_V")

frames <- sample(1:500, 100) %>% sort()

# Martin Luther portrait
simplex_data_ren <- get_simplices(data_files[17], vars)
create_skeleton_animation(simplex_data_ren, 1:100, "martin_luther_complex.gif")

# Jane Seymour portrait
simplex_data_ren <- get_simplices(data_files[16], vars)
create_skeleton_animation(simplex_data_ren, 1:100, "jane-seymour_hans-holbein.gif")

# Cubism portrait (portrait aristarkh lentulov)
simplex_data_cub <- get_simplices(data_files[8], vars, "auto")
create_skeleton_animation(simplex_data_cub, 1:50, "portrait_aristarkh-lentulov.gif")

# Cubism paris
simplex_data_cub <- get_simplices(data_files[2], vars, "auto")
create_skeleton_animation(simplex_data_cub, 1:50, "air-iron-and-water_delaunay.gif")

