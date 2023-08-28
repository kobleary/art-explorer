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

# Define functions -------------------

create_skeleton_simplex_plots <- function(nodes, coords, simplex_trees, out_folder, h = 3, w = 3.5){
  
  i = 1
  for(tree in simplex_trees){
    
    edges <- tree$as_edge_list() %>% as_tibble() %>% set_names(c("from", "to")) 
    
    g <- tbl_graph(nodes = nodes, edges = edges)
    
    plot <- ggraph(g, coords) +
      geom_edge_link(alpha = 0.6) + 
      geom_node_point(aes(color = movement_label, shape = movement_label),
                      size = 3, alpha = 0.01) +
      geom_node_label(aes(label = n, fill = movement_label), color = "white", alpha = 0.9, repel = FALSE) +
      scale_fill_hp_d(option = "lunalovegood") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        plot.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        panel.grid.minor = element_blank(),
        legend.position = c(0.9, 0.9)
      ) +
      labs(x = "", y = "", color = "", shape = "", fill = "") 
    
    ggsave(here(out_folder, str_glue("skeleton_{i}.png")), plot = plot, height = h, width = w)
    i = i + 1
  }
  
  return(plot)
}

plot_patch <- function(loc_x, loc_y, movement_label, painting, n, size = 256){
  
  im <- image_read(str_glue(here("input", "images", movement_label, paste0(painting, ".png")))) %>% 
    magick2cimg()
  
  plot <- extract_patches(im, loc_x, loc_y, size, size)[[1]] %>% 
    plot_image() +
    labs(title = n)
  
  return(plot)
}


out_folder <- here("output", "filtrations", "comparison")

data_files <- list.files(here("output", "data"), recursive = TRUE, full.names = TRUE, pattern = "\\.csv")
vars <- c("entropy", "loc_x", "loc_y", "avg_H", "avg_S", "avg_V", "contrast", "var_H", "var_S", "var_V")

df <- map_dfr(data_files,  read_in_data, folder_depth = 0)
middle_patches_df <- get_middle_patches(df)

filt_df <- middle_patches_df %>% ungroup() %>%
  dplyr::select(all_of(features), -loc_x, -loc_y) %>%
  scale() %>%
  as.data.frame() %>%
  glimpse()

mat <- as.matrix(filt_df)

# Create complex for each threshold ------
trees <- map(sort(create_dist_matrix(mat) ), ~ compute_edges(create_dist_matrix(mat), .x) %>% 
               construct_simplex(., as.list(1:nrow(mat))))

# Get 2 principle components for axes -----
pca_df <- run_pca(filt_df) %>% 
  bind_cols(middle_patches_df) %>% 
  glimpse()

# Nodes and edges for plotting skeletons -----
coords <- pca_df %>% dplyr::select(x = PC1, y = PC2)
nodes <- pca_df %>% dplyr::select(painting, movement_label, n)

# Make skeleton plots -----
# As many as there are simplicial complexes (over 100)
create_skeleton_simplex_plots(nodes, coords, trees[c(15, 34, 46)], 
                              out_folder = out_folder, h = 4, w = 5)
  
# Stitch together .pngs for animation ------
png_files <- list.files(here(out_folder), '*.png', full.names = TRUE) %>% mixedsort()
gifski(png_files, gif_file = here(out_folder, "simplicial_complex.gif"), delay = 0.05)
file.remove(png_files)

# Extract middle patch and plot ------
center_patches_plots <- middle_patches_df %>% 
  dplyr::select(loc_x, loc_y, movement_label, painting, n) %>%
  pmap(plot_patch)

ggarrange(plotlist = center_patches_plots, nrow = 5, ncol = 4) %>% 
  ggsave(filename = here(out_folder, "center_patches.png"), height = 11.4, width = 7)



