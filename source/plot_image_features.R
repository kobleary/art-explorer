# Plot image features
pacman::p_load(
  tidyverse,
  here,
  harrypotter, 
  ggpubr,
  purrr,
  gridExtra
)
source(here("lib.R"))


# Define visualization functions ----
plot_average_colorspace <- function(df){
  
  df %>% 
    mutate(loc_y = -loc_y) %>% 
    ggplot(aes(loc_x, loc_y, fill = hex_code)) + 
    geom_tile() + 
    scale_fill_identity() + 
    coord_equal() +
    theme_void() 
}


plot_feature <- function(df, feat){
  
  df %>% 
    mutate(loc_y = -loc_y) %>% # flip image so origin is in bottom left corner rather than top left
    ggplot(aes_string("loc_x", "loc_y", fill = feat)) +
    geom_tile() + 
    scale_fill_viridis_c(option = "B") +
    coord_equal() +
    theme_void() + theme(legend.position = "right")
  
}


make_individual_plots <- function(infile, features, panel_features){
  
  # Read in data 
  df <- read_csv(infile)
  
  # Individual folders for each piece
  out_folder <- str_replace(file, "\\.csv", "") %>% 
    str_replace("/data/", "/plots/individual/") 
  dir.create(out_folder, showWarnings = FALSE)
  
  plot_average_colorspace(df)
  
  ggsave(here(out_folder, "avg_colorspace.png"), width = 1687, height = 1666, units = "px")
  
  feature_plots <- map(features, ~ plot_feature(df, .x))
  names(feature_plots) <- features
  
  feature_panel_plot <- grid.arrange(grobs = feature_plots[panel_features], nrow = ceiling(length(panel_features) / 2), ncol = 2)
  
  walk2(
    feature_plots, features, 
    function(plt, feat) ggsave(filename = here(out_folder, str_glue("{feat}.png")), 
                               plot = plt, width = 1687, height = 1666, units = "px")
                               
  )
  ggsave(filename = here(out_folder, "feature_panel_plot.png"), plot = feature_panel_plot, width = 1687, height = 1666, units = "px")
  
}



plot_movement_scatter <- function(data, xvar, yvar, out_folder, h = 5, w = 7){
  
  movements <- data %>% 
    count(movement_label) %>% 
    pull(movement_label) %>% sort() %>% 
    paste(collapse = "_")
  
  plot <- data %>% 
    ggplot(aes_string(xvar, yvar, color = "movement_label")) +
    geom_point(alpha = 0.5) +
    theme_minimal() +
    scale_color_hp_d(option = "lunalovegood") +
    labs(color = "") +
    #theme(legend.position = "none") +
    facet_wrap(vars(movement_label)) + 
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5),
      panel.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
      plot.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
      panel.grid.minor = element_blank()
    )
  
  ggsave(here(out_folder, "scatter", str_glue("{movements}_{xvar}_{yvar}.png")), height = h, width = w)
  
  return(plot)
  
}


plot_movement_hist <- function(data, var, out_folder, h = 3, w = 4){
  
  movements <- data %>% 
    count(movement_label) %>% 
    pull(movement_label) %>% sort() %>% 
    paste(collapse = "_")
  
  plot <- data %>% 
    ggplot(aes_string(var, fill = "movement_label", color = "movement_label")) +
    geom_histogram(alpha = 0.4, aes(y = ..density..), position = "identity") +
    geom_density(alpha = 0.4) +
    scale_fill_hp_d(option = "lunalovegood") +
    scale_color_hp_d(option = "lunalovegood") +
    labs(color = "", fill = "") +
    theme(legend.position = "none") + 
    labs(y = "")
  
  ggsave(here(out_folder, "hist", str_glue("{movements}_{var}.png")), height = h, width = w)
  
  return(plot)
  
}


# Make individual artwork plots ------------------------------------------

data_files <- list.files(here("output", "data"), recursive = TRUE, full.names = TRUE, pattern = "\\.csv")
out_folder <- here("output", "plots")
features <- c("avg_H", "avg_S", "avg_V", "var_H", "var_S",
              "var_V", "energy", "entropy", "contrast")

for(file in data_files){
  make_individual_plots(file, features, panel_features = features[5:8])
}

# Compare features by movement ---------------------------------------------------
df <- map_dfr(data_files, read_in_data)

## Scatterplots: ----
# How different are the 2d distributions (scatterplot) of each feature by art movement? 
feature_pairs <- combn(features, 2)

for(c in 1:ncol(feature_pairs)){
  plot_movement_scatter(df, feature_pairs[,c][1], feature_pairs[,c][2], out_folder)
}

## Histograms:  ----
# How different are the distributions (densities) of each feature by art movement?
walk(features, ~ plot_movement_hist(df, .x, out_folder))


