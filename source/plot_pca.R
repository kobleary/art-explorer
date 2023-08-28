# Plot PCA
pacman::p_load(gtools)
source(here("lib.R"))

plot_painting_pca <- function(data, labels, movement, color_opt, out_folder, density = TRUE, h = 6, w = 7){
  
  plot <- data %>% 
    filter(movement_label == movement) %>% 
    mutate(painting = str_glue("{n}. {painting}"),
           painting = factor(painting, levels = mixedsort(unique(painting)))) %>% 
    ggplot(aes(PC1, PC2, color = painting, fill = painting)) +
    geom_point(alpha = 0.9, key_glyph = "rect") 
  
  
  if(density) plot <- plot + stat_density_2d(aes(fill = painting, alpha = ..level..), geom = "polygon", bins = 15) 

  plot <- plot +
    geom_label(data = labels %>% filter(movement_label == movement), aes(label = n), alpha = .9, color = "white", key_glyph = "rect") +
    scale_color_hp_d(option = color_opt) +
    scale_fill_hp_d(option = color_opt) +
    guides(alpha = "none", color = "none", fill = "none") +
    theme(legend.position = "right") +
    labs(x = "", y = "", color = "", fill = "") +
    xlim(-6, 4) + ylim(-4, 5)
  
  suffix <-  ifelse(density, "density", "scatter")
  
  ggsave(filename = here(out_folder, str_glue("{movement}_pca_clusters_{suffix}.png")), height = h, width = w)
  return(plot)
  
}

data_files <- list.files(here("output", "data"), recursive = TRUE, full.names = TRUE, pattern = "\\.csv")
vars <- c("entropy", "loc_x", "loc_y", "avg_H", "avg_S", "avg_V", "contrast", "var_H", "var_S", "var_V")

df <- map_dfr(data_files,  read_in_data, folder_depth = 0)

out_folder <-  here("output", "plots", "pca")

pc_df <- df %>% dplyr::select(-painting, -movement_label, -hex_code) %>% 
  run_pca() %>% 
  bind_cols(df) %>% 
  group_by(movement_label, painting) %>% 
  mutate(n = cur_group_id())

pc_df_painting_labels <- pc_df %>% 
  group_by(movement_label, painting, n) %>% 
  summarize(PC1 = mean(PC1),
            PC2 = mean(PC2)) %>% 
  mutate(painting = str_glue("{n}. {painting}"),
         painting = factor(painting, levels = mixedsort(painting))
         
  )

plot_painting_pca(pc_df, pc_df_painting_labels, "cubism", "lunalovegood", out_folder, density = FALSE)
plot_painting_pca(pc_df, pc_df_painting_labels, "cubism", "lunalovegood", out_folder, density = TRUE)

plot_painting_pca(pc_df, pc_df_painting_labels, "renaissance", "ronweasley2", out_folder)
plot_painting_pca(pc_df, pc_df_painting_labels, "renaissance", "ronweasley2", out_folder, density = FALSE)





