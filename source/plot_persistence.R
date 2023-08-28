# Plot persistence
pacman::p_load(
  tidyverse,
  here,
  harrypotter, 
  ggpubr,
  gridExtra
)
source(here("lib.R"))

# Define plotting functions --------

format_plot <- function(plot){
  plot +
    geom_abline(slope = 1) +
    theme( legend.position = c(0.9, .2) ) +
    labs(
      x = "", y = "", color = "", fill = "", alpha = ""
    ) +
    scale_color_hp(discrete = TRUE, option = "lunalovegood") +
    scale_fill_hp(discrete = TRUE, option = "lunalovegood") + 
    guides(alpha = "none")
}


plot_persistence <- function(infile, color_pal, df = NULL, title = "", caption = "", suffix = "", h = 5, w = 6, out_folder = NULL){
  
  if(is.null(df)) df <- read_csv(infile) 
  
  # Individual folders for each piece
  if(is.null(out_folder)){
  out_folder <- str_replace(infile, "/filtrations/", "/plots/individual/") %>% 
    dirname()
  }
  
  lim <- max(df$birth, df$death)
  max_scale <- unique(df$max_scale)
  if(caption==""){
    caption <- str_glue("Max filtration distance is {round(max_scale, 2)}")
  }
  
  pers_plot <- df %>% 
    ggplot(aes(birth, death, color = as.factor(dim))) +
    geom_point(alpha = 0.7, size = 1.5) +
    geom_abline(slope = 1) +
    scale_x_continuous(limits = c(0, lim)) +
    scale_y_continuous(limits = c(0, lim)) +
    theme_minimal() +
    theme(
      legend.position = c(0.9, .2),
      plot.title = element_text(hjust = 0.5),
      panel.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
      plot.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
      panel.grid.minor = element_blank()
    ) +
    labs(
      x = "Birth", y = "Death", color = "Homology", 
      title = title, caption = caption
    ) +
    scale_color_manual(values = color_pal) 
    #scale_color_hp(discrete = TRUE, option = color_pal)
  
  ggsave(filename = here(out_folder, str_glue("pers_plot{suffix}.png")), height = h, width = w,
         plot = pers_plot)
  
  return(pers_plot)
  
}


plot_violin <- function(data){
  
  ggplot(data, aes(birth, death, color = movement_label, fill = movement_label)) +
    geom_violin(alpha = 0.5, position = "identity") +
    geom_point() +
    theme( legend.position = c(0.9, .9) ) +
    labs(
      x = "", y = "", color = "", fill = "", alpha = "",
      title = "0"
    ) +
    scale_color_hp(discrete = TRUE, option = "lunalovegood") +
    scale_fill_hp(discrete = TRUE, option = "lunalovegood") + 
    guides(alpha = "none")
}



plot_persistence_compare <- function(data,  type = "density", dimension = 1, movements = c("cubism", "renaissance")){
  
  plot <- data %>% 
    filter(dim == dimension, movement_label %in% movements) %>% 
    ggplot(aes(birth, death)) 
  
  if(dimension == 0){
    plot <- data %>% 
      filter(dim == dimension, movement_label %in% movements) %>% 
      plot_violin()
    
    return(plot)
  }
  
  if(type == "density"){
    plot <- plot + 
      stat_density_2d(aes(fill = movement_label, alpha = ..level..), geom = "polygon", bins = 4) 
  }
  
  if(type == "scatter"){
    plot <- plot + 
      geom_point(aes(color = movement_label), alpha = 0.5) 
  }
  plot <- plot +
    labs(title = str_glue("{dimension}"))
  
  return(plot)
}


plot_persistence_paint_labels <- function(data, dimen, quantile = 0.95, out_folder, h = 3, w = 3.5){
  labs <- data %>% group_by(movement_label) %>% distinct(painting) %>%  mutate(n = 1:n())
  
  data %>% filter(dim == dimen) %>% 
    left_join(labs) %>% 
    mutate(diff = abs(birth - death),
           label = ifelse(diff > quantile(diff, quantile), n, NA)) %>% 
    ggplot(aes(birth, death, color = movement_label, label = label)) +
    geom_point(alpha = 0.7) +
    geom_abline(slope = 1) +
    theme_minimal() +
    geom_label_repel(alpha = 0.7) +
    theme(
      legend.position = c(0.9, .2),
      plot.title = element_text(hjust = 0.5),
      panel.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
      plot.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
      panel.grid.minor = element_blank()
    ) +
    labs(
      x = "Birth", y = "Death", color = ""
    ) +
    scale_color_hp(discrete = TRUE, option = "lunalovegood") +
    guides(color = "none")
  
  ggsave(filename = here(out_folder, str_glue("compare_filt_homol_{dimen}.png")), height = h, width = w)
}

## Individual persistence diagrams ---------------------------
## What are the persistent homologies for each artwork?
filt_files <- list.files(here("output", "filtrations"), recursive = TRUE, full.names = TRUE, pattern = "_full\\.csv")

pal <- hp(4, option = "ronweasley2")[c(1,4,2,3)]

for(file in filt_files){
  plot_persistence(file, color_pal = pal, h = 3, w = 3.5)
}

filt_across_movements <- here("output", "filtrations", "comparison", "filt_all_paintings.csv")

filt_across_movements %>% 
  plot_persistence(out_folder = here("output", "plots", "persistence"), color_pal = pal, h = 3, w = 3.5) 

## Compare persistence diagrams by movement --------------------------------------------
## Is there a difference in persistent homologies in Cubism vs. Renaissance art?

filt_df <- map_dfr(filt_files,  read_in_data, folder_depth = 1)

out_folder <- here("output", "plots", "persistence")

plot_persistence_paint_labels(filt_df, 1, .99, out_folder)
plot_persistence_paint_labels(filt_df, 2, .95, out_folder)
plot_persistence_paint_labels(filt_df, 3, 0, out_folder)


scatter_plots <- map(1:3, ~ plot_persistence_compare(filt_df, "scatter", dimension = .)) 
scatter_plots <- map(scatter_plots, format_plot)

density_plots <- map(1:3, ~ plot_persistence_compare(filt_df, "density", dimension = .)) 
density_plots <- map(density_plots, format_plot)

scatter_plots[[4]] <- plot_persistence_compare(filt_df, "scatter", dimension = 0)
density_plots[[4]] <- plot_persistence_compare(filt_df, "scatter", dimension = 0)

ggarrange(plotlist = scatter_plots, common.legend = TRUE) %>% 
  annotate_figure(bottom = "Birth", left = "Death") %>% 
  ggsave(plot = ., filename = here("plots", "persistence", "compare_filt_scatter.png"), 
         height = 6, width = 7)

ggarrange(plotlist = density_plots, common.legend = TRUE) %>% 
  annotate_figure(bottom = "Birth", left = "Death") %>% 
  ggsave(plot = ., filename = here("plots", "persistence", "compare_filt_density.png"), 
         height = 6, width = 7)

