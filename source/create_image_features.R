pacman::p_load(
  wavethresh, 
  imager,
  here,
  magick,
  tidyverse, 
  ggpubr
  )

source(here("lib.R"))

compute_entropy <- function(coefs){
  coefs <- coefs[-length(coefs)] # remove average value of entire image
  total_image_energy <- sum(unlist(coefs)^2)
  subband_energies <- map_dbl(coefs, ~ sum(.x)^2)
  entropy <- map_dbl(subband_energies, ~ .x / total_image_energy * log2(.x / total_image_energy)) %>%
    sum()
  entropy <- -1*entropy
  
  return(entropy)
}

compute_contrast <- function(coefs){
  
  # Remove subbands with just one coefficient (lowest scale subbands)
  coefs <- coefs[-which(map_int(coefs, length)==1)]
  
  map_dbl(coefs, ~ max(.x) - min(.x)) %>% mean()
}

compute_energy <- function(coefs){sum(unlist(coefs[-length(coefs)])^2)}

wavelet_decomposition <- function(image, family = "DaubLeAsymm"){
  l <-  width(image)
  if(sqrt(l) %% 1 != 0) l = l - 1
  
  gray_image <- grayscale(image)
  #reverse_gray_image <- gray_image[rev(1:l), rev(1:l), , ]
  reverse_gray_image <- gray_image[1:l, rev(1:l), , ]
  decomp <- imwd(reverse_gray_image, family = family)
  
  return(decomp)
}

plot_coef <- function(decomp){
  plot(decomp, col = grey(seq(from=0, to=1, length=100)), transform=TRUE)
}

# HSV: hue, saturation (intensity or purity of the color), value/brightness (lightness/darkness of the color)
compute_color_moments <- function(img, colorspace = "HSV"){
  # get hex code in RGB first for plotting
  avg <- map_dbl(1:3, ~ mean(img[, , , .x] )) # Loop over all color channels
  hex_code <- rgb(avg[1], avg[2], avg[3])
  
  if(colorspace == "HSV") img <- RGBtoHSV(img)
  avg <- map_dbl(1:3, ~ mean(img[, , , .x] )) # Loop over all color channels
  var <- map_dbl(1:3, ~ var(as.numeric(img[, , , .x] )))
  
  #print(swatch)
  color_tbl <- tibble(avg = avg, var = var, value = c("H", "S", "V")) %>% 
    pivot_wider(names_from = "value", values_from = c("avg", "var")) %>% 
    mutate(hex_code = hex_code)
  
  return(color_tbl)
}

construct_patch_centers <- function(img, patch_size = 16^2, stride = 100){
  
  # Compute the number of patches in the x and y directions
  num_patches_x <- floor((width(img) - patch_size) / stride) + 1
  num_patches_y <- floor((height(img) - patch_size) / stride) + 1
  
  # Create a matrix to store the patch centers
  patches <- matrix(NA, nrow = num_patches_x * num_patches_y, ncol = 2)
  
  # Loop over the patches
  patch_idx <- 1
  for (x in seq(from = 1, to = width(img) - patch_size + 1, by = stride)) {
    for (y in seq(from = 1, to = height(img) - patch_size + 1, by = stride)) {
      # Compute the center of the patch
      center_x <- x + patch_size / 2
      center_y <- y + patch_size / 2
      
      # Save the center coordinates in the patches matrix
      patches[patch_idx, ] <- c(center_x, center_y)
      patch_idx <- patch_idx + 1
    }
  }
  
  print(str_glue("Created {nrow(patches)} patches."))
  
  return(patches)
}

# Extract coefficients from the wavelet decomposition
extract_coefficients <- function(decomp){
  
  coef_names <- names(decomp)[str_detect(names(decomp), "w")]
  new_names <- coef_names %>% str_replace_all("L", " ") %>% 
    str_replace_all("w", "") %>% 
    str_split(" ") 
  
  return(decomp[coef_names])
  
}

create_patch_row <- function(patch){
  
  decomp <- wavelet_decomposition(patch)
  coefs <- extract_coefficients(decomp)
  
  patch_features_tbl <- tibble(
    # Color features
    compute_color_moments(patch),
    # Texture features
    energy = compute_energy(coefs),
    entropy = compute_entropy(coefs), 
    contrast = compute_contrast(coefs)
  )
  
  return(patch_features_tbl)
  
}

construct_image_features <- function(inpath, patch_size = 256, overwrite = FALSE, create_patch_png = FALSE){
  
  outpath <- str_replace(inpath, "/input/images/", "/output/data/")
  outfile <- str_replace(outpath, "\\.png", ".csv")
  painting <- str_split(outfile, "/")[[1]] 
  painting <- painting[length(painting)] %>% str_replace("\\.csv", "")
  
  # Check if file already exists
  if(file.exists(outfile) & !overwrite & !create_patch_png){
    print(str_glue("File already exists: {painting}."))
    return(NULL)
  }
  
  img <- image_read(inpath) %>% 
    magick2cimg()
  
  patch_centers <- construct_patch_centers(img, patch_size)
  
  im_list <- extract_patches(img, patch_centers[,1], patch_centers[,2], patch_size, patch_size) 
  
  if(create_patch_png){
    patches <- map(im_list, plot_image)
    ggarrange(plotlist = patches) %>% 
      ggsave(plot = ., filename = here("output", "plots", "examples", str_glue("{painting}_patches.png")))
  }
  
  if(!file.exists(outfile) | overwrite){
  
  patch_features_df <- map(im_list, create_patch_row) %>%
    bind_rows() %>%
    mutate(image = im_list, loc_x = patch_centers[,1], loc_y = patch_centers[,2])

  write_csv(dplyr::select(patch_features_df, -image), file = str_replace(outpath, "\\.png", ".csv"))
}
  
}

plot_wavelet_coefficients <- function(infile, patch_no = 55, out_folder = here("output", "plots", "examples")){
  
  painting <- str_split(infile, "/")[[1]] 
  painting <- str_replace(painting[length(painting)], "\\.png", "")
  
  img <- image_read(infile) %>% 
    magick2cimg()
  
  patch_centers <- construct_patch_centers(img, 256)
  
  im_list <- extract_patches(img, patch_centers[,1], patch_centers[,2], 256, 256)
  
  patch <- im_list[[patch_no]]
  
  decomp <- wavelet_decomposition(patch)
  
  png(here(out_folder, str_glue("{painting}_wavelet_coefs.png")), width = 700, height = 700)
  plot_coef(decomp)
  dev.off()
  
  png(here(out_folder, str_glue("{painting}_patch.png")), width = 400, height = 400)
  plot(patch)
  dev.off()
  
}

# Construct several data frames across all the images and visualize each dimension via color & location

image_files <- list.files(here("input", "images"), recursive = TRUE, full.names = TRUE)
walk(image_files, construct_image_features, overwrite = FALSE)

# Make example figures: patches for 2 cubism and renaissance pieces
walk(image_files[c(10, 13)], construct_image_features, overwrite = TRUE, create_patch_png = TRUE)
walk(image_files[17], construct_image_features, overwrite = FALSE, create_patch_png = TRUE)

# Make example wavelet coefficient plots
plot_wavelet_coefficients(image_files[1], 10)
plot_wavelet_coefficients(image_files[13], 55)
plot_wavelet_coefficients(image_files[10], 20)



