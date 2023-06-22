# 0. Initial setup ##########
## Loads libraries
library(ggplot2)
library(magick)
library(pixelart)

## "Hacking" the pipeline() and plot_color_matrix() functions
## of {pixelart} to avoid having lines that go beyond the plot
mod_plot_color_matrix <- function (raster, pixelBorder) 
{
  rows = seq_len(nrow(raster))
  cols = seq_len(ncol(raster))
  cbind(expand.grid(y = rev(cols), x = rows),
        expand.grid(color = t(raster), stringsAsFactors = FALSE)) |>
    ggplot() + 
    ### Simplifies the original plot function
    geom_tile(aes(x = x, y = y, fill = I(color)),
              color = "black", linewidth = pixelBorder) + 
    coord_equal(expand = FALSE) + 
    theme_void()
}

mod_pipeline <- function (url, path, resize1 = 100, ncolors = 7, resize2 = 20, color_bg = "white", 
                          saturation = 100, degrees = 0, left = 0, top = 0, right = 0, 
                          bottom = 0) 
{
  ### Original code slightly modified
  im0 = magick::image_read(url)
  im1 = im0 |> magick::image_background(color_bg) |> 
    pixelart::crop(left = left, right = right, bottom = bottom, top = top) |> 
    magick::image_rotate(degrees) |> 
    magick::image_modulate(saturation = saturation)
  im2 = pixelart::downsize(im1, resize1)
  kmeans = pixelart::kmeans_colors(im2, ncolors)
  im3 = pixelart::downsize(im1, resize2)
  
  ### Gets the original dimensions and calculates the new ones
  info = magick::image_info(im0)
  width = info$width - left - right
  height = info$height - top - bottom
  
  ### Calculates the width of the pixels borders
  pixelBorder = max(c(width, height))/6000
  
  ### Calls modified pixelart::plot_color_matrix()
  mod_plot_color_matrix(colors_kmeans(im3, kmeans), pixelBorder)
  
  ### Saves the image
  ggplot2::ggsave(
    path, bg = color_bg,
    width = width, height = height, units = "px"
  )
}

# 1. Pixelization of images ##########
## Pixelizes and saves the New York City landmark
mod_pipeline(
  url = "2023/week25/originals/new_york_city.jpg",
  path = "2023/week25/pixelated/new_york_city.png",
  resize1 = 300,
  resize2 = 80,
  ncolors = 9,
  color_bg = "black",
  saturation = 200,
  degrees = 0,
  left = 0,
  top = 500,
  right = 0,
  bottom = 1540.75
)

## Pixelizes and saves the Seattle landmark
mod_pipeline(
  url = "2023/week25/originals/seattle.jpg",
  path = "2023/week25/pixelated/seattle.png",
  resize1 = 300,
  resize2 = 80,
  ncolors = 9,
  color_bg = "black",
  saturation = 200,
  degrees = 0,
  left = 0,
  top = 61,
  right = 0,
  bottom = 1450
)

## Pixelizes and saves the Phoenix (Arizona) landmark
mod_pipeline(
  url = "2023/week25/originals/phoenix.jpg",
  path = "2023/week25/pixelated/phoenix.png",
  resize1 = 300,
  resize2 = 80,
  ncolors = 9,
  color_bg = "black",
  saturation = 200,
  degrees = 0,
  left = 400,
  top = 0,
  right = 400,
  bottom = 0
)

## Pixelizes and saves the Las Vegas landmark
mod_pipeline(
  url = "2023/week25/originals/las_vegas.jpg",
  path = "2023/week25/pixelated/las_vegas.png",
  resize1 = 300,
  resize2 = 80,
  ncolors = 9,
  color_bg = "black",
  saturation = 200,
  degrees = 0,
  left = 0,
  top = 46,
  right = 900,
  bottom = 0
)

## Pixelizes and saves the Los Angeles landmark
mod_pipeline(
  url = "2023/week25/originals/los_angeles.jpg",
  path = "2023/week25/pixelated/los_angeles.png",
  resize1 = 300,
  resize2 = 80,
  ncolors = 9,
  color_bg = "black",
  saturation = 200,
  degrees = 0,
  left = 0,
  top = 38,
  right = 0,
  bottom = 0
)
