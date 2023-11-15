library(ggplot2)
library(ggdendro)
library(hexSticker)

gg <- data.frame(x = 1:10, y = 1:10) |>
  dist() |>
  hclust() |>
  ggdendrogram() + theme_void()

sticker(gg,
        package = "piar",
        filename = "man/figures/logo.png",
        s_x = 1,
        s_width = 1,
        p_size = 18,
        p_family = "mono",
        p_color = "#C33251",
        h_fill = "#BFB891",
        h_color = "#C33251")
