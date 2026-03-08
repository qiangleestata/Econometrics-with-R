#常用r软件包

#安装软件包
install.packages("tidyverse")
install.packages("patchwork")
install.packages("viridis")


# 计量软件课程例子

library(tidyverse)
library(patchwork)
library(viridis)

# t 分布数据
t_df <- tibble(x = seq(-5, 5, length.out = 2000)) %>%
  crossing(df = c(3, 5, 10, 30)) %>%
  mutate(y = dt(x, df), df = factor(df))

# F 分布数据（若干 (df1, df2) 组合）
f_params <- tibble(df1 = c(1, 5, 10), df2 = c(5, 10, 30))
f_df <- pmap_dfr(list(f_params$df1, f_params$df2), function(d1, d2) {
  tibble(
    x = seq(0, 8, length.out = 2000),
    label = paste0("F(", d1, ",", d2, ")"),
    y = df(x, d1, d2)
  )
})

# 公共主题（简约且优雅）
theme_clean <- theme_minimal(base_size = 13) +
  theme(
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    plot.margin = margin(8, 8, 8, 8)
  )

# 上：t 分布
p_t <- ggplot(t_df, aes(x = x, y = y, color = df)) +
  geom_line(linewidth = 1) +
  scale_color_viridis_d(option = "plasma", name = "t df") +
  labs(x = "x", y = "密度") +
  theme_clean +
  theme(legend.position = "right")

# 下：F 分布
p_f <- ggplot(f_df, aes(x = x, y = y, color = label)) +
  geom_line(linewidth = 1) +
  scale_color_viridis_d(option = "viridis", name = "F(df1,df2)") +
  coord_cartesian(xlim = c(0, 6)) +
  labs(x = "x", y = "密度") +
  theme_clean +
  theme(legend.position = "right")

# 组合并在下方居中显示标题（作为 caption）
combined <- (p_t / p_f) +
  plot_annotation(
    caption = "图：t 分布（上）与 F 分布（下）",
    theme = theme(
      plot.caption = element_text(
        hjust = 0.5,
        size = 14,
        face = "bold",
        margin = margin(t = 10)
      )
    )
  )

# 返回绘图对象（最后一行应返回对象而非直接打印）
combined
