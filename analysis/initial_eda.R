library(tidyverse)
library(glue)
library(here)
library(sf)
library(jsonlite)
library(showtext)

# ggplot ------------------------------------------------------------------
font_add_google("Barlow Condensed", "bc")
font_add_google("Dancing Script", "dc")
# showtext_auto()
theme_set(theme_minimal(base_family = "mono", base_size = 16))
plot_dir = here("output/plots"); if(!dir.exists(plot_dir)) dir.create(plot_dir)


# path --------------------------------------------------------------------
path = here("output/geodata/cleaned_data.geojson")


# data --------------------------------------------------------------------
data = read_sf(path)



# some_plots --------------------------------------------------------------
data_no_geo = data %>% 
  st_drop_geometry()

### how many trees of each species?
p = here(plot_dir, "count_species.png")
data_no_geo %>%
  count(gattung_art, sort = T) %>%
  head(n = 20) %>%
  mutate(gattung_art = fct_reorder(gattung_art, n),) %>%
  ggplot(aes(n, gattung_art)) +
  geom_col()  +
  geom_text(
    aes(
      x = n - 500,
      label = n
    ),
    color = "white",
    hjust= 1
  ) +
  expand_limits(x = 22000)  +
  labs(x = "#",
       y = "Gattung (Art)",
       title = "Die häufigsten Bäume in Wien")

ggsave(p, width=12, height=16)



### what is the most common three tree per bezirk?
p = here(plot_dir, "most_common_less_common_bezirk.png")
data_no_geo %>% 
  group_by(bezirk) %>% 
  count(gattung_art, sort=T) %>% 
  slice_head(n=1) 

data_no_geo %>%
  group_by(bezirk) %>%
  count(gattung_art, sort = T) %>%
  filter(row_number() == 1 |
           row_number() == n()) %>% arrange(bezirk) %>%
  mutate(r = row_number()) %>%
  mutate(
    gattung_art = glue("{gattung_art} ({n})"),
    r = as.factor(r),
    r = fct_reorder(r, n)
  ) %>% 
  ggplot(aes(x = n, y = r)) +
  geom_col() +
  scale_color_brewer(palette="Set2") +
  geom_text(
    aes(
      x = 0,
      label = gattung_art,
      color = r
    ),
    hjust = 0,
    show.legend = F
  ) + 
  labs(
    x = "#",
    y = ""
  ) +
  facet_wrap( ~ bezirk, ncol=1) +
  theme(
    axis.text.y = element_blank()
  )

ggsave(p, width=10, height=30)


### map of all trees

ggplot() +
  geom_sf(
    data = data %>% sample_frac(0.10),
    size = .1
  )
















 