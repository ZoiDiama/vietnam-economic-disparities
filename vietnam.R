# Economic disparities and tourism across Vietnam's provinces
# Rebuilt fully in R (the original was finished in Adobe Illustrator).
# Data: General Statistics Office of Vietnam, World Bank (see Data.xlsx)

# 1. LIBRARIES -------------------------------------------------------------
library(tidyverse)
library(readxl)
library(sf)
library(ggrepel)
library(ggtext)
library(patchwork)
library(treemapify)
library(showtext)

font_add_google("Roboto Condensed", family = "rc")
font_add_google("Abril Fatface", family = "abril")
showtext_auto()
showtext_opts(dpi = 288)

ink <- "#1A1A1A"; muted <- "#4D4D4D"; faint <- "#7A7A7A"
fix_names <- \(x) str_squish(str_replace_all(x, " ", " "))   # the Excel names contain non-breaking spaces

# 2. DATA ------------------------------------------------------------------
provinces <- read_excel("data/Data.xlsx", sheet = "Data_map") |> mutate(Name = fix_names(Name))
shape     <- st_read("data/diaphantinhenglish.geojson", quiet = TRUE) |>
  mutate(Name = fix_names(Name)) |>
  left_join(provinces, by = "Name") |>
  st_crop(xmin = 100, xmax = 110, ymin = 8, ymax = 24)   # mainland only; drops the offshore archipelagos

tourism <- read_excel("data/Data.xlsx", sheet = "Data") |>
  transmute(Name = fix_names(`Cities, provincies`), turnover = TT, share = `Share TT`, MPR) |>
  filter(!is.na(share))

poverty <- read_excel("data/Data.xlsx", sheet = "Poverty rate", skip = 2) |>
  rename(Name = 1) |>
  mutate(Name = fix_names(Name)) |>
  select(Name, `2016`:`2022`) |>
  pivot_longer(-Name, names_to = "year", values_to = "rate") |>
  mutate(year = as.numeric(year))

national <- read_excel("data/Data.xlsx", sheet = "Whole country(wide)") |>
  rename(var = `Cities, provincies`) |>
  filter(!is.na(var)) |>
  pivot_longer(-var, names_to = "year", values_to = "value") |>
  mutate(year = as.numeric(year)) |>
  filter(year >= 2015)

# 3. BIVARIATE CLASSES (3 x 3, terciles) ------------------------------------
tercile <- \(x) as.integer(cut(x, quantile(x, c(0, 1/3, 2/3, 1), na.rm = TRUE), include.lowest = TRUE))
shape <- shape |>
  mutate(pov = tercile(MPR), gdp = tercile(GDP_per_capita),
         bi  = if_else(is.na(pov) | is.na(gdp), NA_character_, paste0(pov, "-", gdp)))

# poverty (x: 1 low → 3 high) × GDP per capita (y: 1 low → 3 high)
bi_pal <- c("1-1" = "#E8E8E8", "2-1" = "#E4ACAC", "3-1" = "#C85A5A",
            "1-2" = "#B0D5DF", "2-2" = "#AD9EA5", "3-2" = "#985356",
            "1-3" = "#64ACBE", "2-3" = "#627F8C", "3-3" = "#574249")
red <- "#C85A5A"; blue <- "#3E8FA6"; light_blue <- "#B0D5DF"

# provinces to label: 5 poorest (left column) and 5 richest (right, in the sea)
pts <- shape |> mutate(pt = st_point_on_surface(geometry),
                       x = st_coordinates(pt)[, 1], y = st_coordinates(pt)[, 2]) |>
  st_drop_geometry()
poor <- pts |> slice_max(MPR, n = 5) |> arrange(desc(y)) |>
  mutate(label = paste0(Name, " ", round(MPR), "%"),
         lx = 101.6, ly = seq(23.15, by = -0.6, length.out = 5))
rich <- pts |> slice_max(GDP_per_capita, n = 5) |> arrange(desc(y)) |>
  mutate(label = paste0(Name, " ", round(GDP_per_capita)),
         lx = c(108.3, 108.3, 108.3, 108.4, 108.4), ly = c(21.95, 21.35, 20.75, 10.4, 9.8))

# 4. MAP -------------------------------------------------------------------
p_map <- ggplot(shape) +
  geom_sf(aes(fill = bi), colour = "white", linewidth = 0.15) +
  geom_segment(data = poor, aes(x = lx + 0.1, y = ly, xend = x, yend = y), colour = faint, linewidth = 0.3) +
  geom_segment(data = rich, aes(x = lx - 0.1, y = ly, xend = x, yend = y), colour = faint, linewidth = 0.3) +
  geom_point(data = bind_rows(poor, rich), aes(x, y), size = 0.9, colour = ink) +
  annotate("text", x = 101.6, y = 23.95, hjust = 1, family = "rc", fontface = "bold", size = 3,
           colour = red, lineheight = 0.95, label = "Highest poverty rate\n(% of people, 2022)") +
  annotate("text", x = 108.3, y = 22.75, hjust = 0, family = "rc", fontface = "bold", size = 3,
           colour = blue, lineheight = 0.95, label = "Highest GDP per person\n(million dong, 2022)") +
  geom_text(data = poor, aes(lx, ly, label = label), hjust = 1, family = "rc", fontface = "bold",
            size = 3, colour = red) +
  geom_text(data = rich, aes(lx, ly, label = label), hjust = 0, family = "rc", fontface = "bold",
            size = 3, colour = blue) +
  scale_fill_manual(values = bi_pal, na.value = "#CFCFCF", guide = "none") +
  coord_sf(xlim = c(98.2, 112.3), ylim = c(8.3, 24.3), expand = FALSE, clip = "off") +
  theme_void()

# legend as a small 3x3 grid
leg <- expand_grid(pov = 1:3, gdp = 1:3) |> mutate(bi = paste0(pov, "-", gdp))
gdp_rng <- range(shape$GDP_per_capita, na.rm = TRUE)
pov_rng <- range(shape$MPR, na.rm = TRUE)
p_leg <- ggplot(leg, aes(pov, gdp, fill = bi)) +
  geom_tile(colour = "white", linewidth = 0.6) +
  # arrows along both axes
  annotate("segment", x = 0.5, xend = 3.6, y = 0.25, yend = 0.25, colour = muted, linewidth = 0.35,
           arrow = arrow(length = unit(1.3, "mm"), type = "closed")) +
  annotate("segment", x = 0.25, xend = 0.25, y = 0.5, yend = 3.6, colour = muted, linewidth = 0.35,
           arrow = arrow(length = unit(1.3, "mm"), type = "closed")) +
  scale_fill_manual(values = bi_pal, guide = "none") +
  labs(x = paste0("Higher poverty rate\nRange: ", round(pov_rng[1]), "%–", round(pov_rng[2]), "%"),
       y = paste0("Higher GDP per capita\nRange: ", round(gdp_rng[1]), "–", round(gdp_rng[2]), "\nmillion dong")) +
  coord_equal(clip = "off") +
  theme_void(base_family = "rc") +
  theme(axis.title.x = element_text(size = 7.5, colour = muted, lineheight = 1, margin = margin(t = 5)),
        axis.title.y = element_text(size = 7.5, colour = muted, angle = 90, lineheight = 1, margin = margin(r = 5)))

p_map <- p_map + inset_element(p_leg, left = 0, bottom = 0.02, right = 0.3, top = 0.26, align_to = "panel")

# 5. TOURISM: treemap, one rectangle per province ---------------------------
# size = travel turnover (2022), colour = multidimensional poverty rate (2022)
tm <- tourism |>
  filter(turnover > 0) |>
  mutate(short = recode(Name, "Ho Chi Minh city" = "Ho Chi Minh City", "Ha Noi" = "Hanoi"),
         label = if_else(share >= 0.2,
                         paste0(short, "\n", scales::percent(share, accuracy = 1)), ""))
tm_lay <- treemapify(tm, area = "turnover", xlim = c(0, 1), ylim = c(0, 0.6))  # wide, short rectangle
tm_lay <- tm_lay |> mutate(cx = (xmin + xmax) / 2, cy = (ymin + ymax) / 2)
# the corner with all the small provinces, for a callout
rest <- tm_lay |> filter(share < 0.2)
rest_share <- sum(rest$share)

poor5_share <- sum(tourism$share[tourism$Name %in% poor$Name])
top2        <- sum(sort(tourism$share, decreasing = TRUE)[1:2])
n_small     <- sum(tm$share < 0.01)

p_tour <- ggplot(tm_lay) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = MPR),
            colour = "white", linewidth = 0.5) +
  # provinces without poverty data: thin grey outline so they stay visible
  geom_rect(data = filter(tm_lay, is.na(MPR)), aes(xmin = xmin + 0.002, xmax = xmax - 0.002,
                                                   ymin = ymin + 0.002, ymax = ymax - 0.002),
            fill = NA, colour = "#A8A8A8", linewidth = 0.3) +
  geom_text(aes(cx, cy, label = label), family = "rc", fontface = "bold", size = 3.2,
            colour = ink, lineheight = 0.95) +
  # bracket over the corner with all the other provinces
  annotate("segment", x = min(rest$xmin), xend = max(rest$xmax), y = 0.625, yend = 0.625,
           colour = muted, linewidth = 0.35) +
  annotate("text", x = 1, y = 0.645, hjust = 1, vjust = 0, family = "rc", size = 2.9,
           colour = muted, lineheight = 1, label = paste0("The other ", nrow(rest), " provinces: ", round(rest_share * 100),
                                          "% together.\nThe poorest are the dark red slivers.")) +
  # same poverty colours as the map's x-axis: light grey (low) to red (high)
  scale_fill_gradientn(colours = c("#E8E8E8", "#E4ACAC", red, "#8E2F2F"), values = c(0, 0.15, 0.6, 1),
                      na.value = "#F6F6F6", limits = c(0, 35),
                      breaks = c(0, 10, 20, 30), labels = \(x) paste0(x, "%"),
                      name = "Poverty rate, 2022 (outlined = no data)",
                      guide = guide_colourbar(title.position = "top", barwidth = unit(40, "mm"),
                                              barheight = unit(2.5, "mm"))) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  coord_fixed(ratio = 1, clip = "off") +
  labs(title = "Tourism money flows to the richest places",
       subtitle = str_wrap(paste0("Travel turnover in 2022: each rectangle is a province, sized by what tourists spent ",
                                  "with its travel businesses and coloured by its poverty rate. Ho Chi Minh City and Hanoi earn ",
                                  round(top2 * 100), "%; ", n_small, " provinces each earn under 1%, and the five poorest ",
                                  "together only ", scales::percent(poor5_share, accuracy = 0.01), "."), 82)) +
  theme_void(base_family = "rc") +
  theme(plot.title.position = "plot",
        plot.title = element_text(face = "bold", size = 12, colour = ink, margin = margin(b = 3)),
        plot.subtitle = element_text(size = 9, colour = muted, lineheight = 1.2, margin = margin(b = 6)),
        legend.position = "bottom", legend.justification = "left",
        legend.title = element_text(size = 8, colour = muted),
        legend.text = element_text(size = 7.5, colour = muted),
        plot.margin = margin(0, 0, 14, 0))

# 6. POVERTY: dumbbell, 2016 vs 2021 -----------------------------------------
pov <- poverty |>
  filter(Name %in% c(poor$Name, "WHOLE COUNTRY"), year %in% c(2016, 2021)) |>
  mutate(Name = if_else(Name == "WHOLE COUNTRY", "Vietnam", Name)) |>
  pivot_wider(names_from = year, values_from = rate, names_prefix = "y") |>
  mutate(group = if_else(Name == "Vietnam", "Vietnam", "Mountain provinces"),
         Name  = fct_reorder(Name, y2021))
nat21  <- pov$y2021[pov$Name == "Vietnam"]
mtn21  <- mean(pov$y2021[pov$Name != "Vietnam"])
dien16 <- pov$y2016[pov$Name == "Dien Bien"]
dien21 <- pov$y2021[pov$Name == "Dien Bien"]
pale_red <- "#E8B4B4"

p_trend <- ggplot(pov, aes(y = Name)) +
  geom_segment(aes(x = y2021, xend = y2016, yend = Name), colour = "#E3E3E3", linewidth = 2.2) +
  geom_point(aes(x = y2016), colour = pale_red, size = 3.4) +
  geom_point(aes(x = y2021, colour = group), size = 3.4) +
  geom_text(aes(x = y2016, label = paste0(round(y2016, 1), "%")), hjust = -0.35,
            family = "rc", size = 2.8, colour = faint) +
  geom_text(aes(x = y2021, label = paste0(round(y2021, 1), "%"), colour = group), hjust = 1.4,
            family = "rc", size = 2.9, fontface = "bold") +
  annotate("text", x = c(dien16, dien21), y = 6.65, label = c("2016", "2021"), family = "rc",
           size = 2.7, colour = c(faint, red), fontface = "bold") +
  scale_colour_manual(values = c(Vietnam = ink, `Mountain provinces` = red), guide = "none") +
  scale_x_continuous(limits = c(-7, 60), breaks = seq(0, 50, 10), labels = \(x) paste0(x, "%")) +
  scale_y_discrete(expand = expansion(add = c(0.6, 1))) +
  coord_cartesian(clip = "off") +
  labs(title = paste0("Poverty fell fast in the mountains, but is still\nabout ", round(mtn21 / nat21),
                      " times the national rate"),
       subtitle = str_wrap(paste0("Multidimensional poverty rate, % of people, 2016 (light) and 2021 (dark). ",
                                  "Dien Bien fell from ", round(dien16), "% to ", round(dien21), "%."), 85)) +
  theme_minimal(base_family = "rc") +
  theme(plot.title.position = "plot",
        plot.title = element_text(face = "bold", size = 12, colour = ink, margin = margin(b = 3)),
        plot.subtitle = element_text(size = 9, colour = muted, margin = margin(b = 8)),
        axis.title = element_blank(),
        axis.text.y = element_text(size = 9.5, colour = ink, face = "bold"),
        axis.text.x = element_text(size = 7.5, colour = faint),
        panel.grid = element_blank(),
        panel.grid.major.x = element_line(colour = "#EFEFEF", linewidth = 0.3))

# 6b. SCALED-UP NUMBERS -------------------------------------------------------
mpr_dien <- shape$MPR[shape$Name == "Dien Bien"][1]
kpi <- tibble(x = c(0, 1, 2),
              big = c(paste0(round(mpr_dien), "%"),
                      scales::percent(poor5_share, accuracy = 0.01),
                      paste0(round(mtn21 / nat21), "×")),
              small = c("of people in Dien Bien live in<br>multidimensional poverty (2022)",
                        "of tourism turnover reaches the<br>five poorest provinces (2022)",
                        "the national poverty rate in the<br>five poorest provinces (2021)"))
# each number + its text as one text block, so they can never overlap
kpi <- kpi |>
  mutate(label = paste0("<span style='font-family:abril;font-size:30pt;color:", red, "'>", big, "</span>",
                        "<br>", small))
p_kpi <- ggplot(kpi) +
  geom_richtext(aes(x, 1, label = label), hjust = 0, vjust = 1, family = "rc", size = 3,
                colour = muted, fill = NA, label.colour = NA, lineheight = 1.1,
                label.padding = unit(0, "pt")) +
  scale_x_continuous(limits = c(0, 3), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(plot.margin = margin(0, 0, 6, 0))

# 7. LAYOUT ----------------------------------------------------------------
mpr <- national |> filter(var == "MPR", !is.na(value))
st <- paste0(
  "Since 2015 Vietnam's economy has grown by about 7% a year (except during COVID-19), and the share of people ",
  "in multidimensional poverty fell from ", mpr$value[1], "% in ", mpr$year[1], " to ", tail(mpr$value, 1),
  "% in 2022. But the gains are uneven. The map combines GDP per capita and poverty for each province in 2022: ",
  "<span style='color:", blue, "'>**rich, low-poverty**</span> industrial and coastal provinces, against ",
  "<span style='color:", red, "'>**poor**</span> provinces in the northern mountains, where up to a third of people are poor.")

# Two independent columns, so the big numbers start level with the tourism title (no gap):
# left = big numbers above the map; right = tourism waffle above the poverty dumbbell.
left  <- wrap_elements(full = p_kpi / p_map + plot_layout(heights = c(0.12, 0.88)))
right <- wrap_elements(full = p_tour / p_trend + plot_layout(heights = c(0.48, 0.52)))
p <- left + right + plot_layout(widths = c(1.4, 1)) +
  plot_annotation(
    title = "Vietnam's boom has bypassed<br>its northern mountains",
    subtitle = st,
    caption = paste0("Map: terciles of the 57 provinces with both indicators, 2022; grey = no poverty data. ",
                     "Poverty = multidimensional poverty rate. ",
                     "Vietnam's poverty standard changed in 2022, so the time series stops in 2021. ",
                     "Travel turnover is recorded where travel businesses are based, which favours large cities.<br>",
                     "**Data sources:** General Statistics Office of Vietnam, World Bank · **Creator:** Zoi Diamantakou"),
    theme = theme(
      text = element_text(family = "rc"),
      plot.title = element_textbox_simple(family = "abril", size = 30, colour = ink, lineheight = 1.05,
                                         margin = margin(b = 12)),
      plot.subtitle = element_textbox_simple(size = 11.5, colour = muted, lineheight = 1.3,
                                             margin = margin(b = 14)),
      plot.caption = element_textbox_simple(size = 8, colour = faint, lineheight = 1.4,
                                            margin = margin(t = 14)),
      plot.background = element_rect(fill = "white", colour = NA),
      plot.margin = margin(36, 36, 30, 36)))

ggsave("vietnam.png", p, width = 10, height = 10.5, dpi = 288, bg = "white")
