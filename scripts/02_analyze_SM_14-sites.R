
unique(yr_mean_long0$name) %>% sort()
# difference between ambient and 2x intensity
yr_diff <- yr_mean_long0 %>%
  pivot_wider(names_from = "intensity", values_from = "value") %>%
  mutate(diff = `event 2x intensity` - ambient) %>%
  select(-`event 2x intensity`, -ambient) %>%
  mutate(variable = str_extract(name, "^[A-Za-z_]*(?=_Lyr)"),
         layer = str_extract(name, "\\d")) %>%
  select(-name) %>%
  pivot_wider(names_from = variable, values_from = diff)

yr_mean_long <- yr_mean_long0 %>%
  mutate(variable = str_extract(name, "^[A-Za-z_]*(?=_Lyr)"),
         layer = str_extract(name, "\\d")) %>%
  select(-name) %>%
  pivot_wider(names_from = variable, values_from = value)

# Figures -----------------------------------------------------------------

# dir.create("figures")
# dir.create("figures/14sites")
# * across sites ----------------------------------------------------------

pdf("figures/14sites/ambient_vs_2x_14sites.pdf")
# vwc by rcp and layer
yr_mean_long %>%
  filter(layer %in% c(1, 3, 6)) %>%
  ggplot(aes(x = VWCBULK, y = intensity)) +
  geom_boxplot()+
  coord_flip() +
  facet_grid(layer~RCP) +
  labs(subtitle = "Mean site level VWC by depth and RCP")

yr_diff %>%
  filter(layer %in% c(1, 3, 6)) %>%
  ggplot(aes(x = VWCBULK, y = RCP)) +
  geom_boxplot()+
  coord_flip() +
  facet_grid(~layer) +
  labs(subtitle = "mean ambient and 2x intensity vwc difference at site by Layer and RCP",
       x = "VWCBULK diff (intensity - ambient)")

yr_mean_long %>%
  filter(layer %in% c(1, 3, 6)) %>%
  ggplot(aes(x = TRANSP_transp_total, y = intensity)) +
  geom_boxplot()+
  coord_flip() +
  facet_grid(layer~RCP) +
  labs(subtitle = "Mean site level transpiration by depth and RCP")

yr_diff %>%
  filter(layer %in% c(1, 3, 6)) %>%
  ggplot(aes(x = TRANSP_transp_total, y = RCP)) +
  geom_boxplot()+
  coord_flip() +
  facet_grid(~layer) +
  labs(subtitle = "mean ambient and 2x intensity transp_total difference at site by Layer and RCP",
       x = "total transp diff (intensity - ambient)")

dev.off()
