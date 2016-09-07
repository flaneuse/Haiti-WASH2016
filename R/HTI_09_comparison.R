comp = full_join(toilet_admin1_PaP, water_admin1_PaP, by = c("region_name")) %>% 
  select(region_name, `improved latrines` = improved_toilet, 
         `improved water` = impr_water_under30min) %>% 
  arrange((`improved latrines`)) %>% 
  gather(indicator, value, -region_name) %>% 
  mutate(label_colour = ifelse(value < median(value), grey75K, grey10K))


comp$region_name = factor(comp$region_name, levels = comp$region_name)
  
ggplot(comp, aes(y = region_name, x = indicator,
                 fill = value)) +
  geom_tile(colour = 'white', size = 0.25) +
  geom_text(aes(label = percent(value),
                colour = label_colour),
            size = 4,
            family = font_normal) +
  scale_fill_gradientn(colors = brewer.pal(9, colour_water),
                       limits = colour_limits) +
  scale_colour_identity() +
  theme_xylab() +
  theme(axis.title = element_blank())

ggsave('~/Creative Cloud Files/MAV/Haiti_WASH-PAD_2016-09/exported_R/HTI_comp_YlGnBu.pdf', 
       width = 2.9, height = 6.4,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

ggplot(comp, aes(y = region_name, x = indicator,
                 fill = value)) +
  geom_tile(colour = 'white', size = 0.25) +
  geom_text(aes(label = percent(value),
                colour = label_colour),
            size = 4,
            family = font_normal) +
  scale_fill_gradientn(colors = brewer.pal(9, colour_toilet)) +
  scale_colour_identity() +
  theme_xylab() +
  theme(axis.title = element_blank())

ggsave('~/Creative Cloud Files/MAV/Haiti_WASH-PAD_2016-09/exported_R/HTI_comp_RdPu.pdf', 
       width = 2.9, height = 6.4,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)


#-- Dot plot --
ggplot(comp, aes(y = region_name, x = value,
                 colour = value)) +
  geom_point(size = 4) +
  geom_text(aes(label = percent(value),
                colour = value),
            size = 4,
            nudge_x = 0.07,
            family = font_normal) +
  scale_fill_gradientn(colors = brewer.pal(9, colour_toilet)) +
  scale_colour_gradientn(colors = brewer.pal(9, colour_toilet)) +
  scale_x_continuous(labels = scales::percent, name = '') +
  facet_wrap(~indicator) +
  theme_xgrid()

ggsave('~/Creative Cloud Files/MAV/Haiti_WASH-PAD_2016-09/exported_R/HTI_comp_dot_RdPu.pdf', 
       width = 6, height = 6.4,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)