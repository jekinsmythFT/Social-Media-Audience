

ds %>% 
  select(hold_sub, social_follow) %>% 
  mutate(person = row_number(),
         social_follow = str_split(social_follow, ",")) %>% 
  unnest(cols = c(social_follow)) %>% 
  group_by(person) %>%
  pivot_wider(names_from = 2,
              values_from = 2) %>% 
  ungroup() %>% 
  mutate_at(3:9,
            ~if_else(is.na(.x), 0, 1)) %>% 
  select(1, 3:7) %>% 
  group_by(hold_sub) %>% 
  summarise_at(1:5,
               ~sum(.), na.rm = T) %>% 
  drop_na() %>% 
  pivot_longer(2:6) %>% 
  group_by(name) %>% 
  mutate(prop = value / sum(value),
         alpha = if_else(hold_sub == "Non-subscriber", 1, 0)) %>% 
  {  ggplot(., aes(factor(name, levels = rev(c("Instagram", "Facebook", "YouTube",
                                               "Twitter", "LinkedIn"))), prop, 
                   fill = factor(hold_sub, levels = c("Lapsed trialist",
                                                      "Former subscriber",
                                                      "Current subscriber",
                                                      "Non-subscriber")),
                   alpha = alpha, label = percent(prop, 1))) +
      geom_col() +
      geom_text(data = subset(., alpha == 1), position = position_stack(), hjust = 1,
                aes(y = .2)) +
      scale_fill_manual(values = cbPalette[2:5],
                        breaks = c("Lapsed trialist", "Former subscriber", "Current subscriber",
                                   "Non-subscriber"),
                        labels = c("Lapsed trialist", "Former subscriber", "Current subscriber",
                                   "Non-subscriber")) +
      scale_alpha(range = c(.4,1)) +
      scale_y_continuous(labels = percent_format(1)) +
      coord_flip() +
      guides(alpha = F,
             fill = guide_legend(reverse = T, label.position = "bottom")) +
      labs(fill = "Subscriber type",
           y = "Percentage of Respondents", 
           x = "Social media followers",
           subtitle = "\nSusbcriber types of our followers from different social media platforms\n",
           title = "\nNon-subscribers are more likely to follow us on\nInstagram & Facebook") +
      theme_minimal() +
      theme(
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 16),
        legend.position = "bottom") 
  }

ggsave("nonsubssocial.png", plot = last_plot(), dpi = 400, width = 11, height = 7)



ds %>% 
  select(hold_sub, social_follow) %>% 
  mutate(person = row_number(),
         social_follow = str_split(social_follow, ",")) %>% 
  unnest(cols = c(social_follow)) %>% 
  group_by(person) %>%
  pivot_wider(names_from = 2,
              values_from = 2) %>% 
  ungroup() %>% 
  mutate_at(3:9,
            ~if_else(is.na(.x), 0, 1)) %>% 
  select(1, 3:7) %>% 
  group_by(hold_sub) %>% 
  summarise_at(1:5,
               ~sum(.), na.rm = T) %>% 
  drop_na() %>% 
  pivot_longer(2:6) %>% 
  group_by(name) %>% 
  mutate(prop = value / sum(value),
         alpha = if_else(hold_sub == "Current subscriber", 1, 0)) %>% 
  {  ggplot(., aes(factor(name, levels = rev(c("Instagram", "Facebook", "YouTube",
                                               "Twitter", "LinkedIn"))), prop, 
                   fill = factor(hold_sub, levels = c("Lapsed trialist",
                                                      "Former subscriber",
                                                      "Current subscriber",
                                                      "Non-subscriber")),
                   alpha = alpha, label = percent(prop, 1))) +
      geom_col() +
      geom_text(data = subset(., alpha == 1), position = position_stack(), hjust = 1,
                aes(y = .6)) +
      scale_fill_manual(values = cbPalette[2:5],
                        breaks = c("Lapsed trialist", "Former subscriber", "Current subscriber",
                                   "Non-subscriber"),
                        labels = c("Lapsed trialist", "Former subscriber", "Current subscriber",
                                   "Non-subscriber")) +
      scale_alpha(range = c(.4,1)) +
      scale_y_continuous(labels = percent_format(1)) +
      coord_flip() +
      guides(alpha = F,
             fill = guide_legend(reverse = T, label.position = "bottom")) +
      labs(fill = "Subscriber type",
           y = "Percentage of Respondents", 
           x = "Social media followers",
           subtitle = "\nSusbcriber types of our followers from different social media platforms\n",
           title = "\nCurrent subscribers are more likely to follow us on Twitter,\nYouTube, & LinkedIn") +
      theme_minimal() +
      theme(
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 16),
        legend.position = "bottom") 
  }


ggsave("subssocial.png", plot = last_plot(), dpi = 400, width = 11, height = 7)
