

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




ds %>% 
  select(content_associate) %>% 
  drop_na() %>% 
  mutate(person = row_number(),
         content_associate = str_split(content_associate, ",")) %>% 
  unnest(cols = c(content_associate)) %>% 
  group_by(content_associate) %>% 
  summarise(n = n()) %>%
  arrange(-n) %>% 
  mutate(prop = n / 654, 
         content_associate = factor(content_associate),
         is_high = if_else(prop >= .5, 1, 0)) %>% 
  ggplot(aes(x = reorder(content_associate, + prop), y = prop)) + 
  geom_segment(aes(x =reorder(content_associate, + prop), xend = reorder(content_associate, + prop), yend = prop, y = 0 )) +
  geom_point(size = 6, aes(col = factor(is_high)), show.legend = F) +
  scale_color_manual(values = c("#F0E442", "#009E73")) +
  geom_text(aes(label = percent(prop, 1)), vjust = 2) +
  coord_flip() +
  scale_y_continuous(labels = percent_format()) +
  scale_x_discrete(expand = c(.1,.1)) +
  labs(title = "\nSocial media followers most associate us with\nFinance & Markets",
       subtitle = "\nWhich of the following content do you associate with the FT? (Select any that apply)\n",
       y = "\nPercentage of Followers",
       x = "Content Areas") +
  theme_minimal()+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    plot.title = element_text(size = 24),
    plot.subtitle = element_text(size = 16),
    legend.position = "bottom") 

ggsave("contentassoc.png", plot = last_plot(), dpi = 400, width = 11, height = 7)


ds %>% 
  select(social_follow, content_associate) %>% 
  mutate(person = row_number(),
         social_follow = str_split(social_follow, ","),
         content_associate = str_split(content_associate, ",")) %>% 
  unnest(cols = social_follow) %>% 
  unnest(cols = content_associate) %>% 
  group_by(content_associate, social_follow) %>% 
  summarise(n = n()) %>% 
  drop_na() %>% 
  ungroup() %>% 
  left_join(social_media_follows, by = c("social_follow")) %>% 
  mutate(prop = n.x / n.y,
         alpha = if_else(content_associate %in% c("Finance & Markets",
                                                  "Companies",
                                                  "World news",
                                                  "Politics"), 1, 0),
         label = if_else(social_follow == "YouTube", 
                         paste0(content_associate, '\n', percent(prop, 1)),
                         percent(prop, 1))) %>% 
  filter(social_follow %in% c("Facebook", "Instagram", "LinkedIn", "YouTube")) %>% 
  {  ggplot(., aes(social_follow, prop, col = reorder(content_associate, + prop),
                   alpha = alpha)) +
      geom_text_repel(data = subset(., alpha ==1), aes(label = label, col = content_associate),
                      show.legend = F) +
      coord_flip() +
      geom_point(show.legend = F) +
      guides(alpha = F) +
      scale_y_continuous(labels = percent_format()) +
      labs(y = "\nPercentage of followers",
           x = "Social Media Platforms",
           subtitle = "\nSocial media platforms by what content followers associate the FT with\n",
           title = "\nThere is only marginal differences in which content followers\nfrom different social media platforms associate us with")+
      theme_minimal()+
      theme(
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 16),
        legend.position = "bottom") 
  }

ggsave("platcontentassoc1.png", plot = last_plot(), dpi = 400, width = 11, height = 7)



ds %>% 
  select(social_follow, content_associate) %>% 
  mutate(person = row_number(),
         social_follow = str_split(social_follow, ","),
         content_associate = str_split(content_associate, ",")) %>% 
  unnest(cols = social_follow) %>% 
  unnest(cols = content_associate) %>% 
  group_by(content_associate, social_follow) %>% 
  summarise(n = n()) %>% 
  drop_na() %>% 
  ungroup() %>% 
  left_join(social_media_follows, by = c("social_follow")) %>% 
  mutate(prop = n.x / n.y,
         alpha = if_else(content_associate %in% c("Finance & Markets",
                                                  "Companies",
                                                  "World news",
                                                  "Politics"), 0, 1),
         label = if_else(social_follow == "YouTube", 
                         paste0(content_associate, '\n', percent(prop, 1)),
                         percent(prop, 1))) %>% 
  filter(social_follow %in% c("Facebook", "Instagram", "LinkedIn", "YouTube")) %>% 
  {  ggplot(., aes(social_follow, prop, col = reorder(content_associate, + prop),
                   alpha = alpha)) +
      geom_text_repel(data = subset(., alpha ==1), aes(label = label, col = content_associate),
                      show.legend = F) +
      coord_flip() +
      geom_point(show.legend = F) +
      guides(alpha = F) +
      scale_y_continuous(labels = percent_format()) +
      labs(y = "\nPercentage of followers",
           x = "Social Media Platforms",
           subtitle = "\nFacebook followers are less likely to associate us with Tech & Climate, but more likely to\nassoicate us with Life & Arts\n",
           title = "\nFacebooks folowers are the most distinct")+
      theme_minimal()+
      theme(
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 16),
        legend.position = "bottom") 
  }

ggsave("platcontentassoc2.png", plot = last_plot(), dpi = 400, width = 11, height = 7)






