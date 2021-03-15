ds %>% 
  select(hold_sub) %>% 
  count(hold_sub) %>% 
  drop_na() %>% 
  mutate(prop = n / sum(n)) %>% 
  ggplot(aes(reorder(hold_sub, + prop), prop, fill = hold_sub, label = percent(prop, 1))) +
  geom_col(show.legend = F) +
  geom_text(hjust = 1, size = 6) +
  scale_y_continuous(labels = percent_format(1)) +
  coord_flip() +  
  scale_fill_manual(values = cbPalette[2:5],
                    breaks = c("Former subscriber", "Lapsed trialist", "Current subscriber",
                               "Never subscribed")) +
  labs(x = "Subscription type",
       y = "\nPercentage of followers",
       subtitle = "\nHave you ever held an FT subscription?\n",
       title = "\nNever-subscribed made up only 43% of social media\nfollowers who engaged with this survey") +
  theme_minimal()+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    plot.title = element_text(size = 24),
    plot.subtitle = element_text(size = 16),
    legend.position = "bottom") 

ggsave("holdftsubl.png", plot = last_plot(), dpi = 400, width = 12, height = 9)




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
           y = "Percentage of followers", 
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
           y = "Percentage of followers", 
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
  filter(social_follow %in% c("Facebook", "Instagram", "LinkedIn", "YouTube", "Twitter")) %>% 
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
  filter(social_follow %in% c("Facebook", "Instagram", "LinkedIn", "YouTube", "Twitter")) %>% 
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




ds %>% 
  select(hold_sub, content_associate) %>% 
  mutate(person = row_number(),
         content_associate = str_split(content_associate, ",")) %>% 
  unnest(cols = c(content_associate)) %>% 
  group_by(content_associate, hold_sub) %>% 
  summarise(n = n()) %>% 
  drop_na() %>% 
  group_by(content_associate) %>% 
  left_join(hold_sub_total, by = "hold_sub") %>% 
  mutate(prop = n.x / n.y) %>% 
  ungroup() %>% 
  filter(!content_associate == "Other (please specify)") %>% 
  mutate(per_never = if_else(hold_sub == "Current subscriber", prop, 0),
         alpha = if_else(content_associate %!in% c("Finance & Markets",
                                                   "Entertainment") 
                         & hold_sub == "Current subscriber",
                         1, 0)) %>% 
  {  ggplot(., aes(prop, reorder(content_associate, + per_never), 
                   fill = factor(hold_sub, levels = c("Lapsed trialist",
                                                      "Former subscriber",
                                                      "Current subscriber",
                                                      "Never subscribed")),
                   alpha = alpha)) +
      geom_col(position = position_dodge(0.7), width = .7) +
      geom_text(data = subset(., alpha == 1), 
                aes(label = percent(prop, 1)),
                position = position_dodge(0.9), vjust = 1, size = 5) +
      scale_x_continuous(labels = percent_format()) +
      scale_fill_manual(values = cbPalette[2:5],
                        breaks = c("Lapsed trialist", "Former subscriber", "Current subscriber",
                                   "Never subscribed"),
                        labels = c("Lapsed trialist", "Former subscriber", "Current subscriber",
                                   "Never subscribed")) +
      scale_alpha(range = c(.2, 1)) +
      guides(alpha = F, 
             fill = guide_legend(reverse = T, label.position = "bottom")) +
      labs(title = "Followers who are current subscribers associate us with\na much broader range of content",
           subtitle = "\nComparing content association by whether a follower has subcribed\n",
           x = "\nPercentage of Followers",
           y = "Content Association", 
           fill = "Subscriber type") +
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

ggsave("content_ass_sub_type.png", plot = last_plot(), dpi = 400, width = 11, height = 7)


ds %>% 
  select(content_interested) %>% 
  drop_na() %>% 
  mutate(person = row_number(),
         content_interested = str_split(content_interested, ",")) %>% 
  unnest(cols = c(content_interested)) %>% 
  count(content_interested) %>% 
  left_join(content_associate_total, by = c("content_interested" = "content_associate")) %>% 
  mutate(x.prop = n.x / 654, 
         y.prop = n.y / 654,
         content_interested = factor(content_interested),
         is_high = if_else(x.prop >= .4, 1, 0)) %>% 
  ggplot(aes(x = reorder(content_interested, + x.prop), y = x.prop)) + 
  geom_segment(aes(x =reorder(content_interested, + x.prop), 
                   xend = reorder(content_interested, + x.prop), 
                   yend = x.prop, y = 0 )) +
  geom_point(size = 4, aes(col = factor(is_high)), show.legend = F) +
  scale_color_manual(values = c("#F0E442", "#009E73")) +
  geom_text(aes(label = percent(x.prop, 1)), vjust = 1.7) +
  coord_flip() +
  scale_y_continuous(labels = percent_format()) +
  scale_x_discrete(expand = c(.1,.1)) +
  labs(title = "\nSocial media followers are interested in\nFinance & Markets, and World News",
       subtitle = "\nWhich of the following content are you most interested in personally?\n(Select any that apply)\n",
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


ggsave("content_interest.png", plot = last_plot(), dpi = 400, width = 11, height = 7)


ds %>% 
  select(content_interested) %>% 
  drop_na() %>% 
  mutate(person = row_number(),
         content_interested = str_split(content_interested, ",")) %>% 
  unnest(cols = c(content_interested)) %>% 
  count(content_interested) %>% 
  left_join(content_associate_total, by = c("content_interested" = "content_associate")) %>% 
  mutate(x.prop = n.x / 654, 
         y.prop = n.y / 654,
         content_interested = factor(content_interested),
         is_high = if_else(x.prop >= .4, 1, 0)) %>% 
  mutate(diff = x.prop - y.prop) %>% 
  select(content_interested, x.prop, y.prop, diff) %>% 
  pivot_longer(2:3) %>% 
  mutate(col = factor(if_else(diff >= 0, 1, 0)),
         diff = if_else(name == "x.prop", diff, 0 ),
         diff = na_if(diff, 0),
         alpha = if_else(content_interested %in% c("Life & Arts",
                                                   "Finance & Markets",
                                                   "World news", 
                                                   "Companies", 
                                                   "Politics"), 1, 0)) %>% 
{  ggplot(., aes(rev(name), value, group = content_interested, 
                 label = paste0(content_interested, ",\n", percent(diff, 1)), 
                 alpha = alpha, col = col)) +
    geom_line(size = 1.5) +
    geom_point(size = 3) +
    geom_text_repel(data = subset(., alpha == 1 & !is.na(diff)),
              x = 2.2, size = 6) +
    geom_rect(xmin = -1,
              xmax = 1.5,
              ymin = 0,
              ymax = Inf, 
              fill = "#CC79A7",
              alpha = 0.02,
              show.legend = F) +
    geom_rect(xmin = 1.5,
              xmax = Inf,
              ymin = 0,
              ymax = Inf, 
              fill = "#56B4E9",
              alpha = 0.02,
              show.legend = F) +
    scale_y_continuous(labels = percent_format()) +
    scale_color_manual(values = c("#D55E00", "#009E73")) +
    guides(alpha = F, col =F) +
    labs(title = "\nInterest in Finance & Markets, World News, Companies,\n& Politics is lower comparative to association",
         subtitle = "\nComparing content association with content interest in each topic area\n",
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
    axis.text.x = element_blank(),
    legend.position = "bottom") 
}


ggsave("content_ass_interest.png", plot = last_plot(), dpi = 400, width = 12, height = 9)





ds %>% 
  select(social_follow, content_interested) %>% 
  mutate(person = row_number(),
         social_follow = str_split(social_follow, ","),
         content_interested = str_split(content_interested, ",")) %>% 
  unnest(cols = social_follow) %>% 
  unnest(cols = content_interested) %>% 
  group_by(content_interested, social_follow) %>% 
  summarise(n = n()) %>% 
  drop_na() %>% 
  ungroup() %>% 
  left_join(social_media_follows, by = c("social_follow")) %>% 
  mutate(prop = n.x / n.y,
         alpha = if_else(content_interested %in% c("Finance & Markets",
                                                   "Companies",
                                                   "World news",
                                                   "Politics"), 1, 0),
         label = if_else(social_follow == "YouTube", 
                         paste0(content_interested, '\n', percent(prop, 1)),
                         percent(prop, 1))) %>% 
  filter(social_follow %in% c("Facebook", "Instagram", "LinkedIn", "YouTube", "Twitter")) %>% 
  {  ggplot(., aes(social_follow, prop, col = reorder(content_interested, + prop),
                   alpha = alpha)) +
      geom_text_repel(data = subset(., alpha ==1), aes(label = label, col = content_interested),
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




ds %>% 
  select(social_follow, content_interested) %>% 
  mutate(person = row_number(),
         social_follow = str_split(social_follow, ","),
         content_interested = str_split(content_interested, ",")) %>% 
  unnest(cols = social_follow) %>% 
  unnest(cols = content_interested) %>% 
  group_by(content_interested, social_follow) %>% 
  summarise(n = n()) %>% 
  drop_na() %>% 
  ungroup() %>% 
  left_join(social_media_follows, by = c("social_follow")) %>% 
  mutate(prop = n.x / n.y,
         alpha = if_else(content_interested %in% c("Finance & Markets",
                                                  "Companies",
                                                  "World news",
                                                  "Politics"), 0, 1),
         label = if_else(social_follow == "YouTube", 
                         paste0(content_interested, '\n', percent(prop, 1)),
                         percent(prop, 1))) %>% 
  filter(social_follow %in% c("Facebook", "Instagram", "LinkedIn", "YouTube", "Twitter")) %>% 
  {  ggplot(., aes(social_follow, prop, col = reorder(content_interested, + prop),
                   alpha = alpha)) +
      geom_text_repel(data = subset(., alpha ==1), aes(label = label, col = content_interested),
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



ds %>% 
  select(hold_sub, content_interested) %>% 
  mutate(person = row_number(),
         content_interested = str_split(content_interested, ",")) %>% 
  unnest(cols = c(content_interested)) %>% 
  group_by(content_interested, hold_sub) %>% 
  summarise(n = n()) %>% 
  drop_na() %>% 
  group_by(content_interested) %>% 
  left_join(hold_sub_total, by = "hold_sub") %>% 
  mutate(prop = n.x / n.y) %>% 
  ungroup() %>% 
  filter(!content_interested == "Other (please specify)") %>% 
  mutate(per_never = if_else(hold_sub == "Current subscriber", prop, 0),
         alpha = if_else(content_interested == "Politics" 
                         & (prop >= .59 
                         | prop <= .40),
                         1, 0)) %>% 
  {  ggplot(., aes(prop, reorder(content_interested, + per_never), 
                   fill = factor(hold_sub, levels = c("Lapsed trialist",
                                                      "Former subscriber",
                                                      "Current subscriber",
                                                      "Never subscribed")),
                   alpha = alpha)) +
      geom_col(position = position_dodge(0.7), width = .7) +
      geom_text(data = subset(., alpha == 1), 
                aes(label = percent(prop, 1)),
                position = position_dodge(0.9), vjust = 1, size = 5) +
      scale_x_continuous(labels = percent_format()) +
      scale_fill_manual(values = cbPalette[2:5],
                        breaks = c("Lapsed trialist", "Former subscriber", "Current subscriber",
                                   "Never subscribed"),
                        labels = c("Lapsed trialist", "Former subscriber", "Current subscriber",
                                   "Never subscribed")) +
      scale_alpha(range = c(.2, 1)) +
      guides(alpha = F, 
             fill = guide_legend(reverse = T, label.position = "bottom")) +
      labs(title = "Current Subscribers are more interested in Politics,\nFormer are much less",
           subtitle = "\nComparing content interest by whether a follower has subcribed\n",
           x = "\nPercentage of Followers",
           y = "Content Association", 
           fill = "Subscriber type") +
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

ggsave("content_inter_sub_pol.png", plot = last_plot(), dpi = 400, width = 11, height = 7)



ds %>% 
  select(hold_sub, content_interested) %>% 
  mutate(person = row_number(),
         content_interested = str_split(content_interested, ",")) %>% 
  unnest(cols = c(content_interested)) %>% 
  group_by(content_interested, hold_sub) %>% 
  summarise(n = n()) %>% 
  drop_na() %>% 
  group_by(content_interested) %>% 
  left_join(hold_sub_total, by = "hold_sub") %>% 
  mutate(prop = n.x / n.y) %>% 
  ungroup() %>% 
  filter(!content_interested == "Other (please specify)") %>% 
  mutate(per_never = if_else(hold_sub == "Current subscriber", prop, 0),
         alpha = if_else(content_interested == "Life & Arts" 
                         & prop >= .3,
                         1, 0)) %>% 
  {  ggplot(., aes(prop, reorder(content_interested, + per_never), 
                   fill = factor(hold_sub, levels = c("Lapsed trialist",
                                                      "Former subscriber",
                                                      "Current subscriber",
                                                      "Never subscribed")),
                   alpha = alpha)) +
      geom_col(position = position_dodge(0.7), width = .7) +
      geom_text(data = subset(., alpha == 1), 
                aes(label = percent(prop, 1)),
                position = position_dodge(0.9), vjust = 1, size = 5) +
      scale_x_continuous(labels = percent_format()) +
      scale_fill_manual(values = cbPalette[2:5],
                        breaks = c("Lapsed trialist", "Former subscriber", "Current subscriber",
                                   "Never subscribed"),
                        labels = c("Lapsed trialist", "Former subscriber", "Current subscriber",
                                   "Never subscribed")) +
      scale_alpha(range = c(.2, 1)) +
      guides(alpha = F, 
             fill = guide_legend(reverse = T, label.position = "bottom")) +
      labs(title = "Current & Former are more interested in our Life & Arts",
           subtitle = "\nComparing content interest by whether a follower has subcribed\n",
           x = "\nPercentage of Followers",
           y = "Content Association", 
           fill = "Subscriber type") +
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

ggsave("content_inter_sub_la.png", plot = last_plot(), dpi = 400, width = 11, height = 7)


ds %>% 
  select(hold_sub, content_interested) %>% 
  mutate(person = row_number(),
         content_interested = str_split(content_interested, ",")) %>% 
  unnest(cols = c(content_interested)) %>% 
  group_by(content_interested, hold_sub) %>% 
  summarise(n = n()) %>% 
  drop_na() %>% 
  group_by(content_interested) %>% 
  left_join(hold_sub_total, by = "hold_sub") %>% 
  mutate(prop = n.x / n.y) %>% 
  ungroup() %>% 
  filter(!content_interested == "Other (please specify)") %>% 
  mutate(per_never = if_else(hold_sub == "Current subscriber", prop, 0),
         alpha = if_else(content_interested %in% c("Technology", "Companies")
                         & (prop >= .56|
                            prop >= .43 &
                            prop <= .47),
                         1, 0)) %>% 
  {  ggplot(., aes(prop, reorder(content_interested, + per_never), 
                   fill = factor(hold_sub, levels = c("Lapsed trialist",
                                                      "Former subscriber",
                                                      "Current subscriber",
                                                      "Never subscribed")),
                   alpha = alpha)) +
      geom_col(position = position_dodge(0.7), width = .7) +
      geom_text(data = subset(., alpha == 1), 
                aes(label = percent(prop, 1)),
                position = position_dodge(0.7), vjust = 1.5, hjust = -.5, size = 5) +
      scale_x_continuous(labels = percent_format()) +
      scale_fill_manual(values = cbPalette[2:5],
                        breaks = c("Lapsed trialist", "Former subscriber", "Current subscriber",
                                   "Never subscribed"),
                        labels = c("Lapsed trialist", "Former subscriber", "Current subscriber",
                                   "Never subscribed")) +
      scale_alpha(range = c(.2, 1)) +
      guides(alpha = F, 
             fill = guide_legend(reverse = T, label.position = "bottom")) +
      labs(title = "Former subs & Lapsed trialists are more interested in\nCompanies and in Tech",
           subtitle = "\nComparing content interest by whether a follower has subcribed\n",
           x = "\nPercentage of Followers",
           y = "Content Association", 
           fill = "Subscriber type") +
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

ggsave("content_inter_sub_techcomp.png", plot = last_plot(), dpi = 400, width = 11, height = 7)




