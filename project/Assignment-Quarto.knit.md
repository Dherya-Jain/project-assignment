---
title: "***Millions Denied a Basic Right***"
subtitle: "**Exploring the Global Disparity in Safe Drinking Water and its Importance**"
author: "***Dherya Jain***"
date: "**2024-04-23**"
format: html
---



::: {.cell}

```{.r .cell-code}
unicef_indicator_1 %>%
  filter(Year==2022) %>%
  full_join(map_world, by = c("country"="region")) %>%
ggplot(aes(x=long, y=lat, group=group, fill=obs_value)) +
  geom_polygon(color = "grey30")+
  labs(title = "Proportion of population drinking clean drinking water in the year 2022", 
       subtitle = "Countries in grey have no data due to mismatch with their names",
        caption = "Source: UNICEF",
       x = "longitude",
       y = "latitude",
       fill = "Observed Value") +
  scale_fill_gradient(low= "green", high = "lightblue4", na.value = "grey90")+
  theme_classic()+
  theme(plot.title = element_text(size = 13, face = "bold", color = "black", family = "serif", vjust = 1.5),
        plot.subtitle = element_text(size = 10, color = "black", family = "serif"))
```

::: {.cell-output-display}
![The world map sheds light on ***access to improved drinking water*** in the year **2022**, but also highlights persistent disparities. It has been observed that the ***European continent*** exhibits **greater access** to clean drinking water **compared to** **Africa and parts of Asia**. This underscores both the potential for universal clean water access and the ongoing challenges many nations face in providing this fundamental resource to all their citizens.](Assignment-Quarto_files/figure-html/unnamed-chunk-2-1.png){width=672}
:::
:::

::: {.cell}

```{.r .cell-code}
unicef_metadata_1 %>%
  filter(Year==2021)%>%
  full_join(map_world, by = c("country"="region"))%>%
ggplot(aes(x=long, y=lat, group=group, fill=Life_expectancy_at_birth)) +
  geom_polygon(color = "grey30")+
  labs(title = "Life Expectancy at birth (in years) in the year 2021", 
       subtitle = "Countries in grey have no data due to mismatch with their names",
        caption = "Source: UNICEF",
       x = "longitude",
       y = "latitude",
       fill = "Observed Value") +
  scale_fill_gradient(low= "lightgreen", high = "lightblue4", na.value = "grey90")+
  theme_classic()+
  theme(plot.title = element_text(size = 13, face = "bold", color = "black", family = "serif", vjust = 1.5),
        plot.subtitle = element_text(size = 10, color = "black", family = "serif"))
```

::: {.cell-output-display}
![To comprehend the profound influence of clean drinking water on public health, it is imperative to **delve into the life expectancy** rates across various countries. This map serves as a lens through which we can discern the stark disparities in life expectancy among continents. It becomes evident that the *European continent emerges prominently* in *comparison to* others, showcasing higher life expectancy rates. Conversely, the *African and certain parts of the Asian continent* exhibit significant *deficits in life expectancy*, mirroring the findings of the earlier map depicting access to clean drinking water. This ***correlation*** underscores the robust link between access to ***clean drinking water and life expectancy***. In the subsequent sections of the report, we will embark on a more comprehensive exploration of this vital relationship.](Assignment-Quarto_files/figure-html/unnamed-chunk-3-1.png){width=672}
:::
:::

data_join_filtered <- data_join %>%
  filter(Year == 2021)

ggplot(data_join_filtered) +
  aes(Life_expectancy_at_birth, obs_value, color = Continent) +
  geom_point(alpha = .5) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Add regression line
  scale_x_continuous(breaks = c(40, 80)) +  
  labs(y = "Life Expectancy at Birth",
       x = "Proportion of Population Drinking Clean Water", 
       title = "Analyzing the Influence of Access to Clean Drinking Water on the Life Expectancy Across Continents") +
  theme_classic() +
  theme(text = element_text(family = "serif")) +
  theme(panel.background = element_rect(fill = "grey96"))


