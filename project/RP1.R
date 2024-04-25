install.packages("tidyverse")
install.packages("plotly")
install.packages("maps")
install.packages("countrycode")
install.packages("patchwork")
install.packages("ggtext")


library(tidyverse)
library(plotly)
library(maps)
library(countrycode)
library(ggplot2)
library(patchwork)
library(ggtext)
unicef_indicator_1 <- read_csv("unicef_indicator_1_new.csv")
show_col_types = FALSE
map_world <- map_data("world")
unicef_metadata_1 <- read_csv("unicef_metadata_1.csv")
unicef_indicator_1 <- read_csv("unicef_indicator_1.csv")


data_join <-full_join(unicef_indicator_1_new,unicef_metadata_1)
data_join <-full_join(unicef_indicator_1,unicef_metadata_1)

#map 2022
data_2022 <- unicef_indicator_1 %>%
  filter(Year==2022)
map_data_join_2022 <- full_join(data_2022, map_world, by = c("country"="region"))
ggplot(map_data_join_2022) +
  aes(x=long, y=lat, group=group, fill=obs_value)+
  geom_polygon(color = "grey50")+
  ggtitle(label = "The World Map", subtitle = "Proportion of population drinking clean drinking water in the year 2022")
unicef_metadata_1 <- read_csv("unicef_metadata_1.csv")

#map 2020
data_2020 <- unicef_indicator_1 %>%
  filter(Year==2020)
map_data_join_2020 <- full_join(data_2020, map_world, by = c("country"="region"))
ggplot(map_data_join_2020) +
  aes(x=long, y=lat, group=group, fill=obs_value)+
  geom_polygon(color = "grey50")+
  ggtitle(label = "The World Map", subtitle = "Percentage of people drinking fresh water in the year 2020")

#map life 2021
data_life_2021 <- unicef_metadata_1 %>%
  filter(Year==2021)
map_data_join_life_2020 <- full_join(data_life_2020, map_world, by = c("country"="region"))
ggplot(map_data_join_life_2020) +
  aes(x=long, y=lat, group=group, fill=Life_expectancy_at_birth)+
  geom_polygon(color = "grey50")+
  ggtitle(label = "The World Map", subtitle = "Life expectancy at birth of people(in years) in the year 2021")
#map life 2000
data_life_2000 <- unicef_metadata_1 %>%
  filter(Year==2000)
map_data_join_life_2000 <- full_join(data_life_2000, map_world, by = c("country"="region"))
ggplot(map_data_join_life_2000) +
  aes(x=long, y=lat, group=group, fill=Life_expectancy_at_birth)+
  geom_polygon(color = "grey50")+
  ggtitle(label = "The World Map", subtitle = "Life expectancy at birth of people(in years) in the year 2000")
unicef_indicator_2 <- read_csv("unicef_indicator_2.csv")
options(scipen = 999)

world_map <- map_data("world")
world_map$continent <- countrycode(world_map$region, "country.name", "continent")  
data_join <-full_join(unicef_indicator_2, world_map, by=c("country"="region"))

#timeseries

timeseries_plot_1 <- data_join %>%
  filter(Year >= 2000) %>%
  ggplot()+
  aes(Year, obs_value, group = countries, colour = Continent)+
  geom_line()
ggplotly(timeseries_plot_1, tooltip = "text")


# scatter plot 1 
data_join_filtered <- data_join %>%
  filter(Year >= 2010 & Year <= 2021)

ggplot(data_join_filtered) +
  aes(obs_value, Life_expectancy_at_birth, color = Continent) +
  geom_point(alpha = .6) +
  facet_wrap(~ Year, nrow = 1) + 
  scale_x_continuous(breaks = c(40, 100)) +  
  labs(y = "Life Expectancy at birth",
       x = "Proportion of Population Drinking Clean Water", 
       title = "Analyzing the Influence of Clean Drinking Water on the Life Expectancy Across Continents") +
  guides(color = "none") + 
  theme_classic() +
  theme(text = element_text(family = "serif"))+
  theme(panel.background = element_rect(fill = "white"))

# scatterplot 2

custom_palette <- c("Asia" = "#1f77f0", 
                    "Europe" = "#ff7f0e", 
                    "Africa" = "#2ca02c", 
                    "Americas" = "#d62711",  
                    "Oceania" = "#9467fd")   


ggplot(data_join_filtered %>% filter(Year == 2021)) +
  aes(Life_expectancy_at_birth, obs_value, color = Continent) +
  geom_point(alpha = 0.8) +
  geom_smooth(method = "loess", se = FALSE, color = "black") +  
  scale_color_manual(values = custom_palette)+
  labs(
    y = "Life Expectancy at Birth",
    x = "Proportion of Population Drinking Clean Water", 
    title = "Impact of Clean Drinking Water on the Life Expectancy Across Continents in year 2021"
  ) +
  theme_classic() +
  theme(
    text = element_text(family = "serif"),
    panel.background = element_rect(fill = "grey96")
  )+
  guides(color = guide_legend(override.aes = list(size = 4)))

#bar chart


data_join %>%
  filter(Year >= 2010 & Year <= 2021) %>%
  group_by(Continent, Year) %>%
  summarise(m_Life_expectancy_at_birth = mean(Life_expectancy_at_birth, na.rm = TRUE)) %>%
  ggplot() + 
  aes(x = reorder(Continent, m_Life_expectancy_at_birth), y = m_Life_expectancy_at_birth, fill = Continent) +
  geom_col() + 
  facet_wrap(~ Year, nrow = 1) +
  labs(title = "The Life Expectancy Across Continents by Year",
       x = "Continent",
       y = "Life Expectancy at birth (in Years)") +
  theme_classic()+
  theme(
    text= element_text(family = "serif"),
    axis.text.x = element_blank()
  ) + 
  theme(panel.background = element_rect(fill = "lightgrey"))+ 
  scale_fill_manual(values= c("red3", "yellow2", "orange", "pink2", "green4", "blue"))






