
#libraries
library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(gganimate)
library(gifski)
#Don't install unless using it!
library(plotly)
library(av)
library(scales)
library(hrbrthemes)

#create directories
dir.create("data")
dir.create("output")
dir.create("html")
setwd('..')
setwd('html')

######################################################################################
#data
#read excel spreadsheets
provinces <- read_excel("data/x_Live_COVID cases by Province and GP district_For Viz_CH.xlsx", 
                        sheet = "Provinces_Viz")
districts <- read_excel("data/x_Live_COVID cases by Province and GP district_For Viz_CH.xlsx",
                        sheet = "GP Districts_Viz")

#create data frames
provinces_data<- data.frame(provinces)
gp_districts <- data.frame(districts)

View(data)

#filter data by province
provinces_selection <- provinces_data %>% 
  filter(Province %in% c("Gauteng", "KwaZulu-Natal", "Western Cape", "Eastern Cape"))


#########################################################################################
#graph for provinces
#create line graph
# for selection data
line <- provinces_selection %>% ggplot(aes(x= Cumulative, y= Count, group=Province, color=Province)) +
  geom_line(size = 1) +
  #set colours
  scale_color_manual(values=c("#CC5A31", "#832A2F", "#6E9C73", "#3E5D92"))+
  geom_point(size = 1) +
  #labels
  geom_segment(aes(xend = 200000, yend = Count), linetype = 2, colour = 'grey')+
  geom_text(aes(x = 200000.1, label = Province), hjust = 0)+
  theme_minimal() +
  theme(plot.margin = margin(5.5, 100, 5.5, 5.5))  +
  theme(legend.position = "none") +
  coord_cartesian(clip = "off")+
  #scales and axes
  scale_x_continuous(name="Cumulative number of confirmed cases",
                     trans = log10_trans(),
                     limits=c(NA,1000000), 
                     labels = comma_format(big.mark = ",", decimal.mark = " "))+
  scale_y_continuous(name="Number of confirmed cases per week",
                     trans = log10_trans(),
                     limits=c(NA,1000000), 
                     labels = comma_format(big.mark = ",", decimal.mark = " "))+
  #moving chart
  transition_reveal(Week)+
  #labels for title and subtitle
  labs(title = "Change in exponential growth of confirmed COVID-19 cases  \n  in selected provinces",
       subtitle = 'Week {round(frame_along)}')+
  #cosmetics for text
  theme(plot.title = element_text(size= 13, face = "bold",hjust = 0.5),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 11))


#See animation in viewer
animate(line, fps=6, width = 800, height = 500,  end_pause = 40, renderer = gifski_renderer())
#animate(line, width = 600, height = 600, renderer = av_renderer())

# Save at gif:
anim_save(filename = "growth_chart_log10_v2.gif", save_animation = "gif")

#####################################################################################
#graph for districts

district_line <- gp_districts %>% ggplot(aes(x= Cumulative, y= Count, group=District, color=District)) +
  geom_line(size = 1) +
  #set colours
  scale_color_manual(values=c("#3E5D92", "#832A2F", "#CC5A31", "#6E9C73","#70555D"))+
  geom_point(size = 1) +
  #labels
  geom_segment(aes(xend = 100000, yend = Count), linetype = 2, colour = 'grey')+
  geom_text(aes(x = 100000.1, label = District), hjust = 0)+
  theme_minimal() +
  theme(plot.margin = margin(5.5, 200, 5.5, 5.5))  +
  theme(legend.position = "none") +
  coord_cartesian(clip = "off")+
  #scales and axes
  scale_x_continuous(name="Cumulative number of confirmed cases",
                     trans = log10_trans(),
                     limits=c(NA,100005),
                     labels = comma_format(big.mark = ","))+
  scale_y_continuous(name="Number of confirmed cases per week",
                     trans = log10_trans(),
                     limits=c(NA,100005),
                     labels = comma_format(big.mark = ","))+
  #moving chart
  transition_reveal(Week)+
  #labels for title and subtitle
  labs(title = "Change in exponential growth of confirmed COVID-19 cases  \n  in Gauteng districts",
       subtitle = 'Week {round(frame_along)}')+
  #cosmetics for text
  theme(plot.title = element_text(size= 13, face = "bold",hjust = 0.5),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 11))


#See animation in viewer
animate(district_line, fps=6, width = 800, height = 500,  end_pause = 40, renderer = gifski_renderer())
#animate(line, width = 600, height = 600, renderer = av_renderer())

# Save at gif:
anim_save(filename = "district_chart_log10_v3.gif", save_animation = "gif")
####################################################################################