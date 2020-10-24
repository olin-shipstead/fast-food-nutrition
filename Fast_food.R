# Olin Shipstead
# 28 March 2020
# exploring tidytuesday fastfood data

library(tidyverse)
library(RColorBrewer)

data_url <-"https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018/2018-09-04/fastfood_calories.csv"
fast_food <- read_csv(data_url)

head(fast_food$item)

dev.new()

this_food <- fast_food %>%
    mutate(restaurant=fct_reorder(restaurant, trans_fat))%>%
    na.omit()

this_food %>%
    ggplot(aes(x=restaurant, y=trans_fat, color=restaurant))+
    geom_boxplot()+
    geom_jitter(width=.15, alpha=.5)+
    #scale_y_log10()+
    theme(legend.position = "none")

# bacon solo

bacon_items <- fast_food$item %>%
    contains(match = "Bacon",ignore.case = TRUE)

bacon <- rep(0,length(fast_food$item))
bacon[bacon_items] = 1
bacon <- factor(bacon, levels=c(1,0), labels=c("Yes","No"))

fast_food <- fast_food %>%
    add_column(bacon = bacon)

# bacon boxplots
dev.new()
fast_food %>%
    ggplot(aes(x=bacon, y=cholesterol, color=bacon))+
    #geom_violin(draw_quantiles = TRUE)+
    geom_boxplot(varwidth = TRUE)+
    geom_jitter(width=.05,alpha=.25)+
    #scale_y_log10()+
    theme(legend.position = "none")+
    scale_color_hue(h=c(220,10))+
    lims(y=c(0,300))

# restaurant boxplots using bacon as color
dev.new()
fast_food %>%
    ggplot(aes(x=restaurant, y=cholesterol, fill=bacon))+
    scale_y_log10()+
    geom_boxplot(outlier.shape = "X")+
    #geom_jitter(width=.15, alpha=.5)+
    #theme(legend.position = "none")+
    scale_x_discrete(name="",guide = guide_axis(n.dodge = 2))+
    scale_fill_manual(values = c("#00AFFF","#FF255E"))+
    theme_classic()

# using facet_wrap to separate restaurants, then plot histograms of cholesterol using colors for bacon
dev.new()
fast_food %>%
    ggplot(aes(x=total_fat, fill=bacon))+
    scale_x_binned()+
    geom_bar(position="stack")+
    facet_wrap(~restaurant)+
    labs(x="Total fat (g)",y="Item count")+
    theme(legend.position = c(.82,.25), legend.direction = "horizontal", axis.text.x = element_text(angle = 90))

# future: produce a model that can classify items into their restaurants






