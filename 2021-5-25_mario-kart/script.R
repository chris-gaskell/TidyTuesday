library(tidytuesdayR)
library(tidyverse)
library(showtext)
library(sysfonts)
library(stringr)
library(ggstream)
library(cowplot)

# Aes
showtext_auto()
font_add_google("Six Caps")
font_add_google("Fira Sans Extra Condensed")
font1 <- "Six Caps"
font2 <- "Fira Sans Extra Condensed"

# Get the Data
# tuesdata <- tidytuesdayR::tt_load('2021-05-25')
tuesdata <- tidytuesdayR::tt_load(2021, week = 22)
records <- tuesdata$records
drivers <- tuesdata$drivers

# Quick tidy and bar plot of 10 people with most records
p <- drivers %>%
  group_by(player) %>%
  slice(which.max(total)) %>%
  arrange(position) %>% as.data.frame() %>%
  slice_head(n = 10) %>% 
  ggplot(aes
         (x = reorder(
           player, desc(total)),
           weight = total, color = nation)) + 
  geom_bar(fill = "white")

ggdraw(p) +
  theme(plot.background = element_rect(fill="red", color="#fbf7f0"),
        plot.margin = margin(2.5, 0, 2.5, 0, "cm")) +
  draw_text(text="The top 10 players of MarioKart\n by cumualtive amount of records held.",
            family=font2, size=15, x=0.5, y=1.095, color="#4f2217") +
  draw_text(text = "Twitter: @cgaskell92 | Source: MarioKart | GitHub: chris-gaskell", 
            x=0.4, y=-0.11, color="#4f2217", size=10, family=font2, fontface="bold")


ggsave("2021-5-25_mario-kart/top10.png", height=15, width=10)
