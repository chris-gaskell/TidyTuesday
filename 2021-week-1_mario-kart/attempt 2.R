# Get the Data
# tuesdata <- tidytuesdayR::tt_load('2021-05-25')
tuesdata <- tidytuesdayR::tt_load(2021, week = 22)
records <- tuesdata$records
drivers <- tuesdata$drivers

records_track <- records %>% 
  group_by(track)

records_track <- summarise(records_track, 
            mean_lap_time = mean(time, na.rm = T),
            sd_lap_time = sd(time, na.rm = T)) %>% 
            mutate(track  = fct_reorder(track, mean_lap_time,
                                        .desc = T))

p <- ggplot(
  data = records_track) +
  geom_col(mapping = aes(x = track, y = mean_lap_time,
                         fill = mean_lap_time)) +
  scale_fill_gradient(low = "green",
                      high = "darkgreen", name = "Mean Lap Time") +
  theme(legend.position = "none") +
  ggdark::dark_theme_classic() + # New package
  scale_x_discrete(
        labels = function(x) stringr::str_wrap(x, width = 10)) + # New package
  labs(title = "Mean lap time across records holders for Mario Kart" 
       #subtitle = "..."
       )+ theme(plot.title = element_text(size=22))

cool_text <- '<span style="color:#00FF00;">**Mario Kart**</span><br>
     Was a great game and the <br>first non-platform
     game <br>that mario appeared in'

p <- p + annotate( # New package
    geom = "richtext",
    x = 5.1, y = 115,
    label = cool_text,
    label.colour = NA, fill = NA,
    fontface = "bold", color = "white",
    size = 5) +
  labs(caption = "Data from #TidyTuesday project.
       Plot by Chris Gaskell
       @cgaskell92.") +
  theme(text = element_text(
        family = "sans")) +
  xlab("Track") + ylab("Mean Lap Time") 

ggsave("track-times.png", width = 13, height = 8, dpi = 300)

# get_png <- function(filename) {
#   grid::rasterGrob(png::readPNG(filename), interpolate = TRUE)
# }
# 
# l <- ("mario.png")
# 
# t <- grid::roundrectGrob()
# # 
# p +
#   annotation_custom(logo, xmin = 13.5, xmax = 16.5, ymin = -40, ymax = -68.5) +
#   coord_cartesian(clip = "off") +
#   theme(plot.margin = unit(c(1, 1, 3, 1), "lines"))
# # 
# p +
#   annotation_custom(l, xmin = 13.5, xmax = 16.5, ymin = -40, ymax = -68.5) +
#   coord_cartesian(clip = "off") +
#   theme(plot.margin = unit(c(1, 1, 3, 1), "lines"))

logo <- image_read(paste0(here("/"), "/Users/christophergaskell/Desktop/TidyTuesday/2021-5-25_mario-kart/mario.png"))
plot <- image_read(paste0(here("/"), "/Users/christophergaskell/Desktop/TidyTuesday/2021-5-25_mario-kart/track-times.png"))

logo <- logo %>%
  image_scale("400") %>% 
  image_background("black", flatten = T) #%>%
 # image_border("black", "600x10") #%>%
  #image_annotate("Powered By R", color = "white", size = 30, 
                 #location = "+10+50", gravity = "northeast")

#final_plot <- image_append(image_scale(c(plot, logo), "500"), stack = T)
plot <- plot %>% 
image_composite(logo, offset = "+2250+300") #%>%
  #image_resize("800x800")

#ggsave("final-track-times.png", width = , height = 4, dpi = 300)
magick::image_write(plot, "final-track-times.png")



