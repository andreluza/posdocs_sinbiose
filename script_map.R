
# data centers
require(openxlsx)
require(dplyr)
dat <- read.xlsx ("data_map.xlsx")

# remove CHina and South Africa ( we dont know their current status )
dat <- dat %>% 
  filter (Country %in% c("South Africa") == F)


require(rnaturalearth)
require(ggplot2)
require(ggrepel)

## maps
# mapa mundi
world <- ne_countries(scale = "medium", returnclass = "sf")
world$colour_Country <- ifelse (world$name_sort %in% dat$Country, "orange","gray90")

# cortar o mapa para ver a america do Sul e parte da central
wm <- ggplot() + 
  geom_sf (data=world, size = 0.1, 
           fill= world$colour_Country,
           colour="gray40") +
  #coord_sf (xlim = c(-50, -25),  ylim = c(-30, 4), expand = FALSE) +
  theme_bw() + #xlab ("Longitude")  + ylab ("Latitude") +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#f4f9f9",#darkslategray1
                                        colour = "#f4f9f9"),
        axis.text.x = element_text(size=8),
        axis.ticks.x=element_line(size=1),
        axis.text.y = element_text(size=8),
        axis.ticks.y=element_line(size=1),
        axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=10),
        #title = element_blank(),
        plot.margin = unit(c(0,-0.8,0,0.3), "cm")) +
  labs (title = "Synthesis Centers/Initiatives around the Globe",
        subtitle = "Active (triangles) and discontinued centers (points)",
        caption = "Data sources: The International Synthesis Consortium\nBaron et al., 2017\nAuthors' knowledge")


# require(dplyr)
map_SC <- wm + geom_point(data  = dat,aes (x=Long,y=Lat,
                                  group=Active,
                                  shape = Active), 
                size=3) + 
  geom_label_repel(data = dat, aes(x=Long, 
                                  y=Lat,
                                  label = paste (Abbreviation, 
                                                 Abb_Country,sep=", ")),
                  size = 3,
                  fill = "white",
                  alpha=0.8,
                  min.segment.length = 0,
                  box.padding = 0.3,
                  max.overlaps=100)+
  xlab("") + ylab("") + 
  theme(legend.position = "none") 


ggsave(filename = "map_SC.png", width = 12,
       height = 6,dpi=300) 
