library(lubridate)
library(RColorBrewer)
library(imputeTS)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
               "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

sims.all.df <- read.csv('sd_data.csv')

g <- ggplot(data=subset(sims.all.df, year == 2014), 
            aes(x=date, y = proportion, group=stage, fill=stage)) +
  scale_fill_manual(values = cbPalette[c(7, 2, 5, 4, 3, 6, 8)]) +
  geom_density(position="fill", stat = 'identity', lwd = 0.05) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
  facet_wrap(~prov, scales = 'free_x', ncol = 1, 
             labeller = labeller(prov = prov.labs)) +
  labs(y = 'Proportion of Population in Stage', 
       fill = 'Larval Stage', x = 'Date', 
       title = '2014 Spruce Budworm Development by Location') +
  guides(alpha = FALSE)

ggplotly(g, tooltip = c("proportion", "stage")) %>%
  layout(xaxis = list(showspikes = TRUE, spikedash = 'dash', 
                      spikemode = 'across', spikesnap = 'cursor',
                      spikethickness = 1), spikedistance = -1)

