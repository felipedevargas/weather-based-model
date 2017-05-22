library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

teste = read_delim('LONDRINA_2004.csv',
                   delim = ';',
                   col_types = cols(dd = col_date(format = "%Y-%m-%d"))) %>%
  mutate(chcp = cumsum(hcp)) %>%
  select(dd, temp, chuva, ur, dif_adj_lag3, diaFavInf, chcp) %>%
  gather(variable, value, -dd) %>%
  mutate(variable = factor(variable,
                           levels = c('temp', 'chuva', 'ur',
                                      'chcp', 'dif_adj_lag3', 'diaFavInf')))

annDate = teste %>%
  filter(variable %in% c('diaFavInf', 'chcp','dif_adj_lag3')) %>%
  spread(variable, value) %>%
  filter(chcp >= 30, diaFavInf == 1,dif_adj_lag3>=0.4) %>%
  select(dd) %>% as.data.frame

dGraph = teste %>% filter(variable != 'diaFavInf')

varNames = c(
  temp = "Temperature",
  chuva = "Rainfall",
  ur = "Relative\nHumidity",
  chcp = "Inoculum\nPressure",
  dif_adj_lag3 = "Spore\nCloud"
)

p<-ggplot(dGraph,
       aes(x = dd, y = value, ymin = 0, ymax = value)) +
  facet_grid(variable~., scales = "free",
             as.table = FALSE,
             labeller = labeller( variable = varNames, .multi_line = TRUE )) +
  geom_step(data = teste %>%
              filter(variable %in% c( 'chcp'))) +
  #                  geom_hline(yintercept = 30)+
  geom_bar(data = teste %>%
              filter(variable %in% c('chuva')),stat= "identity") +
  geom_line(data = teste %>%
              filter(variable %in% c('ur', 'temp', 'dif_adj_lag3'))) +
  annotate("rect", xmin=annDate$dd, xmax = annDate$dd+1,
           ymin=-Inf, ymax=Inf, alpha=0.2, fill="red") +
  theme_bw() + xlab("Date") + ylab("Values")+
  scale_x_date(date_breaks = "1 week",labels = date_format("%d-%m"))
ggsave(filename = "Figure 3.png",plot=p, width=8, height=10, dpi=600 )
