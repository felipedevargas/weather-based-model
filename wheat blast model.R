hcp<- function(temp,rh) { # verificar
  ifelse(temp >= 15 && temp < 27 && rh > 93,
         1/(14.35-0.25*temp),
         ifelse(temp >= 27 && temp < 35 && rh > 93,1/(-8.5+0.59*temp),0))
}
diasFavInfe2<-function(tMax,rhMax,rain){
  ifelse(rhMax >=93,ifelse(tMax>23,ifelse(rain<=5,1,0),0),0)
  
}
diasFavInfe<-function(tempMax,tempMin,rh){
  difTemp= tempMax-tempMin
  ifelse(tempMax>=23,ifelse(difTemp >=13,ifelse(rh>=70,1,0),0),0)
}
dados$hcp <- round(mapply('hcp', dados$temp, dados$rh), 3)

calc_dif <- function(temp,rh) {
  ifelse(temp >= 15 && temp < 27 && rh > 93, 14.35-0.25*temp,ifelse(temp >= 27 && temp < 35 && rh > 93,-8.5+0.59*temp,0))
}
dados$datetime <- paste(dados$date,dados$time)
wall <<- cbind(datetime2=dados$datetime, dados,  dif = mapply('calc_dif', dados$temp, dados$rh)) %>%
  mutate(dd=as.Date(strptime(as.character(datetime2), format="%Y-%m-%d %H:%M:%S")),
         dif = ifelse(dif > 0, 1/dif, dif),
         doy = as.numeric(strftime(dd, format="%j"))) %>%
  group_by(dd) %>% summarise(  maxTemp = max(temp),
                               minTemp = min(temp),
                               maxUr = max(rh),
                               temp=mean(temp),
                               chuva=sum(as.double(rain)),
                               ur=mean(rh),
                               doy = first(doy),
                               s_dif = sum(dif),
                               hcp = sum(hcp))
wall$diaFavInf<-diasFavInfe(wall$maxTemp,wall$minTemp,wall$ur)

w1516 <<- wall %>% mutate(yr = year(dd),
                          leap = leap_year(dd)) %>%
  group_by(yr) %>%
  mutate(acc_dif=cumsum(s_dif)) %>%
  ungroup %>% select(dd:acc_dif)

roll_dif = function(dif_vector, dif_new, id, discount_factor = NULL) {
  if(is.null(discount_factor)) discount_factor = seq(1, by = -0.1, length.out = 7)
  nmax = length(discount_factor)
  idx = seq(from=(id-1), by=-1, length.out = nmax-1)
  idx = idx[idx > 0]
  if(id == 1) {
    return(dif_vector[id])
  } else if(id > 1  && id <= nmax) {
    y = matrix(discount_factor[1:id], ncol = 1)
  } else {
    y = matrix(discount_factor, ncol = 1)
  }
  x = c(dif_vector[id],dif_new[idx])
  temp = as.numeric(x %*% y) / nmax
  return(temp)
}

#
# Fator de desconto linear 7 dias, step = 0.10
ddisc7 = seq(1, by = -0.1, length.out = 7)

# Fator de desconto linear 3 dias, step = 0.25
ddisc3 <- seq(1, by=-0.25,length.out = 3)


dif16 = w1516$s_dif
dif_new16_lag10 = vector(mode = mode(dif16), length = length(dif16))
dif_new16_lag3 = vector(mode = mode(dif16), length = length(dif16))

sapply(1:length(dif16), function(x) {
  dif_new16_lag10[x] <<- roll_dif(dif16, dif_new16_lag10, x, discount_factor = ddisc7)
  dif_new16_lag3[x] <<- roll_dif(dif16, dif_new16_lag3, x, discount_factor = ddisc3)
})

w1516[['dif_adj_lag10']] = c(dif_new16_lag10)
w1516[['dif_adj_lag3']] = c( dif_new16_lag3)

rm(list = ls(pattern = '^dif'))

w1516 = w1516 %>%
  group_by(yr) %>%
  mutate(acc_dif_adj_lag10 = cumsum(dif_adj_lag10),
         acc_dif_adj_lag3 = cumsum(dif_adj_lag3))
dados$hcp <- round(mapply('hcp', dados$temp, dados$rh), 3)

dados$dd<-as.Date(paste(dados$date,sep=''),"%Y-%m-%d")
# Calculate daily mean TInsterature and acculuted hcp in a given day
days<-ddply(dados,.(dd),summarise,tempMax =max(temp),tempMin = min(temp),
            maxUr=max(rh),minUr=min(rh),temp=mean(temp),chuva=sum(as.double(rain)),
            ur=mean(rh),hcp=sum(hcp))

days$dif_adj_lag3 <- w1516$dif_adj_lag3

days$diaFavInf<-diasFavInfe2(days$tempMax,days$maxUr,days$chuva)
days$chcp<-cumsum(days$hcp)
days<-days[order(days$dd),]
days$tempMax<-NULL
days$tempMin<-NULL
days$hcp<-NULL
days$minUr <-NULL
days$maxUr<-NULL
#days$dif_lag10 <- w1516$dif_adj_lag10
#days$acc_dif <- w1516$acc_dif

teste <- melt(days, id.vars=c("dd"))
annDate = teste %>%
  filter(variable %in% c('diaFavInf', 'chcp','dif_adj_lag3')) %>%
  spread(variable, value) %>%
  filter(chcp >= chcpp, diaFavInf == 1,dif_adj_lag3>=0.2) %>%
  select(dd) %>% as.data.frame

dGraph = teste %>% filter(variable != 'diaFavInf')

varNames = c(
  temp = "Temperature",
  chuva = "Rainfall",
  ur = "Relative Humidity",
  chcp = "Inoculum Potential",
  dif_adj_lag3 = "Spores"
)

p<-ggplot(dGraph,
          aes(x = dd, y = value, ymin = 0, ymax = value)) +
  facet_grid(variable~., scales = "free",
             as.table = FALSE,
             labeller = labeller( variable = varNames, .multi_line = TRUE )) +
  geom_step(data = teste %>%
              filter(variable %in% c( 'chcp'))) +
  geom_bar(data = teste %>%
             filter(variable %in% c('chuva')),stat= "identity") +
  geom_line(data = teste %>%
              filter(variable %in% c('ur', 'temp', 'dif_adj_lag3'))) +
  theme_bw() +theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Date") + ylab("")+
  scale_x_date(date_breaks = "2 day",labels = date_format("%d-%b"),position="bottom")+
  ggtitle(lista$weatherStation$name)


p + annotate("rect", xmin=annDate$dd, xmax = annDate$dd+1,
               ymin=-Inf, ymax=Inf, alpha=0.2, fill="red")

p