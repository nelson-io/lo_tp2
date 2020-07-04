library(tidyverse)
library(rio)
library(janitor)

#importamos datos
data <- import("Groupon - Restaurant Estilo Campestre.xlsx", skip = 4) %>% clean_names()


#evaluamos main stats
data_stats <- list()
data_stats$consumo_promedio <- mean(data$consumo_promedio_x_mesa)
data_stats$mesas_promedio <- mean(data$mesas_ocupadas)
data_stats$mesas_desvio <- sd(data$mesas_ocupadas)

#Simulamos 10000 obs
set.seed(0)
sim_1 <-
  tibble(
    mesas_ocupadas = rnorm(
      mean = data_stats$mesas_promedio,
      sd = data_stats$mesas_desvio,
      n = 1e4
    )
  ) %>% # fijamos limites en [0:100], y asumimos consumo promedio por mesa
  mutate(mesas_ocupadas = case_when(mesas_ocupadas > 100 ~ 100,
                                    mesas_ocupadas < 0 ~ 0,
                                    T ~ mesas_ocupadas),
  consumo_promedio_mesa = data_stats$consumo_promedio,
  profit = mesas_ocupadas * consumo_promedio_mesa) 

sim_data <- tibble()

for(i in 0:100){
  
  
  
  data_loop <- sim_1 %>% 
    mutate(canibalized = if_else(i > .15*mesas_ocupadas, .15*mesas_ocupadas, as.double(i)),
           mesas_ocupadas = mesas_ocupadas - canibalized,
           mesas_ocupadas_n = pmin(100, mesas_ocupadas + i),
           profit_n = (i * (consumo_promedio_mesa - 750)) + (mesas_ocupadas_n - i)*consumo_promedio_mesa) %>% 
    summarise(profit_est = mean(profit_n)) %>% pull()
  
  new_data <- tibble(groupones = i,
                     profit = data_loop)
  sim_data <- rbind(sim_data, new_data)
  
}




ggplot(sim_data)+
  geom_line(aes(x = groupones, y = profit))+
  theme_bw()+
  ggtitle("Groupones otorgados y profits")
