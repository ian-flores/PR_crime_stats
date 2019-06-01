library(tidyverse)
library(lubridate)

crimes <- read_csv('type_I_crimes.csv')

crimes <- crimes %>%
    mutate(Fecha = paste0('01 ', Fecha)) %>%
    mutate(date = dmy(Fecha, locale = 'es_PR.UTF-8')) %>%
    select(district = Distrito, crime = variable, number = value, date)

robo_sj <- crimes %>%
    filter(crime == 'Ases.',
           district == 'San Juan') 

robo_sj %>%
    ggplot(aes(x = date, y = number)) +
        geom_line()

library(prophet)

m <- robo_sj %>%
    select(ds = date, y = number) %>%
    prophet()

future <- make_future_dataframe(m, periods = 365)

forecast <- predict(m, future)

plot(forecast)
prophet_plot_components(m, forecast)
