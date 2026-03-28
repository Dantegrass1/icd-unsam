library(nycflights13)

library(tidyverse)

?flights

data <- flights

length(data)

nrow(data)

glimpse(data)

colnames(data)

view(data$carrier)

unique(data['carrier'])

unique(select(data, carrier))

unique(data$carrier)

data %>%
  count(carrier) %>%
  filter(n > 1000)

data %>%
  filter(dest == "LAX") %>%
  group_by(carrier) %>%
  summarise(n = n()) %>%
  filter(n > 1000)

data %>%
  filter(carrier %in% c("AA", "DL", "UA", "VX", "B6")) %>%  # las aerolíneas que filtraste antes
  ggplot(aes(x = carrier, y = dep_delay)) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.03) +
  geom_boxplot(alpha = 0.5) +
  ylim(-30, 300) +                    # ajustá estos límites según lo que veas
  labs(
    title = "Retraso en salidas por aerolínea",
    x = "Aerolínea",
    y = "Retraso en salida (minutos)"
  )



data %>%
  filter(carrier %in% c("AA", "DL", "UA", "B6", "VX")) %>%
  filter(dest == "LAX") %>%
  group_by(carrier) %>%
  summarise(
    media = mean(dep_delay, na.rm = TRUE),
    mediana = median(dep_delay, na.rm = TRUE),
    sd = sd(dep_delay, na.rm = TRUE),
    iqr = IQR(dep_delay, na.rm = TRUE),
    q95 = quantile(dep_delay, 0.95, na.rm = TRUE),
    max = max(dep_delay, na.rm = TRUE)
  )




top2_carriers <- data %>%
  group_by(carrier) %>%
  filter(n() > 1000, dest == "LAX") %>%
  summarise(max_delay = max(dep_delay, na.rm = TRUE)) %>%
  slice_min(max_delay, n = 2) %>%
  pull(carrier)

data %>%
  filter(carrier %in% top2_carriers, dest == "LAX") %>%
  ggplot(aes(x = arr_delay, fill = carrier)) +
  geom_histogram(binwidth = 15, position = "identity", alpha = 0.6) +
  labs(
    title = "Distribución de retrasos en llegada a LAX",
    x = "Retraso en llegada (minutos)",
    y = "Cantidad de vuelos",
    fill = "Aerolínea"
  )


data %>%
  filter(carrier %in% top2_carriers, dest == "LAX") %>%
  ggplot(aes(x = arr_delay, fill = carrier, color = carrier)) +
  geom_density(alpha = 0.4) +
  labs(
    title = "Densidad de retrasos en llegada a LAX",
    x = "Retraso en llegada (minutos)",
    y = "Densidad",
    fill = "Aerolínea",
    color = "Aerolínea"
  )