library(tidyverse)
data <- read_csv('insurance.xls')
view(data)
glimpse(data)
dim(data)

ggplot(data, aes(x = age, y = charges, color = smoker, shape = smoker)) +
  geom_point(size = 3) +
  geom_smooth(method = 'lm') +
  scale_shape_manual(values = c("yes" = 15, "no" = 17)) + 
  ggtitle('Comparison Between Age and Charges (Dante Grassi)') +
  xlab("Age") +
  ylab("Charges")

ggplot(data, aes(x=region, fill=smoker)) +
  geom_bar(position = 'fill') +
  ggtitle('Bars comparasion between smoker per region (Dante Grassi)') +
  xlab("Region") +
  ylab("Is smoker")

filterdata <- filter(data, smoker=='yes')

ggplot(filterdata, aes(x=region, fill=smoker)) +
  geom_bar() +
  ggtitle('Bars comparasion between smokers per region (Dante Grassi)') +
  xlab("Region") +
  ylab("Smokers")
  
nuevodata <- data %>%
  group_by(region) %>%
  count(smoker) %>%
  mutate(porcentaje = n / sum(n))

ggplot(nuevodata, aes(x=region, fill=smoker)) +
  geom_bar(position = 'fill') +
  ggtitle('Bars comparasion between smokers per region (Dante Grassi)') +
  xlab("Region") +
  ylab("Smokers")

ggplot(nuevodata, aes(x = region, y = porcentaje, fill = smoker)) +
  geom_col() + 
  ggtitle('Bars comparison between smokers per region (Dante Grassi)') +
  xlab("Region") +
  ylab("Percentage of smokers")


