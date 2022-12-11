library(tidyverse)
library(party)
library(magrittr)
library(e1071)
library(patchwork)

rm(list = ls())

df_clean_butterfly <- read.csv("Data/Clean/Butterfly.csv",) %>%
  mutate(date = as.Date(date))

options(scipen = 999)



df_butterfly_traits <- df_clean_butterfly %>%
  select(-coreId, -date, -year, -interspotLW, -interspotRW, -latitude, -longitude)

df_butterfly_known <- df_butterfly_traits %>%
  filter((sex == "male" | sex == "female") | (lengthLW == 0 | lengthRW == 0 | widthLW == 0 | widthRW == 0)) %>%
  na.omit()

df_butterfly_unkown <- df_butterfly_traits %>%
  anti_join(df_butterfly_known)


  
df_butterfly_training <- slice_head(df_butterfly_known, prop = 0.75) %>%
  mutate(sex = as.factor(sex))

df_butterfly_training_reduced <- df_butterfly_training %>%
  mutate(wingLength = rowMeans(cbind(lengthLW, lengthRW))) %>%
  mutate(wingWidth = rowMeans(cbind(widthLW, widthRW))) %>%
  mutate(wingApex = rowMeans(cbind(apexLW, apexRW))) %>%
  mutate(posteriorSpot = rowMeans(cbind(posteriorSpotLW, posteriorSpotRW))) %>%
  mutate(anteriorSpot = rowMeans(cbind(anteriorSpotLW, anteriorSpotRW))) %>%
  select(wingLength, wingWidth, wingApex, posteriorSpot, anteriorSpot, sex)

df_butterfly_testing  <- slice_head(df_butterfly_known, prop = -0.75) %>%
  mutate(sex = as.factor(sex))

df_butterfly_testing_reduced <- df_butterfly_testing %>%
  mutate(wingLength = rowMeans(cbind(lengthLW, lengthRW))) %>%
  mutate(wingWidth = rowMeans(cbind(widthLW, widthRW))) %>%
  mutate(wingApex = rowMeans(cbind(apexLW, apexRW))) %>%
  mutate(posteriorSpot = rowMeans(cbind(posteriorSpotLW, posteriorSpotRW))) %>%
  mutate(anteriorSpot = rowMeans(cbind(anteriorSpotLW, anteriorSpotRW))) %>%
  select(wingLength, wingWidth, wingApex, posteriorSpot, anteriorSpot, sex)

# Tree
treeFull <- ctree(sex ~ ., df_butterfly_training, controls = ctree_control(mincriterion = .95, testtype = "MonteCarlo")) %T>%
  plot()

treeReduced <- ctree(sex ~ ., df_butterfly_training_reduced, controls = ctree_control(mincriterion = .95, testtype = "MonteCarlo")) %T>%
  plot()

# Calculate confusion matrices
df_treeFull <- as.data.frame(table(predict(treeFull, newdata = df_butterfly_testing), df_butterfly_testing$sex))

# Calculate confusion matrices
df_treeReduced <- as.data.frame(table(predict(treeReduced, newdata = df_butterfly_testing_reduced), df_butterfly_testing_reduced$sex))

treePlot <- ggplot(df_treeReduced, aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#177244") +
  labs(x = "Reference", y = "Prediction") +
  scale_x_discrete(labels=c("Female", "Male")) +
  scale_y_discrete(labels=c("Female", "Male"))

plot(treePlot)

sexPlot <- ggplot(df_butterfly_known, aes(x = posteriorSpotLW, y = posteriorSpotRW)) +
  geom_point(aes(color = sex))

plot(sexPlot)

# Identify regions ; islands == NA, so they go unused
df_butterfly_coordinates <- df_clean_butterfly %>%
  select(longitude, latitude) %>%
  group_by(longitude, latitude) %>%
  mutate(country = gsub(":.*", "", maps::map.where("world", longitude, latitude))) %>%
  mutate(numberOfObservations = length(longitude))

mapUS <- map_data("state")
mapVancouver <- map_data("world", "canada") %>%
  filter(subregion == "Vancouver Island") %>%
  mutate(group = (group+1 - group) + max(mapUS$group))

mapNorthAmerica <- rbind(mapUS, mapVancouver) %>%
  mutate(order = 1:n())

mapUnitedKingdom <- map_data("world", c("uk", "ireland")) 

df_us <- filter(df_butterfly_coordinates, country == "USA" | (country == "Canada" & latitude < 50)) 

df_uk <- filter(df_butterfly_coordinates, country == "UK" | (country =="Ireland"))

ggplot() +
  scale_x_continuous(limits = c(-140, -48), expand = c(0, 0), labels = NULL, breaks = NULL) +
  scale_y_continuous(limits = c(20, 55), expand = c(0, 0), labels = NULL, breaks = NULL) +
  geom_density2d_filled(data =  df_us, aes(longitude, latitude, color = "red"), contour_var = "ndensity") +
  geom_map(data = mapNorthAmerica, map = mapNorthAmerica, aes(map_id = region), fill = NA, color = "black") +
  geom_point(data = df_us, aes(longitude, latitude, size = numberOfObservations)) +
  theme(panel.grid = element_blank(), panel.border = element_blank())


ggplot() +
  scale_x_continuous(expand = c(-12, 5), labels = NULL, breaks = NULL) +
  scale_y_continuous(expand = c(65, 45), labels = NULL, breaks = NULL) +
  geom_density2d_filled(data =  df_uk, aes(longitude, latitude, color = "red"), contour_var = "density") +
  geom_map(data = mapUnitedKingdom, map = mapUnitedKingdom, aes(map_id = region), fill = NA, color = "black") +
  geom_point(data = df_uk, aes(longitude, latitude, size = numberOfObservations)) +
  theme(panel.grid = element_blank(), panel.border = element_blank())


df_butterfly_wings <- df_butterfly_known %>%
  select(lengthLW, lengthRW, widthLW, widthRW) %>%
  mutate(wingArea = rowMeans(cbind(lengthLW, lengthRW)) * rowMeans(cbind(widthLW, widthRW)))

df_wing_length_scatter <- df_butterfly_traits %>%
  select(lengthLW, lengthRW, sex) %>%
  filter(sex == "male" | sex == "female") 

length_scatterPlot <- ggplot(df_wing_length_scatter, aes(lengthLW, lengthRW, color = sex)) +
  geom_point() +
  labs( title = "Wing Length Sizes") +
  coord_fixed(xlim = c(0,60), ylim = c(0,60))

df_wing_width_scatter <- df_butterfly_traits %>%
  select(widthLW, widthRW, sex) %>%
  filter(sex == "male" | sex == "female")

width_scatterPlot <- ggplot(df_wing_width_scatter, aes(widthLW, widthRW, color = sex)) +
  geom_point() +
  labs(title = "Wing Width Sizes") +
  coord_fixed(xlim = c(0,60), ylim = c(0,60))

df_wing_apex_scatter <- df_butterfly_traits %>%
  select(apexLW, apexRW, sex) %>%
  filter(sex == "male" | sex == "female")

apex_scatterPlot <- ggplot(df_wing_apex_scatter, aes(apexLW, apexRW, color = sex)) +
  geom_point() +
  labs(title = "Apex Sizes") +
  coord_fixed(xlim = c(0,60), ylim = c(0,60))

df_wing_anteriorSpot_scatter <- df_butterfly_traits %>%
  select(anteriorSpotLW, anteriorSpotRW, sex) %>%
  filter(sex == "male" | sex == "female")

anteriorSpot_scatterPlot <- ggplot(df_wing_anteriorSpot_scatter, aes(anteriorSpotLW, anteriorSpotRW, color = sex)) +
  geom_point() +
  labs(title = "Anterior Spot Sizes")

df_wing_posteriorSpot_scatter <- df_butterfly_traits %>%
  select(posteriorSpotLW, posteriorSpotRW, sex) %>%
  filter(sex == "male" | sex == "female")

posteriorSpot_scatterPlot <- ggplot(df_wing_posteriorSpot_scatter, aes(posteriorSpotLW, posteriorSpotRW, color = sex)) +
  geom_point() +
  labs(title = "Posterior Spot Sizes")

plot(length_scatterPlot + width_scatterPlot + apex_scatterPlot +
       anteriorSpot_scatterPlot +
       posteriorSpot_scatterPlot +
       plot_layout(ncol = 3)) +
       plot_annotation("Butterfly Measurements by Gender")















