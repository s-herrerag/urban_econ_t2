# Libraries ---------------------------------------------------------------
require(pacman)
p_load(tidyverse, sf, units, viridis, scales, modelsummary)

# Load and inspect data ------------------------------------------------------------

#Prices
dataTaller2 <- readRDS("dataTaller2.Rds") #crs not available, assuming WGS84

dataTaller2_sf <- dataTaller2 %>%
  st_as_sf(coords = c("lon", "lat"), crs=4326) %>%
  st_transform(crs=3116) #Longitudinal CRS for Bogotá

#MGN (Marco Geoestadístico Nacional)
mgn_2021_mpio <- st_read("MGN/MGN_MPIO_POLITICO") %>%
  st_transform(crs = 3116) %>%
  dplyr::select(c("MPIO_CNMBR", "MPIO_CDPMP", "geometry")) %>%
  rename(c("COD_MPIO"="MPIO_CDPMP"))

mgn_2021_manzana <- st_read("MGN/MGN_URB_MANZANA") %>%
  st_transform(crs = 3116) %>%
  dplyr::select(c("COD_MPIO", "COD_DANE", "SHAPE_Area", "geometry")) %>%
  rename(c("COD_DANE_ANM"="COD_DANE"))

#Censo Nacional
censo_medellin_mgn <- read.csv("Censo/05Antioquia/05_Antioquia_CSV/CNPV2018_MGN_A2_05.CSV", 
                              colClasses=c("COD_ENCUESTAS"="character", "U_VIVIENDA"="character","COD_DANE_ANM"="character")) %>%
  dplyr::select(c("COD_ENCUESTAS", "U_VIVIENDA", "COD_DANE_ANM")) %>%
  unite("LLAVE_VIV",c("COD_ENCUESTAS", "U_VIVIENDA"), sep = "")

censo_medellin_hogares <- read.csv("Censo/05Antioquia/05_Antioquia_CSV/CNPV2018_2HOG_A2_05.CSV", 
                                      colClasses=c("COD_ENCUESTAS"="character", "U_VIVIENDA"="character")) %>%
  unite("LLAVE_VIV",c("COD_ENCUESTAS", "U_VIVIENDA"), sep = "") 
                      

censo_bogota_mgn <- read.csv("Censo/11Bogota/11_Bogota_CSV/CNPV2018_MGN_A2_11.CSV", 
                             colClasses=c("COD_ENCUESTAS"="character", "U_VIVIENDA"="character", "COD_DANE_ANM"="character")) %>%
  dplyr::select(c("COD_ENCUESTAS", "U_VIVIENDA", "COD_DANE_ANM")) %>%
  unite("LLAVE_VIV",c("COD_ENCUESTAS", "U_VIVIENDA"), sep = "")


censo_bogota_hogares <- read.csv("Censo/11Bogota/11_Bogota_CSV/CNPV2018_2HOG_A2_11.CSV", 
                                   colClasses=c("COD_ENCUESTAS"="character", "U_VIVIENDA"="character")) %>%
  unite("LLAVE_VIV",c("COD_ENCUESTAS", "U_VIVIENDA"), sep = "") 


# Clean, join and define final datasets for analysis ----------------------------

#Densities for Bogotá and Medellín:
poblacion_bogota_manzanas <- censo_bogota_hogares %>%
  group_by(LLAVE_VIV) %>%
  summarise(personas_vivienda=sum(HA_TOT_PER, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(censo_bogota_mgn) %>%
  distinct(LLAVE_VIV, .keep_all=TRUE) %>%
  group_by(COD_DANE_ANM) %>%
  summarise(personas_manzana=sum(personas_vivienda, na.rm = T)) %>%
  ungroup %>%
  left_join(mgn_2021_manzana) %>%
  left_join(dplyr::select(as.data.frame(mgn_2021_mpio), -geometry)) %>%
  subset(COD_MPIO=="11001") %>%
  st_as_sf(crs = 3116) %>%
  mutate(area_km2=set_units(st_area(geometry), "km2")) %>%
  mutate(densidad=as.numeric(personas_manzana/area_km2)) %>%
  dplyr::select(c(COD_MPIO, MPIO_CNMBR, COD_DANE_ANM, area_km2, personas_manzana, densidad, geometry)) %>%
  drop_na()


poblacion_medellin_manzanas <- censo_medellin_hogares %>%
  group_by(LLAVE_VIV) %>%
  summarise(personas_vivienda=sum(HA_TOT_PER, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(censo_medellin_mgn) %>%
  distinct(LLAVE_VIV, .keep_all=TRUE) %>%
  group_by(COD_DANE_ANM) %>%
  summarise(personas_manzana=sum(personas_vivienda, na.rm = T)) %>%
  ungroup %>%
  left_join(mgn_2021_manzana) %>%
  left_join(dplyr::select(as.data.frame(mgn_2021_mpio), -geometry)) %>%
  subset(COD_MPIO=="05001") %>%
  st_as_sf(crs = 3116) %>%
  mutate(area_km2=set_units(st_area(geometry), "km2")) %>%
  mutate(densidad=as.numeric(personas_manzana/area_km2)) %>%
  dplyr::select(c(COD_MPIO, MPIO_CNMBR, COD_DANE_ANM, area_km2, personas_manzana, densidad, geometry)) %>%
  drop_na()

poblacion_manzanas <- rbind(poblacion_bogota_manzanas, poblacion_medellin_manzanas)

#Spatial joins dataTaller2 (verifies that all points are inside the cities)

dataTaller2_manzana <- st_join(dataTaller2_sf, mgn_2021_manzana, 
                                left = TRUE,
                                join = st_nearest_feature) %>%
  subset(COD_MPIO%in%c("05001", "11001")) %>%
  mutate(price_m2=price/surface_covered) #Price by sq. meter

dataTaller2_bogota <- dataTaller2_manzana %>%
  subset(COD_MPIO=="11001") 

dataTaller2_medellin <- dataTaller2_manzana %>%
  subset(COD_MPIO=="05001")

#Data with the number of properties by square

manzanas_bogota <- mgn_2021_manzana %>%
  subset(COD_MPIO=="11001")

manzanas_medellin <- mgn_2021_manzana %>%
  subset(COD_MPIO=="05001")

n_propiedades_bogota <- dim(dataTaller2_bogota)[1]
n_propiedades_medellin<- dim(dataTaller2_medellin)[1]
  
proporcion_viviendas_bogota <- full_join(manzanas_bogota, 
                                         dplyr::select(as.data.frame(dataTaller2_bogota), 
                                                -c(SHAPE_Area, geometry))) %>%
  mutate(propiedad=ifelse(is.na(title),0,1)) %>%
  group_by(COD_DANE_ANM) %>%
  summarise(n_viviendas=sum(propiedad)) %>%
  mutate(proporcion_propiedades=n_viviendas*100/n_propiedades_bogota) %>%
  st_as_sf(crs=3116)

proporcion_viviendas_medellin <- full_join(manzanas_medellin, 
                                         dplyr::select(as.data.frame(dataTaller2_medellin), 
                                                -c(SHAPE_Area, geometry))) %>%
  mutate(propiedad=ifelse(is.na(title),0,1)) %>%
  group_by(COD_DANE_ANM) %>%
  summarise(n_viviendas=sum(propiedad)) %>%
  mutate(proporcion_propiedades=n_viviendas*100/n_propiedades_medellin) %>%
  st_as_sf(crs=3116)

# 1) Descriptive statistics -----------------------------------------------

#Density: Tables

manzanas_bog_info <- left_join(as.data.frame(proporcion_viviendas_bogota), 
                               as.data.frame(poblacion_bogota_manzanas), 
                               by="COD_DANE_ANM") %>%
  mutate(area_m2=as.numeric(area_km2)*10^6) %>%
  as.data.frame()

manzanas_med_info <- left_join(as.data.frame(proporcion_viviendas_medellin), 
                               as.data.frame(poblacion_medellin_manzanas), 
                               by="COD_DANE_ANM") %>%
  mutate(area_m2=as.numeric(area_km2)*10^6) %>%
  as.data.frame()

manzana_formula <- formula((`Área m2`= area_m2)+(Población=personas_manzana)+(`Densidad (hab/km2)` = densidad) ~ 
                             N+(Min+Max)*Arguments(na.rm = TRUE)+(`Perc 25`=P25)+(Mediana=P50)+(`Perc 75`=P75)+((Media=mean)+SD)*Arguments(na.rm = TRUE))

tab_densidad_bog <- datasummary(data=manzanas_bog_info,
                                formula=manzana_formula, 
                                output = "Tables/descriptivas_manzana_bog.tex")

tab_densidad_med <- datasummary(data=manzanas_med_info,
                                formula=manzana_formula,
                                output = "Tables/descriptivas_manzana_med.tex")

#Prices: Tables
price_formula <- formula((Operación=operation)*((`Precio m2`=price_m2)+(Habitaciones=rooms)+(Dormitorios=bedrooms)+(Baños=bathrooms)+(`Superficie m2`=surface_covered))~
                    N+(Min+Max)*Arguments(na.rm = TRUE)+(`Perc 25`=P25)+(Mediana=P50)+(`Perc 75`=P75)+((Media=mean)+SD)*Arguments(na.rm = TRUE))

tab_price_bog <- datasummary(data = dataTaller2_bogota,
                             formula=price_formula,
                             output = "Tables/descriptivas_precios_bog.tex")
tab_price_med <- datasummary(data = dataTaller2_medellin,
                             formula=price_formula,
                             output = "Tables/descriptivas_precios_med.tex")


#Density: Plots

densidad_boxplot<- ggplot(poblacion_manzanas) +
  geom_boxplot(aes(y=densidad, fill=COD_MPIO, x=COD_MPIO)) +
  ylim(c(NA, 300000)) +
  ylab("Densidad") +
  xlab("Ciudad (Código DANE)") +
  labs(fill="") +
  scale_fill_manual(values=c("#6980f0", "#f0e269"), labels=c("Medellín", "Bogotá")) +
  theme_bw()

ggsave("Graphs/densidad_boxplot.png", densidad_boxplot, 
       width = 15, height = 15, units = "cm", dpi = 500)

#Price: Plots
price_boxplot_venta <- ggplot(subset(dataTaller2_manzana, operation=="Venta")) +
  geom_boxplot(aes(y=price_m2, fill=COD_MPIO, x=COD_MPIO)) +
  ylab("Precio por m2") +
  xlab("Ciudad (Código DANE)") +
  labs(fill="") +
  scale_fill_manual(values=c("#6980f0", "#f0e269"), labels=c("Medellín", "Bogotá")) +
  ylim(c(NA,2*10^7)) +
  theme_bw()

ggsave("Graphs/venta_boxplot.png", price_boxplot_venta, 
       width = 15, height = 15, units = "cm", dpi = 500)

price_boxplot_Alquiler <- ggplot(subset(dataTaller2_manzana, operation=="Alquiler")) +
  geom_boxplot(aes(y=price_m2, fill=COD_MPIO, x=COD_MPIO)) +
  ylab("Precio por m2") +
  xlab("Ciudad (Código DANE)") +
  labs(fill="") +
  scale_fill_manual(values=c("#6980f0", "#f0e269"), labels=c("Medellín", "Bogotá")) +
  ylim(c(NA,150000)) +
  theme_bw()

ggsave("Graphs/Alquiler_boxplot.png", price_boxplot_Alquiler, 
       width = 15, height = 15, units = "cm", dpi = 500)


# 2) Maps -----------------------------------------------------------------

#Data of city centers (extracted from Google Earth)
cbd_bogota <- st_read("Centros/cib_centroid.gpkg") %>%
  st_transform(crs=3116)
cbd_medellin <- st_read("Centros/med_plaza_botero.gpkg") %>%
  st_transform(crs=3116)

# 2.a) Proportion of properties by square: 
#Bogotá
propiedades_manzana_bogota <- ggplot(proporcion_viviendas_bogota) +
  geom_sf(aes(fill = proporcion_propiedades), color=NA) +
  geom_sf(data=cbd_bogota, color="black") +
  geom_sf_text(data=cbd_bogota, aes(label=Name), size=3, nudge_y = -500) +
  ylim(c(986500, NA)) +
  xlim(c(980000,NA)) +
  scale_fill_viridis(option = "rocket", direction = -1, limits=c(0.001,NA)) +
  labs(fill="Propiedades\nen la manzana (%)", y="", x="") +
  theme_bw()

ggsave("Graphs/propiedades_manzana_bogota.png", propiedades_manzana_bogota, 
       width = 20, height = 15, units = "cm", dpi = 500)

#Medellín
propiedades_manzana_medellin <- ggplot(proporcion_viviendas_medellin) +
  geom_sf(aes(fill = proporcion_propiedades), color=NA) +
  geom_sf(data=cbd_medellin, color="black") +
  geom_sf_text(data=cbd_medellin, aes(label=Name), size=3, nudge_y = -500) +
  scale_fill_viridis(option = "rocket", direction = -1, limits=c(0.001, NA)) +
  labs(fill="Propiedades\nen la manzana (%)", y="", x="") +
  theme_bw()

ggsave("Graphs/propiedades_manzana_medellin.png", propiedades_manzana_medellin, 
       width = 20, height = 15, units = "cm", dpi = 500)

# 2.b) Densities by square 
density_map_bogota <- ggplot(poblacion_bogota_manzanas) +
  geom_sf(aes(fill = densidad), color=NA) +
  geom_sf(data=cbd_bogota, color="black") +
  geom_sf_text(data=cbd_bogota, aes(label=Name), size=3, nudge_y = -500) +
  ylim(c(986500, NA)) +
  xlim(c(980000,NA)) +
  scale_fill_viridis(option = "inferno", direction = -1, labels=scales::number, limits = c(NA, quantile(poblacion_bogota_manzanas$densidad,.98))) +
  labs(fill="Densidad\n(habitantes/km2)", y="", x="") +
  theme_bw()

ggsave("Graphs/densidad_manzana_bogota.png", density_map_bogota, 
       width = 20, height = 15, units = "cm", dpi = 500)

density_map_medellin <- ggplot(poblacion_medellin_manzanas) +
  geom_sf(aes(fill = densidad), color=NA) +
  geom_sf(data=cbd_medellin, color="black") +
  geom_sf_text(data=cbd_medellin, aes(label=Name), size=3, nudge_y = -500) +
  scale_fill_viridis(option = "inferno", direction = -1, labels=scales::number, limits = c(NA, quantile(poblacion_medellin_manzanas$densidad,.98))) +
  labs(fill="Densidad\n(habitantes/km2)", y="", x="") +
  theme_bw()

ggsave("Graphs/densidad_manzana_medellin.png", density_map_medellin, 
         width = 20, height = 15, units = "cm", dpi = 500)

# 3) Gradients ---------------------------------------------------------------

#In order to estimate gradients, we must first find the distances (we can also create variables for logs)

#Density data:
outliers_bogota_densidad <- quantile(poblacion_bogota_manzanas$densidad,.98)
outliers_bogota_distancia <- quantile(poblacion_bogota_manzanas$distance_cbd_km,.98)


poblacion_bogota_manzanas <- poblacion_bogota_manzanas %>%
  mutate(distance_cbd_km=as.numeric(set_units(st_distance(geometry, cbd_bogota), "km"))) %>%
  mutate(across(c(densidad, distance_cbd_km), 
                .fns=list(log = log), 
                .names="{.fn}_{.col}")) %>%
  filter(densidad<=outliers_bogota_densidad) 

outliers_bogota_distancia <- quantile(poblacion_bogota_manzanas$distance_cbd_km,.98)
poblacion_bogota_manzanas <- poblacion_bogota_manzanas%>%
  filter(distance_cbd_km<=outliers_bogota_distancia)

outliers_medellin_densidad <- quantile(poblacion_medellin_manzanas$densidad,.98)
outliers_medellin_distancia <- quantile(poblacion_medellin_manzanas$distance_cbd_km,.98)

poblacion_medellin_manzanas <- poblacion_medellin_manzanas %>%
  mutate(distance_cbd_km=as.numeric(set_units(st_distance(geometry, cbd_medellin), "km"))) %>%
  mutate(across(c(densidad, distance_cbd_km), 
                .fns=list(log = log), 
                .names="{.fn}_{.col}"))  %>%
  filter(densidad<=outliers_medellin_densidad) 

outliers_medellin_distancia <- quantile(poblacion_medellin_manzanas$distance_cbd_km,.98)
poblacion_medellin_manzanas <- poblacion_medellin_manzanas %>%
  filter(distance_cbd_km<=outliers_medellin_distancia)

#Price data:
outliers_bogota_precio_Alquiler <-quantile(subset(dataTaller2_bogota, operation=="Alquiler")$price_m2, .98)
outliers_bogota_precio_venta <-quantile(subset(dataTaller2_bogota, operation=="Venta")$price_m2, .98)

dataTaller2_bogota <- dataTaller2_bogota %>%
  mutate(distance_cbd_km=as.numeric(set_units(st_distance(geometry, cbd_bogota), "km"))) %>%
  mutate(across(c(price_m2, distance_cbd_km), 
         .fns=list(log = log), 
         .names="{.fn}_{.col}")) %>%
  filter(price_m2>0) %>%
  filter(ifelse(operation=="Venta",
                price_m2<=outliers_bogota_precio_venta,
                price_m2<=outliers_bogota_precio_Alquiler))%>%
  filter(distance_cbd_km<=outliers_bogota_distancia)


outliers_medellin_precio_Alquiler <-quantile(subset(dataTaller2_medellin, operation=="Alquiler")$price_m2, .98)
outliers_medellin_precio_venta <-quantile(subset(dataTaller2_medellin, operation=="Venta")$price_m2, .98)

dataTaller2_medellin <- dataTaller2_medellin %>%
  mutate(distance_cbd_km=as.numeric(set_units(st_distance(geometry, cbd_medellin), "km"))) %>%
  mutate(across(c(price_m2, distance_cbd_km), 
                .fns=list(log = log), 
                .names="{.fn}_{.col}")) %>%
  filter(price_m2>0) %>%
  filter(ifelse(operation=="Venta",
                price_m2<=outliers_bogota_precio_venta,
                price_m2<=outliers_bogota_precio_Alquiler)) %>%
  filter(distance_cbd_km<=outliers_medellin_distancia)
  
### Estimates:

###Functional form

#Density
png("Graphs/boxcox_bog_densidad.png", width = 365, height = 325, units='mm', res = 300)
boxcox(poblacion_bogota_manzanas$densidad ~ poblacion_bogota_manzanas$distance_cbd_km)
dev.off()

png("Graphs/boxcox_med_densidad.png", width = 365, height = 325, units='mm', res = 300)
boxcox(poblacion_medellin_manzanas$densidad ~ poblacion_medellin_manzanas$distance_cbd_km)
dev.off()

#Price
png("Graphs/boxcox_bog_precio_a.png", width = 365, height = 325, units='mm', res = 300)
boxcox(subset(dataTaller2_bogota, operation=="Alquiler")$price_m2 ~ subset(dataTaller2_bogota, operation=="Alquiler")$distance_cbd_km)
dev.off()

png("Graphs/boxcox_bog_precio_v.png", width = 365, height = 325, units='mm', res = 300)
boxcox(subset(dataTaller2_bogota, operation=="Venta")$price_m2 ~ subset(dataTaller2_bogota, operation=="Venta")$distance_cbd_km)
dev.off()

png("Graphs/boxcox_med_precio_v.png", width = 365, height = 325, units='mm', res = 300)
boxcox(subset(dataTaller2_medellin, operation=="Venta")$price_m2 ~ subset(dataTaller2_medellin, operation=="Venta")$distance_cbd_km)
dev.off()

png("Graphs/boxcox_med_precio_a.png", width = 365, height = 325, units='mm', res = 300) #Save
boxcox(subset(dataTaller2_medellin, operation=="Alquiler")$price_m2 ~ subset(dataTaller2_medellin, operation=="Alquiler")$distance_cbd_km)
dev.off() 

##Prices (Alquiler): 1/3
##Prices (Venta): 1/2
## Densidad: 1/2

BCTransform <- function(y, lambda=0) {
  if (lambda == 0L) { log(y) }
  else { (y^lambda - 1) / lambda }
}

vectorBCTransform <- Vectorize(BCTransform)

#Apply
poblacion_medellin_manzanas<-poblacion_medellin_manzanas %>%
  mutate(trans_densidad=vectorBCTransform(densidad, 0.5))

poblacion_bogota_manzanas<-poblacion_bogota_manzanas %>%
  mutate(sqrt_densidad=sqrt(densidad)) %>%
  mutate(trans_densidad=vectorBCTransform(densidad, 0.5))

dataTaller2_bogota <- dataTaller2_bogota %>%
  mutate(trans_precio_05=vectorBCTransform(price_m2, 0.5)) %>%
  mutate(trans_precio_03=vectorBCTransform(price_m2, 0.33))

dataTaller2_medellin <- dataTaller2_medellin %>%
  mutate(trans_precio_05=vectorBCTransform(price_m2, 0.5)) %>%
  mutate(trans_precio_03=vectorBCTransform(price_m2, 0.33))

#Density:
models_lin_densidad_bogota <- poblacion_bogota_manzanas %>%
  lm(formula = densidad~distance_cbd_km)

models_log_densidad_bogota <- poblacion_bogota_manzanas %>%
  lm(formula = log_densidad~log_distance_cbd_km)

models_lin_densidad_medellin <- poblacion_medellin_manzanas %>%
  lm(formula = densidad~distance_cbd_km)

models_log_densidad_medellin <- poblacion_medellin_manzanas %>%
  lm(formula = log_densidad~log_distance_cbd_km)

#Price:

models_lin_price_bogota <- dataTaller2_bogota %>%
  group_by(operation) %>%
  do(model=lm(formula = price_m2~distance_cbd_km, na.action = na.omit, data = .))

models_log_price_bogota <- dataTaller2_bogota %>%
  group_by(operation) %>%
  do(model=lm(formula = log_price_m2~log_distance_cbd_km, na.action = na.omit, data = .))

models_lin_price_medellin <- dataTaller2_medellin %>%
  group_by(operation) %>%
  do(model=lm(formula = price_m2~distance_cbd_km, na.action = na.omit, data = .))

models_log_price_medellin <- dataTaller2_medellin %>%
  group_by(operation) %>%
  do(model=lm(formula = log_price_m2~log_distance_cbd_km, na.action = na.omit, data = .))

#Definitive models

model_trans_densidad_bogota <- poblacion_bogota_manzanas %>%
  lm(formula = trans_densidad~distance_cbd_km)
model_trans_densidad_medellin <- poblacion_medellin_manzanas %>%
  lm(formula = trans_densidad~distance_cbd_km)

model_trans_precio_a_bog <- dataTaller2_bogota %>%
  subset(operation=="Alquiler") %>%
  lm(formula = trans_precio_03~distance_cbd_km)

model_trans_precio_v_bog <- dataTaller2_bogota %>%
  subset(operation=="Venta") %>%
  lm(formula = trans_precio_05~distance_cbd_km)

model_trans_precio_a_med <- dataTaller2_medellin %>%
  subset(operation=="Alquiler") %>%
  lm(formula = trans_precio_03~distance_cbd_km)

model_trans_precio_v_med <- dataTaller2_medellin %>%
  subset(operation=="Venta") %>%
  lm(formula = trans_precio_05~distance_cbd_km)


##Outputs from the models
coefs <- c("distance_cbd_km" = "Distancia CBD (km)","(Intercept)" = "Constante")
gm <- tibble::tribble(
  ~raw,        ~clean,          ~fmt,
  "nobs",      "N",             0,
  "r.squared", "R2",            2,
  "f", "        F",             2)

models_bog<-list()
models_bog[["Densidad poblacional transformada (hab/km2)"]] <- model_trans_densidad_bogota
models_bog[["Precio de alquiler transformado (COP/m2)"]] <- model_trans_precio_a_bog
models_bog[["Precio de venta transformado (COP/m2)"]] <- model_trans_precio_v_bog

models_med<-list()
models_med[["Densidad poblacional transformada (hab/km2)"]] <- model_trans_densidad_medellin
models_med[["Precio de alquiler transformado (COP/m2)"]] <- model_trans_precio_a_med
models_med[["Precio de venta transformado (COP/m2)"]] <- model_trans_precio_v_med


modelsummary(models_bog,
             stars = TRUE,
             coef_map = coefs,
             gof_map = gm,
             output = "Tables/modelos_bogota.tex")

modelsummary(models_med,
             stars = TRUE,
             coef_map = coefs,
             gof_map = gm,
             output = "Tables/modelos_medellin.tex")


##Elasticities
elast_boxcox <- function(x, y, coef, lambda){
  elast<-(coef*mean(x, na.rm=T))/(mean(y)^(lambda))
  elast
}

elast_bog_densidad<-elast_boxcox(poblacion_bogota_manzanas$distance_cbd_km,
                                 poblacion_bogota_manzanas$densidad,
                                 model_trans_densidad_bogota$coefficients[2],
                                 0.5)

elast_med_densidad<-elast_boxcox(poblacion_medellin_manzanas$distance_cbd_km,
                                 poblacion_medellin_manzanas$densidad,
                                 model_trans_densidad_medellin$coefficients[2],
                                 0.5)

elast_bog_precio_a <- elast_boxcox(subset(dataTaller2_bogota, operation=="Alquiler")$distance_cbd_km,
                                   subset(dataTaller2_bogota, operation=="Alquiler")$price_m2,
                                   model_trans_precio_a_bog$coefficients[2],
                                   0.5)

elast_bog_precio_v <- elast_boxcox(subset(dataTaller2_bogota, operation=="Venta")$distance_cbd_km,
                                   subset(dataTaller2_bogota, operation=="Venta")$price_m2,
                                   model_trans_precio_v_bog$coefficients[2],
                                   0.5)

elast_med_precio_a <- elast_boxcox(subset(dataTaller2_medellin, operation=="Alquiler")$distance_cbd_km,
                                   subset(dataTaller2_medellin, operation=="Alquiler")$price_m2,
                                   model_trans_precio_a_med$coefficients[2],
                                   0.5)

elast_med_precio_v <- elast_boxcox(subset(dataTaller2_medellin, operation=="Venta")$distance_cbd_km,
                                   subset(dataTaller2_medellin, operation=="Venta")$price_m2,
                                   model_trans_precio_v_med$coefficients[2],
                                   0.5)

elast_df<- data.frame("Ciudad"=c("Bogotá", "Medellín"), 
           "Elasticidad densidad vs. distancia"=c(round(elast_bog_densidad,2), round(elast_med_densidad,2)),
           "Elasticidad precio Alquiler vs. distancia"=c(round(elast_bog_precio_a,2), round(elast_med_precio_a,2)),
           "Elasticidad precio venta vs. distancia"=c(round(elast_bog_precio_v,2), round(elast_med_precio_v,2)))

datasummary_df(elast_df,
               output = "Tables/Elasticidades densidades.tex")

### Plots (log):
trans_bogota_densidad_plot <- ggplot(poblacion_bogota_manzanas, aes(x=distance_cbd_km, y=trans_densidad)) +
  geom_point(alpha=0.2) +
  geom_smooth(method = "lm", se=TRUE) +
  ylab("Densidad Poblacional (Transformada)") +
  xlab("Distancia CBD (km)") +
  theme_bw()

ggsave("Graphs/bogota_modelo_densidad.png", trans_bogota_densidad_plot, 
       width = 15, height = 15, units = "cm", dpi = 500)

trans_medellin_densidad_plot <- ggplot(poblacion_medellin_manzanas, aes(x=distance_cbd_km, y=trans_densidad)) +
  geom_point(alpha=0.2) +
  geom_smooth(method = "lm", se=TRUE) +
  ylab("Densidad Poblacional (Transformada)") +
  xlab("Distancia CBD (km)") +
  theme_bw()

ggsave("Graphs/medellin_modelo_densidad.png", trans_medellin_densidad_plot, 
       width = 15, height = 15, units = "cm", dpi = 500)

trans_bogota_precio_a_plot <- ggplot(subset(dataTaller2_bogota, operation=="Alquiler"), 
                                     aes(x=distance_cbd_km, y=trans_precio_03)) +
  geom_point(alpha=0.2) +
  geom_smooth(method = "lm", se=TRUE) +
  ylab("Precio por m2 de alquiler (Transformado)") +
  xlab("Distancia CBD (km)") +
  theme_bw()

ggsave("Graphs/bogota_modelo_precio_a.png", trans_bogota_precio_a_plot, 
       width = 15, height = 15, units = "cm", dpi = 500)

trans_bogota_precio_v_plot <- ggplot(subset(dataTaller2_bogota, operation=="Venta"), 
                                     aes(x=distance_cbd_km, y=trans_precio_05)) +
  geom_point(alpha=0.2) +
  geom_smooth(method = "lm", se=TRUE) +
  ylab("Precio por m2 de venta (Transformado)") +
  xlab("Distancia CBD (km)") +
  theme_bw()

ggsave("Graphs/bogota_modelo_precio_v.png", trans_bogota_precio_v_plot, 
       width = 15, height = 15, units = "cm", dpi = 500)

trans_medellin_precio_a_plot <- ggplot(subset(dataTaller2_medellin, operation=="Alquiler"), 
                                     aes(x=distance_cbd_km, y=trans_precio_03)) +
  geom_point(alpha=0.2) +
  geom_smooth(method = "lm", se=TRUE) +
  ylab("Precio por m2 de alquiler (Transformado)") +
  xlab("Distancia CBD (km)") +
  theme_bw()

ggsave("Graphs/medellin_modelo_precio_a.png", trans_medellin_precio_a_plot, 
       width = 15, height = 15, units = "cm", dpi = 500)

trans_medellin_precio_v_plot <- ggplot(subset(dataTaller2_medellin, operation=="Venta"), 
                                     aes(x=distance_cbd_km, y=trans_precio_05)) +
  geom_point(alpha=0.2) +
  geom_smooth(method = "lm", se=TRUE) +
  ylab("Precio por m2 de venta (Transformado)") +
  xlab("Distancia CBD (km)") +
  theme_bw()

ggsave("Graphs/medellin_modelo_precio_v.png", trans_medellin_precio_v_plot, 
       width = 15, height = 15, units = "cm", dpi = 500)


log_bogota_densidad_plot <- ggplot(poblacion_bogota_manzanas, aes(x=log_distance_cbd_km, y=log_densidad)) +
  geom_point(alpha=0.2) +
  geom_smooth(method = "lm", se=TRUE) +
  ylab("Log Densidad Poblacional") +
  xlab("Log Distancia CBD (km)") +
  theme_bw()

ggsave("Graphs/log_bogota_densidad_ols.png", log_bogota_densidad_plot, 
       width = 15, height = 15, units = "cm", dpi = 500)

log_bogota_price_plot <- ggplot(dataTaller2_bogota, aes(x=log_distance_cbd_km, y=log_price_m2, color=operation)) +
  geom_point(alpha=0.2) +
  geom_smooth(method = "lm", se=TRUE) +
  labs(color="") +
  ylab("Log Precio m2") +
  xlab("Log Distancia CBD (km)") +
  theme_bw()

ggsave("Graphs/log_bogota_precio_ols.png", log_bogota_price_plot, 
       width = 15, height = 15, units = "cm", dpi = 500)

log_medellin_densidad_plot <- ggplot(poblacion_medellin_manzanas, aes(x=log_distance_cbd_km, y=log_densidad)) +
  geom_point(alpha=0.2) +
  geom_smooth(method = "lm", se=TRUE) +
  ylab("Log Densidad Poblacional") +
  xlab("Log Distancia CBD (km)") +
  theme_bw()

ggsave("Graphs/log_medellin_densidad_ols.png", log_medellin_densidad_plot, 
       width = 15, height = 15, units = "cm", dpi = 500)

log_medellin_price_plot <- ggplot(dataTaller2_medellin, aes(x=log_distance_cbd_km, y=log_price_m2, color=operation)) +
  geom_point(alpha=0.2) +
  geom_smooth(method = "lm", se=TRUE) +
  labs(color="") +
  ylab("Log Precio m2") +
  xlab("Log Distancia CBD") +
  theme_bw()

ggsave("Graphs/log_medellin_precio_ols.png", log_medellin_price_plot, 
       width = 15, height = 15, units = "cm", dpi = 500)


  
