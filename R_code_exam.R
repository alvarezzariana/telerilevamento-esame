
#Analisi dei cambiamenti del territorio a seguito dell'incendio 
#avvenuto in Sardegna il 21 luglio 2021

#Caricamento dei pacchetti necessari per l'analisi 
library(raster)
library(ggplot2)
library(viridis)
library(patchwork)


#Impostazione cartella di lavoro dove contenenti i dati 
setwd("/Users/ari/Documents/Telerilevamento /Sardegna")

#Caricamento immagini Sentinel2 per una prima visualizzazione 
sardegna_pre <- raster ("Sardegna_12_Luglio.jp2")  #pre-incendio
sardegna_post <- raster ("Sardegna_6_Agosto.jp2") #post-incendio

plot(sardegna_pre)
plot(sardegna_post)


#Importazione delle bande Sentinel-2 che mi serviranno per l'analisi 
#Importazione bande pre incendio
b_pre_nir   <- raster ( "Sardegna_12_Luglio_NIR.jp2" )
b_pre_red   <- raster ( "Sardegna_12_Luglio_RED.jp2" )
b_pre_green <- raster ( "Sardegna_12_Luglio_GREEN.jp2" )
b_pre_blue  <- raster ( "Sardegna_12_Luglio_BLUE.jp2")

# Importazione bande post incendio 
b_post_nir   <- raster ( "Sardegna_6_Agosto_NIR.jp2" )
b_post_red   <- raster ( "Sardegna_6_Agosto_RED.jp2" )
b_post_green <- raster ( "Sardegna_6_Agosto_GREEN.jp2" )
b_post_blue  <- raster ( "Sardegna_6_Agosto_BLUE.jp2" )


#Creazione di stack che contegano le bande necessarie a creare le composizioni: 
# RGB:r= red, g=green , b= blue 
stack_pre_rgb  <- stack (b_pre_red,  b_pre_green,   b_pre_blue) #pre
stack_post_rgb <- stack (b_post_red,  b_post_green,  b_post_blue) #post

#False Color:r = nir, g=red, b=green
#Composizione utile per evidenziare la vegetazione 
stack_pre_ir   <- stack (b_pre_nir,   b_pre_red,     b_pre_green)  
stack_post_ir  <- stack (b_post_nir,  b_post_red,    b_post_green)


#Visualizzazione delle composizioni RGB e False Color 
par(mfrow = c(1, 2))
plotRGB(stack_pre_rgb,  r = 1, g = 2, b = 3, stretch = "Lin")  # RGB pre
plotRGB(stack_post_rgb, r = 1, g = 2, b = 3, stretch = "Lin") #RGB post 

dev.off()

par(mfrow = c(1, 2))
plotRGB(stack_pre_ir,   r = 1, g = 2, b = 3, stretch = "Lin") #False color pre 
plotRGB(stack_post_ir,  r = 1, g = 2, b = 3, stretch = "Lin") #False color post

dev.off()

#Definizione dell'estensione dell'area che voglio ritagliare 
ext <- c(460000, 470000, 4435000, 4450000)

#Ritaglio delle bande necessarie
#Pre-Incendio
b_pre_nir_crop <- crop (b_pre_nir, ext) 
b_pre_red_crop <- crop (b_pre_red, ext)
b_pre_green_crop <- crop (b_pre_green, ext)
#Post- Incendio
b_post_nir_crop <- crop (b_post_nir, ext)
b_post_red_crop <- crop (b_post_red, ext)
b_post_green_crop <- crop (b_post_green, ext)

#Creazione di stack per visualizzazione a False Color 
stack_pre_crop <- stack  (b_pre_red_crop, b_pre_green_crop, b_pre_nir_crop)
stack_post_crop <- stack (b_post_red_crop, b_post_green_crop, b_post_nir_crop)

plotRGB(stack_pre_crop,r = 3, g = 1, b = 2, stretch= "lin")
plotRGB(stack_post_crop,r = 3, g = 1,b = 2, stretch = "lin") 


# Calcolo indice NDVI (Normalized Difference Vegetation Index)
#Indica la salute della vegetazione 
# NDVI = (NIR - RED) / (NIR + RED)
ndvi_pre <- (b_pre_nir_crop - b_pre_red_crop) / (b_pre_nir_crop + b_pre_red_crop)
ndvi_post <- (b_post_nir_crop - b_post_red_crop) / (b_post_nir_crop + b_post_red_crop)

#Calcolo la differenza dell'indice NDVI prima e dopo l'incendio 
ndvi_diff <- ndvi_post - ndvi_pre

# Definizione di una palette di colori 
cl <- colorRampPalette(c('darkred', 'bisque2', 'blue', 'green'))(100)

# Plot degli indici NDVI pre e post 
par(mfrow = c(1, 2))
plot(ndvi_pre, col = cl, main = "NDVI Pre-Incendio")
plot(ndvi_post, col = cl, main = "NDVI Post-Incendio")

dev.off()

plot(ndvi_diff, col = cl, main = "Differenza NDVI (Pre - Post)") # differenza degli indici 


# Conversione dei raster in dataframe per usare ggplot 
ndvi_pre_df <- as.data.frame(ndvi_pre, xy = TRUE)
ndvi_post_df <- as.data.frame(ndvi_post, xy = TRUE)


#Visualizzazione con ggplot
p1 <- ggplot() + geom_raster(data = ndvi_pre_df, aes(x = x, y = y, fill = layer)) +
  scale_fill_viridis(option = "magma") + ggtitle("NDVI Pre-Incendio")   #ndvi pre 

p2 <- ggplot() + geom_raster(data = ndvi_post_df, aes(x = x, y = y, fill = layer)) +
  scale_fill_viridis(option = "magma") + ggtitle("NDVI Post-Incendio") #ndvi post

#plot affiancati 
p1+p2 

