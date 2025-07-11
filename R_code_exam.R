#Evidenze cambiamento territorio a seguito dell'incendio 
#avvenuto in Sardegna nel 21 luglio 2021

#Caricamento pacchetti
library(raster)
library(ggplot2)
library(viridis)
library(patchwork)


#Imposto la cartella di lavoro
setwd("/Users/ari/Documents/Telerilevamento /Sardegna")

#Carico i raster per una prima visualizzazione pre e post incendio
sardegna_pre <- raster ("Sardegna_12_Luglio.jp2")
sardegna_post <- raster ("Sardegna_6_Agosto.jp2")
plot(sardegna_pre)
plot(sardegna_post)

#Importo le bande Sentinel-2 che mi serviranno per l'analisi

# Bande importate Pre-incendio (Luglio)
b_pre_nir   <- raster ( "Sardegna_12_Luglio_NIR.jp2" )
b_pre_red   <- raster ( "Sardegna_12_Luglio_RED.jp2" )
b_pre_green <- raster ( "Sardegna_12_Luglio_GREEN.jp2" )
b_pre_blue  <- raster ( "Sardegna_12_Luglio_BLUE.jp2")

# Bande importate Post-incendio (Agosto)
b_post_nir   <- raster ( "Sardegna_6_Agosto_NIR.jp2" )
b_post_red   <- raster ( "Sardegna_6_Agosto_RED.jp2" )
b_post_green <- raster ( "Sardegna_6_Agosto_GREEN.jp2" )
b_post_blue  <- raster ( "Sardegna_6_Agosto_BLUE.jp2" )


#Creo uno stack per creare le composizioni
#rgb : r=red, g=green, b=blue
stack_pre_rgb  <- stack (b_pre_red,  b_pre_green,   b_pre_blue) #pre
stack_post_rgb <- stack (b_post_red,  b_post_green,  b_post_blue) #post

#Creo composizone False Color,  mettendo nel r = nir, g=red, b=green
stack_pre_ir   <- stack (b_pre_nir,   b_pre_red,     b_pre_green)
stack_post_ir  <- stack (b_post_nir,  b_post_red,    b_post_green)


#Visualizzo i due raster a composizione RGB e False Color 
par(mfrow = c(1, 2))
# composizione RGB pre/ post incendio 
plotRGB(stack_pre_rgb,  r = 1, g = 2, b = 3, stretch = "Lin")
plotRGB(stack_post_rgb, r = 1, g = 2, b = 3, stretch = "Lin")

dev.off()

# composizone false color 
par(mfrow = c(1, 2))
plotRGB(stack_pre_ir,   r = 1, g = 2, b = 3, stretch = "Lin")
plotRGB(stack_post_ir,  r = 1, g = 2, b = 3, stretch = "Lin")

dev.off()



#Ritaglio una zona interessa maggiormente 
ext <- c(460000, 470000, 4435000, 4450000)

#Ritaglio Pre- incendio 
b_pre_nir_crop <- crop (b_pre_nir, ext)
b_pre_red_crop <- crop (b_pre_red, ext)
b_pre_green_crop <- crop (b_pre_green, ext)
#Ritaglio Post- incendio
b_post_nir_crop <- crop (b_post_nir, ext)
b_post_red_crop <- crop (b_post_red, ext)
b_post_green_crop <- crop (b_post_green, ext)

#Faccio lo stack e visualizzo a false color 
stack_pre_crop <- stack  (b_pre_red_crop, b_pre_green_crop, b_pre_nir_crop)
stack_post_crop <- stack (b_post_red_crop, b_post_green_crop, b_post_nir_crop)

plotRGB(stack_pre_crop,r = 3, g = 1, b = 2, stretch= "lin")
plotRGB(stack_post_crop,r = 3, g = 1,b = 2, stretch = "lin")



#Calcolo indice NDVI per l'area interessata
ndvi_pre <- (b_pre_nir_crop - b_pre_red_crop)/ (b_pre_nir_crop + b_pre_red_crop)
ndvi_post <- (b_post_nir_crop - b_post_red_crop)/ (b_post_nir_crop + b_post_red_crop)

#calcolo indice NDWI- contenuto acqua, per vedere se suolo arido o secco 

ndwi_pre <- (b_pre_green_crop- b_pre_nir_crop)/ (b_pre_green_crop+ b_pre_nir_crop)
ndwi_post <- (b_post_green_crop - b_post_nir_crop) / (b_post_green_crop + b_post_nir_crop)


#Definisco la palette di colori che voglio utilizzare 
cl <- colorRampPalette(c('darkred', 'bisque2' , 'blue' , 'green' )) (100)

#Visaulizzo raster con la palette personalizzata 
par(mfrow= c(2,2))
plot ( ndvi_pre,  col= cl, main = "NDVI Pre-Incendio ")
plot ( ndvi_post, col=cl,  main = "NDVI Post-Incendio")
plot ( ndwi_pre,  col=cl,  main = "NDWI Pre-Incendio")
plot( ndwi_post,  col=cl,  main = "NDWI Post-Incendio")

#Visualizzazione con ggplot

#conversione raster in dataframe  

# pre
ndvi_pre_df <- as.data.frame   (ndvi_pre, xy=TRUE)
ndwi_pre_df <- as.data.frame   (ndwi_pre, xy= TRUE)
#post
ndvi_post_df <- as.data.frame (ndvi_post, xy= TRUE)
ndwi_post_df <- as.data.frame (ndwi_post, xy= TRUE)

#Plot 
p1 <- ggplot()+ geom_raster (data = ndvi_pre_df, aes (x =x, y=y, fill=layer)) + scale_fill_viridis(option="magma") + ggtitle("NDVI Pre-Incendio")

p2 <- ggplot() + geom_raster(data=ndwi_pre_df, aes (x=x, y=y, fill= layer)) + scale_fill_viridis (option ="magma") + ggtitle ("NDWI Pre-Incendio")

p1+p2 # ndvi e ndwi pre 

dev.off()

p3 <-ggplot() + geom_raster(data= ndvi_post_df, aes (x=x, y=y, fill= layer)) + scale_fill_viridis (option ="magma") + ggtitle ("NDVI Post-Incendio")

p4 <- ggplot() +geom_raster (data = ndwi_post_df, aes (x = x , y= y, fill= layer)) + scale_fill_viridis(option= "magma") + ggtitle ("NDWI Post-Incendio")



p3+p4 #ndvi e ndwi post 
