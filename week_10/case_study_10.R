#Installing necessary packages
#install.packages("rasterVis")
#install.packages("ggmap")
#install.packages("ncdf4")


#Loading the packages
library(raster)
library(terra)
library(rasterVis)
library(ggmap)
library(tidyverse)
library(knitr)
library(sf)
library(ncdf4) # to import data from netcdf format

# Create afolder to hold the downloaded data
dir.create("data",showWarnings = F) #create a folder to hold the data

lulc_url="https://github.com/adammwilson/DataScienceData/blob/master/inst/extdata/appeears/MCD12Q1.051_aid0001.nc?raw=true"
lst_url="https://github.com/adammwilson/DataScienceData/blob/master/inst/extdata/appeears/MOD11A2.006_aid0001.nc?raw=true"

# download them
download.file(lulc_url,destfile="data/MCD12Q1.051_aid0001.nc", mode="wb")
download.file(lst_url,destfile="data/MOD11A2.006_aid0001.nc", mode="wb")

#Load data into R
lulc=rast("data/MCD12Q1.051_aid0001.nc",subds="Land_Cover_Type_1")
lst=rast("data/MOD11A2.006_aid0001.nc",subds="LST_Day_1km")

plot(lulc)

lulc=lulc[[13]]
plot(lulc)


Land_Cover_Type_1 = c(
  Water = 0, 
  `Evergreen Needleleaf forest` = 1, 
  `Evergreen Broadleaf forest` = 2,
  `Deciduous Needleleaf forest` = 3, 
  `Deciduous Broadleaf forest` = 4,
  `Mixed forest` = 5, 
  `Closed shrublands` = 6,
  `Open shrublands` = 7,
  `Woody savannas` = 8, 
  Savannas = 9,
  Grasslands = 10,
  `Permanent wetlands` = 11, 
  Croplands = 12,
  `Urban & built-up` = 13,
  `Cropland/Natural vegetation mosaic` = 14, 
  `Snow & ice` = 15,
  `Barren/Sparsely vegetated` = 16, 
  Unclassified = 254,
  NoDataFill = 255)

lcd=data.frame(
  ID=Land_Cover_Type_1,
  landcover=names(Land_Cover_Type_1),
  col=c("#000080","#008000","#00FF00", "#99CC00","#99FF99", "#339966", "#993366", "#FFCC99", 
        "#CCFFCC", "#FFCC00", "#FF9900", "#006699", "#FFFF00", "#FF0000", "#999966", "#FFFFFF", 
        "#808080", "#000000", "#000000"),
  stringsAsFactors = F)
# colors from https://lpdaac.usgs.gov/about/news_archive/modisterra_land_cover_types_yearly_l3_global_005deg_cmg_mod12c1
kable(head(lcd))


# convert to raster (easy)
lulc=as.factor(lulc)

# plot it
gplot(lulc)+
  geom_raster(aes(fill=as.factor(value)))+
  scale_fill_manual(values=setNames(lcd$col,lcd$ID),
                    labels=lcd$landcover,
                    breaks=lcd$ID,
                    name="Landcover Type")+
  coord_equal()+
  theme(legend.position = "right")+
  guides(fill=guide_legend(ncol=1,byrow=TRUE))


plot(lst[[1:12]])
scoff(lst)=cbind(0.02,-273.15)
plot(lst[[1:10]])

#1
lw= data.frame(x= -78.791547,y=43.007211) %>% st_as_sf(coords=c("x","y"),crs=4326)
?st_crs
raster_crs <- st_crs(lst)
lw_transformed <- st_transform(lw, crs = raster_crs)
lst_values <- extract(lst, lw_transformed, buffer = 1000, fun = mean, na.rm = TRUE)
lst_value_t <- t(lst_values)[-1]
dates <- time(lst)
comb_df <- cbind.data.frame(Date = dates, LST = lst_value_t)
ggplot(comb_df, aes(x = Date, y = LST)) +
  geom_point(color = "black") +
  geom_smooth(span=0.03,n=200,se=FALSE,color="blue")+# Raw data points
  labs(title = "LST Time",
       x = "Date", y = "LST (°C)") +
  theme_minimal()
?geom_smooth

?tapp

#2
lst_month <- tapp(lst,index="month",fun=mean,na.rm=TRUE)
names(lst_month)=month.name[as.numeric(str_replace(names(lst_month),"m_",""))]
gplot(lst_month) +
  geom_tile(aes(fill=value)) +
  facet_wrap(~variable,ncol=3) +  # Arrange plots in a grid
  scale_fill_viridis_c(option="magma")+           
  labs(title="Monthly Mean LST",x="Longitude",y="Latitude") +
  theme_minimal()
monthly_mean <- round(global(lst_month,fun=mean,na.rm=TRUE),2)
print(monthly_mean)

#3

lulc2 <- resample(lulc,lst,method="near") #Resampling the data

#Extract the values from lst_month and lulc2 into a data.frame 
lcds1=cbind.data.frame(values(lst_month),ID=values(lulc2[[1]]))%>% na.omit()

#Gather the data into a ‘tidy’ format
lcds1_tidy <- lcds1 %>%
  gather(key = 'month', value = 'value', -Land_Cover_Type_1_13) %>%
  mutate(
    ID = as.numeric(Land_Cover_Type_1_13),
    month = factor(month, levels = month.name, ordered = TRUE)
  )
#Left joined tidy data with lcd which was created earlier
lcds1_joined <- lcds1_tidy %>%
  left_join(lcd,by="ID")
#Filtered the data for landcover in Urban and Deciduous
lcds1_filtered <- lcds1_joined %>%
  filter(landcover %in% c("Urban & built-up", "Deciduous Broadleaf forest"))

#Plotting
ggplot(lcds1_filtered, aes(x = month, y = value, fill = landcover)) +
  geom_boxplot(alpha = 0.7, color = "black") +  # Boxplot with transparency
  geom_point(position = position_jitter(width = 0.2), alpha = 0.3, size = 0.5) +  
  facet_wrap(~landcover,scales ="free_y") + # facet by land cover type
  labs(
    title="Land Surface Temperature in Urban & Forest",
    x="Month",
    y = "Monthly Mean Land Surface Temperature (°C)"
  ) +
  scale_x_discrete(limits = month.name) +  # to ensure months are in order
  scale_fill_manual(values = c("Deciduous Broadleaf forest" = "forestgreen", "Urban & built-up" = "orangered")) +  # Custom colors
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_rect(fill = "lightgray"),
    strip.text = element_text(size = 10)
  )
?geom_

