require(tidyverse)
require(spict)
require(maps)


# 沖底データの読み込み ----------------------------------------------------
dir_okisoko = "/Users/Yuki/Dropbox/same"
setwd(dir = dir_okisoko)


# PO ------------------------------------------------------------
# # 2019年のデータに位置情報がなかったため，加筆する
# dir_temp = "/Users/Yuki/Dropbox/same (1)/"
# 
# tag = read.csv(paste0(dir_temp, "BTC_FishingZone_SeaRegion_LatitudeLongitude.csv")) %>% select(FishingZone, Longitude, Latitude)
# df = read.csv(paste0(dir_temp, "BottomTrawl_NWPO_2019.csv")) #11927*59
# df = left_join(df, tag, by = "FishingZone")
# 
# write.csv(df, paste0("/Users/Yuki/Dropbox/same/PO/BottomTrawl_NWPO_2019.csv"))



dir_po = "/Users/Yuki/Dropbox/same/PO"
setwd(dir_po)
path = dir_po
files = list.files(path)

po = NULL
for(i in 1:length(files)){
  data = read.csv(paste0(files[i]), fileEncoding = "CP932")
  
  data2 = data %>% select(CatchDate, Effort_tow, Menuke, Latitude, Longitude) %>% mutate(sp = "Menuke") %>% dplyr::rename(kg = Menuke)
  kg = data2$kg
  kg[is.na(kg)] = 0
  data2$kg = kg
  
  # if("Same" %in% colnames(data)){
  #   data2 = data %>% select(CatchDate, Effort_tow, Same, Latitude, Longitude) %>% mutate(sp = "Same") %>% dplyr::rename(kg = Same)
  #   kg = data2$kg
  #   kg[is.na(kg)] = 0
  #   data2$kg = kg
  # }
  # if("Aburatsunozame" %in% colnames(data)){
  #   data2 = data %>% select(CatchDate, Effort_tow, Aburatsunozame, Latitude, Longitude) %>% mutate(sp = "Aburatsunozame") %>% dplyr::rename(kg = Aburatsunozame)
  #   kg = data2$kg
  #   kg[is.na(kg)] = 0
  #   data2$kg = kg
  # }
  po = rbind(po, data2)
}

unique(po$CatchDate)
summary(po$CatchDate)
po2 = po %>% mutate(year = as.numeric(str_sub(CatchDate, 1, 4)), month = as.numeric(str_sub(CatchDate, 5, 6)), day = as.numeric(str_sub(CatchDate, 7, 8)))
summary(po2)
unique(po2$year)

# setwd(dir1)
# write.csv(po2, "po.csv", fileEncoding = "CP932")

check = po2 %>% filter(Latitude > 44)
summary(check)



# deta check (map) ----------------------------------------------
same = po2
same = same %>% mutate(lon = ((same$Longitude - trunc(same$Longitude)) / 0.6) + trunc(same$Longitude), lat = ((same$Latitude - trunc(same$Latitude)) / 0.6) + trunc(same$Latitude)) %>% na.omit()
summary(same)

# 北海道も推定してみる
# same = same %>% filter(lat < 42) #うまいこと北海道のデータは除去できる


map = ggplot() + coord_fixed() + xlab("Longitude") + ylab("Latitude")
world_map = map_data("world")
region2 = subset(world_map, world_map$region == region)
local_map = map + geom_polygon(data = region2, aes(x = long, y = lat, group = group), colour = "black", fill = "white") + coord_map(xlim = c(min(same$lon), max(same$lon)), ylim = c(min(same$lat), max(same$lat)))
th = theme(#panel.grid.major = element_blank(),
  #panel.grid.minor = element_blank(),
  axis.text.x = element_text(size = rel(0.5), angle = 90),
  axis.text.y = element_text(size = rel(0.5)),
  axis.title.x = element_text(size = rel(0.5)),
  axis.title.y = element_text(size = rel(0.5)),
  legend.title = element_text(size = 13))
p = geom_point(data = same %>% filter(kg > 0), aes(x = lon, y = lat, colour = kg), shape = 16, size = 1)
# p = geom_point(data = same %>% filter(kg > 10000), aes(x = lon, y = lat, colour = kg), shape = 16, size = 1)
c = scale_colour_gradientn(colours = c("black", "blue", "cyan", "green", "yellow", "orange", "red", "darkred"))
labs = labs(x = "Longitude", y = "Latitude", colour = "kg")
f = facet_wrap(~ year, ncol = 8)

fig = local_map+theme_bw()+th+p+c+labs+scale_x_continuous(breaks=seq(129,152,by=0.5))+scale_y_continuous(breaks=seq(34,47,by=0.5))



# data area -----------------------------------------------------
summary(po2)

index_mn = same %>% filter(lat < 42) %>% na.omit() %>% mutate(cpue = kg/Effort_tow) %>% group_by(year) %>% summarize(cpue = mean(cpue))
# index_ak[is.na(index_ak)] = 0

catch_mn = same %>% filter(lat < 42) %>% group_by(year) %>% summarize(catch = sum(kg))
