require(tidyverse)
require(spict)


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
  
  data2 = data %>% select(CatchDate, Effort_tow, Akagarei, Latitude, Longitude) %>% mutate(sp = "Akagarei") %>% dplyr::rename(kg = Akagarei)
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

# data area -----------------------------------------------------
summary(po2)

index_ak = po2 %>% na.omit() %>% mutate(cpue = kg/Effort_tow) %>% group_by(year) %>% summarize(cpue = mean(cpue))
# index_ak[is.na(index_ak)] = 0

catch_ak = po2 %>% group_by(year) %>% summarize(catch = sum(kg))
