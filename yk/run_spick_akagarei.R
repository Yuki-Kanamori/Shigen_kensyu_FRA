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

index_ak = same %>% filter(lat < 42) %>% na.omit() %>% mutate(cpue = kg/Effort_tow) %>% group_by(year) %>% summarize(cpue = mean(cpue)) %>% filter(year != 1972)
# index_ak[is.na(index_ak)] = 0

catch_ak = same %>% filter(lat < 42) %>% group_by(year) %>% summarize(catch = sum(kg)) %>% filter(year != 1972)



# データリストの作成 -----------------------------------------------------
data_test <- list(timeC = catch_ak$year,
                  obsC  = catch_ak$catch,
                  timeI = index_ak$year,
                  obsI  = index_ak$cpue)


### データのプロット
plotspict.ci(data_test)

### 解析のための諸設定を自動生成
input <- check.inp(data_test)
names(input)
input$dteuler <- 1 # ?? 1だと1年ごとに計算．季節などもっと細かい時系列で計算したい時は1/16まで細かくできる（各季節をさらに4つに分けてる？）．資源量が季節ごとに推定できるから，これはこれで面白いかも


## 事前分布の確認 ----
names(input$priors)

# ヘルプでcheck.inpを調べるとinputの中身を教えてくれる
input$priors$logn # 形状パラメータ（正規分布を仮定．平均，標準偏差，事前分布として用いるかどうか(1なら罰則あり，0なら罰則なしで推定))

input$priors$logr # 内的自然増加率
input$priors$logK # 環境収容力

input$priors$logq # 採集効率
# input$priors$logq = c(log(2), 0.5, 1)

input$priors$logsdb # biomass?
input$priors$logsdi # cpueの観測誤差
### 最初の設定は無情報事前分布である
### 


## とりあえずspictで推定 ==================================================
res0 <- fit.spict(input)

### 1. 解析結果の確認 =====================================================

# 結果を要約する
summary(res0)

# 推定された初期資源量の割合がデフォルトでは出ないので，出す
get.par("logbkfrac", res0, exp = TRUE) #オプションexp=TRUEによって、log推定値を非対数に戻す
# estの値が，環境収容力のどの位置から採っていたのか?????

#入力データの初期値を確認する：入力データが正しく設定されているかを事後のチェックをしておきましょう
res0$inp$ini  # res$inp (spict解析に用いた入力データのオブジェクト)

##------------------------------------------
## 推定が上手くいっているかの確認事項・その１
##
## その１−１: 収束しているかどうかを判定
res0$opt$convergence  #これが0だったら，収束しているのでOK; もし1だったら、収束していないので結果は信頼できない
##
##
## その１−２: 推定パラメータの分散が有限かどうかを判定
all(is.finite(res0$sd))  # TRUEだったらパラメータの分散が全て有限であるということでOK
##
##
## その１−３: B/BmsyやF/Fmsyの信用区間が一桁以上に広がっていないかどうかを確認
calc.om(res0) #戻り値のmagnitudeが1 以下ならばOK
##
##
## その１−４: 初期値によってパラメータの推定値が変わらないかどうかを確認
## check.ini(res)で初期値を変えたときの影響をみることができる．
## そしてfit<-check.ini(res)としてfit$check.ini$resmatとすると10回分の推定パラの値の一覧が出てくる．
options(max.print = 1e+05)
fit <- check.ini(res0, ntrials = 10)  #ntrials = 20に増やしてもよい？
##
fit$check.ini$inimat  #trial毎に与えた初期値を確認しておく
##
fit$check.ini$resmat  #初期値を変えたtrialによって推定された値。初期値によってはNAとなる場合も。。。



### ２．結果のプロット =====================================================

plot(res0) #全体的な結果のプロット
##-------------------------------------------
## 推定が上手くいっているかの確認事項・その２
## 余剰生産曲線の形が現実的であるかどうか
calc.bmsyk(res0)　
##この値が0.1—0.9の範囲外にある場合は、余剰生産曲線の形が偏っている
##-------------------------------------------


### ３．推定パラメーターの事前分布と事後分布のプロット  ========================

plotspict.priors(res0)  #事前分布と事後分布


### ４．残差診断（バイアス、自己相関、正規性の診断） ==========================

res_resi <- calc.osa.resid(res0)
plotspict.diagnostic(res_resi)

##------------------------------------------- 
##　推定が上手くいっているか確認事項・その３
## p値が0.05より大きい(有意に差がない．有意に差があると，
## 図の上のp値の文字が赤色になる．緑色ならOK)
##-------------------------------------------

