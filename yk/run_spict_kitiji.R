require(tidyverse)
require(spict)


# 漁獲量 -----------------------------------------------------------
dir_catch = "/Users/Yuki/Dropbox/業務/キチジ太平洋北部/SA2022/inputdata/output/"
setwd(dir = dir_catch)
catch = read.csv("rawdata_fig5_for_nextSA.csv", fileEncoding = "CP932")


# ノミナルCPUE ----------------------------------------------------------
dir_index = "/Users/Yuki/Dropbox/業務/キチジ太平洋北部/SA2022/SAM/"
setwd(dir = dir_index)
index = read.csv("index_0720.csv",  row.names=1)
index_n = index[1, ] # ノミナル（沖底）


# 標準化CPUE by VAST -----------------------------------------------
index_so = index[3, ] # 標準化（沖底）


# 面積密度法で推定した資源量 -------------------------------------------------
dir_ad = "/Users/Yuki/Dropbox/SS/kitiji/"
setwd(dir = dir_ad)
area_density = read.csv("estimatedtrend_for_SS.csv")


