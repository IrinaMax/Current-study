# The code with DQ data, to fit linear regression for every die
library(glmnet)
library(dplyr)
library(plyr)
library(corrplot)
library(caret)
library(devtools)
library(data.table)
library(ggplot2)
start.time <- Sys.time()

#...Relevent codes...

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# diequal table HTPD
b3 <- read.csv("htpd_10.csv")
c <- ggplot(b3, aes(fbc))
c+ geom_bar()
summary(b3)
dim(b3)
#[1] 3648    7
# rename colomns for easy to use
b3 <- rename(b3, c("X_c6"="FBC", "a.lot"="lot", "a.wafer"="wafer","a.x"= "x", "a.y"= "y", "a.page_type" = "level", "a.cycling" = "cycling"))
b3 %>% names   # [1] "a.lot"     "a.wafer"   "a.x"       "a.y"       "a.blk"     "a.cycling" "X_c6" 
which.max(b3$FBC)
outh <- data.frame(which( b3$FBC >9, arr.ind = T))
outh
# remove outlier
b3[c(152,3336),]
# lot wafer  x  y level cycling FBC
# 152  CP0938453     6 28 13    LP    3000 826
# 3336 CP0938453     6 28 13    MP    3000 999

b3 <- b3[-c(152,3336), ]
row.names(b3) <- 1:nrow(b3)
row.names(b3)
b3 %>% dim  #[1] 1215    6
write.csv(b3, "10_HTPD_dq.csv")
head(b3)

# diequal table RTPD
b3_r <- read.csv("rtpd_10.csv")
#      on the cluster
##     b3_r <- read.csv("/home/irinam/BICS3/rtpd_maxfbc_0918.csv")
summary(b3_r)
b3_r %>% dim
#[1] 7296    7
# rename colomns for easy to use
b3_r <- rename(b3_r, c("X_c6"="FBC", "a.lot"="lot", "a.wafer"="wafer","a.x"= "x", "a.y"= "y", "a.page_type" = "level", "a.cycling" = "cycling"))
b3_r %>% names   # [1] "a.lot"     "a.wafer"   "a.x"       "a.y"       "a.blk"     "a.cycling" "X_c6" 
which.max(b3_r$FBC)
outr <- data.frame(which( b3_r$FBC >9, arr.ind = T))
outr
# remove outlier

b3_r[c(6707,370,4411,6674),]  #  check out the outliers
# lot wafer  x  y level cycling FBC
# 6707 CP0938453     6 28 13    MP    3000 996
# 370  CP0938453     6 28 13    LP    3000 801
# 4411 CP0939223    18  7 15    MP    3000  99
# 6674 CP0938453     6 28 13    UP    3000  99
b3_r <- b3_r[-c(6707,370,4411,6674), ]

row.names(b3_r) <- 1:nrow(b3_r)
row.names(b3_r)
dim(b3_r)
#[1] 7292    7 with level
#[1] 2431    6
write.csv(b3_r, "10_RTPD_dq.csv")
head(b3_r)

## LTPD
b3_l <- read.csv("ltpd_10.csv")
# on the cluster
##     b3_r <- read.csv("/home/irinam/BICS3/rtpd_maxfbc_0918.csv")
summary(b3_l)
dim(b3_l)
#[1] 7296    7
# rename colomns for easy to use
b3_l <- rename(b3_l, c("X_c6"="FBC", "a.lot"="lot", "a.wafer"="wafer","a.x"= "x", "a.y"= "y", "a.page_type" = "level", "a.cycling" = "cycling"))
b3_l %>% names   # [1] "a.lot"     "a.wafer"   "a.x"       "a.y"       "a.blk"     "a.cycling" "X_c6" 
which.max(b3_l$FBC)
outl <- data.frame(which( b3_l$FBC >9, arr.ind = T))
outl

#[1]318
b3_l[c(3277, 4411,6707),]
# lot wafer  x  y level cycling FBC
# 3277 CP0937681    22 10 21    MP    3000  99
# 4411 CP0939223    18  7 15    MP    3000  99
# 6707 CP0938453     6 28 13    MP    3000 993
# remove outlier by removing the row 318 with fbc 993 and all > 9
b3_l <- b3_l[-c(6707,4411,3277), ]
row.names(b3_l) <- 1:nrow(b3_l)
row.names(b3_l)
dim(b3_l)
#[1] 2431    6
write.csv(b3_l, "10_LTPD_dq.csv")
head(b3_l)

# 
# add temp column to the data 
b3$tem <- rep(85, nrow(b3))   # new col with temt 85
b3_r$tem<- rep(25, nrow(b3_r))   # new col with temt 25
b3_l$tem<- rep(-15, nrow(b3_l))   # new col with temt -15

# stuck all data together with cycling, FBC and temerature
st_dq <- rbind(b3, b3_r,b3_l)
st_dq %>% dim
#[1] 18231     8
st_dq %>% names
write.table(st_dq, "Stuck_HTRTPD_withTemp.csv", sep = ',', row.names = FALSE)

library(plotly)
devtools::install_github('hadley/ggplot2')
set.seed(123)

#df <- diamonds[sample(1:nrow(diamonds), size = 1000),]

p <- ggplot(df, aes(x = fbc)) + 
  geom_bar(aes(y = ..count../sum(..count..), fill = cycling)) + 
  scale_fill_brewer(palette = "Set3") + 
  ylab("Percent") + 
  ggtitle("Show precentages in bar chart")

p <- ggplotly(p)
p

# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
chart_link = plotly_POST(p, filename="geom_bar/bar-percentages")
chart_link
# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
chart_link = plotly_POST(p, filename="geom_bar/bar-ordered")
chart_link
# ------------------------------------------------ END OF WORK WITH DIE QUAL----------------------------


#after retreaving data in HIVE join th DQ and DS tables by query
# SELECT *
#   FROM default.ds_bics_t6rn1_sme1_plot c 
# join irina_db.dq_crosstemp u 
# on u.lot=(regexp_extract(c.lotno,'(.*)\\.',1))
# and u.x=c.originaldiex as int
# and u.y=c.originaldiey as int
# and cast(u.wafer as int)=cast(regexp_extract(c.waferno, '.*\\.(\\d+)',1) as int);


# CLEANING DATA FROM DS
adf <- read.csv("cross_temp10dsdq.csv", header = TRUE, sep = ",", na.strings = c("NULL", "NaN", "NA"), stringsAsFactors = TRUE)
adf %>% dim
# make copy of original table and work with copy in case if something going wrong...
df <- adf

df %>% dim  # [1] 18231  1781

#names(df) <- substring(names(df[,1:348]), 3) remove  first 2 elements from names
nm<- names(df)
nm
str(df)
tail(df)# I am going to save cycling fbc and tem columns for future
df.c.f.t <- df[,c(1779:1781)]
df.c.f.t %>% head

# identefy and remove fisrt 36colomn and last 5 and remove them
n1 <- names(df[,1:36])
n1
# [1] "c.key"              "c.product"          "c.module"           "c.process"          "c.lotno"           
# [6] "c.testerno"         "c.stageno"          "c.foupno"           "c.slotno"           "c.waferid"         
# [11] "c.waferno"          "c.dsstartdate"      "c.dsenddate"        "c.testqty"          "c.wafersize"       
# [16] "c.testerrecipe"     "c.testerpgm"        "c.proberrecipe"     "c.chucktemp"        "c.preformanceboard"
# [21] "c.probecard"        "c.notch"            "c.userid"           "c.originaldiex"     "c.originaldiey"    
# [26] "c.indexno"          "c.dutno"            "c.passfail"         "c.bin_pcs_"         "c.ds_xadd__"       
# [31] "c.ds_yadd__"        "c.ds_r__"           "c.ftestnum__"       "c.fstress__"        "c.param__"         
# [36] "c.vf_mode_mv_"
n2 <- names(df[, 1774:1781])
n2
#[1] "u.id"      "u.lot"     "u.wafer"   "u.x"       "u.y"       "u.cycling" "u.fbc"     "u.tem"
# cut away n1 and n2
df <- df[, -c(1:36, 1774:1781) ]


# sub("c.*", "", df[,1])
# names(df) = gsub(pattern = "b.*", replacement = "", x = names(df))

df %>% names 
# remove first 2 simbols in col names to make it easy to read
names(df) <- substring(names(df) , 3)

df1 <- df  # save original  df as df1
df %>% dim 
df %>%  names
#df$u._c6
#which.max(df$u._c6)   #  show the only one row with 999 fbc, Im going to check it out and remove
#df[13052, 1650:1658] 
# c.bl_pl_ers_sgs__ c.bbk_singlepulse_ev_d_pcs_     u.lot u.wafer u.x u.y u.blk u.cycling u._c6
# 13052                NA                          NA CP0938453       6  28  13   99B      3000   999
#df <- df[ -13052, ]  # remove line 13052 as outlier
#row.names( df) <- 1:nrow(df)
# just look the row is gone
#df[13052, 1650:1658]
#rename colomn with FBC
#df <- rename(df, c("u._c6" = "FBC" ))

# df <- rename(df, c("u._c6"="FBC", "u.lot"="lot",     "u.wafer"="wafer",  "u.x"= "x"      , "u.y"= "y",       "u.blk" ="blk" ,   "u.cycling" = "cycling"))
# df2 <- df   # make copy before 2 step cleaning procedure

# save diequal parametres 
# saving <-df[,colnames(df) %in% c("lot",     "wafer" ,  "x"  ,     "y" ,      "blk" ,    "cycling", "FBC" ) ]
# saving %>% head
# #lm <- lm(FBC~., bf)
# #summary(lm)


##______________________________________


##Let's look how many columns without missing value to see possibility to shrink it.
# NoMissingdf <- colnames(df)[colSums(is.na(df)) == 0]
# NoMissingdf   # 1253 col without NA

cat('Number of Columns in the original table: ' , ncol(df),'\n')
#cat(ncol(df),'\n')   # 1658 

# Columns with more than 20% missing values
na <- (colSums(is.na(df) | df=='', na.rm = TRUE)/nrow(df) > 0.2)
na1 <- na[na==TRUE]
na1.names <- names(na1)
df <- df[ , !names(df) %in% na1.names]
df2 <-df
cat('Number of columns with more than 20% missing values: ',length(na1),'\n')   
#   shrinked to 1257
cat('Name of these columns: ', na1.names,'\n', sep = ',' )
#cat(na1.names,'\n', sep = ',')
#print('Omitting more than 20% missing result')
print(ncol(df))

# # cleaning rows with na
# rna <- (rowSums(is.na(df) | df=='', na.rm = TRUE)/ncol(df) > 0.2)
# rna1 <- rna[rna == TRUE]
# df <- df[!rna,]
# cat('Number of rows with more than 20% missing values: ',sum(rna),'\n')

# cleaning all columns close to 0
nzv1 <- nearZeroVar(df, saveMetrics= TRUE)
num_a<-row.names(nzv1)[nzv1$nzv == TRUE]
df<-df[, !names(df) %in% num_a]
print('Omitting nzv result')
print(ncol(df))
# 1214
cat('Number of columns with near zero variance: ,',length(a),'\n')
cat('Name of These columns: ,')
cat(a,'\n', sep = ',')
df3 <- df
# names_b_cor <- names(df3)
# names_b_cor
# write.csv(names_b_cor, "names_b_cor.csv")

# find correlation
df.cor <- data.matrix(df)

df.cor <- cor(df.cor, use = "pairwise.complete.obs")
CorPath <- capture.output(cat(substr(outputPath,1,nchar(outputPath)-4),'_CorMatrix_CrossTemp.csv',sep = ""))
write.csv(df.cor, CorPath, row.names=FALSE, na="")
write.csv(df.cor, "CorMatrix_CrossTemp.csv", row.names=FALSE, na="")

#The absolute values of pair-wise correlations are considered. If two variables have a high correlation, the function looks at the mean absolute correlation 
#of each variable and removes the variable with the largest mean absolute correlation.
hccor = findCorrelation(df.cor, cutoff = 0.75, names = TRUE)
hccor
df <- df[ , !names(df) %in% hccor]
print('Omitting dependent result')

cat('Number of elemenated columns:     ',length(hc),'\n')
cat('Name of these columns:     ')
cat(hccor,'\n', sep = ',')
cat('Number of   columns:    ',length(df),'\n')
print(dim(df))
##  df$FBC n  id df$u._c6 rename it
#df <- rename(df, c("u._c6" = "FBC"))
# will make copy of the data after cleaning

df5 <- df
col.name <- colnames(df)
col.name
# now I will add back cycling, FBC and temperature
df %>% dim
df.c.f.t %>% dim
df$cycling <- adf$u.cycling
df$fbc <- df.c.f.t$u.fbc
df$tem <- df.c.f.t$u.tem
df$ level <- adf$u.level
df %>% dim
# [1] 18231   353

library(data.table)
df <- data.table(df)
drop.cols <- grep("avro_file_name$", colnames(df))
drop.cols
df[, (drop.cols) := NULL]
df %>% dim
df6 <- df
# final resul after cleaning [1] 6077  352

##  save as csv file for next step
write.csv(df, outputPath, row.names=FALSE, na="")
write.csv(df, "crosstemp_DSDQ_cleaned_1002.csv", row.names=FALSE, na="")
sink()

# which.max(df$fbc)  # for check outliers

ct <- read.csv("crosstemp_DSDQ_cleaned_1002.csv")
ct <- data.frame(ct)
ct <- group_by(ct, tem) 
ct %>% dim
ct#ct <- arrange(ct, tem)
#ct %>% ct$tem %>%  summary
sub.ht <- filter(ct, "tem"> 85  )
sub.ht
sub.lt <- filter(ct, "tem" < -15  )
sub.lt 
mod1 <- lm(fbc~., data= df)
summary(mod1)
#-----------------LASSO--------------------------------------------------------

#df[1,350:352]
# model with CV and lambda min
set.seed(5)

df <- na.omit(df)
df[!complete.cases(df),]
row.has.na <- apply(df, 1, function(x){any(is.na(x))})
row.has.na    ##  row 35 was been removed

df %>% dim
x <- model.matrix(fbc~. , -351, data=df )
x %>% dim
x[, 350:354]
df[, 350:353]
y <- as.matrix(df[, 351]) # Only fbc
y %>% dim
y
cv = cv.glmnet(x, y)
cv
cv %>% names
cv$lambda.min
#[1] 0.0001471211
model = glmnet(x, y, type.gaussian="covariance",  lambda=cv$lambda.min,standardize = TRUE, standardize.response = TRUE)
model
summary(model)
# Call:  glmnet(x = x, y = y, lambda = cv$lambda.min, type.gaussian = "covariance") 
# 
# Df   %Dev  Lambda
# [1,] 34 0.1331 0.04575

plot(cv, main="CrossTemp LASSO coefficients capture based on Lambda.min 0.0001471211 ")
cv %>% names
cv$lambda.min
#[1]  0.04574851

# need to standardize all predictors 
# install.packages("ggfortify")
library(ggfortify)
model_s = glmnet(x, y, type.gaussian="covariance",  lambda=cv$lambda.min, standardize = TRUE, standardize.response = TRUE )
model_s
#p_model_s <-glmnet::glmnet(x, y, type.gaussian="covariance",  lambda=cv$lambda.min, standardize = TRUE, standardize.response = TRUE )
#autoplot(model_s, pch=19)
#plot(model_s, xvar = "lambda", label = TRUE)
#plot(model_s, xvar = "dev", label = TRUE)

summary(model_s)

# extracting names of the LASSO MODEL------------------------------------------------
model %>% names
model$lambda
# [1] 0.04574851
pred1 <- predict(model_s, type="coefficients")
pred1
pred_300<-predict(model_s, newx = x[1:300,], s = "lambda.min")
pred_300

pred2 <- predict(model_s,x, s="lambda.min",type="response")
plot(pred2,y)
plot(pred2, y,  xlab = "prediction", ylab = "fbc",col = "dark red", main = "expectation on model pred2, lambda min= 0.0001471211") 
abline(lm(y ~ pred2, data=df))
# plot prediction as a slop for the 151 dies
abline(lm(y ~ pred2 + 0),  col="blue")

# I want to make the result table with coloms
temp <- pred1%>% summary
temp$i

#  [1]   1   5  22  45  64  69  78  80  81 163 166 168 174 189 196 205 215 220 289 290 303 304 312 314 331 334 335 339 344
#[30] 347 348 349 351 352 353



pr <- as.matrix(pred1)
pr
res <- data.frame(which( !pr ==0, arr.ind = T))
res$col<- NULL
res
res$sl_coef <- pr[which( !pr ==0, arr.ind = T)]
res %>% summary
res
#                                  row       sl_coef
# (Intercept)                        1  1.565442e+01
# crd_ttl_pl0_p_                     5 -5.534852e-03
# vx2_s_dac_                        22 -2.604081e-02
# vcg_av3_t_mv_                     45  2.507298e-06
# vpass3_t_mv_                      64 -1.683300e-04
# psdovccq_t_mv_                    69 -4.372255e-04
# ron_po18_s_dac_                   78 -3.985157e-03
# ron_po33_t_ua_                    80 -1.780538e+00
# ron_no33_s_dac_                   81 -1.900668e-02
# fbc_sdllk4_pcs_                  163 -6.863496e-05
# tempcode_b9_pcs_                 166 -1.528051e-02
# crd_m1bll_224_c_pcs_             168  4.454403e-06
# vreads_p_                        174  7.415582e-02
# crd_m1bll_slc1_p_                189  1.153987e-02
# layer3_vpgms_s_dac_              196  7.892574e-02
# treadm8k_us_                     205 -1.278913e-01
# vth_wlds0last_l3s_mv_            215  2.842987e-04
# wlleak_post_00_na_               220 -3.356609e-04
# vth_12pwl3_lt_mv_                289  3.066994e-04
# vth_08pwl60_lt_mv_               290  2.138443e-04
# prgloop_wl00_mv_                 303 -2.490419e-02
# frlt_vcgrsft_ar3_pcs_            304 -3.402689e-02
# tlcwc_we0_wl0_fr_s25_f2g_pcs_    312  1.588261e-02
# tlcwc_we0_wl31_fr_s25_f2g_pcs_   314  7.862807e-03
# sfbc_b32wlsaup_drpost_pcs_       331  1.363392e-04
# sfbc_slcerslp_5kpost_pcs_        334 -1.190453e-02
# sfbc_slcerslp_7kpost_pcs_        335 -6.842064e-02
# crd_scrnslcbot_p_                339  3.337523e-01
# vpgmulooptrial2__                344 -7.369102e-02
# bbk_wlmh_6_d_pcs_                347 -6.157009e-03
# bbk_high_wldd0_ev_d_pcs_         348  1.177988e-06
# sfbc_t32wlsalp_far_dc_fresh_pcs_ 349  1.297020e-05
# sfbc_t32wlsalp_far_fresh_pcs_    351  5.999705e-05
# cyclinng                         352  2.334356e-04
# tem                              353 -1.415534e-02

write.csv(res, "CrossTemp_result_of_CV_pred1_lasso.csv")


######----------------LASSO 2-------------------------------------
set.seed(777)
x <- model.matrix(fbc~. , -351, data=df )
y <- as.matrix(df[, 351]) # Only fbc

cv.lasso <- cv.glmnet(x, y, nfold=10, alpha=1, parallel=TRUE, standardize=TRUE, standardize.response = TRUE, type.measure='mae')
cv.lasso
# Results
plot(cv.lasso, main = "CrossTemp LASSO coefficients capture with type.measure='mae', Lambda min 0.2224568 ")
plot(cv.lasso$glmnet.fit, xvar = "lambda", label = TRUE, main ="CrossTemp LASSO coefficients capture with type.measure='mae', Lambda min 0.2224568")
plot(cv.lasso$glmnet.fit, xvar = "dev", label = TRUE, main="CrossTemp LASSO coefficients capture with type.measure='mae', Lambda min 0.2224568 ")
cv.lasso$lambda.min
#[1] 0.2224568
cv.lasso$lambda.1se
#[1]  0.26795

coeff_lasso <- coef(cv.lasso, s=cv.lasso$lambda.min,exact=TRUE) [which(coef(cv.lasso, s = "lambda.min") != 0)]

coeff_lasso
#extract coefficient with min lambda and names of LASSO result
# c<-coef(glmnet2, s='lambda.min',exact=TRUE) 
# c
# coef_l <- coef(glmnet2, s='lambda.min',exact=TRUE) [which(coef(glmnet2, s = "lambda.min") != 0)]
colnames <- colnames(df)[which(coef(cv.lasso, s = "lambda.min") != 0)]
colnames
##  Updated frame of coeff with names of the variable
l_coeffs <- coef(cv.lasso, s = "lambda.min")
l_name_coeff<- data.frame(name = l_coeffs@Dimnames[[1]][l_coeffs@i + 1], coefficient = l_coeffs@x)
l_name_coeff
#                         name   coefficient
# 1                (Intercept)  8.533999e+00
# 2 sfbc_b32wlsaup_drpost_pcs_  6.555852e-05
# 3          crd_scrnslcbot_p_  1.043924e-01
# 4                   cyclinng  6.051258e-05
# 5                        tem -9.300889e-03

write.csv(l_name_coeff, "CrossTemp_lasso_name_coeff_mae.csv")
as.data.frame(l_name_coeff)



####--------------------------    GLM based on the LASSO pded1-----------------------------
# try to make subset of variables picked by Lasso pred1 
# vector of columns you DON'T want
aaa <- read.csv("CrossTemp_result_of_CV_pred1_lasso.csv")
aaa[,1]
f <- c("crd_ttl_pl0_p_",
       "vx2_s_dac_",
       "vcg_av3_t_mv_",
       "vpass3_t_mv_",
       "psdovccq_t_mv_",
       "ron_po18_s_dac_",
       "ron_po33_t_ua_",
       "ron_no33_s_dac_",
       "fbc_sdllk4_pcs_",
       "tempcode_b9_pcs_",
       "crd_m1bll_224_c_pcs_",
       "vreads_p_",
       "crd_m1bll_slc1_p_",
       "layer3_vpgms_s_dac_",
       "treadm8k_us_",
       "vth_wlds0last_l3s_mv_",
       "wlleak_post_00_na_",
       "vth_12pwl3_lt_mv_",
       "vth_08pwl60_lt_mv_",
       "prgloop_wl00_mv_",
       "frlt_vcgrsft_ar3_pcs_",
       "tlcwc_we0_wl0_fr_s25_f2g_pcs_",
       "tlcwc_we0_wl31_fr_s25_f2g_pcs_",
       "sfbc_b32wlsaup_drpost_pcs_",
       "sfbc_slcerslp_5kpost_pcs_",
       "sfbc_slcerslp_7kpost_pcs_",
       "crd_scrnslcbot_p_",
       "vpgmulooptrial2__",
       "bbk_wlmh_6_d_pcs_",
       "bbk_high_wldd0_ev_d_pcs_",
       "sfbc_t32wlsalp_far_dc_fresh_pcs_",
       "sfbc_t32wlsalp_far_fresh_pcs_",
       "cyclinng",
       "tem") 

f %>% str
# subset of all data with selected  21 veriables
ss <-df%>% select(f)
ss %>% dim
ss %>% names
ss
# add responce 
df$fbc
ss$fbc <-df$fbc
write.csv(ss, "CrossTemp_Lasso_var_subset.csv")
lmod_ss <- lm(fbc~., ss)
lmod_ss
summary(lmod_ss)
lmod_summary <-lmod_ss %>% summary
capture.output(lmod_summary, file = "GLM_crosstemp_lassosubset.txt")

# Call:
#   lm(formula = fbc ~ ., data = ss)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -5.692 -0.455  0.023  0.466 87.344 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                       6.027e+01  1.995e+01   3.021 0.002528 ** 
#   crd_ttl_pl0_p_                   -2.295e-02  4.282e-03  -5.360 8.63e-08 ***
#   vx2_s_dac_                       -1.242e-01  4.393e-02  -2.827 0.004717 ** 
#   vcg_av3_t_mv_                     7.831e-03  5.906e-03   1.326 0.184899    
# vpass3_t_mv_                     -7.338e-04  4.351e-04  -1.686 0.091788 .  
# psdovccq_t_mv_                   -8.841e-03  6.363e-03  -1.389 0.164793    
# ron_po18_s_dac_                  -1.359e-02  2.047e-02  -0.664 0.506782    
# ron_po33_t_ua_                   -1.289e+00  4.198e+00  -0.307 0.758770    
# ron_no33_s_dac_                  -5.902e-02  2.671e-02  -2.210 0.027167 *  
#   fbc_sdllk4_pcs_                  -1.770e-04  4.354e-05  -4.064 4.89e-05 ***
#   tempcode_b9_pcs_                 -1.534e-01  9.882e-02  -1.552 0.120698    
# crd_m1bll_224_c_pcs_              5.908e-06  5.122e-06   1.153 0.248814    
# vreads_p_                         1.280e-01  3.225e-02   3.970 7.28e-05 ***
#   crd_m1bll_slc1_p_                 3.672e-02  1.225e-02   2.999 0.002722 ** 
#   layer3_vpgms_s_dac_               8.913e-02  2.577e-02   3.459 0.000545 ***
#   treadm8k_us_                     -2.333e-01  6.459e-02  -3.613 0.000305 ***
#   vth_wlds0last_l3s_mv_             5.815e-05  1.032e-03   0.056 0.955045    
# wlleak_post_00_na_               -2.252e-03  1.704e-03  -1.321 0.186464    
# vth_12pwl3_lt_mv_                 1.265e-04  5.844e-04   0.216 0.828619    
# vth_08pwl60_lt_mv_                9.605e-04  5.040e-04   1.906 0.056697 .  
# prgloop_wl00_mv_                 -7.173e-02  7.706e-02  -0.931 0.351978    
# frlt_vcgrsft_ar3_pcs_            -7.916e-02  2.864e-02  -2.764 0.005730 ** 
#   tlcwc_we0_wl0_fr_s25_f2g_pcs_     2.603e-02  9.256e-03   2.813 0.004927 ** 
#   tlcwc_we0_wl31_fr_s25_f2g_pcs_    1.234e-02  8.625e-03   1.431 0.152617    
# sfbc_b32wlsaup_drpost_pcs_        1.452e-04  5.053e-05   2.874 0.004068 ** 
#   sfbc_slcerslp_5kpost_pcs_        -8.900e-02  1.128e-01  -0.789 0.430340    
# sfbc_slcerslp_7kpost_pcs_        -1.150e-01  6.196e-02  -1.855 0.063596 .  
# crd_scrnslcbot_p_                 5.156e-01  4.775e-02  10.797  < 2e-16 ***
#   vpgmulooptrial2__                -1.305e-01  2.184e-01  -0.598 0.550048    
# bbk_wlmh_6_d_pcs_                -2.836e-02  2.425e-02  -1.170 0.242115    
# bbk_high_wldd0_ev_d_pcs_          1.203e-05  3.134e-05   0.384 0.701094    
# sfbc_t32wlsalp_far_dc_fresh_pcs_ -6.567e-05  5.696e-05  -1.153 0.248985    
# sfbc_t32wlsalp_far_fresh_pcs_     4.094e-04  1.549e-04   2.643 0.008241 ** 
#   cyclinng                          2.782e-04  2.594e-05  10.722  < 2e-16 ***
#   tem                              -1.532e-02  7.311e-04 -20.949  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.058 on 6002 degrees of freedom
# Multiple R-squared:  0.1448,	Adjusted R-squared:   0.14 
# F-statistic: 29.89 on 34 and 6002 DF,  p-value: < 2.2e-16
plot(lmod_ss)

# ------------------------------------Random Forest -----------------------------------------------
library(caret)
library(dplyr)
library(corrplot)
library(plyr)
library(rpart)
library(randomForest)
set.seed(123)

## 75% of the sample size  or may be 80%  ???

adf <- read.csv("crosstemp_DSDQ_cleaned_1002.csv")
df <- na.omit(adf)
set.seed(123)
smp_size <- floor(0.90 * nrow(df))

train_ind <- sample(seq_len(nrow(df)), size = smp_size, replace = FALSE  )

train <- df[train_ind, ]
test <- df[-train_ind, ]

#TRAIN
train %>% dim
names(train)
tx <- model.matrix(fbc~. , -351, data=train )
ty <- as.matrix(train[, 351]) # Only fbc
ty %>% dim

# TEST
test %>% dim
names(test)
y_test <- test[,351]
y_test %>% summary  # we need to to campare result
#test <- model.matrix(fbc~. , -351, data=test )
xtest <- test[, -351]
#Train Random Forest
df %>% dim
start.time <- Sys.time()

rf <-randomForest(df$fbc~.,data=df, teskeep.forest=FALSE, importance=TRUE,ntree=200)
rf
# Call:
#   randomForest(formula = df$fbc ~ ., data = df, teskeep.forest = FALSE,      importance = TRUE, ntree = 200) 
# Type of random forest: regression
# Number of trees: 200
# No. of variables tried at each split: 117
# 
# Mean of squared residuals: 0.2431967
# % Var explained: 86.55

# Call:
#   randomForest(formula = df$fbc ~ ., data = df, mtry = 50, teskeep.forest = FALSE,      importance = TRUE, ntree = 100) 
# Type of random forest: regression
# Number of trees: 100
# No. of variables tried at each split: 50
# 
# Mean of squared residuals: 0.3671717
# % Var explained: 79.69

# Call:
#   randomForest(formula = df$fbc ~ ., data = df, importance = TRUE,      ntree = 100) 
# Type of random forest: regression
# Number of trees: 100
# No. of variables tried at each split: 117
# 
# Mean of squared residuals: 0.2451144
# % Var explained: 86.44
print(rf, main =" Random forest")
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

plot(rf , main =" Random forest 200 trees")

#    will see importence var
impRF<-rf$importance    #worked
importance(rf, type =1 )
impRF = importance(rf, type =1)
impRF <- data.frame(predictors=rownames(impRF),impRF)

rf$importanceSD
rf %>% summary
rf$mse
# Order the predictor levels by importance 
# I guess %IncMSE of j'th is (mse(j)-mse0)/mse0 * 100%   so the higher number more importent

imp.sortRF <- arrange(impRF,desc(impRF$X.IncMSE))
imp.sortRF
imp.sortRF$predictors <- factor(imp.sortRF$predictors,levels=imp.sortRF$predictors)
imp.sortRF$predictors
imp.sortRF
# Select the top 20 predictors
imp.30<- imp.sortRF[1:30,]
print(imp.30)
write.csv(imp.30, "RandomForest_200t_30var.csv")

# Now we can compare the Out of Bag Sample Errors and Error on Test set
# The above Random Forest model chose Randomly 4 variables to be considered at each split. We could now try all possible 13 predictors which can be found at each split.

oob.err=double(13)
test.err=double(13)

#mtry is no of Variables randomly chosen at each split
for(mtry in 1:13) 
{
  rf=randomForest(medv ~ . , data = Boston , subset = train,mtry=mtry,ntree=400) 
  oob.err[mtry] = rf$mse[400] #Error of all Trees fitted
  
  pred<-predict(rf,Boston[-train,]) #Predictions on Test Set for each Tree
  test.err[mtry]= with(Boston[-train,], mean( (medv - pred)^2)) #Mean Squared Test Error
  
  cat(mtry," ") #printing the output to the console
  
}



# let try to do tain and predict on test--------------------------------------------------------------------
#ptm <- proc.time()
start.time <- Sys.time()
set.seed(123)
rftrain <-randomForest(train$fbc ~., data = train, teskeep.forest=FALSE, importance=TRUE,ntree=150)
rftrain
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken  
# Time difference of 17.44257 mins
# Call:
#   randomForest(formula = train$fbc ~ ., data = train, teskeep.forest = FALSE, importance = TRUE, ntree = 150) 
# Type of random forest: regression
# Number of trees: 150
# No. of variables tried at each split: 117
# 
# Mean of squared residuals: 0.2490133
# % Var explained: 86.15
varImpPlot(rftrain)
pred_rf <- predict(rftrain, test)
pred_rf
plot(pred_rf, alpha=.2)
plot(rftrain)

#------------------- PLOTING----------------------------------
library(ggplot2)
library(plot3D)
library(randomForestSRC)
library(ggRandomForests)
ggplot(train)+
  geom_boxplot(aes(y=fbc,x=cycling,fill=tem),notch=T,notchwidth=0.5)+
  facet_grid(~tem,margins=T)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab(label='fbc')+
  xlab('cycling')+
  ggtitle('Plot of the FBC by cycling on the cross temperaure ')

p1=ggplot(train)+
  geom_point(aes(y=fbc,x=cycling,color=as.factor(tem)))+
  scale_x_log10()+scale_y_log10()+
  facet_grid(~cycling)+
  ylab('FBC')+
  xlab('Cycling')+
  ggtitle('Plot of the FBC by cycling with cross temperature')
p1
varImpPlot(rftrain)

## plot Predicted VS Actual

pred<-predict(object=rftrain,newdata=test[,-351])
actual<-test$fbc
result.plot<-data.frame(actual=actual,predicted=pred)
paste('Function Call: ', rftrain$call)
result.plot
# [1] "Function Call:  randomForest"  "Function Call:  train$fbc ~ ." "Function Call:  train"        
# [4] "Function Call:  FALSE"         "Function Call:  TRUE"          "Function Call:  150" 

paste('Mean Squared error: ',mean(rftrain$mse))
#[1] "Mean Squared error:  0.266526574525833"
paste('Root Mean Squared error: ',mean(sqrt(rftrain$mse)))
#[1] "Root Mean Squared error:  0.51527253953601"
gg <-ggplot(result)+
  geom_point(aes(x=actual,y=predicted,color=predicted-actual),alpha=0.3)+
  ggtitle('Predicted vs Actual on test data')
result.plot
gg
# add abline

# plot  perfect prediction line red
reg <- lm(actual~predicted, data=result.plot )
reg
# Call:
#   lm(formula = actual ~ predicted, data = result.plot)
# 
# Coefficients:
#   (Intercept)    predicted  
# -0.4143       1.0508  
gg1 <-gg + geom_abline(intercept = -0.4143, slop = 1.0508, color = "light blue", linetype = "dashed", size = 1)
## perfect prediction
reg <- lm(actual~predicted+ 0, data=result.plot )
reg


#abline(lm(actual ~ predicted +0, data=result.plot, col="green"))
gg1 + geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", size = 0.5)



gg_e <- gg_error(rftrain)
plot(gg_e)
plot(gg_rfsrc(rftrain), alpha=.2 )  

## plot Predicted VS Actual

pred<-predict(object=rftrain,newdata=test[,-351])
actual<-test$fbc
result<-data.frame(actual=actual,predicted=pred)
paste('Function Call: ', rftrain$call)

# Plot the VIMP rankings of independent variables.
#rfsrc_tr <- rfsrc(fbc ~ ., data = train)
plot(gg_vimp(rftrain), 20)

ggplot(rftrain, aes(x=fbc, y=cycling, color=chas))+
   geom_point(alpha=.4)+
   geom_rug(data=rftrain %>% filter(is.na(cycling)))+
   labs(y="", x=st.labs["mfbc"]) +
   scale_color_brewer(palette="Set2")+
   facet_wrap(~variable, scales="free_y", ncol=3)


# Now we can compare the Out of Bag Sample Errors and Error on Test set
# The above Random Forest model chose Randomly 4 variables to be considered at each split. We could now try all possible 13 predictors which can be found at each split.

oob.err=double(30)
test.err=double(30)
#mtry is no of Variables randomly chosen at each split
for(mtry in 1:100) 
{
  rf1=randomForest(fbc ~ . , data = df , subset = train, mtry=mtry,ntree=100) 
  oob.err[mtry] = rf1$mse[200] #Error of all Trees fitted
  
  pred<-predict(rf1, test) #Predictions on Test Set for each Tree
  test.err[mtry]= with(test, mean( (medv - pred)^2)) #Mean Squared Test Error
  
  cat(mtry," ") #printing the output to the console
}

#Test error 
test.err
# Out if Bag Error Estimation
oob.err
#Plotting both Test Error and Out of Bag Error
matplot(1:mtry , cbind(oob.err,test.err), pch=19 , col=c("red","blue"),type="b",ylab="Mean Squared Error",xlab="Number of Predictors Considered at each Split")
legend("topright",legend=c("Out of Bag Error","Test Error"),pch=19, col=c("red","blue"))



proc.time()- ptm
test
predRF <- predict(rf, newdata = xtest)
predRF %>% dim
table(predRF,train[,353] )







#rf1 <-randomForest(df$fbc~.,data=df, mtry=30 importance=TRUE,ntree=500)
# matrix interfae seems to be faster...
df %>% dim
x %>% dim
y %>% dim
ptm <- proc.time()
rf1_mat <-randomForest(y= y, x= x, mtry=30, importance=TRUE,ntree=300)
proc.time() - ptm
print(rf1)
# Call:
# randomForest(formula = df$fbc ~ ., data = df, mtry = 30, importance = TRUE,      ntree = 500) 
# Type of random forest: regression
# Number of trees: 500
# No. of variables tried at each split: 30
# 
# Mean of squared residuals: 0.5698504
#% Var explained: 68.48

#Evaluate variable importance
tx <- model.matrix(fbc~. , -351, data=train )
ty <- train[, 351] # Only slop
mse_rf <- mean((df[,351] - rf1$predicted  )^2)
mse_rf

impRF<-rf1$importance    #worked
importance(rf1, type =1 )
impRF = importance(rf1, type =1)
impRF <- data.frame(predictors=rownames(impRF),impRF)
rf1$importanceSD
rf1 %>% summary
rf1$mse
# Order the predictor levels by importance 
# I guess %IncMSE of j'th is (mse(j)-mse0)/mse0 * 100%   so the higher number more importent

imp.sortRF <- arrange(impRF,desc(impRF$X.IncMSE))
imp.sortRF
imp.sortRF$predictors <- factor(imp.sortRF$predictors,levels=imp.sortRF$predictors)
imp.sortRF$predictors
# Select the top 20 predictors
imp.30<- imp.sortRF[1:30,]
print(imp.30)
write.csv(imp.30, "RandomForest_500t_30impvar.csv")
#write.csv(imp.20, "RandomForest_100t_20Var.csv")

#try to arrange by IncNodePurity
imp.sortRFNode <- arrange(impRF,desc(impRF$IncNodePurity))
imp.sortRFNode
imp.sortRFNode$predictors <- factor(imp.sortRFNode$predictors,levels=imp.sortRFNode$predictors)
imp.sortRNodeF$predictors
# Select the top 20 predictors
imp.30Node<- imp.sortRFNode[1:30,]
print(imp.30Node)
write.csv(imp.30Node, "RandomForest_500t_30impvarIncNodePurity.csv")

imp.max <- as.matrix(imp.sortRF)
plot(imp.sortRF[1:30,])
barplot(imp.sortRF[1:30,2])

# linear reg with RanForest vars
rfv <- c( "tem",
          "level",
          "cycling",
          "sfbc_b32wlsaup_drpost_pcs_",
          "vth_sgs_med_mv_",
          "sfbc_t32wlsalp_far_dc_fresh_pcs_",
          "sfbc_b32wlsalp_dc_post_pcs_",
          "sfbc_b32wlsalp_post_pcs_",
          "fbc_sdllk4_pcs_",
          "vf_sk_mv_",
          "crd_m1bll_224_c_pcs_",
          "sfbc_b32wlsalp_drpost_pcs_",
          "sfbc_t32wlsalp_far_fresh_pcs_",
          "tlcwc_we0_wl63_fr_s25_a2r_pcs_",
          "halfvccq33_t_mv_",
          "ron_odt_33n_t_ua_",
          "vreadhh_t_mv_",
          "vx4_t_mv_",
          "vcg_br3_t_mv_",
          "tlcwc_we0_wl31_fr_s25_a2r_pcs_",
          "vth_wlds1_med_mv_",
          "vpgmu_sgld_mon1_t_mv_",
          "veralooptrial1__",
          "eires20x1__",
          "vcgrv2_t_mv_",
          "sfbc_drtime_s_",
          "sfbc_t32wlsaerx_dc_lijl1_pcs_",
          "veramaxt_p_",
          "vth_sgdprog_win_mv_",
          "bbk_high_wldd0_ev_d_pcs_")
          
rfv %>% str
# subset of all data with selected  30 veriables
# need to convert level to the 3 col with binary 
df_m <- df
df_l <- model.matrix(~df$level-1, data=df$level)
df_l 
df_m <- cbind(df_m, df_l)
df_m %>% names
df_m <- df_m[, -353]
#df_m$levelMP <- rename(df_m$`df$levelMP`)
colnames(df_m)[353] <- "levelLP" 
colnames(df_m)[354] <- "levelMP"
colnames(df_m)[355] <- "levelUP"


rfv <- df %>% select(rfv)
rfv %>% dim
rfv %>% names

#un <- na.omit(un)
# add responce 
df$fbc
rfv$fbc <-df$fbc
write.csv(rfv, "RandomForest_18111row_subset.csv")

# Linear model without standardizing predictors
lmod_rfv <- lm(fbc~., rfv)
lmod_rfv
summary(lmod_rfv)
sum_lmod_rfv <-lmod_rfv %>% summary
capture.output(sum_lmod_rfv, file = "RF_GLM_crosstemp.txt")

plot(lmod_rfv)

# Linear model with standardizing predictors
rfv %>% dim
rfv %>% names
# standartization  # normalize

#rfv.norm <- transform(rfv, rfv.norm = ave(rfv, Area, FUN = scale))  not working
rfv1 <- rfv[, -2]
rfv1 %>% names
rfv_st <- lapply(rfv1, scale)

rfv$level <-as.numeric(rfv$level)
rfv1$level <- rfv$level
rfv1 %>% names
st_lmod_rfv <- lm(fbc~., rfv_st)
st_lmod_rfv %>% summary
plot(st_lmod_rfv)
rfv1

# 
# I proved that the percentage of variation explained by a given predictor in a multiple linear regression is the product of the 
#slope coefficient and the correlation of the predictor with the fitted values of the dependent variable (assuming that all variables have been standardized to have mean zero and variance one; which is without loss of generality). Find it here:
#   
#   https://www.researchgate.net/publication/306347340_A_Natural_Decomposition_of_R2_in_Multiple_Linear_Regression

# Percentage of vriance explaned  Contribution Percent (%) (Ssi/SSt)
af.rf <- anova(st_lmod_rfv)
afss.rf <- af.rf$"Sum Sq"     # there we have the incremental variance explained; how do we get the proportion?
#  trivially, scale them by 100 divided by their sum.
print(cbind(af.rf,PctExp=afss.rf/sum(afss.rf)*100))
result<-cbind(af.rf,PctExp=afss.rf/sum(afss.rf)*100)
# Df                                        Sum Sq      Mean Sq      F value        Pr(>F)       PctExp
# tem                                  1 8.551510e+03 8.551510e+03 2.066262e+04  0.000000e+00 4.721982e+01
# cycling                              1 5.968735e+02 5.968735e+02 1.442198e+03 1.079073e-303 3.295823e+00
# sfbc_b32wlsaup_drpost_pcs_           1 9.031899e+02 9.031899e+02 2.182336e+03  0.000000e+00 4.987244e+00
# vth_sgs_med_mv_                      1 2.217785e+02 2.217785e+02 5.358732e+02 7.424496e-117 1.224619e+00
# sfbc_t32wlsalp_far_dc_fresh_pcs_     1 1.148046e+02 1.148046e+02 2.773969e+02  8.009036e-62 6.339291e-01
# sfbc_b32wlsalp_dc_post_pcs_          1 6.108348e+00 6.108348e+00 1.475932e+01  1.225596e-04 3.372914e-02
# sfbc_b32wlsalp_post_pcs_             1 1.091024e+01 1.091024e+01 2.636192e+01  2.860028e-07 6.024431e-02
# fbc_sdllk4_pcs_                      1 3.839834e+01 3.839834e+01 9.278012e+01  6.596729e-22 2.120284e-01
# vf_sk_mv_                            1 4.284465e+00 4.284465e+00 1.035235e+01  1.295374e-03 2.365801e-02
# crd_m1bll_224_c_pcs_                 1 1.524785e+00 1.524785e+00 3.684268e+00  5.494379e-02 8.419577e-03
# sfbc_b32wlsalp_drpost_pcs_           1 2.491305e+01 2.491305e+01 6.019625e+01  9.040619e-15 1.375652e-01
# sfbc_t32wlsalp_far_fresh_pcs_        1 4.342758e-03 4.342758e-03 1.049321e-02  9.184115e-01 2.397989e-05
# tlcwc_we0_wl63_fr_s25_a2r_pcs_       1 4.314464e+00 4.314464e+00 1.042484e+01  1.245536e-03 2.382365e-02
# halfvccq33_t_mv_                     1 5.655703e+00 5.655703e+00 1.366561e+01  2.190631e-04 3.122972e-02
# ron_odt_33n_t_ua_                    1 7.589347e-01 7.589347e-01 1.833779e+00  1.756988e-01 4.190694e-03
# vreadhh_t_mv_                        1 6.860365e-01 6.860365e-01 1.657638e+00  1.979390e-01 3.788164e-03
# vx4_t_mv_                            1 2.150919e-01 2.150919e-01 5.197166e-01  4.709719e-01 1.187697e-03
# vcg_br3_t_mv_                        1 5.698646e+00 5.698646e+00 1.376937e+01  2.072975e-04 3.146685e-02
# tlcwc_we0_wl31_fr_s25_a2r_pcs_       1 3.031226e+00 3.031226e+00 7.324211e+00  6.809566e-03 1.673786e-02
# vth_wlds1_med_mv_                    1 9.154981e+00 9.154981e+00 2.212076e+01  2.579184e-06 5.055208e-02
# vpgmu_sgld_mon1_t_mv_                1 2.186870e+00 2.186870e+00 5.284032e+00  2.153321e-02 1.207548e-02
# veralooptrial1__                     1 2.448772e+00 2.448772e+00 5.916855e+00  1.500634e-02 1.352166e-02
# eires20x1__                          1 9.681785e-03 9.681785e-03 2.339365e-02  8.784396e-01 5.346099e-05
# vcgrv2_t_mv_                         1 1.405529e+00 1.405529e+00 3.396114e+00  6.536657e-02 7.761065e-03
# sfbc_drtime_s_                       1 4.584781e+00 4.584781e+00 1.107799e+01  8.753266e-04 2.531630e-02
# sfbc_t32wlsaerx_dc_lijl1_pcs_        1 8.092529e+00 8.092529e+00 1.955360e+01  9.838649e-06 4.468542e-02
# veramaxt_p_                          1 4.247503e-01 4.247503e-01 1.026304e+00  3.110418e-01 2.345391e-03
# vth_sgdprog_win_mv_                  1 2.909017e+00 2.909017e+00 7.028923e+00  8.027348e-03 1.606304e-02
# bbk_high_wldd0_ev_d_pcs_             1 2.887926e+00 2.887926e+00 6.977961e+00  8.259056e-03 1.594658e-02
# level                                1 9.857653e+01 9.857653e+01 2.381859e+02  2.142040e-53 5.443210e-01
# Residuals                        18080 7.482658e+03 4.138638e-01           NA            NA 4.131783e+01

#After removal of Error and residual (Pure SS should be caclulated I think)				
capture.output(result, file ="Contribution of union predictors.txt")
plot(result)
###   --------------------------------------PLOTS--------------------------
# 3D Scatterplot
library(scatterplot3d)
#attach(mtcars)
#scatterplot3d(wt,disp,mpg, main="3D Scatterplot")

scatterplot3d(ss$fbc, ss$cyclinng,ss$tem, main="3D Scatterplot")

# 3D Scatterplot with Coloring and Vertical Drop Lines

sc_plot1 <-scatterplot3d(ss$cyclinng,ss$tem,ss$fbc,  pch=16, highlight.3d=TRUE,
                         type="h", main="3D Cross Temperature")
# 3D Scatterplot with Coloring and Vertical Lines
# and Regression Plane 
rfv1
s3d <-scatterplot3d(rfv1$tem, rfv1$cyclinng, rfv1$fbc, pch=16, highlight.3d=TRUE,
                    type="h", main="3D Scatterplot cross temperature 
                    FBC growth rate in terms of an observable set ")
fit <- lm(df$fbc ~ ., data = df) 
s3d$plane3d(fit)
# try to catch outlies
which.max(ss2$fbc)
#2228  3827   3834
ss1[3827, "fbc"]
ss1 <- ss[-c(2228),]
ss2 <- ss1[-c(3827),]
ss3 <- ss2[-c(3834),]
s3d <-scatterplot3d(ss3$tem, ss3$cyclinng,ss3$fbc,  pch=16, highlight.3d=TRUE,
                    type="h", main="3D Scatterplot cross temperature 
                    FBC growth rate in terms of an observable set ")
rfv$cycling
rfv3d <-scatterplot3d(rfv$tem,rfv$cycling,rfv$fbc,  pch=16, highlight.3d=TRUE,
                    type="h", main="3D Scatterplot cross temperature 
                    FBC growth rate in terms of an observable set ")
# Spinning 3d Scatterplot
library(rgl)

plot3d(rfv$fbc, rfv$cycling, rfv$tem, col="red", size=3)
# Another Spinning 3d Scatterplot
library(Rcmdr)

scatter3d(rfv$fbc, rfv$cyclinng, rfv$tem)

# still have outliers
which.max(rfv$fbc)
#df[3827, 'fbc']  #99

#dfc <- df[-c(2228), ] 
rfv
s3d <-scatterplot3d(rfv$tem, rfv$cycling, rfv$fbc, pch=16, highlight.3d=TRUE,
                    type="h", main="3D Scatterplot cross temperature 
                    FBC growth rate in terms of an observable set ")
fit <- lm(rfv$fbc ~., rfv) 
s3d$plane3d(fit)

hist3D_fancy(rfv$tem, rfv$cycling, rfv$fbc, colvar=as.numeric(rfv$tem))
# Make the rgl version
library("plot3Drgl")
plotrgl(rfv)


# library(ggfortify)
# autoplot(lm(fbc ~ ., data = rfv_st), label.size = 3)
# m <- lm(fbc ~ ., data = rfv_st)
# m
# autoplot(m, which = 1:6, ncol = 2, label.size = 3)

#num_<-row.names(nzv1)[nzv1$nzv == TRUE]
#df<-df[, !names(df) %in% num_a]

words <- "tem,level,cycling,sfbc_b32wlsaup_drpost_pcs_,vth_sgs_med_mv_,sfbc_t32wlsalp_far_dc_fresh_pcs_,sfbc_b32wlsalp_dc_post_pcs_,sfbc_b32wlsalp_post_pcs_,fbc_sdllk4_pcs_,
vf_sk_mv_,
crd_m1bll_224_c_pcs_,
sfbc_b32wlsalp_drpost_pcs_,
sfbc_t32wlsalp_far_fresh_pcs_,
tlcwc_we0_wl63_fr_s25_a2r_pcs_,
halfvccq33_t_mv_,
ron_odt_33n_t_ua_,
vreadhh_t_mv_,
vx4_t_mv_,
vcg_br3_t_mv_,
tlcwc_we0_wl31_fr_s25_a2r_pcs_,
vth_wlds1_med_mv_,
vpgmu_sgld_mon1_t_mv_,
veralooptrial1__,
eires20x1__,
vcgrv2_t_mv_,
sfbc_drtime_s_,
sfbc_t32wlsaerx_dc_lijl1_pcs_,
veramaxt_p_,
vth_sgdprog_win_mv_,
bbk_high_wldd0_ev_d_pcs_"
sapply(strsplit(words, '[, ]+'), function(x) toString(dQuote(x)))


# plotting for prediction
pred<-predict(object=model1,newdata=test)
actual<-test$price
result<-data.frame(actual=actual,predicted=pred)
paste('Function Call: ', model1$call)

