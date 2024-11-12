######################################################
########  effect of match on perceived fit  ##########
#  effect of overall match                           #
#      method 1: profile correlation                 #
#      method 2: random slope                        #
#  effect of one value  match 
#      method 1: level metric                        #  
#      method 2: RSA                                 #
######################################################

############    effect of overall match   ############
############        method 1       ###################
### Profile Correlation -- Corrected Pattern(after grand meaning)
### It used Correlation individual gramXX and region gramXXreg 
### Raw Pattern: XXcen and XXreg

### define zfisher function
zfisher <- function(r){
  return(0.5 * log((1 + r) / (1 - r)))
}
### cauculate Corrected Pattern Metric
fit_data <- fit_data %>%   #gramXX & gramXXreg -- 98
  rowwise() %>% 
  do(data.frame(., r_corrected= cor(unlist(.[109:118]), unlist(.[119:128]), method = c("pearson")))) %>%
  mutate(corrected_patmet = zfisher(r_corrected))

### cauculate Raw Pattern Metric
fit_data <- fit_data %>%   #XXcen & XXregcen -- 98
  rowwise() %>% 
  do(data.frame(., r_raw= cor(unlist(.[58:67]), unlist(.[79:88]), method = c("pearson")))) %>%
  mutate(raw_patmet = zfisher(r_raw))

### cauculate unispatized Raw Pattern Metric
fit_data <- fit_data %>%   #XXcen & XXregcen -- 98
  rowwise() %>% 
  do(data.frame(., r_unispatize_raw= cor(unlist(.[47:56]), unlist(.[68:77]), method = c("pearson")))) %>%
  mutate(raw_unispatize_patmet = zfisher(r_unispatize_raw))

### z-score to standalize the overall match
fit_data$z_subject_fit <- as.vector(scale(fit_data$subject_fit))
fit_data$z_corrected_patmet <- as.vector(scale(fit_data$corrected_patmet))
fit_data$z_raw_patmet <- as.vector(scale(fit_data$raw_patmet))
fit_data$z_per_slope <- as.vector(scale(fit_data$per_slope))

#write.csv(fit_data, './数据文件/0928data.csv', row.names = FALSE)
fit_data <- read.csv('./数据文件/0928data.csv')
colnames(fit_data)

############        method 2       ####################
### Data Pre-process
### personID and regionID need to be reserved
fit_data$ID <- 1:(dim(fit_data)[1])

### ipsatized but not grand mean -- random slope
sub_data1 <- fit_data %>% dplyr::select(ID,regionID,COcen,TRcen,BEcen,UNcen,SDcen,
         STcen,HEcen,ACcen,POcen,SEcen)
colnames(sub_data1)[3:12] <- c("CO","TR","BE","UN","SD",
                               "ST","HE","AC","PO","SE")
long_sub_data1 <- gather(sub_data1,key='item',
                         value='self_value',
                         -`regionID`, -`ID`)
sub_data2 <- fit_data %>%
  dplyr::select(ID,regionID,COregcen,TRregcen,BEregcen,UNregcen,SDregcen,
         STregcen,HEregcen,ACregcen,POregcen,SEregcen)
colnames(sub_data2)[3:12] <- c("CO","TR","BE","UN","SD",
                               "ST","HE","AC","PO","SE")
long_sub_data2 <- gather(sub_data2,key='item',
                         value='zcta_value',
                         -`regionID`, -`ID`)
sub_data3 <- fit_data %>%
  dplyr::select(ID,regionID,CO_mean,TR_mean,BE_mean,UN_mean,SD_mean,
         ST_mean,HE_mean,AC_mean,PO_mean,SE_mean)
colnames(sub_data3)[3:12] <- c("CO","TR","BE","UN","SD",
                               "ST","HE","AC","PO","SE")
long_sub_data3 <- gather(sub_data3,key='item',
                         value='normative',
                         -`regionID`, -`ID`)
pefit_data <- cbind(long_sub_data1,long_sub_data2$zcta_value,long_sub_data3$normative)
colnames(pefit_data)[5:6] <- c('zcta_value','normative')
write.csv(pefit_data,'ispatized_pefit_data.csv')

### establish hlm model
### Self Rating = ZCTA Average + Normative + e, each one has 10 ratings  ???
pefit <- lmer(self_value~normative+zcta_value+(1+zcta_value|regionID/ID), pefit_data,REML = FALSE) # g1/g2,g2 within g1,g2在g1中
summary(pefit)
save(pefit, file = "ipsatized_PE-fit1.RData")
# pefit 真的对吗
pefit2 <- lmer(self_value~normative+zcta_value+(1+zcta_value|regionID)+(1+zcta_value|ID), pefit_data,REML = FALSE)
summary(pefit2)  #pefit和pefit2是同等效力的

pefit3 <- lmer(self_value~normative+zcta_value+(zcta_value|regionID)+(zcta_value|ID), pefit_data,REML = FALSE)
summary(pefit3)
save(pefit3, file = "PE-fit2.RData")

### get random effect of each individual
rs_fuction <- function(model){
  random_slope <- ranef(model)
  random_slope_df <- as.data.frame(random_slope$`ID:regionID`)
  random_slope_df[c('ID', 'regionID')] <- str_split_fixed(rownames(random_slope_df),":", 2)
  random_slope_df2 <- as.data.frame(random_slope$`regionID`)
  random_slope_df2$regionID <- rownames(random_slope_df2)
  random_slope_df <- random_slope_df %>% 
    dplyr::select(-1) %>%
    rename(per_slope = zcta_value)
  random_slope_df2<- random_slope_df2 %>% 
    dplyr::select(-1)%>%
    rename(zcta_slope = zcta_value)
  random_slope_df <- merge(random_slope_df,random_slope_df2,on='regionID',x.all=TRUE)
  return(random_slope_df)
}
rs_df <- rs_fuction(pefit)
### add random slope effect of everyone into whole dataset
fit_data <- merge(fit_data,rs_df,on='ID',x.all=TRUE)

#Regression: perceived fit on overall objective match #
ov_ma1 <- lm(subject_fit ~ scale(corrected_patmet), fit_data)  #profile
summary(ov_ma1)
save(ov_ma1, file = "PatternMetric_fit.RData")
ov_ma2 <- lm(subject_fit ~ scale(per_slope), fit_data)  #random slope
summary(ov_ma2)
save(ov_ma2, file = "RandomSlope_fit.RData")
ov_ma3 <- lm(subject_fit ~ scale(raw_patmet), fit_data)  #profile
summary(ov_ma3)
ov_ma4 <- lm(subject_fit ~ scale(raw_unispatize_patmet), fit_data)  #profile
summary(ov_ma4)

############  effect of individual item  match #######
############     method 1-level metric    ############
############    step:1 interaction model    ##########
#######    step:2 simple slope analysis    ###########
# Define a function to analysis simple slope
Plot_LM <- function(z_n,x_n,y_n,df,
                    name_independent, name_dependent, name_moderate){
  result_list <- list()
  y = as.vector(df[z_n]) 
  xc = as.vector(df[x_n]) 
  mc = as.vector(df[y_n]) 
  county = as.vector(df['county']) 
  test_df = data.frame(y,xc,mc,county)
  colnames(test_df) <- c('Y','X_C','M_C','county')
  
  # fit the level metric
  model <- lmer(Y ~ X_C + M_C + X_C*M_C + (1 | county), data = test_df)
  summary_model <- summary(model)
  table_ssa <- NULL
  inter.sd <- NULL
  # test if to do simple slope analysis
  if(summary_model$coefficients[4,5] <0.05){
    table_ssa<- sim_slopes(model = model, pred = X_C, modx = M_C, confint = T, digits = 3)
    test_df = test_df
    inter.sd <- effect(c("X_C*M_C"), mod=model,
                       xlevels = list(M_C = c(mean(test_df$M_C)-sd(test_df$M_C),
                                              mean(test_df$M_C),
                                              mean(test_df$M_C)+sd(test_df$M_C)),
                                      X_C = c(mean(test_df$X_C)-sd(test_df$X_C),
                                              mean(test_df$X_C),
                                              mean(test_df$X_C)+sd(test_df$X_C))))
    inter.sd <- as.data.frame(inter.sd)
    inter.sd$M_C_label <- factor(inter.sd$M_C,
                                 levels = c(mean(test_df$M_C)-sd(test_df$M_C),
                                            mean(test_df$M_C),
                                            mean(test_df$M_C)+sd(test_df$M_C)),
                                 labels = c("High Moderator (M+SD)", 
                                            "Mean Moderator", 
                                            "Low Moderator (M-SD)"))
    inter.sd$X_C_label <- factor(inter.sd$X_C,
                                 levels = c(mean(test_df$X_C)-sd(test_df$X_C),
                                            mean(test_df$X_C),
                                            mean(test_df$X_C)+sd(test_df$X_C)),
                                 labels = c("High Predictor (M+SD)", 
                                            "Mean Predictor", 
                                            "Low Predictor (M-SD)"))
    plot.sd <- ggplot(data = inter.sd, aes(x=X_C, y=fit, group=M_C_label)) +
      geom_line(size=1, aes(linetype=M_C_label)) + #设定不同调节变量水平下线型不同
      geom_point(size=2) + #图中点的大小
      geom_ribbon(aes(ymin=fit-se, ymax=fit+se), fill="gray", alpha=.4)+ #在图中加入error bar，颜色为灰色，不透明度设为0.5
      ylab(name_dependent)+ #纵轴标题
      xlab(name_independent)+ #横轴标题
      theme_bw()+ #设置白色背景
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.key = element_blank())+ #将网格线去掉
      scale_fill_grey()+ #主题颜色为黑灰色
      theme(legend.position="right")+ 
      guides(linetype=guide_legend(title=name_moderate))+ #设置图例的标题
      theme(text=element_text(family="Times New Roman")) #设定图片中文字的字体
    print(plot.sd)
  }
  result_list$model_fit <- model
  result_list$table <- table_ssa
  result_list$inter_df <- inter.sd
  return(result_list)
}

# Level Metric: CO
LMetric_CO <-  Plot_LM('subject_fit', 'gramCO', 'gramCOreg',fit_data,
                       'perceived fit',
                       'individual-level conformity',
                       'couny-level conformity'
)
summary(LMetric_CO$model_fit)

# Level Metric: TR
LMetric_TR <- Plot_LM('subject_fit', 'gramTR', 'gramTRreg',fit_data,
                      'perceived fit',
                      'individual-level tradtion',
                      'couny-level tradtion'
)
summary(LMetric_TR$model_fit)

# Level Metric: BE
LMetric_BE <- Plot_LM('subject_fit', 'gramBE', 'gramBEreg',fit_data,
                      'perceived fit',
                      'individual-level benevolence',
                      'couny-level benevolence'
)
summary(LMetric_BE$model_fit)

# Level Metric: UN
LMetric_UN <- Plot_LM('subject_fit', 'gramUN', 'gramUNreg',fit_data,
                      'perceived fit',
                      'individual-level universalism',
                      'couny-level universalism'
)
summary(LMetric_UN$model_fit)

# Level Metric: SD
LMetric_SD <- Plot_LM('subject_fit', 'gramSD', 'gramSDreg',fit_data,
                      'perceived fit',
                      'individual-level self-direction',
                      'couny-level self-direction'
)
summary(LMetric_SD$model_fit)

# Level Metric: ST
LMetric_ST <- Plot_LM('subject_fit', 'gramST', 'gramSTreg',fit_data,
        'perceived fit',
        'individual-level stimulation',
        'couny-level stimulation'
)
summary(LMetric_ST$model_fit)

# Level Metric: HE
LMetric_HE <- Plot_LM('subject_fit', 'gramHE', 'gramHEreg',fit_data,
                      'perceived fit',
                      'individual-level hedonism',
                      'couny-level hedonism'
)
summary(LMetric_HE$model_fit)

# Level Metric: AC
LMetric_AC <- Plot_LM('subject_fit', 'gramAC', 'gramACreg',fit_data,
                      'perceived fit',
                      'individual-level achievement',
                      'couny-level achievement'
)
summary(LMetric_AC$model_fit)

# Level Metric: PO
LMetric_PO <- Plot_LM('subject_fit', 'gramPO', 'gramPOreg',fit_data,
                      'perceived fit',
                      'individual-level power',
                      'couny-level power'
)
summary(LMetric_PO$model_fit)

# Level Metric: SE
LMetric_SE <- Plot_LM('subject_fit', 'gramSE', 'gramSEreg',fit_data,
                      'perceived fit',
                      'individual-level security',
                      'couny-level security'
)
summary(LMetric_SE$model_fit)
fixef(LMetric_SE)

############       method 2-RSA       ################
# step1: calculate the interaction and quadratic term#
# individual: gram-XX                                #
# county average: gram-XX-reg                        #
######################################################
# define a function to fit the quantric model
RSA_fit_fx <- function(x, y, z, co_df){
  x2 <- x^2  
  y2 <- y^2
  xy <- x*y
  co_df <- cbind(x,y,z,x2,y2,xy,co_df)  #co_df contain the value of region's name
  m.fit <- lmer(z ~ x + y + x2 + xy + y2 + (1 | county), data = co_df)
  return_list <- list()
  return_list$fit_model <- m.fit
  return_list$all_df <- co_df
  return(return_list)
}
# ---------------------------
# Test CO - conformity
CO_fit<- RSA_fit_fx(fit_data$gramCO,fit_data$gramCOreg,
           fit_data$subject_fit,fit_data[c('age','gender','county')])
summary(CO_fit$fit_model)
# RSA analysis
MLRSA_AverageSurface(CO_fit$fit_model, 
                     name_vars=c("x","y","x2","xy","y2"),
                     random_vars=c(NA,NA,NA,NA,NA))
# RSA plot
MLRSA_AverageSurfacePlot(CO_fit$fit_model, 
                         name_vars=c("x","y","x2","xy","y2"),
                         outcome="subject_fit",
                         data=CO_fit$all_df, param=F,
                         xlab="Individual-level values", ylab="County-level values", zlab="Perceived Fit",main="conformity")

# ---------------------------
# Test TR - tradition
TR_fit<- RSA_fit_fx(fit_data$gramTR,fit_data$gramTRreg,
                    fit_data$subject_fit,fit_data[c('age','gender','county','subject_fit')])
summary(TR_fit$fit_model)
# RSA analysis
MLRSA_AverageSurface(TR_fit$fit_model, 
                     name_vars=c("x","y","x2","xy","y2"),
                     random_vars=c(NA,NA,NA,NA,NA))
# RSA plot
MLRSA_AverageSurfacePlot(TR_fit$fit_model, 
                         name_vars=c("x","y","x2","xy","y2"),
                         outcome="subject_fit",
                         data=TR_fit$all_df, param=F,
                         xlab="Individual-level values", ylab="County-level values", zlab="Perceived Fit",main="tradition")

# ---------------------------
# Test BE - benevolence
BE_fit<- RSA_fit_fx(fit_data$gramBE,fit_data$gramBEreg,
                    fit_data$subject_fit,fit_data[c('age','gender','county','subject_fit')])
summary(BE_fit$fit_model)
# RSA analysis
MLRSA_AverageSurface(BE_fit$fit_model, 
                     name_vars=c("x","y","x2","xy","y2"),
                     random_vars=c(NA,NA,NA,NA,NA))
# RSA plot
MLRSA_AverageSurfacePlot(BE_fit$fit_model, 
                         name_vars=c("x","y","x2","xy","y2"),
                         outcome="subject_fit",
                         data=BE_fit$all_df, param=F,
                         xlab="Individual-level values", ylab="County-level values", zlab="Perceived Fit",main="benevolence")

# ---------------------------
# Test UN - universalism
UN_fit<- RSA_fit_fx(fit_data$gramUN,fit_data$gramUNreg,
                    fit_data$subject_fit,fit_data[c('age','gender','county','subject_fit')])
summary(UN_fit$fit_model)
# RSA analysis
MLRSA_AverageSurface(UN_fit$fit_model, 
                     name_vars=c("x","y","x2","xy","y2"),
                     random_vars=c(NA,NA,NA,NA,NA))
# RSA plot
MLRSA_AverageSurfacePlot(UN_fit$fit_model, 
                         name_vars=c("x","y","x2","xy","y2"),
                         outcome="subject_fit",
                         data=UN_fit$all_df, param=F,
                         xlab="Individual-level values", ylab="County-level values", zlab="Perceived Fit",main="universalism")

# ---------------------------
# Test SD - self-direction
SD_fit<- RSA_fit_fx(fit_data$gramSD,fit_data$gramSDreg,
                    fit_data$subject_fit,fit_data[c('age','gender','county','subject_fit')])
summary(SD_fit$fit_model)
# RSA analysis
MLRSA_AverageSurface(SD_fit$fit_model, 
                     name_vars=c("x","y","x2","xy","y2"),
                     random_vars=c(NA,NA,NA,NA,NA))
# RSA plot
MLRSA_AverageSurfacePlot(SD_fit$fit_model, 
                         name_vars=c("x","y","x2","xy","y2"),
                         outcome="subject_fit",
                         data=SD_fit$all_df, param=F,
                         xlab="Individual-level values", ylab="County-level values", zlab="Perceived Fit",main="self-direction")

# ---------------------------
# Test ST - stimulation
ST_fit<- RSA_fit_fx(fit_data$gramST,fit_data$gramSTreg,
                    fit_data$subject_fit,fit_data[c('age','gender','county','subject_fit')])
summary(SD_fit$fit_model)
# RSA analysis
MLRSA_AverageSurface(ST_fit$fit_model, 
                     name_vars=c("x","y","x2","xy","y2"),
                     random_vars=c(NA,NA,NA,NA,NA))
# RSA plot
MLRSA_AverageSurfacePlot(ST_fit$fit_model, 
                         name_vars=c("x","y","x2","xy","y2"),
                         outcome="subject_fit",
                         data=ST_fit$all_df, param=F,
                         xlab="Individual-level values", ylab="County-level values", zlab="Perceived Fit",main="stimulation")

# ---------------------------
# Test HE - hedonism
HE_fit<- RSA_fit_fx(fit_data$gramHE,fit_data$gramHEreg,
                    fit_data$subject_fit,fit_data[c('age','gender','county','subject_fit')])
summary(HE_fit$fit_model)
# RSA analysis
MLRSA_AverageSurface(HE_fit$fit_model, 
                     name_vars=c("x","y","x2","xy","y2"),
                     random_vars=c(NA,NA,NA,NA,NA))
# RSA plot
MLRSA_AverageSurfacePlot(HE_fit$fit_model, 
                         name_vars=c("x","y","x2","xy","y2"),
                         outcome="subject_fit",
                         data=HE_fit$all_df, param=F,
                         xlab="Individual-level values", ylab="County-level values", zlab="Perceived Fit",main="hedonism")

# ---------------------------
# Test AC - achievement
AC_fit<- RSA_fit_fx(fit_data$gramAC,fit_data$gramACreg,
                    fit_data$subject_fit,fit_data[c('age','gender','county','subject_fit')])
summary(AC_fit$fit_model)
# RSA analysis
MLRSA_AverageSurface(AC_fit$fit_model, 
                     name_vars=c("x","y","x2","xy","y2"),
                     random_vars=c(NA,NA,NA,NA,NA))
# RSA plot
MLRSA_AverageSurfacePlot(AC_fit$fit_model, 
                         name_vars=c("x","y","x2","xy","y2"),
                         outcome="subject_fit",
                         data=AC_fit$all_df, param=F,
                         xlab="Individual-level values", ylab="County-level values", zlab="Perceived Fit",main="achievement")

# ---------------------------
# Test PO - power
PO_fit<- RSA_fit_fx(fit_data$gramPO,fit_data$gramPOreg,
                    fit_data$subject_fit,fit_data[c('age','gender','county','subject_fit')])
summary(PO_fit$fit_model)
# RSA analysis
MLRSA_AverageSurface(PO_fit$fit_model, 
                     name_vars=c("x","y","x2","xy","y2"),
                     random_vars=c(NA,NA,NA,NA,NA))
# RSA plot
MLRSA_AverageSurfacePlot(PO_fit$fit_model, 
                         name_vars=c("x","y","x2","xy","y2"),
                         outcome="subject_fit",
                         data=PO_fit$all_df, param=F,
                         xlab="Individual-level values", ylab="County-level values", zlab="Perceived Fit",main="power")

# ---------------------------
# Test SE - security
SE_fit<- RSA_fit_fx(fit_data$gramSE,fit_data$gramSEreg,
                    fit_data$subject_fit,fit_data[c('age','gender','county','subject_fit')])
summary(SE_fit$fit_model)
# RSA analysis
MLRSA_AverageSurface(SE_fit$fit_model, 
                     name_vars=c("x","y","x2","xy","y2"),
                     random_vars=c(NA,NA,NA,NA,NA))
# RSA plot
MLRSA_AverageSurfacePlot(SE_fit$fit_model, 
                         name_vars=c("x","y","x2","xy","y2"),
                         outcome="subject_fit",
                         data=SE_fit$all_df, param=F,
                         xlab="Individual-level values", ylab="County-level values", zlab="Perceived Fit",main="power")


# ####################################################
####   effect of match and fit on self-esteem    #####
#  step1: overall match + fit -> Self-Esteem         #
#                                                    #
#  ·Regression with interaction between match & fit  #
#      - overall:random slope                        #
#      - overall:pattern metric                      #
#  ·RSA                                              #
#      - overall:random slope                        #
#      - overall:pattern metric                      #
#                                                    #
#  step2: individual match + fit -> Self-Esteem      #
# (build on the basis of regression of match to fit )#
#      (perceived fit acts as a covarite )           #
#      method 1: level metric                        #  
#      method 2: RSA                                 #
######################################################
####    overall match + fit -> Self-Esteem      ######
# ------------------------------------
####  Regression
ov_fit_se1 <- lm(self_esteem ~ scale(corrected_patmet)*scale(subject_fit), fit_data)  #corrected_profile
summary(ov_fit_se1)
save(ov_ma1, file = "PatternMetric_fit.RData")
ov_fit_se2 <- lm(self_esteem ~ scale(per_slope)*scale(subject_fit), fit_data)  #random slope
summary(ov_fit_se2)
ov_fit_se3 <- lm(self_esteem ~ scale(raw_patmet)*scale(subject_fit), fit_data)  #raw_profile
summary(ov_fit_se3)
ov_fit_se4 <- lm(self_esteem ~ scale(raw_unispatize_patmet) + scale(subject_fit), fit_data)  #raw_profile
summary(ov_fit_se4) #raw_unispatize_patmet 和 raw_patmet 效应一样

# -----------------------------------
####   RSA
#### match: Corrected Pattern Metric
cpatmet_fit_rsa <- RSA_fit_fx(fit_data$z_corrected_patmet,fit_data$z_subject_fit,
                    fit_data$self_esteem,fit_data[c('age','gender','county','self_esteem')])
summary(cpatmet_fit_rsa$fit_model)
# RSA analysis
MLRSA_AverageSurface(cpatmet_fit_rsa$fit_model, 
                     name_vars=c("x","y","x2","xy","y2"),
                     random_vars=c(NA,NA,NA,NA,NA))
# RSA plot
MLRSA_AverageSurfacePlot(cpatmet_fit_rsa$fit_model, 
                         name_vars=c("x","y","x2","xy","y2"),
                         outcome="self_esteem",
                         data=cpatmet_fit_rsa$all_df, param=F,
                         xlab="Overall Match", ylab="Perceived Fit", zlab="Self Esteem", 
                         main="Corrected Pattern Metric")

#### match: Random Slope Metric
radslp_fit_rsa <- RSA_fit_fx(fit_data$z_per_slope,fit_data$z_subject_fit,
                              fit_data$self_esteem,fit_data[c('age','gender','county','self_esteem')])
summary(radslp_fit_rsa$fit_model)
# RSA analysis
MLRSA_AverageSurface(radslp_fit_rsa$fit_model, 
                     name_vars=c("x","y","x2","xy","y2"),
                     random_vars=c(NA,NA,NA,NA,NA))
# RSA plot
MLRSA_AverageSurfacePlot(radslp_fit_rsa$fit_model, 
                         name_vars=c("x","y","x2","xy","y2"),
                         outcome="self_esteem",
                         data=radslp_fit_rsa$all_df, param=F,
                         xlab="Overall Match", ylab="Perceived Fit", zlab="Self Esteem", 
                         main="Random Slope Metric")

# --------------------------------------------
###  step2: individual match + fit -> Self-Esteem  ###
### Level Metric with Perceived Fit
Plot_LM_withfit <- function(z_n, x_n, y_n, df,
                    name_independent, name_dependent, name_moderate){
  result_list <- list()
  y = as.vector(df[z_n]) 
  xc = as.vector(df[x_n]) 
  mc = as.vector(df[y_n]) 
  zsf = as.vector(df['z_subject_fit']) 
  county = as.vector(df['county']) 
  temp_df = data.frame(y,xc,mc,zsf,county)
  colnames(temp_df) <- c('Y','X_C','M_C','z_subfit','county')
  # fit the level metric
  model <- lmer(Y ~ X_C + M_C + X_C*M_C + z_subfit + (1 | county), data = temp_df)
  summary_model <- summary(model)
  table_ssa <- NULL
  inter.sd <- NULL
  # test if to do simple slope analysis
  if(summary_model$coefficients[5,5] <0.05){
    table_ssa<- sim_slopes(model = model, pred = X_C, modx = M_C, confint = T, digits = 3)
    inter.sd <- effect(c("X_C*M_C"), mod=model,
                       xlevels = list(M_C = c(mean(temp_df$M_C)-sd(temp_df$M_C),
                                              mean(temp_df$M_C),
                                              mean(temp_df$M_C)+sd(temp_df$M_C)),
                                      X_C = c(mean(temp_df$X_C)-sd(temp_df$X_C),
                                              mean(temp_df$X_C),
                                              mean(temp_df$X_C)+sd(temp_df$X_C))))
    inter.sd <- as.data.frame(inter.sd)
    inter.sd$M_C_label <- factor(inter.sd$M_C,
                                 levels = c(mean(temp_df$M_C)-sd(temp_df$M_C),
                                            mean(temp_df$M_C),
                                            mean(temp_df$M_C)+sd(temp_df$M_C)),
                                 labels = c("High Moderator (M+SD)", 
                                            "Mean Moderator", 
                                            "Low Moderator (M-SD)"))
    inter.sd$X_C_label <- factor(inter.sd$X_C,
                                 levels = c(mean(temp_df$X_C)-sd(temp_df$X_C),
                                            mean(temp_df$X_C),
                                            mean(temp_df$X_C)+sd(temp_df$X_C)),
                                 labels = c("High Predictor (M+SD)", 
                                            "Mean Predictor", 
                                            "Low Predictor (M-SD)"))
    plot.sd <- ggplot(data = inter.sd, aes(x=X_C, y=fit, group=M_C_label)) +
      geom_line(size=1, aes(linetype=M_C_label)) + #设定不同调节变量水平下线型不同
      geom_point(size=2) + #图中点的大小
      geom_ribbon(aes(ymin=fit-se, ymax=fit+se), fill="gray", alpha=.4)+ #在图中加入error bar，颜色为灰色，不透明度设为0.5
      ylab(name_dependent)+ #纵轴标题
      xlab(name_independent)+ #横轴标题
      theme_bw()+ #设置白色背景
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.key = element_blank())+ #将网格线去掉
      scale_fill_grey()+ #主题颜色为黑灰色
      theme(legend.position="right")+ 
      guides(linetype=guide_legend(title=name_moderate))+ #设置图例的标题
      theme(text=element_text(family="Times New Roman")) #设定图片中文字的字体
    print(plot.sd)
  }
  result_list$model_fit <- model
  result_list$table <- table_ssa
  result_list$inter_df <- inter.sd
  return(result_list)
}

# Level Metric: CO
LM_CO_withfit <- Plot_LM_withfit('self_esteem', 'gramCO', 'gramCOreg',df = fit_data,
                         'self_esteem',
                         'individual-level conformity',
                         'couny-level conformity'
  )
summary(LM_CO_withfit$model_fit)

# Level Metric: TR
LM_TR_withfit <- Plot_LM_withfit('self_esteem', 'gramTR', 'gramTRreg',df = fit_data,
                      'self_esteem',
                      'individual-level tradtion',
                      'couny-level tradtion'
)
summary(LM_TR_withfit$model_fit)

# Level Metric: BE
LM_BE_withfit <- Plot_LM_withfit('self_esteem', 'gramBE', 'gramBEreg',fit_data,
                      'self_esteem',
                      'individual-level benevolence',
                      'couny-level benevolence'
)
summary(LM_BE_withfit$model_fit)

# Level Metric: UN
LM_UN_withfit <- Plot_LM_withfit ('self_esteem', 'gramUN', 'gramUNreg',fit_data,
                      'self_esteem',
                      'individual-level universalism',
                      'couny-level universalism'
)
summary(LM_UN_withfit$model_fit)

# Level Metric: SD
LM_SD_withfit <- Plot_LM_withfit ('self_esteem', 'gramSD', 'gramSDreg',fit_data,
                      'self_esteem',
                      'individual-level self-direction',
                      'couny-level self-direction'
)
summary(LM_SD_withfit$model_fit)

# Level Metric: ST
LM_ST_withfit <- Plot_LM_withfit('self_esteem', 'gramST', 'gramSTreg',fit_data,
                      'self_esteem',
                      'individual-level stimulation',
                      'couny-level stimulation'
)
summary(LM_ST_withfit$model_fit)

# Level Metric: HE
LM_HE_withfit <- Plot_LM_withfit('self_esteem', 'gramHE', 'gramHEreg',fit_data,
                      'self_esteem',
                      'individual-level hedonism',
                      'couny-level hedonism'
)
summary(LM_HE_withfit$model_fit)

# Level Metric: AC
LM_AC_withfit <- Plot_LM_withfit('self_esteem', 'gramAC', 'gramACreg',fit_data,
                      'self_esteem',
                      'individual-level achievement',
                      'couny-level achievement'
)
summary(LM_AC_withfit$model_fit)

# Level Metric: PO
LM_PO_withfit <- Plot_LM_withfit('self_esteem', 'gramPO', 'gramPOreg',fit_data,
                      'self_esteem',
                      'individual-level power',
                      'couny-level power'
)
summary(LM_PO_withfit$model_fit)

# Level Metric: SE
LM_SE_withfit <- Plot_LM_withfit('self_esteem', 'gramSE', 'gramSEreg',fit_data,
                      'self_esteem',
                      'individual-level security',
                      'couny-level security'
)
summary(LM_SE_withfit$model_fit)

# ------------------------------------
### RSA with Perceived Fit
# ---------------------------
RSA_wf_fit_fx <- function(x, y, z, co_df){
  x2 <- x^2  
  y2 <- y^2
  xy <- x*y
  co_df <- cbind(x,y,z,x2,y2,xy,co_df)  #co_df contain the value of region's name
  m.fit <- lmer(z ~ x + y + x2 + xy + y2 + z_subject_fit + (1 | county), data = co_df)
  return_list <- list()
  return_list$fit_model <- m.fit
  return_list$all_df <- co_df
  return(return_list)
}
# Test CO - conformity
CO_wf_fit<- RSA_wf_fit_fx(fit_data$gramCO,fit_data$gramCOreg,
                    fit_data$self_esteem,fit_data[c('county','z_subject_fit',"self_esteem")])
summary(CO_wf_fit$fit_model)
# RSA analysis
MLRSA_AverageSurface(CO_wf_fit$fit_model, 
                     name_vars=c("x","y","x2","xy","y2"),
                     random_vars=c(NA,NA,NA,NA,NA))
# RSA plot
MLRSA_AverageSurfacePlot(CO_wf_fit$fit_model, 
                         name_vars=c("x","y","x2","xy","y2"),
                         outcome="self_esteem",
                         data=CO_wf_fit$all_df, param=F,
                         xlab="Individual-level values", ylab="County-level values", zlab="Self Esteem",main="conformity")

# ---------------------------
# Test TR - tradition
TR_wf_fit<- RSA_wf_fit_fx(fit_data$gramTR,fit_data$gramTRreg,
                          fit_data$self_esteem,fit_data[c('county','z_subject_fit',"self_esteem")])
summary(TR_wf_fit$fit_model)
# RSA analysis
MLRSA_AverageSurface(TR_wf_fit$fit_model, 
                     name_vars=c("x","y","x2","xy","y2"),
                     random_vars=c(NA,NA,NA,NA,NA))
# RSA plot
MLRSA_AverageSurfacePlot(TR_wf_fit$fit_model, 
                         name_vars=c("x","y","x2","xy","y2"),
                         outcome="self_esteem",
                         data=TR_wf_fit$all_df, param=F,
                         xlab="Individual-level values", ylab="County-level values", zlab="Self Esteem",main="tradition")

# ---------------------------
# Test BE - benevolence
BE_wf_fit<- RSA_wf_fit_fx(fit_data$gramBE,fit_data$gramBEreg,
                          fit_data$self_esteem,fit_data[c('county','z_subject_fit',"self_esteem")])
summary(BE_wf_fit$fit_model)
# RSA analysis
MLRSA_AverageSurface(BE_wf_fit$fit_model, 
                     name_vars=c("x","y","x2","xy","y2"),
                     random_vars=c(NA,NA,NA,NA,NA))
# RSA plot
MLRSA_AverageSurfacePlot(BE_wf_fit$fit_model, 
                         name_vars=c("x","y","x2","xy","y2"),
                         outcome="self_esteem",
                         data=BE_wf_fit$all_df, param=F,
                         xlab="Individual-level values", ylab="County-level values", zlab="Self Esteem",main="benevolence")

# ---------------------------
# Test UN - universalism
UN_wf_fit<- RSA_wf_fit_fx(fit_data$gramUN,fit_data$gramUNreg,
                          fit_data$self_esteem,fit_data[c('county','z_subject_fit',"self_esteem")])
summary(UN_wf_fit$fit_model)
# RSA analysis
MLRSA_AverageSurface(UN_wf_fit$fit_model, 
                     name_vars=c("x","y","x2","xy","y2"),
                     random_vars=c(NA,NA,NA,NA,NA))
# RSA plot
MLRSA_AverageSurfacePlot(UN_wf_fit$fit_model, 
                         name_vars=c("x","y","x2","xy","y2"),
                         outcome="self_esteem",
                         data=UN_wf_fit$all_df, param=F,
                         xlab="Individual-level values", ylab="County-level values", zlab="Self Esteem",main="universalism")

# ---------------------------
# Test SD - self-direction
SD_wf_fit<- RSA_wf_fit_fx(fit_data$gramSD,fit_data$gramSDreg,
                          fit_data$self_esteem,fit_data[c('county','z_subject_fit',"self_esteem")])
summary(SD_wf_fit$fit_model)
# RSA analysis
MLRSA_AverageSurface(SD_wf_fit$fit_model, 
                     name_vars=c("x","y","x2","xy","y2"),
                     random_vars=c(NA,NA,NA,NA,NA))
# RSA plot
MLRSA_AverageSurfacePlot(SD_wf_fit$fit_model, 
                         name_vars=c("x","y","x2","xy","y2"),
                         outcome="self_esteem",
                         data=SD_wf_fit$all_df, param=F,
                         xlab="Individual-level values", ylab="County-level values", zlab="Self Esteem",main="self-direction")

# ---------------------------
# Test ST - stimulation
ST_wf_fit<- RSA_wf_fit_fx(fit_data$gramST,fit_data$gramSTreg,
                          fit_data$self_esteem,fit_data[c('county','z_subject_fit',"self_esteem")])
summary(ST_wf_fit$fit_model)
# RSA analysis
MLRSA_AverageSurface(ST_wf_fit$fit_model, 
                     name_vars=c("x","y","x2","xy","y2"),
                     random_vars=c(NA,NA,NA,NA,NA))
# RSA plot
MLRSA_AverageSurfacePlot(ST_wf_fit$fit_model, 
                         name_vars=c("x","y","x2","xy","y2"),
                         outcome="self_esteem",
                         data=ST_wf_fit$all_df, param=F,
                         xlab="Individual-level values", ylab="County-level values", zlab="Self Esteem",main="stimulation")

# ---------------------------
# Test HE - hedonism
HE_wf_fit<- RSA_wf_fit_fx(fit_data$gramHE,fit_data$gramHEreg,
                          fit_data$self_esteem,fit_data[c('county','z_subject_fit',"self_esteem")])
summary(HE_wf_fit$fit_model)
# RSA analysis
MLRSA_AverageSurface(HE_wf_fit$fit_model, 
                     name_vars=c("x","y","x2","xy","y2"),
                     random_vars=c(NA,NA,NA,NA,NA))
# RSA plot
MLRSA_AverageSurfacePlot(HE_wf_fit$fit_model, 
                         name_vars=c("x","y","x2","xy","y2"),
                         outcome="self_esteem",
                         data=HE_wf_fit$all_df, param=F,
                         xlab="Individual-level values", ylab="County-level values", zlab="Self Esteem",main="hedonism")

# ---------------------------
# Test AC - achievement
AC_wf_fit<- RSA_wf_fit_fx(fit_data$gramAC,fit_data$gramACreg,
                          fit_data$self_esteem,fit_data[c('county','z_subject_fit',"self_esteem")])
summary(AC_wf_fit$fit_model)
# RSA analysis
MLRSA_AverageSurface(AC_wf_fit$fit_model, 
                     name_vars=c("x","y","x2","xy","y2"),
                     random_vars=c(NA,NA,NA,NA,NA))
# RSA plot
MLRSA_AverageSurfacePlot(AC_wf_fit$fit_model, 
                         name_vars=c("x","y","x2","xy","y2"),
                         outcome="self_esteem",
                         data=AC_wf_fit$all_df, param=F,
                         xlab="Individual-level values", ylab="County-level values", zlab="Self Esteem",main="achievement")

# ---------------------------
# Test PO - power
PO_wf_fit<- RSA_wf_fit_fx(fit_data$gramPO,fit_data$gramPOreg,
                          fit_data$self_esteem,fit_data[c('county','z_subject_fit',"self_esteem")])
summary(PO_wf_fit$fit_model)
# RSA analysis
MLRSA_AverageSurface(PO_wf_fit$fit_model, 
                     name_vars=c("x","y","x2","xy","y2"),
                     random_vars=c(NA,NA,NA,NA,NA))
# RSA plot
MLRSA_AverageSurfacePlot(PO_wf_fit$fit_model, 
                         name_vars=c("x","y","x2","xy","y2"),
                         outcome="self_esteem",
                         data=PO_wf_fit$all_df, param=F,
                         xlab="Individual-level values", ylab="County-level values", zlab="Self Esteem",main="power")

# ---------------------------
# Test SE - security
SE_wf_fit<- RSA_wf_fit_fx(fit_data$gramSE,fit_data$gramSEreg,
                          fit_data$self_esteem,fit_data[c('county','z_subject_fit',"self_esteem")])
summary(SE_wf_fit$fit_model)
# RSA analysis
MLRSA_AverageSurface(SE_wf_fit$fit_model, 
                     name_vars=c("x","y","x2","xy","y2"),
                     random_vars=c(NA,NA,NA,NA,NA))
# RSA plot
MLRSA_AverageSurfacePlot(SE_wf_fit$fit_model, 
                         name_vars=c("x","y","x2","xy","y2"),
                         outcome="self_esteem",
                         data=SE_wf_fit$all_df, param=F,
                         xlab="Individual-level values", ylab="County-level values", zlab="Self Esteem",main="security")
