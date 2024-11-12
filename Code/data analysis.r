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
