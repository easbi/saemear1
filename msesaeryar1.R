library(rio)
library(lme4)
library(dplyr)

# tes par -----------------------------------------------------------------
formula = y_star~x1+x2
data= import("Data_2.xlsx") %>% mutate(x2=scale(x2))

t=~t
w=~w
area = ~Area


# fungsi ------------------------------------------------------------------
msesaemear1 = function(formula, t, area, w, data, ME=TRUE){
  # Constants ----------------------------------------------------------------
  T_<-2
  
  # Persiapan Data ----------------------------------------------------------
  
  df=cbind(
    model.frame(formula, data),
    model.frame(t, data),
    model.frame(area, data),
    model.frame(w, data)
  )
  
  y=as.character(formula)[2]
  x=as.character(formula)[3]
  t=as.character(t)[2]
  area=as.character(area)[2]
  w=as.character(w)[2]
  m<-length(unique(df %>% pull(area)))
  
  formula_full = paste0(
    paste(format(formula)), " + ",
    "(1|", t, ")", " + ",
    "(1|", area, ")", " + ",
    "(1|", w, ")"
  )
  
  out_me=saeryme1(
    formula = y_star~x1+x2,
    t = ~t,
    area = ~Area,
    w = ~w,
    data = data, 
    ME = T
  )
  
  out_nme=saeryme1(
    formula = y_star~x1+x2,
    t = ~t,
    area = ~Area,
    w = ~w,
    data = data, 
    ME = F
  )
  
  
  # Persiapan Bootstrap -----------------------------------------------------------------
  m<-length(unique(df %>% pull(area)))
  
  n_frame=data %>% group_by(!!as.symbol(area)) %>% summarise(ni=n()) %>% mutate(ni1=ni/2, ni2=ni1)
  
  ni1=n_frame %>% pull(ni1)
  ni2=n_frame %>% pull(ni1)
  
  n_gab=c(ni1, ni2)
  
  ni=n_frame %>% pull(ni)
  w_bar = out_me$w_bar
  Ragam_Uit = out_me$Ragam_Uit
  Beta = out_me$beta
  Beta_fix = out_nme$beta
  Alpha = out_me$Alpha
  
  n<-sum(ni)
  n1<-sum(n_frame %>% pull(ni1))
  n2<-sum(n_frame %>% pull(ni2))
  Sigma_v = out_me$Sigma2_v; Sigma_w = out_me$Sigma2_w; Sigma_e = out_me$Sigma2_e
  vi_bintang<-rnorm(m,0,Sigma_v)
  vij_bintang<-as.matrix(cbind(rep(vi_bintang,ni)))
  wi_bintang<-rnorm(m,w_bar,Sigma_w)
  witj_bintang<-rep(c(wi_bintang,wi_bintang),n_gab)
  eitj_bintang<-NULL
  for(i in n_gab){
    eitj_bintang<-c(eitj_bintang,rnorm(i,0,Sigma_e))
  }
  ui_bintang<-rnorm(m,0,Ragam_Uit)
  uij_bintang<-rep(c(ui_bintang,ui_bintang),n_gab)
  X<-as.matrix(model.matrix(formula,df))
  yitj_bintang<-X%*%Beta+witj_bintang*Alpha+vij_bintang+uij_bintang+eitj_bintang
  
  mean_area=df %>% group_by(!!as.symbol(area)) %>% summarise_all(mean) %>% select(-t)
  X1_bar_i=mean_area %>% pull(x1)
  X2_bar_i=mean_area %>% pull(x1)
  w_bar_i = mean_area %>% pull(w)
  
  B<-1000
  dtsample1b <- list()
  Hasil_y_bar_i_duga_btg<-NULL;
  Hasil_y_bar_i_duga_btg_fix<-NULL
  Hasil_Bias_Relatif_SAE_AR1_ME<-NULL;
  Hasil_mse_SAE_AR1_ME<-NULL;
  Hasil_Bias_Relatif_SAE_AR1<-NULL;
  Hasil_mse_SAE_AR1<-NULL
  
  for (k in 1:B) {

    # Membuat versi bootstrap dari target parameter ---------------------------
    vi_bintang_bs<-rnorm(m,0,Sigma_v)
    ui_bintang_bs<-rnorm(m,0,Ragam_Uit)
    Alpha.mu_w_area<-as.matrix(cbind(rep(out_me$Alpha.mu_w,m)))  #####Alpha.mu
    Satu_area<-as.matrix(cbind(rep(1,m)))
    
    X_area_bs<-as.matrix(cbind(Satu_area,X1_bar_i,X2_bar_i)) #untuk me
    X_area_bs_fix<-as.matrix(cbind(Satu_area,X1_bar_i,X2_bar_i,w_bar_i)) #untuk non me
    
    y_bar_i_duga_btg_l<-X_area_bs%*%Beta+Alpha.mu_w_area+vi_bintang_bs+ui_bintang_bs #untuk me
    y_bar_i_duga_btg_l_fix<-X_area_bs_fix%*%Beta_fix+vi_bintang_bs+ui_bintang_bs  #untuk non me
    
    y_bar_i_duga_btg<-exp(y_bar_i_duga_btg_l)*exp(0.5*out_me$Ragam_y_vi_ui)
    Hasil_y_bar_i_duga_btg<-cbind(Hasil_y_bar_i_duga_btg,y_bar_i_duga_btg)
    y_bar_i_duga_btg_fix<-exp(y_bar_i_duga_btg_l_fix)*exp(0.5*out_nme$Ragam_y_vi_ui_fix)
    Hasil_y_bar_i_duga_btg_fix<-cbind(Hasil_y_bar_i_duga_btg_fix,y_bar_i_duga_btg_fix)
    

    # Membuat versi bootstrap dari EBLUP --------------------------------------
    Data_2<-data.frame(Area = df[area],
                       y_star = yitj_bintang,
                       t = data %>% pull(t),
                       x1 = data %>% pull(x1) ,
                       x2 = data %>% pull(x2),
                       witj_bintang)
    
    out_me_boost=saeryme1(
      formula = y_star~x1+x2,
      t = ~t,
      area = ~Area,
      w = ~witj_bintang,
      data = Data_2, 
      ME = T
    )
    
    out_nme_boost=saeryme1(
      formula = y_star~x1+x2,
      t = ~t,
      area = ~Area,
      w = ~witj_bintang,
      data = Data_2, 
      ME = F
    )
    y_bar_i_bintang_duga_h_l<-X_area_boost%*%Beta_boost+Alpha.mu_w_rep_boost_area+v_i_boost+ui_boost
    y_bar_i_bintang_duga_h_l_fix<-X_area_boost_fix%*%Beta_boost_fix+v_i_boost_fix+ui_boost_fix
    y_bar_i_bintang_duga_h<-exp(y_bar_i_bintang_duga_h_l)*exp(0.5*Ragam_y_vi_ui_boost)
    y_bar_i_bintang_duga_h_fix<-exp(y_bar_i_bintang_duga_h_l_fix)*exp(0.5*Ragam_y_vi_ui_boost_fix)
    
    Bias_SAE_AR1_ME<-y_bar_i_bintang_duga_h-y_bar_i_duga_btg
    Bias_Relatif_SAE_AR1_ME<-Bias_SAE_AR1_ME/y_bar_i_duga_btg
    mse_SAE_AR1_ME<-(Bias_SAE_AR1_ME)^2
    Hasil_Bias_Relatif_SAE_AR1_ME<-cbind(Hasil_Bias_Relatif_SAE_AR1_ME,Bias_Relatif_SAE_AR1_ME)
    Hasil_mse_SAE_AR1_ME<-cbind(Hasil_mse_SAE_AR1_ME,mse_SAE_AR1_ME)
    
    Bias_SAE_AR1<-y_bar_i_bintang_duga_h_fix-y_bar_i_duga_btg_fix
    Bias_Relatif_SAE_AR1<-Bias_SAE_AR1/y_bar_i_duga_btg_fix
    mse_SAE_AR1<-(Bias_SAE_AR1)^2
    Hasil_Bias_Relatif_SAE_AR1<-cbind(Hasil_Bias_Relatif_SAE_AR1,Bias_Relatif_SAE_AR1)
    Hasil_mse_SAE_AR1<-cbind(Hasil_mse_SAE_AR1,mse_SAE_AR1)
    
  }
  
  Data.hasil_Bias_Relatif_SAE_AR1_ME<-as.data.frame(Hasil_Bias_Relatif_SAE_AR1_ME)
  Data.hasil_mse_SAE_AR1_ME<-as.data.frame(Hasil_mse_SAE_AR1_ME)
  Data.hasil_Bias_Relatif_SAE_AR1<-as.data.frame(Hasil_Bias_Relatif_SAE_AR1)
  Data.hasil_mse_SAE_AR1<-as.data.frame(Hasil_mse_SAE_AR1)
  
  Data.hasil.Hasil_y_bar_i_duga_btg<-as.data.frame(Hasil_y_bar_i_duga_btg)
  Data.hasil.Hasil_y_bar_i_duga_btg_fix<-as.data.frame(Hasil_y_bar_i_duga_btg_fix)
  
  BR_SAE_AR1_ME<-apply(Hasil_Bias_Relatif_SAE_AR1_ME,1,mean)
  mean_overall_BR_SAE_AR1_ME<-mean(BR_SAE_AR1_ME)
  BR_SAE_AR1<-apply(Hasil_Bias_Relatif_SAE_AR1,1,mean)
  mean_overall_BR_SAE_AR1<-mean(BR_SAE_AR1)
  
  mse.hasil_SAE_AR1_ME<- apply(Hasil_mse_SAE_AR1_ME,1,mean) #1 artinya baris 2 artinya kolom
  mse.hasil_SAE_AR1<- apply(Hasil_mse_SAE_AR1,1,mean)
  
  rmse_SAE_AR1_ME<-mse.hasil_SAE_AR1_ME^0.5
  mean_overall_rmse_SAE_AR1_ME<-mean(rmse_SAE_AR1_ME)
  rmse_SAE_AR1<-mse.hasil_SAE_AR1^0.5
  mean_overall_rmse_SAE_AR1<-mean(rmse_SAE_AR1)
  
  Penduga_area_SAE_AR1_ME<-apply(Hasil_y_bar_i_duga_btg,1,mean)
  Penduga_area_SAE_AR1<-apply(Hasil_y_bar_i_duga_btg_fix,1,mean)
  Rata2_Penduga_Area_SAE_AR1_ME<-mean(Penduga_area_SAE_AR1_ME)
  Rata2_Penduga_Area_SAE_AR1<-mean(Penduga_area_SAE_AR1)
  
  cv_SAE_AR1_ME<-(rmse_SAE_AR1_ME/Rata2_Penduga_Area_SAE_AR1_ME)*100
  mean_overall_cv_SAE_AR1_ME<-mean(cv_SAE_AR1_ME)
  cv_SAE_AR1<-(rmse_SAE_AR1/Rata2_Penduga_Area_SAE_AR1)*100
  mean_overall_cv_SAE_AR1<-mean(cv_SAE_AR1)
  
  BR_SAE_AR1_ME
  mean_overall_BR_SAE_AR1_ME
  BR_SAE_AR1
  mean_overall_BR_SAE_AR1
  rmse_SAE_AR1_ME
  rmse_SAE_AR1
  mean_overall_rmse_SAE_AR1_ME
  mean_overall_rmse_SAE_AR1
  cv_SAE_AR1_ME
  cv_SAE_AR1
  mean_overall_cv_SAE_AR1_ME
  mean_overall_cv_SAE_AR1
  
}












