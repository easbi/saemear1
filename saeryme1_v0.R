library(rio)
library(lme4)
library(dplyr)

saeryme1 = function(formula, t, area, w, data, ME=TRUE){
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
  
  formula_full = paste0(
    paste(format(formula)), " + ",
    "(1|", t, ")", " + ",
    "(1|", area, ")", " + ",
    "(1|", w, ")"
  )
  
  # A. Estimate Variance Component using REML --------------------------------------------------------------------
  Model1<-lmer(formula_full,df)
  Ragam1<-as.data.frame(VarCorr(Model1))
  Sigma_w<-Ragam1$sdcor[1]; Sigma2_w<-Sigma_w^2
  Sigma_v<-Ragam1$sdcor[2]; Sigma2_v<-Sigma_v^2
  Sigma_eps<-Ragam1$sdcor[3]; Sigma2_eps<-Sigma_eps^2
  Sigma_e<-Ragam1$sdcor[4]; Sigma2_e<-Sigma_e^2
  
  
  # B. Estimate rho ------------------------------------------------------------
  res1<-resid(Model1)
  res1_1_0<-c(0,diff(res1,differences=1))
  Model2<-lm(res1~res1_1_0)
  
  #Sum2<-summary(Model2)
  Hasil_Coef_Model2<-coefficients(Model2)
  Rho<-as.data.frame(Hasil_Coef_Model2)[2,1]
  
  # Persiapan Data #2 -------------------------------------------------------
  m<-length(unique(df %>% pull(area)))
  
  n_frame=data %>% group_by(!!as.symbol(area)) %>% summarise(ni=n()) %>% mutate(ni1=ni/2, ni2=ni1)
  
  ni1=n_frame %>% pull(ni1)
  ni2=n_frame %>% pull(ni1)
  
  n_gab=c(ni1, ni2)
  
  ni=n_frame %>% pull(ni)
  
  n<-sum(ni)
  n1<-sum(n_frame %>% pull(ni1))
  n2<-sum(n_frame %>% pull(ni2))
  
  
  # C. Uit Variance ---------------------------------------------------------
  I_m<-diag(m)
  I_T<-diag(T_)
  
  Gamma_u<-outer(1:T_, 1:T_, FUN=function(x,y) Rho^abs(x-y))/(1 - Rho^2) #Equation 4.4
  Ragam_u<-Sigma2_eps*Gamma_u #Equation 4.3
  Ragam_Uit<-Sigma2_eps/(1-Rho^2) #Equation 4.19
  
  
  # D-E. Estimate alpha cap and calculate random variable with measurement error---------------------------------------------------
  Area_gab<-as.matrix(c(rep(1:m,ni1),rep(1:m,ni2))) #?
  mean_area=df %>% group_by(!!as.symbol(area)) %>% summarise_all(mean) %>% select(-t)
  
  y_bar_i = mean_area %>% pull(y)
  w_bar_i = mean_area %>% pull(w)
  
  
  y_bar<-sum(y_bar_i)/m
  w_bar<-sum(w_bar_i)/m 
  
  w_frame=df %>% select(area, w) %>% left_join(mean_area %>% select(area, w), area)
  
  SSWw<-sum((w_frame[,2]-w_frame[,3])^2) #step d halaman 56
  Sigma2_eta<-SSWw/(n-m) 
  
  kw<-Sigma2_w/(Sigma2_w+Sigma2_eta)
  
  Syw<-1/n*(sum((y_bar_i-y_bar)*(w_bar_i-w_bar)))
  Sww<-1/n*(sum((w_bar_i-w_bar)^2))
  Alpha_ols<-Syw/Sww
  Alpha<-(1/kw)*Alpha_ols
  
  
  
  # Persiapan #3 -----------------------------------------------------------------
  J_n1<-matrix(1,nrow=ni[1],ncol=ni[1])
  I_n1<-diag(1,nrow=ni[1],ncol=ni[1])
  J_n1t<-matrix(1,nrow=ni1[1],ncol=ni1[1])
  Satuij<-rep(1,n)
  Vektor_y<-as.matrix(df %>% pull(y))
  y_area<-as.matrix(y_bar_i)
  Satu_area<-as.matrix(cbind(rep(1,m)))
  
  if(ME){
    # ME ----------------------------------------------------------------------
    V1<-Sigma2_v*J_n1 + Alpha^2*Sigma2_w*J_n1 + Sigma2_eps*(kronecker(Gamma_u,J_n1t)) + Sigma2_e*I_n1
    V<-diag(V1[1],nrow=n,ncol=n)
    X<-as.matrix(model.matrix(formula,df))
    
    Ragam_y<-V1[1]
    Alpha.mu_w<-Alpha*w_bar
    Alpha.mu_w_rep<-rep(Alpha.mu_w,n)
    
    Matriks_Alpha.mu_w<-as.matrix(cbind(rep(Alpha.mu_w,n)))
    Beta<-(solve(t(X)%*%solve(V)%*%X))%*%((t(X)%*%solve(V)%*%Vektor_y)-(t(X)%*%solve(V)%*%Matriks_Alpha.mu_w))
    X_area<-as.matrix(cbind(Satu_area, mean_area[,-c(1,2, ncol(mean_area))]))
    
    w_i<-as.matrix(cbind(w_bar_i))
    
    # h -> 4.21
    v_i_1<-y_area-X_area%*%Beta-as.matrix(cbind(w_i*Alpha))
    v_i_2<-cbind(Sigma2_v/(Sigma2_v+Alpha^2*Sigma2_w+(Sigma2_e/ni)+Sigma2_eps))
    v_i<-diag(c(v_i_2))%*%v_i_1
    v_i.rep<-rep(c(v_i,v_i),n_gab)
    
    # i -> 4.23
    u_i_1<-y_area-X_area%*%Beta-as.matrix(cbind(w_i*Alpha))
    u_i_2<-cbind(Sigma2_eps/(Sigma2_v+Alpha^2*Sigma2_w+(Sigma2_e/ni)+Sigma2_eps))
    u_i<-diag(c(u_i_2))%*%u_i_1
    u_i.rep<-rep(c(u_i,u_i),n_gab)
    
    Ragam_y_vi_ui<-Alpha^2*Sigma2_w+Sigma2_e
    
    y_l_duga<-X%*%Beta+Alpha.mu_w_rep+v_i.rep+u_i.rep
    y_duga<-exp(y_l_duga)*exp(0.5*Ragam_y_vi_ui)
    
    est=data.frame(area=df[area], est=as.vector(y_duga))
    
    est=est %>% group_by(!!as.symbol(area)) %>% summarise(est=mean(est))
    
    res=list()
    
    res$est=est
    res$REML=Model1
    res$beta=Beta
    res$Alpha.mu_w = Alpha.mu_w
    res$Ragam_y_vi_ui = Ragam_y_vi_ui
  }else{
    V1_fix<-Sigma2_v*J_n1 + Sigma2_eps*(kronecker(Gamma_u,J_n1t)) + Sigma2_e*I_n1
    V_fix<-diag(V1_fix[1],nrow=n,ncol=n)
    X_fix<-as.matrix(data.frame(model.matrix(formula, df), model.frame(as.formula(paste0("~",w)), df)))
    
    Ragam_y_fix<-V1_fix[1]
    Beta_fix<-(solve(t(X_fix)%*%solve(V_fix)%*%X_fix))%*%((t(X_fix)%*%solve(V_fix)%*%Vektor_y))
    X_area_fix<-as.matrix(cbind(Satu_area, mean_area[,-c(1,2)]))
    
    v_i_1_fix<-y_area-X_area_fix%*%Beta_fix
    v_i_2_fix<-cbind(Sigma2_v/(Sigma2_v+(Sigma2_e/ni)+Sigma2_eps))
    v_i_fix<-diag(c(v_i_2_fix))%*%v_i_1_fix
    v_i.rep_fix<-rep(c(v_i_fix,v_i_fix),n_gab)
    
    u_i_1_fix<-y_area-X_area_fix%*%Beta_fix
    u_i_2_fix<-cbind(Sigma2_eps/(Sigma2_v+(Sigma2_e/ni)+Sigma2_eps))
    u_i_fix<-diag(c(u_i_2_fix))%*%u_i_1_fix
    u_i.rep_fix<-rep(c(u_i_fix,u_i_fix),n_gab)
    
    Ragam_y_vi_ui_fix<-Sigma2_e
    
    y_l_duga_fix<-X_fix%*%Beta_fix+v_i.rep_fix+u_i.rep_fix
    y_duga_fix<-exp(y_l_duga_fix)*exp(0.5*Ragam_y_vi_ui_fix)
    
    est_fix=data.frame(area=df[area], est=as.vector(y_duga_fix))
    
    est_fix=est_fix %>% group_by(!!as.symbol(area)) %>% summarise(est=mean(est))
    
    res=list()
    
    res$est=est_fix
    res$REML=Model1
    res$beta=Beta_fix
    res$Ragam_y_vi_ui_fix= Ragam_y_vi_ui_fix
  }
  res$Sigma2_v = Sigma2_v
  res$Sigma2_w = Sigma2_w
  res$Sigma2_e = Sigma2_e
  res$Sigma2_eps = Sigma2_eps
  res$Alpha = Alpha
  
  res$Ragam_Uit = Ragam_Uit
  res$w_bar = w_bar
  return(res)
}



