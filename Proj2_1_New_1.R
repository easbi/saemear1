NmArea<-Data_2[1]
NmUnit<-Data_2[2]
Area<-Data_2[3]
Unit<-Data_2[4]
yitj<-Data_2[5]
t_<-Data_2[6]
X1_itj<-Data_2[7]
X2_itj_<-Data_2[8]
X2_itj<-scale(X2_itj_)
w_ij<-Data_2[9]
Data_1<-data.frame(Area,yitj,t_,X1_itj,X2_itj,w_ij)
Model1<-lmer(y_star~x1+x2+(1|t)+(1|Area)+(1|w),Data_1)
res1<-resid(Model1)
res1_1<-diff(res1,differences=1)
Sum1<-summary(Model1)
Hasil_Ragam1<-VarCorr(Model1)
Ragam1<-as.data.frame(Hasil_Ragam1)
Sigma2_w<-Ragam1$vcov[1]
Sigma_w<-Ragam1$sdcor[1]
Sigma2_v<-Ragam1$vcov[2]
Sigma_v<-Ragam1$sdcor[2]
Sigma2_eps<-Ragam1$vcov[3]
Sigma_eps<-Ragam1$sdcor[3]
Sigma2_e<-Ragam1$vcov[4]
Sigma_e<-Ragam1$sdcor[4]
res1_1_0<-c(0,res1_1)
Model2<-lm(res1~res1_1_0)
Sum2<-summary(Model2)
Hasil_Coef_Model2<-coefficients(Model2)
Coef_Model2<-as.data.frame(Hasil_Coef_Model2)
Rho<-Coef_Model2$Hasil_Coef_Model2[2]
m<-27
T_<-2
ni1<-c(40,44,31,31,36,39,26,31,40,26,26,31,29,16,29,23,16,10,6,7,29,5,12,11,3,10,4)
ni2<-c(40,44,31,31,36,39,26,31,40,26,26,31,29,16,29,23,16,10,6,7,29,5,12,11,3,10,4)
n_gab<-c(40,44,31,31,36,39,26,31,40,26,26,31,29,16,29,23,16,10,6,7,29,5,12,11,3,10,4,40,44,31,31,36,39,26,31,40,26,26,31,29,16,29,23,16,10,6,7,29,5,12,11,3,10,4)
ni<-c(80,88,62,62,72,78,52,62,80,52,52,62,58,32,58,46,32,20,12,14,58,10,24,22,6,20,8)
n<-sum(ni)
n1<-sum(ni1)
n2<-sum(ni2)
I_m<-diag(m)
I_T<-diag(T_)
baris.t<-matrix(rep(1:T_, times=T_), nrow=T_, ncol=T_)
kolom.t <- t(baris.t)
abs.jk <- abs(baris.t - kolom.t)
Gamma_u<-Rho^(abs.jk)/(1 - Rho^2)
Ragam_u<-Sigma2_eps*Gamma_u
Ragam_Uit<-Sigma2_eps/(1-Rho^2)
Area1<-rep(1:m,ni1)
Area2<-rep(1:m,ni2)
Area_gab<-as.matrix(c(Area1,Area2))

y<-Data_2[5]
x1<-Data_2[7]
x2<-Data_2[8]
w<-Data_2[9]

Data1<-data.frame(Area_gab,y)
y_bar_i<-NULL;n_gab<-c(40,44,31,31,36,39,26,31,40,26,26,31,29,16,29,23,16,10,6,7,29,5,12,11,3,10,4,40,44,31,31,36,39,26,31,40,26,26,31,29,16,29,23,16,10,6,7,29,5,12,11,3,10,4);dt.sample1<-NULL
for(i in 1:m){
  yi<-Data1[Data1$Area==i,]
  y_bar_i<-c(y_bar_i,mean(yi[,2]))
  n_yi<-nrow(yi)
  dt.sample1<-rbind(dt.sample1,yi[sample(1:n_yi,n_gab[i]),])
}

Data2<-data.frame(Area_gab,x1)
X1_bar_i<-NULL;n_gab<-c(40,44,31,31,36,39,26,31,40,26,26,31,29,16,29,23,16,10,6,7,29,5,12,11,3,10,4,40,44,31,31,36,39,26,31,40,26,26,31,29,16,29,23,16,10,6,7,29,5,12,11,3,10,4);dt.sample2<-NULL
for(i in 1:m){
  X1i<-Data2[Data2$Area==i,]
  X1_bar_i<-c(X1_bar_i,mean(X1i[,2]))
  n_X1i<-nrow(X1i)
  dt.sample2<-rbind(dt.sample2,X1i[sample(1:n_X1i,n_gab[i]),])
}

Data3<-data.frame(Area_gab,x2)
X2_bar_i<-NULL;n_gab<-c(40,44,31,31,36,39,26,31,40,26,26,31,29,16,29,23,16,10,6,7,29,5,12,11,3,10,4,40,44,31,31,36,39,26,31,40,26,26,31,29,16,29,23,16,10,6,7,29,5,12,11,3,10,4);dt.sample3<-NULL
for(i in 1:m){
  X2i<-Data3[Data3$Area==i,]
  X2_bar_i<-c(X2_bar_i,mean(X2i[,2]))
  n_X2i<-nrow(X2i)
  dt.sample3<-rbind(dt.sample3,X2i[sample(1:n_X2i,n_gab[i]),])
}

Data4<-data.frame(Area_gab,w)
w_bar_i<-NULL;n_gab<-c(40,44,31,31,36,39,26,31,40,26,26,31,29,16,29,23,16,10,6,7,29,5,12,11,3,10,4,40,44,31,31,36,39,26,31,40,26,26,31,29,16,29,23,16,10,6,7,29,5,12,11,3,10,4);dt.sample4<-NULL
for(i in 1:m){
  wi<-Data4[Data4$Area==i,]
  w_bar_i<-c(w_bar_i,mean(wi[,2]))
  n_wi<-nrow(wi)
  dt.sample4<-rbind(dt.sample4,wi[sample(1:n_wi,n_gab[i]),])
}

y_bar<-sum(y_bar_i)/m
w_bar<-sum(w_bar_i)/m
w_bar_i_rep<-rep(c(w_bar_i,w_bar_i),n_gab)

SSWw<-sum((w-w_bar_i_rep)^2)
Sigma2_eta<-SSWw/(n-m)
kw<-Sigma2_w/(Sigma2_w+Sigma2_eta)
Syw<-1/n*(sum((y_bar_i-y_bar)*(w_bar_i-w_bar)))
Sww<-1/n*(sum((w_bar_i-w_bar)^2))
Alpha_ols<-Syw/Sww
Alpha<-(1/kw)*Alpha_ols

J_n1<-matrix(1,nrow=80,ncol=80)
J_n2<-matrix(1,nrow=88,ncol=88)
J_n3<-matrix(1,nrow=62,ncol=62)
J_n4<-matrix(1,nrow=62,ncol=62)
J_n5<-matrix(1,nrow=72,ncol=72)
J_n6<-matrix(1,nrow=78,ncol=78)
J_n7<-matrix(1,nrow=52,ncol=52)
J_n8<-matrix(1,nrow=62,ncol=62)
J_n9<-matrix(1,nrow=80,ncol=80)
J_n10<-matrix(1,nrow=52,ncol=52)
J_n11<-matrix(1,nrow=52,ncol=52)
J_n12<-matrix(1,nrow=62,ncol=62)
J_n13<-matrix(1,nrow=58,ncol=58)
J_n14<-matrix(1,nrow=32,ncol=32)
J_n15<-matrix(1,nrow=58,ncol=58)
J_n16<-matrix(1,nrow=46,ncol=46)
J_n17<-matrix(1,nrow=32,ncol=32)
J_n18<-matrix(1,nrow=20,ncol=20)
J_n71<-matrix(1,nrow=12,ncol=12)
J_n72<-matrix(1,nrow=14,ncol=14)
J_n73<-matrix(1,nrow=58,ncol=58)
J_n74<-matrix(1,nrow=10,ncol=10)
J_n75<-matrix(1,nrow=24,ncol=24)
J_n76<-matrix(1,nrow=22,ncol=22)
J_n77<-matrix(1,nrow=6,ncol=6)
J_n78<-matrix(1,nrow=20,ncol=20)
J_n79<-matrix(1,nrow=8,ncol=8)
I_n1<-diag(1,nrow=80,ncol=80)
I_n2<-diag(1,nrow=88,ncol=88)
I_n3<-diag(1,nrow=62,ncol=62)
I_n4<-diag(1,nrow=62,ncol=62)
I_n5<-diag(1,nrow=72,ncol=72)
I_n6<-diag(1,nrow=78,ncol=78)
I_n7<-diag(1,nrow=52,ncol=52)
I_n8<-diag(1,nrow=62,ncol=62)
I_n9<-diag(1,nrow=80,ncol=80)
I_n10<-diag(1,nrow=52,ncol=52)
I_n11<-diag(1,nrow=52,ncol=52)
I_n12<-diag(1,nrow=62,ncol=62)
I_n13<-diag(1,nrow=58,ncol=58)
I_n14<-diag(1,nrow=32,ncol=32)
I_n15<-diag(1,nrow=58,ncol=58)
I_n16<-diag(1,nrow=46,ncol=46)
I_n17<-diag(1,nrow=32,ncol=32)
I_n18<-diag(1,nrow=20,ncol=20)
I_n71<-diag(1,nrow=12,ncol=12)
I_n72<-diag(1,nrow=14,ncol=14)
I_n73<-diag(1,nrow=58,ncol=58)
I_n74<-diag(1,nrow=10,ncol=10)
I_n75<-diag(1,nrow=24,ncol=24)
I_n76<-diag(1,nrow=22,ncol=22)
I_n77<-diag(1,nrow=6,ncol=6)
I_n78<-diag(1,nrow=20,ncol=20)
I_n79<-diag(1,nrow=8,ncol=8)
J_n1t<-matrix(1,nrow=40,ncol=40)

V1<-Sigma2_v*J_n1+Alpha^2*Sigma2_w*J_n1+Sigma2_eps*(kronecker(Gamma_u,J_n1t))+Sigma2_e*I_n1
V1_fix<-Sigma2_v*J_n1+Sigma2_eps*(kronecker(Gamma_u,J_n1t))+Sigma2_e*I_n1
V<-diag(V1[1],nrow=1222,ncol=1222)
V_fix<-diag(V1_fix[1],nrow=1222,ncol=1222)

Satuij<-rep(1,n)
X<-as.matrix(cbind(Satuij,x1,x2))
X_fix<-as.matrix(cbind(Satuij,x1,x2,w))
Vektor_y<-as.matrix(y)

Ragam_y<-V1[1]
Ragam_y_fix<-V1_fix[1]

Alpha.mu_w<-Alpha*w_bar
Matriks_Alpha.mu_w<-as.matrix(cbind(rep(Alpha.mu_w,n)))

Beta<-(solve(t(X)%*%solve(V)%*%X))%*%((t(X)%*%solve(V)%*%Vektor_y)-(t(X)%*%solve(V)%*%Matriks_Alpha.mu_w))
Beta_fix<-(solve(t(X_fix)%*%solve(V_fix)%*%X_fix))%*%((t(X_fix)%*%solve(V_fix)%*%Vektor_y))
Alpha.mu_w_rep<-rep(Alpha.mu_w,n)

y_area<-as.matrix(y_bar_i)
Satu_area<-as.matrix(cbind(rep(1,m)))
X_area<-as.matrix(cbind(Satu_area,X1_bar_i,X2_bar_i))
X_area_fix<-as.matrix(cbind(Satu_area,X1_bar_i,X2_bar_i,w_bar_i))
w_i<-as.matrix(cbind(w_bar_i))

v_i_1<-y_area-X_area%*%Beta-as.matrix(cbind(w_i*Alpha))
v_i_1_fix<-y_area-X_area_fix%*%Beta_fix
v_i_2<-cbind(Sigma2_v/(Sigma2_v+Alpha^2*Sigma2_w+(Sigma2_e/ni)+Sigma2_eps))
v_i_2_fix<-cbind(Sigma2_v/(Sigma2_v+(Sigma2_e/ni)+Sigma2_eps))
v_i<-diag(c(v_i_2))%*%v_i_1
v_i_fix<-diag(c(v_i_2_fix))%*%v_i_1_fix
v_i.rep<-rep(c(v_i,v_i),n_gab)
v_i.rep_fix<-rep(c(v_i_fix,v_i_fix),n_gab)

u_i_1<-y_area-X_area%*%Beta-as.matrix(cbind(w_i*Alpha))
u_i_1_fix<-y_area-X_area_fix%*%Beta_fix
u_i_2<-cbind(Sigma2_eps/(Sigma2_v+Alpha^2*Sigma2_w+(Sigma2_e/ni)+Sigma2_eps))
u_i_2_fix<-cbind(Sigma2_eps/(Sigma2_v+(Sigma2_e/ni)+Sigma2_eps))
u_i<-diag(c(u_i_2))%*%u_i_1
u_i_fix<-diag(c(u_i_2_fix))%*%u_i_1_fix
u_i.rep<-rep(c(u_i,u_i),n_gab)
u_i.rep_fix<-rep(c(u_i_fix,u_i_fix),n_gab)

Ragam_y_vi_ui<-Alpha^2*Sigma2_w+Sigma2_e
Ragam_y_vi_ui_fix<-Sigma2_e

y_l_duga<-X%*%Beta+Alpha.mu_w_rep+v_i.rep+u_i.rep
y_duga<-exp(y_l_duga)*exp(0.5*Ragam_y_vi_ui)
y_l_duga_fix<-X_fix%*%Beta_fix+v_i.rep_fix+u_i.rep_fix
y_duga_fix<-exp(y_l_duga_fix)*exp(0.5*Ragam_y_vi_ui_fix)

Data5<-data.frame(Area_gab,y_duga)
y_i<-NULL;y_i_duga<-NULL;n_gab<-c(40,44,31,31,36,39,26,31,40,26,26,31,29,16,29,23,16,10,6,7,29,5,12,11,3,10,4,40,44,31,31,36,39,26,31,40,26,26,31,29,16,29,23,16,10,6,7,29,5,12,11,3,10,4);dt.sample5<-NULL
for(i in 1:m){
  y_i<-Data5[Data5$Area==i,]
  y_i_duga<-c(y_i_duga,mean(y_i[,2]))
  n_y_i<-nrow(y_i)
  dt.sample5<-rbind(dt.sample5,y_i[sample(1:n_y_i,n_gab[i]),])
}
Data6<-data.frame(Area_gab,y_duga_fix)
y_i_fix<-NULL;y_i_duga_fix<-NULL;n_gab<-c(40,44,31,31,36,39,26,31,40,26,26,31,29,16,29,23,16,10,6,7,29,5,12,11,3,10,4,40,44,31,31,36,39,26,31,40,26,26,31,29,16,29,23,16,10,6,7,29,5,12,11,3,10,4);dt.sample6<-NULL
for(i in 1:m){
  y_i_fix<-Data6[Data6$Area==i,]
  y_i_duga_fix<-c(y_i_duga_fix,mean(y_i_fix[,2]))
  n_y_i_fix<-nrow(y_i_fix)
  dt.sample6<-rbind(dt.sample6,y_i_fix[sample(1:n_y_i_fix,n_gab[i]),])
}
N_penduduk_i<-c(5459668,2434221,2243904,3534114,2548723,1735998,1168682,1055417,2126179,1182109,1137273,1691386,1529388,921598,2273579,3246013,1629423,390483,1047922,318117,2481469,307494,2714825,2106102,586580,657477,181425)
N_penduduk<-sum(N_penduduk_i)

Y_Prov<-sum(y_i_duga*N_penduduk_i)/N_penduduk
Y_Prov_fix<-sum(y_i_duga_fix*N_penduduk_i)/N_penduduk

Hasil_SAE_AR1_ME<-y_i_duga
Hasil_SAE_AR1<-y_i_duga_fix
Hasil_Susenas_Maret<-c(907682,700506,553869,834803,513366,489726,587214,721786,619552,698224,789992,629355,873718,1030583,855416,1168767,605302,783266,1324986,1010902,1433908,828197,1434648,1503423,1153348,964434,817072)

Hasil_Kab<-cbind(Hasil_Susenas_Maret,Hasil_SAE_AR1_ME,Hasil_SAE_AR1)
Prov_Sept<-981968
Prov_Mar<-896895
Penduga_provinsi<-c(Prov_Mar,Prov_Sept,Y_Prov,Y_Prov_fix)
barplot(Penduga_provinsi,col=c("grey","grey","grey","grey"))
abline(h=0)

Penduga_langsung<-c(907682,700506,553869,834803,513366,489726,587214,721786,619552,698224,789992,629355,873718,1030583,855416,1168767,605302,783266,1324986,1010902,1433908,828197,1434648,1503423,1153348,964434,817072)
Penduga_SAE_AR1_ME<-c(872930,922168,939178,828551,988430,959818,933834,868323,924567,914675,907778,935140,808375,775037,819626,877138,847035,809734,1040251,952071,923639,1154793,943669,1022586,937919,839330,904503)
Penduga_SAE_AR1<-c(938374,797681,659246,817024,648836,605723,682479,757879,710528,786194,866075,734590,825099,904521,828081,1196513,624451,726469,1569321,1118228,1486214,997037,1567067,1806383,1286326,946833,890024)
plot(Penduga_SAE_AR1,type="o",lty=2,xlab="No Kabupaten/Kota",ylab="Rata-Rata Konsumsi Perkapita");
lines(Penduga_SAE_AR1_ME,type="o",pch=2,lty=3,lwd=0.1);
lines(Penduga_Langsung_Susenas_Maret,type="o",pch=8,lty=4,lwd=1.25)
legend("topleft",c("Penduga SAE-AR1-ME September","Penduga SAE-AR1 September","Penduga Langsung Maret"),pch=c(2,1,8),lty=c(3,2,4))