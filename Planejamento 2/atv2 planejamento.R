###############################################################################################
#hipóteses
#H0:Os fatores Peneiras e Densidades agem de modo independente sobre a produção media de amendoim
#emvagens por planta.
#H1: os fatores não agem de modo independente

require(dae)

y<- c(11.82, 12.03, 12.55,
      12.34, 14.08, 12.13,
      13.41, 12.98, 13.35,
      6.97, 10.26, 9.02,
      8.96, 9.02, 9.84,
      8.48, 9.66, 8.5,
      7.53, 7.67, 7.81,
      6.71, 7.87, 9.49,
      7.82, 9.44, 9.37);  y
n<- length(y); n
i<- 3 # n?mero de n?veis do fator P:
j<- 3 # n?mero de n?veis do fator D: 
r<- 3 # n?mero de repeti??es

parcelas<- factor(c(1:n)); parcelas 
P <- factor(rep(c(1:i), each=r, times=j), labels=c("P1","P2","P3")); P
D<- factor(rep(c(1:j), each=r*i), labels=c("D1","D2","D3"));        D
trat<- factor(rep(c("T1","T2","T3","T4","T5","T6","T7","T8","T9"),each=3));                 trat

DIC<- data.frame(parcelas,P,D,trat,y); DIC

### An?lise descritiva
#par(mfrow=c(1,3))
boxplot(split(y,D), style.bxp="old", xlab="D",   ylab="produção de amendoim", medchar = T, medpch = 8)
boxplot(split(y,P),  style.bxp="old", xlab="P",    ylab="produção de amendoim", medchar = T, medpch = 8)
boxplot((y~D*P), style.bxp="old", xlab="D X P",ylab="produção de amendoim", medchar = T, medpch = 8)

# gr?ficos de intera??o
#par(mfrow=c(1,2))
interaction.plot(D,P,y, lwd=2)
interaction.plot(P,D,y, lwd=3)

# media, vari?ncia e erro padronizado
Var_P<-    tapply(y,P,var)
Var_D<-    tapply(y,D,var)
Var_trat<- tapply(y,trat,var)

Media_P<-    tapply(y,P,mean)
Media_D<-    tapply(y,D,mean)
Media_trat<- tapply(y,trat,mean)

Desvio_pad_P<- sqrt(Var_P)
Desvio_pad_D<- sqrt(Var_D)
Desvio_pad_trat<- sqrt(Var_trat)

V_P<- data.frame(Var_P, Media_P, Desvio_pad_P);  V_P
V_D<- data.frame(Var_D, Media_D, Desvio_pad_D);  V_D
V_trat<- data.frame(Var_trat, Media_trat, Desvio_pad_trat); V_trat

### Construindo a ANOVA
require(MASS)

Xg<- model.matrix(lm(y~1));             Xg
Xu<- model.matrix(lm(y~parcelas-1));    Xu    # Xu = Xp
XA<- model.matrix(lm(y~P-1));        XA    # fator P
XB<- model.matrix(lm(y~D-1));       XB    # fator D
Xt<- model.matrix(lm(y~trat-1));        Xt    # trat = P*D

# Operadores de M?dias
Mg<- Xg %*% solve(t(Xg) %*% Xg) %*% t(Xg);   fractions(Mg)
Mu<- Xu %*% solve(t(Xu) %*% Xu) %*% t(Xu);   fractions(Mu)
MA<- XA %*% solve(t(XA) %*% XA) %*% t(XA);   fractions(MA)
MB<- XB %*% solve(t(XB) %*% XB) %*% t(XB);   fractions(MB)
Mt<- Xt %*% solve(t(Xt) %*% Xt) %*% t(Xt);   fractions(Mt)

#  M?dias
G_barra<- Mg %*% y ;    G_barra  # Fi estimado de G  (nenhum fator afeta na resposta)
T_barra<- Mt %*% y ;    T_barra  # Fi estimado de T=A*B (intera??o de A e B na resposta)
A_barra<- MA %*% y ;    A_barra  # Fi estimado de A (somente A afeta na resposta)
B_barra<- MB %*% y ;    B_barra  # Fi estimado de B (somente B afeta na resposta)
A_barra + B_barra - G_barra  # Fi estimado de A+B  (A e B afetam a resposta)

# Matrizes N?cleo
Qp<- Mu - Mg           # Total = Parcelas
QA<- MA - Mg
QB<- MB - Mg
Qt<- Mt - MA - MB + Mg
Qures<- (Mu-Mg) - (MA-Mg) - (MB-Mg) - (Mt-MA-MB+Mg)   # Qures = Qp-QA-QB-Qt

#  Desvios
Dg<-   Qp %*% y ;    Dg
Ae<-   QA %*% y ;    Ae
Be<-   QB %*% y ;    Be
Te<-   Qt %*% y ;    Te
Dbt<-  Qures %*% y ;    Dbt

# Somas de Quadrados
SQp    <- t(y) %*% Qp    %*% y;  SQp  # SQTotal = SQp
SQA<- t(y) %*% QA    %*% y;  SQA
SQB<- t(y) %*% QB    %*% y;  SQB
SQTrat<- t(y) %*% Qt    %*% y;  SQTrat  # SQA*B
SQRes <- t(y) %*% Qures %*% y;  SQRes

# Graus de Liberdade
gl_p  <-  sum(round(eigen(Qp)   $value,1));   gl_p
gl_A<-     sum(round(eigen(QA)   $value,1));  gl_A 
gl_B<-     sum(round(eigen(QB)   $value,1));  gl_B 
gl_trat<-  sum(round(eigen(Qt)   $value,1));  gl_trat 
gl_res <-  sum(round(eigen(Qures)$value,1));  gl_res  

# Quadrados M?dios
QMA<- SQA/gl_A;          QMA
QMB<- SQB/gl_B;          QMB
QMTrat<- SQTrat/gl_trat; QMTrat
QMRes <- SQRes/gl_res;   QMRes

# F    
F_calc_A<- QMA/QMRes;      F_calc_A
F_calc_B<- QMB/QMRes;      F_calc_B
F_calc_T<- QMTrat/QMRes;   F_calc_T

# p_valor
valor_p_A = 1-pf(F_calc_A,gl_A,gl_res);     valor_p_A
valor_p_B = 1-pf(F_calc_B,gl_B,gl_res);     valor_p_B
valor_p_T = 1-pf(F_calc_T,gl_trat,gl_res);  valor_p_T


SQ<- cbind(rbind("SQp","SQA","SQB","SQTrat","SQRes"),round(rbind(SQp,SQA,SQB,SQTrat,SQRes),2)); SQ
gl<- cbind(rbind("gl_p","gl_A","gl_B","gl_trat","gl_res"),round(rbind(gl_p,gl_A,gl_B,gl_trat,gl_res),2)); gl
QM<- cbind(rbind("QMA","QMB","QMTrat","QMRes"),round(rbind(QMA,QMB,QMTrat,QMRes),2));           QM
F_calc<- cbind(rbind("F_calc_A","F_calc_B","F_calc_T"),round(rbind(F_calc_A,F_calc_B,F_calc_T),2));            F_calc
p_valor<- cbind(rbind("valor_p_A","valor_p_B","valor_p_T"),round(rbind(valor_p_A,valor_p_B,valor_p_T),2));     p_valor

#---------------------------------------------------------------
### ANOVA pronta!
planta_Error<- aov(y~ P*D + Error(parcelas))
summary(planta_Error)
planta.aov  <- aov(y~ P*D);                      summary(planta.aov)
