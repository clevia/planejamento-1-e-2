########################################################################################################
#H0: Os fatores Recipientes e Especies possuem efeitos diferente sobre o desenvolvimento
#medio da altura das mudas
#H1: Os fatores Recipientes e Especies possuem efeitos semelhantes sobre o desenvolvimento
#medio da altura das mudas

require(dae)

y<- c(26.2,  26,  25,  25.4,
      24.8, 24.6, 26.7, 25.2,
      25.7, 26.3, 25.1, 26.4,
      19.6, 21.1, 19,  18.6, 
     22.8, 19.4, 18.8, 19.2,
     19.8, 21.4, 22.8, 21.3);  y
n<- length(y); n
i<- 3 # numero de niveis do fator recipiente:
j<- 2 # numero de niveis do fator eucalipito: 
r<- 4 # numero de repetiçoes

parcelas<- factor(c(1:n)); parcelas 
Recipiente <- factor(rep(c(1:i), each=r, times=j), labels=c("R1","R2","R3")); Recipiente
Eucalipto<- factor(rep(c(1:j), each=r*i), labels=c("E1","E2"));        Eucalipto
trat<- factor(rep(c("T1","T2","T3","T4","T5","T6"),each=4));                 trat

DIC<- data.frame(parcelas,Recipiente,Eucalipto,trat,y); DIC

### An?lise descritiva
#par(mfrow=c(1,3))
boxplot(split(y,Eucalipto), style.bxp="old", xlab="Eucalipto",   ylab="produção de mudas", medchar = T, medpch = 8)
boxplot(split(y,Recipiente),  style.bxp="old", xlab="P",    ylab="produção de mudas", medchar = T, medpch = 8)
boxplot((y~Eucalipto*Recipiente), style.bxp="old", xlab="D X P",ylab="produção de mudas", medchar = T, medpch = 8)

# gr?ficos de intera??o
#par(mfrow=c(1,2))
interaction.plot(Eucalipto,Recipiente,y, lwd=2)
interaction.plot(Recipiente,Eucalipto,y, lwd=3)

# media, vari?ncia e erro padronizado
Var_R<-    tapply(y,Recipiente,var)
Var_E<-    tapply(y,Eucalipto,var)
Var_trat<- tapply(y,trat,var)

Media_R<-    tapply(y,Recipiente,mean)
Media_E<-    tapply(y,Eucalipto,mean)
Media_trat<- tapply(y,trat,mean)

Desvio_pad_R<- sqrt(Var_R)
Desvio_pad_E<- sqrt(Var_E)
Desvio_pad_trat<- sqrt(Var_trat)

V_R<- data.frame(Var_R, Media_R, Desvio_pad_R);  V_R
V_E<- data.frame(Var_E, Media_E, Desvio_pad_E);  V_E
V_trat<- data.frame(Var_trat, Media_trat, Desvio_pad_trat); V_trat

### Construindo a ANOVA
require(MASS)

Xg<- model.matrix(lm(y~1));             Xg
Xu<- model.matrix(lm(y~parcelas-1));    Xu    # Xu = Xp
XA<- model.matrix(lm(y~Recipiente-1));        XA    # fator R
XB<- model.matrix(lm(y~Eucalipto-1));       XB    # fator E
Xt<- model.matrix(lm(y~trat-1));        Xt    # trat = R*E

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
planta_Error<- aov(y~ Recipiente*Eucalipto + Error(parcelas))
summary(planta_Error)
planta.aov  <- aov(y~ Recipiente*Eucalipto);                      summary(planta.aov)
