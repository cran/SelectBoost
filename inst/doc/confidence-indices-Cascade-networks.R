## ----setup, include = FALSE----------------------------------------------
LOCAL <- identical(Sys.getenv("LOCAL"), "TRUE")
knitr::opts_chunk$set(purl = LOCAL)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval = FALSE-------------------------------------------------------
#  devtools::install_github("fbertran/SelectBoost", ref = "doMC")

## ----Cascade, cache= FALSE, eval = LOCAL---------------------------------
library(Cascade)

## ---- cache= TRUE, eval = LOCAL------------------------------------------
T<-4
F<-array(0,c(T-1,T-1,T*(T-1)/2))

for(i in 1:(T*(T-1)/2)){diag(F[,,i])<-1}
F[,,2]<-F[,,2]*0.2
F[2,1,2]<-1
F[3,2,2]<-1
F[,,4]<-F[,,2]*0.3
F[3,1,4]<-1
F[,,5]<-F[,,2]

## ---- cache= TRUE, eval = LOCAL------------------------------------------
set.seed(1)
Net<-Cascade::network_random(
  nb=100,
  time_label=rep(1:4,each=25),
  exp=1,
  init=1,
  regul=round(rexp(100,1))+1,
  min_expr=0.1,
  max_expr=2,
  casc.level=0.4
)
Net@F<-F

## ----message=FALSE, cache=TRUE, eval = LOCAL-----------------------------
M <- Cascade::gene_expr_simulation(
  network=Net,
  time_label=rep(1:4,each=25),
  subject=5,
  level_peak=200)

## ---- cache= TRUE, fig.keep='none', eval = LOCAL-------------------------
Net_inf_C <- Cascade::inference(M,cv.subjects=TRUE)

## ---- cache= TRUE, fig.keep='none', eval = LOCAL-------------------------
stats::heatmap(Net_inf_C@network, Rowv = NA, Colv = NA, scale="none", revC=TRUE)

## ---- cache= TRUE, eval = LOCAL------------------------------------------
Fab_inf_C <- Net_inf_C@F

## ---- cache= TRUE, eval = LOCAL------------------------------------------
library(SelectBoost)
set.seed(1)

## ---- cache= TRUE, eval = LOCAL------------------------------------------
net_pct_selected <- selectboost(M, Fab_inf_C, use.parallel = FALSE)

## ---- cache= TRUE, eval = LOCAL------------------------------------------
net_pct_selected_.5 <- selectboost(M, Fab_inf_C, c0value = .5, use.parallel = FALSE)

## ---- cache= TRUE, eval = LOCAL------------------------------------------
net_pct_selected_thr <- selectboost(M, Fab_inf_C, group = group_func_1, use.parallel = FALSE)

## ---- cache= TRUE, eval = LOCAL------------------------------------------
net_pct_selected_cv <- selectboost(M, Fab_inf_C, cv.subject=FALSE, use.parallel = FALSE)

## ---- cache= TRUE, eval = LOCAL------------------------------------------
plot(net_pct_selected)

## ---- cache= TRUE, fig.keep='none', eval = LOCAL-------------------------
plot(net_pct_selected_.5)

## ---- cache= TRUE, fig.keep='none', eval = LOCAL-------------------------
plot(net_pct_selected_thr)

## ---- cache= TRUE, fig.keep='none', eval = LOCAL-------------------------
plot(net_pct_selected_cv)

## ---- cache= FALSE, fig.keep="none", eval = LOCAL------------------------
hist(Net_inf_C@network[abs(Net_inf_C@network)>1e-5])

## ---- cache= FALSE, fig.keep="none", eval = LOCAL------------------------
plot(Net_inf_C@network[abs(Net_inf_C@network)>1e-5],net_pct_selected@network.confidence[abs(Net_inf_C@network)>1e-5])

## ---- cache= TRUE, fig.keep='none', eval = LOCAL-------------------------
hist(net_pct_selected@network.confidence[abs(Net_inf_C@network)>1e-5])

## ---- cache= TRUE, fig.keep='none', eval = LOCAL-------------------------
plot(Net_inf_C@network[abs(Net_inf_C@network)>1e-5],net_pct_selected_.5@network.confidence[abs(Net_inf_C@network)>1e-5])

## ---- cache= TRUE, fig.keep='none', eval = LOCAL-------------------------
hist(net_pct_selected_.5@network.confidence[abs(Net_inf_C@network)>1e-5])

## ---- cache= TRUE, fig.keep='none', eval = LOCAL-------------------------
plot(Net_inf_C@network[abs(Net_inf_C@network)>1e-5],net_pct_selected_thr@network.confidence[abs(Net_inf_C@network)>1e-5])

## ---- cache= TRUE, fig.keep='none', eval = LOCAL-------------------------
hist(net_pct_selected_thr@network.confidence[abs(Net_inf_C@network)>1e-5])

## ---- cache= TRUE, fig.keep='none', eval = LOCAL-------------------------
plot(Net_inf_C@network[abs(Net_inf_C@network)>1e-5],net_pct_selected_cv@network.confidence[abs(Net_inf_C@network)>1e-5])

## ---- cache= TRUE, fig.keep='none', eval = LOCAL-------------------------
hist(net_pct_selected_cv@network.confidence[abs(Net_inf_C@network)>1e-5])

