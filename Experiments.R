install.packages("metap")

n = 10
f0 = 0.3

alpha = 0.05

x = c(rep(0.2,n*f0),rep(0.2,n*(1-f0)))
x = c(0.005	,0.047,	0.058	,0.6,	0.83,	0.9	,0.93	,0.97,	0.98,	0.99)
x = c(0.07,	0.09,	0.11,	0.15,	0.18,	0.2,	0.208,	0.21,	0.26,	0.27)
x = runif(20)

x = c(0.09792, 0.16621 ,0.00466, 0.03761, 0.09696, 0.09121 ,0.08233)
n = length(x)
x = c(0.00013, 0.00011 ,0.0, 0.0, 0.00017, 0.00087, 0.0)
n = length(x)
x = c(0.2175,0.3797,0.0002,0.4221,0.0239,0.3225,0.3763,0.3018)
n = length(x)
x = c(0.026856 , 0.001154 , 0.011908)
n = length(x)
x = c(0.168,  0.297 , 0.357  ,0.019,  0.218,  0.001)
n = length(x)
Pval = matrix(x,n,1)


find_khat<-function(Pval,alpha,type = 'S')
{
  N = length(Pval)
  puN = rep(0,N)
  prev = 0
  for (u in 1:N){
    puN[u]=Partial_Conjunction(Pval,u,type =type)
    if (u>1) {prev = puN[u-1]}
    puN[u] = max(prev,puN[u])
  }
  return(sum(puN<=alpha))
}

Partial_Conjunction<-function(Pval,u,type = 'S'){
  library(metap)
  n = length(Pval)
  pnmup1 = sort(Pval,decreasing = TRUE)[1:(n-u+1)]
  if (length(pnmup1)==1) {p_comb=pnmup1}
  else if (0.0 %in% pnmup1) {p_comb=0}
  else if (1.0 %in% pnmup1) {p_comb=1}
  else if (type =='S'){p_comb = sumz(pnmup1)$p}
  else if (type =='F'){p_comb = sumlog(pnmup1)$p}
  else if (type =='B'){p_comb = (n-u+1)*sort(Pval)[u]}
  return(p_comb)
}


find_khat(Pval,alpha,'S')
find_khat(Pval,alpha,'F')
find_khat(Pval,alpha,'B')




sum(Pval<=0.05)