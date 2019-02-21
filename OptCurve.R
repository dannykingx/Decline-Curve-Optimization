Hyp.q.forward.fun = function( time, Hyp.qi, Hyp.b, Hyp.Di ){
  Hyp.q.theo = Hyp.qi*(1 + Hyp.b*Hyp.Di*time)^(-1/Hyp.b)
  return(Hyp.q.theo)
}
#------------------------------------------------------------------------------
residfun = function(x,x.days,y.prod){
  Hyp.qi = x[1]
  Hyp.b  = x[2]
  Hyp.Di = x[3]
  q.theo = (365.25/12)*Hyp.q.forward.fun(
    time=x.days, 
    Hyp.qi=Hyp.qi, 
    Hyp.b=Hyp.b, 
    Hyp.Di=Hyp.Di)
  residual = sqrt(sum((q.theo-y.prod)^2))
  return(residual)
}
#------------------------------------------------------------------------------
min.data = 3

result = data.frame(
  time.days =  numeric(0),
  time.months = numeric(0),
  production = numeric(0),
  theo = numeric(0)
)
Hyp.qi       =as.numeric(NA)
Hyp.b        =as.numeric(NA)
Hyp.Di.daily =as.numeric(NA)
Hyp.Di.annual=as.numeric(NA)

ok = length(col.Production)>0
if(ok) ok = length(col.Date) == length(col.Production)

if(ok){
  u.order = order(col.Date)
  
  col.Date       = col.Date[u.order]
  col.Production = col.Production[u.order]
  
  t.days = as.numeric(difftime(col.Date,min(col.Date),units="days"))
  t.months = round(t.days*12/365.25)
  
  u0 = which.max(col.Production)
  u = u0:length(col.Production)
  
  x.months = t.months[u]
  x.days = t.days[u]
  y.prod = col.Production[u]
  
  x0 = c(y.prod[1]*12/365.25,1.00,0.005)
  
  optim.result = optim(
    par = x0,
    fn=residfun,
    x.days=x.days, y.prod=y.prod
  )
  
  Hyp.qi = optim.result$par[1]*(365.25/12)
  Hyp.b  = optim.result$par[2]
  Hyp.Di.daily  = optim.result$par[3]
  Hyp.Di.annual = optim.result$par[3]*365.25
  
  result = data.frame(
    time.days   = x.days,
    time.months = x.months,
    production = y.prod
  )
}
#TIBCO Component Exchange License
#Copyright (c) 2016 TIBCO Software Inc. All Rights Reserved.
#------------------------------------------------------------------------------
