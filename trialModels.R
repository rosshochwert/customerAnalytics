Exponential.Log_Likelihood = function(params,data,size,nt){
  lambda = params[1]
  new_trialers = data[,2][-1] - data[,2][-length(data[,2])]
  new_trialers = c(data[,2][1],new_trialers)
  time = data[,1]
  p_t = 1-exp(-lambda*time)
  if(nt==TRUE){
    p = params[2]
    p_t = p_t*p
  }
  new_trialers_p_t = p_t[-1] - p_t[-length(p_t)]
  new_trialers_p_t = c(p_t[1],new_trialers_p_t)
  yet_to_trial = (size - data[,2][length(data)])*log(1-p_t[length(p_t)])
  ll = new_trialers*log(new_trialers_p_t)
  ll = c(ll,yet_to_trial)
  return(sum(-ll))
}

Exponential.graph = function(params,data,size,nt){
  lambda = params[1]
  time = seq(from=1,to=length(data[,1])*2,by=1)
  p_t = 1-exp(-lambda*time)
  if(nt==TRUE){
    p = params[2]
    p_t = p_t*p
  }
  cumulative_trialers = p_t*size
  plot(cumulative_trialers,type="l",ylab="Cumulative Trialers",xlab="Time", main = "Projected vs. Actual Trial Rates")
  points(data[,2])
}

Exponential.Gamma.Log_Likelihood = function(params,data,size,nt){
  alpha = params[1]
  r = params[2]
  new_trialers = data[,2][-1] - data[,2][-length(data[,2])]
  new_trialers = c(data[,2][1],new_trialers)
  time = data[,1]
  p_t = 1-(alpha/(alpha+time))^r
  if(nt==TRUE){
    p = params[3]
    p_t = p_t*p
  }
  new_trialers_p_t = p_t[-1] - p_t[-length(p_t)]
  new_trialers_p_t = c(p_t[1],new_trialers_p_t)
  yet_to_trial = (size - data[,2][length(data)])*log(1-p_t[length(p_t)])
  ll = new_trialers*log(new_trialers_p_t)
  ll = c(ll,yet_to_trial)
  return(sum(-ll))
}

Exponential.Gamma.graph = function(params,data,size,nt){
  alpha = params[1]
  r = params[2]
  time = seq(from=1,to=length(data[,1])*2,by=1)
  p_t = 1-(alpha/(alpha+time))^r
  if(nt==TRUE){
    p = params[3]
    p_t = p_t*p
  }
  cumulative_trialers = p_t*size
  plot(cumulative_trialers,type="l",ylab="Cumulative Trialers",xlab="Time", main = "Projected vs. Actual Trial Rates")
  points(data[,2])
}


Weibull.Gamma.Log_Likelihood = function(params,data,size,nt){
  alpha = params[1]
  r = params[2]
  c = params[3]
  new_trialers = data[,2][-1] - data[,2][-length(data[,2])]
  new_trialers = c(data[,2][1],new_trialers)
  time = data[,1]
  p_t = 1-(alpha/(alpha+time^c))^r
  if(nt==TRUE){
    p = params[4]
    p_t = p_t*p
  }
  new_trialers_p_t = p_t[-1] - p_t[-length(p_t)]
  new_trialers_p_t = c(p_t[1],new_trialers_p_t)
  yet_to_trial = (size - data[,2][length(data)])*log(1-p_t[length(p_t)])
  ll = new_trialers*log(new_trialers_p_t)
  ll = c(ll,yet_to_trial)
  return(sum(-ll))
}

Weibull.Gamma.graph = function(params,data,size,nt){
  alpha = params[1]
  r = params[2]
  c = params[3]
  time = seq(from=1,to=length(data[,1])*2,by=1)
  p_t = 1-(alpha/(alpha+time^c))^r
  if(nt==TRUE){
    p = params[4]
    p_t = p_t*p
  }
  cumulative_trialers = p_t*size
  plot(cumulative_trialers,type="l",ylab="Cumulative Trialers",xlab="Time", main = "Projected vs. Actual Trial Rates")
  points(data[,2])
}