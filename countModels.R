NBD.log_likelihod = function(params,data){
  r = params[1]
  alpha = params[2]
  x = data[,1]
  f_x = data[,2]
  prob_x = gamma(r+x)/(gamma(r)*factorial(x))*(alpha/(alpha+1))^r*(1/(alpha+1))^x
  ll = sum(f_x*log(prob_x))
  return(-ll)
}

NBD.histogram = function(params,data){
  r = params[1]
  alpha = params[2]
  x = data[,1]
  f_x = data[,2]
  prob_x = gamma(r+x)/(gamma(r)*factorial(x))*(alpha/(alpha+1)^r)*(1/(alpha+1)^x)
  predicted_f_x = as.integer(prob_x*sum(f_x))
  data = t(as.matrix(data.frame(f_x, predicted_f_x)))
  barplot(data, names.arg=x, beside=TRUE)
}

ZNBD.log_likelihod = function(params,data){
  r = params[1]
  alpha = params[2]
  spike = params[3]
  x = data[,1]
  f_x = data[,2]
  prob_x = gamma(r+x)/(gamma(r)*factorial(x))*(alpha/(alpha+1))^r*(1/(alpha+1))^x
  prob_x  = (1-spike)*prob_x
  prob_x[1] = prob_x[1]+spike
  ll = sum(f_x*log(prob_x))
  return(-ll)
}

ZNBD.histogram = function(params,data){
  r = params[1]
  alpha = params[2]
  spike = params[3]
  x = data[,1]
  f_x = data[,2]
  prob_x = gamma(r+x)/(gamma(r)*factorial(x))*(alpha/(alpha+1)^r)*(1/(alpha+1)^x)
  prob_x  = (1-spike)*prob_x
  prob_x[1] = prob_x[1]+spike
  predicted_f_x = as.integer(prob_x*sum(f_x))
  data = t(as.matrix(data.frame(f_x, predicted_f_x)))
  barplot(data, names.arg=x, beside=TRUE)
}