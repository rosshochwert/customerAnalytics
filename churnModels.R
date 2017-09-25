sBG.log.likelihood<-function(params, data) {
  a<-params[1]
  b<-params[2]
  alive = data$Subscribers[length(data$Subscribers)]
  lost_subscribers = dataInput()$Subscribers[-length(data$Subscribers)] - data$Subscribers[-1]
  time_periods = length(lost_subscribers)
  time = seq(from=1,to=time_periods,by=1)
  ll<-0
  ll<-ll+lost_subscribers*log(beta(a+1,b+time-1)/beta(a,b))
  ll.alive = alive*log(beta(a,b+time_periods)/beta(a,b))
  return(-sum(ll,ll.alive))
}


sBG.graph<-function(params, data) {
  a<-params[1]
  b<-params[2]
  time_periods = length(data$Time)*2
  time = seq(from=1,to=time_periods,by=1)
  probability_alive = beta(a,b+time)/beta(a,b)
  number_probability_alive = probability_alive*data$Subscribers[1]
  plot(number_probability_alive,type="l",ylab="Subscribers",xlab="Time Periods Since Acquisition", main = "Projected sBG vs. Actual Churn Rates")
  points(data$Subscribers)
}

sBG.table<-function(params,data){
  a<-params[1]
  b<-params[2]
  alive = df$Subscribers[length(df$Subscribers)]
  lost_subscribers = df$Subscribers[-length(df$Subscribers)] -df$Subscribers[-1]
  time_periods = length(lost_subscribers)
  time = seq(from=1,to=time_periods,by=1)
  prob_churn = beta(a+1,b+time-1)/beta(a,b)
  survival_function = 1-cumsum(prob_churn)
  ll = lost_subscribers*log(prob_churn)
  ll = c(ll,alive*log(1-sum(prob_churn)))
  ll = c(NA,ll)
  survival_function = c(1,survival_function,NA)
  time = c(0,time,time_periods+1)
  lost_subscribers = c(NA,lost_subscribers,NA)
  prob_churn = c(NA,prob_churn,1-sum(prob_churn))
  subcribers = c(df$Subscribers,NA)
  df = data.frame(time, subcribers, lost_subscribers, prob_churn, ll, survival_function)
}

BdW.log.likelihood<-function(params,data) {
  a<-params[1]
  b<-params[2]
  c<-params[3]
  alive = data$Subscribers[length(data$Subscribers)]
  lost_subscribers = data$Subscribers[-length(data$Subscribers)] -data$Subscribers[-1]
  time_periods = length(lost_subscribers)
  time = seq(from=1,to=time_periods,by=1)
  survival_function = exp(lgamma(b+time^c) + lgamma(a+b) - lgamma(b) - 
                            lgamma(a+b+time^c))
  prob_churn = c(1,survival_function[-length(survival_function)])-survival_function
  ll<-0
  ll<-ll+lost_subscribers*log(prob_churn)
  ll.alive = alive*log(1-sum(prob_churn))
  return(-sum(ll,ll.alive))
}

BdW.graph<-function(params,data) {
  a<-params[1]
  b<-params[2]
  c<-params[3]
  time_periods = length(data$Time)*2
  time = seq(from=1,to=time_periods,by=1)
  survival_function = exp(lgamma(b+time^c) + lgamma(a+b) - lgamma(b) - 
                            lgamma(a+b+time^c))
  number_probability_alive = survival_function*data$Subscribers[1]
  plot(number_probability_alive,type="l",ylab="Subscribers",xlab="Time Periods Since Acquisition", main = "Projected BdW vs. Actual Churn Rates")
  points(data$Subscribers)
}

BdW.table<-function(params,data){
  a<-params[1]
  b<-params[2]
  c<-params[3]
  alive = data$Subscribers[length(data$Subscribers)]
  lost_subscribers = data$Subscribers[-length(data$Subscribers)] -data$Subscribers[-1]
  time_periods = length(lost_subscribers)
  time = seq(from=1,to=time_periods,by=1)
  survival_function = exp(lgamma(b+time^c) + lgamma(a+b) - lgamma(b) - 
                            lgamma(a+b+time^c))
  prob_churn = c(1,survival_function[-length(survival_function)])-survival_function
  ll = lost_subscribers*log(prob_churn)
  ll = c(ll,alive*log(1-sum(prob_churn)))
  ll = c(NA,ll)
  survival_function = c(1,survival_function,NA)
  time = c(0,time,time_periods+1)
  lost_subscribers = c(NA,lost_subscribers,NA)
  prob_churn = c(NA,prob_churn,1-sum(prob_churn))
  subcribers = c(df$Subscribers,NA)
  df = data.frame(time, subcribers, lost_subscribers, prob_churn, ll, survival_function)
}

dW.log.likelihood<-function(params,data) {
  theta<-params[1]
  c<-params[2]
  alive = data$Subscribers[length(data$Subscribers)]
  lost_subscribers = data$Subscribers[-length(data$Subscribers)] -data$Subscribers[-1]
  time_periods = length(lost_subscribers)
  time = seq(from=1,to=time_periods,by=1)
  survival_function = (1-theta)^(time^c)
  prob_churn = c(1,survival_function[-length(survival_function)])-survival_function
  ll<-0
  ll<-ll+lost_subscribers*log(prob_churn)
  ll.alive = alive*log(1-sum(prob_churn))
  return(-sum(ll,ll.alive))
}

dW.graph<-function(params,data) {
  theta<-params[1]
  c<-params[2]
  time_periods = length(data$Time)*2
  time = seq(from=1,to=time_periods,by=1)
  survival_function = (1-theta)^(time^c)
  number_probability_alive = survival_function*data$Subscribers[1]
  plot(number_probability_alive,type="l",ylab="Subscribers",xlab="Time Periods Since Acquisition", main = "Projected dW vs. Actual Churn Rates")
  points(data$Subscribers)
}

dW.table<-function(params,data){
  theta<-params[1]
  c<-params[2]
  alive = df$Subscribers[length(df$Subscribers)]
  lost_subscribers = df$Subscribers[-length(df$Subscribers)] -df$Subscribers[-1]
  time_periods = length(lost_subscribers)
  time = seq(from=1,to=time_periods,by=1)
  survival_function = (1-theta)^(time^c)
  prob_churn = c(1,survival_function[-length(survival_function)])-survival_function
  ll = lost_subscribers*log(prob_churn)
  ll = c(ll,alive*log(1-sum(prob_churn)))
  ll = c(NA,ll)
  survival_function = (1-theta)^(time^c)
  survival_function = c(1,survival_function,NA)
  time = c(0,time,time_periods+1)
  lost_subscribers = c(NA,lost_subscribers,NA)
  prob_churn = c(NA,prob_churn,1-sum(prob_churn))
  subcribers = c(df$Subscribers,NA)
  df = data.frame(time, subcribers, lost_subscribers, prob_churn, ll, survival_function)
}