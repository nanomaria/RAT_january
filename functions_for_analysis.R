######
#Functions used for RAT analysis
#Maria M. Martignoni (mmartignonim@mun.ca)
######

# Functions



# Functions:

#prop.pos.hh gives the percent of households testing positives on Jan 22 OR on Jan 25.

prop.pos.hh = function(data,tot.h)
{
  hpos = c()
  for (i in 1:dim(data)[1]){
    #select columns that have positive results for Jan 22 or Jan 25
    household = data[i,c(2,3,5,6,8,9,11,12)]  
    # if there is any positives, count 1, otherwise, count zero
    pos = length(household[household>0])
    if (pos > 0){hpos[i] = 1}
    else{hpos[i]=0}
  }
  # number of of households reporting positive tests
  tot.hpos = sum(hpos)
  #percentage of households with at least one positive results
  prop.hpos = tot.hpos/tot.h
  prop.hpos.r = round(prop.hpos, digits = 3)
  mylist = c(tot.hpos,prop.hpos.r*100)
  return(mylist)
}


## Confidence intervals

# mplus : total positive tests
# N : total number of tests

f.jc<-function(mplus,N,splus=0.9044,sminus=0.994,
               B=10000, confcoef=0.95){
  b0<-floor(B*(1-confcoef)/2)
  b1<-ceiling(B*(1+confcoef)/2)
  pb<-rep(NA,B)
  n<-N
  smi<-1-sminus
  q0<-smi-splus
  p0<-(1-sminus-mplus/n)/q0
  if(p0<0) p0<-0
  if(p0>1) p0<-1
  theta<-p0*splus+(1-p0)*smi
  for(i in seq(1,B)){
    m<-rbinom(1,size=n,prob=theta)
    pb[i]<-(smi-m/n)/q0
    if(pb[i]<0) pb[i]<-0
    if(pb[i]>1) pb[i]<-1
  }
  pb<-sort(pb)
  v=c(p0,pb[b0],pb[b1])
  return(v)
}





# prop.pos.K6712 gives the percent of positive tests in K-6 with respect to 7-12.
# We define positive cases as positives on Jan 25,
# and positive cases on Jan 22 that have not been reported on Jan 25.
# We define negative cases as negative cases on Jan 25.

prop.pos.K6712 = function(data){
  # consider positive cases on Jan 25
  pos.25.K6 = c()
  pos.25.712 = c()
  neg.25.K6 = c()
  neg.25.712 = c()
  for (i in 1:dim(data)[1]){
    #add up how many asymptomatic and symptomatic positives there is in each household (on Jan 25)
    pos.25.K6[i] = sum(data[i,c(8,9)] )
    pos.25.712[i] = sum(data[i,c(11,12)] )
    neg.25.K6[i] = sum(data[i,7])
    neg.25.712[i] = sum(data[i,10])
  }
  # consider positive cases on Jan 22,
  # that have then not successively been reported on Jan 25
  pos.no.K6 = c()
  pos.no.712 = c()
  pos.K6 = c()
  pos.712 = c()
  for (i in 1:dim(data)[1]){
    if (data$X22.pos.no.K6[i] > data$X25.pos.no.K6[i] & sum(data[i,1:6]) > sum(data[i,7:12]) )
    {pos.no.K6[i] = data$X22.pos.no.K6[i]-data$X25.pos.no.K6[i]} else
    {pos.no.K6[i] = 0}
    if (data$X22.pos.K6[i] > data$X25.pos.K6[i] & sum(data[i,1:6]) > sum(data[i,7:12]))
    {pos.K6[i] = data$X22.pos.K6[i]-data$X25.pos.K6[i]} else
    {pos.K6[i] = 0}
    if (data$X22.pos.no.712[i] > data$X25.pos.no.712[i] & sum(data[i,1:6]) > sum(data[i,7:12]))
    {pos.no.712[i] = data$X22.pos.no.712[i]-data$X25.pos.no.712[i]} else
    {pos.no.712[i] = 0}
    if (data$X22.pos.712[i] > data$X25.pos.712[i] & sum(data[i,1:6]) > sum(data[i,7:12]))
    {pos.712[i] = data$X22.pos.712[i]-data$X25.pos.712[i]} else
    {pos.712[i] = 0}
  }
  tot.pos.22.K6 = sum(pos.no.K6)+sum(pos.K6)
  tot.pos.22.712 = sum(pos.no.712) + sum(pos.712)
  # add positive cases on Jan 22 (not reported then on Jan 25) and positive cases on Jan 25 
  tot.pos.K6.t = tot.pos.22.K6+sum(pos.25.K6)
  tot.pos.712.t = tot.pos.22.712+sum(pos.25.712)
  tot.neg.K6.t = sum(neg.25.K6)
  tot.neg.712.t = sum(neg.25.712)
  
  tot.pos.t = tot.pos.K6.t+tot.pos.712.t
  tot.neg.t = tot.neg.K6.t+tot.neg.712.t
  
  prop.pos = tot.pos.t/(tot.neg.t+tot.pos.t)
  prop.pos.K6.m = tot.pos.K6.t/(tot.pos.K6.t+tot.pos.712.t)
  prop.pos.712.m = tot.pos.712.t/(tot.pos.K6.t+tot.pos.712.t)
  prop.pos.K6 = tot.pos.K6.t/(tot.neg.t+tot.pos.t)
  prop.pos.712 = tot.pos.712.t/(tot.neg.t+tot.pos.t)
  
  
  
  labels = c('prop positives','prop positives in K6', 'prop positives in 7-12','prop of total in K6','prop of total in 7-12',
             'total positives','total positives in K6','total positives in 7-12',
             'total negatives','total negatives in K6','total negatives in 7-12')
  stat.K6712 = c(prop.pos, prop.pos.K6, prop.pos.712,prop.pos.K6.m,prop.pos.712.m,
                 tot.pos.t,tot.pos.K6.t,tot.pos.712.t,
                 tot.neg.t, tot.neg.K6.t, tot.neg.712.t)
  round.stat = round(stat.K6712,digits=3)
  df.pos.K6712 = data.frame(labels,round.stat)
  return(df.pos.K6712)
}  
  
  
  
  ## Functions to count asymptomatic cases
  
  
  # prop.pos.no gives the percent of asymptomatic and symptomatic cases
  # We consider positive cases on Jan 25 and because PH instructed to not repeat the rapid test
  # if positive (and as some households have done so),
  # we also consider positive tests on Jan 22, that have no been reported again on Jan 22.
  
  #prop.pos.no.K6 and prop.pos.no.712 gives the percent of positives in K-6 with respect to 7-12.
  # We define positive cases as in the function above. 
  
  
  prop.pos.no = function(data){
    # consider positive cases on Jan 25
    tot.pos.no.25 = c()
    tot.pos.25 = c()
    for (i in 1:dim(data)[1]){
      #add up how many asymptomatic and symptomatic positives there is in each household (on Jan 25)
      pos.no.25 = sum(data[i,c(8,11)] )
      pos.25 = sum(data[i,c(9,12)]  )
      tot.pos.no.25[i] = pos.no.25
      tot.pos.25[i] = pos.25
    }
    # consider positive cases on Jan 22,
    # that have then not successively been reported on Jan 25
    pos.no.K6 = c()
    pos.K6 = c()
    pos.no.712 = c()
    pos.712 = c()
    for (i in 1:dim(data)[1]){
      if (data$X22.pos.no.K6[i] > data$X25.pos.no.K6[i] & sum(data[i,1:6]) > sum(data[i,7:12]) )
      {pos.no.K6[i] = data$X22.pos.no.K6[i]-data$X25.pos.no.K6[i]} else
      {pos.no.K6[i] = 0}
      if (data$X22.pos.K6[i] > data$X25.pos.K6[i] & sum(data[i,1:6]) > sum(data[i,7:12]))
      {pos.K6[i] = data$X22.pos.K6[i]-data$X25.pos.K6[i]} else
      {pos.K6[i] = 0}
      if (data$X22.pos.no.712[i] > data$X25.pos.no.712[i] & sum(data[i,1:6]) > sum(data[i,7:12]))
      {pos.no.712[i] = data$X22.pos.no.712[i]-data$X25.pos.no.712[i]} else
      {pos.no.712[i] = 0}
      if (data$X22.pos.712[i] > data$X25.pos.712[i] & sum(data[i,1:6]) > sum(data[i,7:12]))
      {pos.712[i] = data$X22.pos.712[i]-data$X25.pos.712[i]} else
      {pos.712[i] = 0}
    }
    tot.pos.no.22 = sum(pos.no.K6)+sum(pos.no.712)
    tot.pos.22 = sum(pos.K6) + sum(pos.712)
    # add positive cases on Jan 22 (not reported then on Jan 25) and positive cases on Jan 25 
    tot.pos.no.t = tot.pos.no.22+sum(tot.pos.no.25)
    tot.pos.t = tot.pos.22+sum(tot.pos.25)
    prop.no = tot.pos.no.t/(tot.pos.no.t+tot.pos.t)
    
    labels.t = c('prop pos no symptoms','total pos no symptoms','total positives (symptoms+no symptoms)')
    stat.asympt = c(prop.no, tot.pos.no.t,tot.pos.t+tot.pos.no.t)
    round.stat = round(stat.asympt, digits=3)
    df.no.t = data.frame(labels.t,round.stat)
    return(df.no.t)
  }
  
  
  ## Function to count asymptomatics between K6
  prop.pos.no.K6 = function(data){
    # consider positive cases on Jan 25
    tot.pos.no.25 = c()
    tot.pos.25 = c()
    for (i in 1:dim(data)[1]){
      #add up how many asymptomatic and symptomatic positives there is in each household (on Jan 25)
      pos.no.25 = sum(data[i,8] )
      pos.25 = sum(data[i,9]  )
      tot.pos.no.25[i] = pos.no.25
      tot.pos.25[i] = pos.25
    }
    # consider positive cases on Jan 22,
    # that have then not successively been reported on Jan 25
    pos.no.K6 = c()
    pos.K6 = c()
    for (i in 1:dim(data)[1]){
      if (data$X22.pos.no.K6[i] > data$X25.pos.no.K6[i] & sum(data[i,1:6]) > sum(data[i,7:12]) )
      {pos.no.K6[i] = data$X22.pos.no.K6[i]-data$X25.pos.no.K6[i]} else
      {pos.no.K6[i] = 0}
      if (data$X22.pos.K6[i] > data$X25.pos.K6[i] & sum(data[i,1:6]) > sum(data[i,7:12]))
      {pos.K6[i] = data$X22.pos.K6[i]-data$X25.pos.K6[i]} else
      {pos.K6[i] = 0}
    }
    tot.pos.no.22 = sum(pos.no.K6)
    tot.pos.22 = sum(pos.K6)
    # add positive cases on Jan 22 (not reported then on Jan 25) and positive cases on Jan 25 
    tot.pos.no.t = tot.pos.no.22+sum(tot.pos.no.25)
    tot.pos.t = tot.pos.22+sum(tot.pos.25)
    prop.no = tot.pos.no.t/(tot.pos.no.t+tot.pos.t)
    
    
    labels.K6 = c('prop pos no symptoms in K6','total pos no symptoms in K6','total positives (symptoms+no symptoms) in K6')
    stat.asympt = c(prop.no, tot.pos.no.t,tot.pos.t+tot.pos.no.t)
    round.stat = round(stat.asympt, digits=3)
    df.no.K6 = data.frame(labels.K6,round.stat)
    return(df.no.K6)
    
  }
  
  
  ## percentage of asymptomatic between 7-12
  
  prop.pos.no.712 = function(data){
    # consider positive cases on Jan 25
    tot.pos.no.25 = c()
    tot.pos.25 = c()
    for (i in 1:dim(data)[1]){
      #add up how many asymptomatic and symptomatic positives there is in each household (on Jan 25)
      pos.no.25 = sum(data[i,11] )
      pos.25 = sum(data[i,12]  )
      tot.pos.no.25[i] = pos.no.25
      tot.pos.25[i] = pos.25
    }
    # consider positive cases on Jan 22,
    # that have then not successively been reported on Jan 25
    pos.no.712 = c()
    pos.712 = c()
    for (i in 1:dim(data)[1]){
      if (data$X22.pos.no.712[i] > data$X25.pos.no.712[i] & sum(data[i,1:6]) > sum(data[i,7:12]))
      {pos.no.712[i] = data$X22.pos.no.712[i]-data$X25.pos.no.712[i]} else
      {pos.no.712[i] = 0}
      if (data$X22.pos.712[i] > data$X25.pos.712[i] & sum(data[i,1:6]) > sum(data[i,7:12]))
      {pos.712[i] = data$X22.pos.712[i]-data$X25.pos.712[i]} else
      {pos.712[i] = 0}
    }
    tot.pos.no.22 = sum(pos.no.712)
    tot.pos.22 = sum(pos.712)
    # add positive cases on Jan 22 (not reported then on Jan 25) and positive cases on Jan 25 
    tot.pos.no.t = tot.pos.no.22+sum(tot.pos.no.25)
    tot.pos.t = tot.pos.22+sum(tot.pos.25)
    prop.no = tot.pos.no.t/(tot.pos.no.t+tot.pos.t)
    
    labels.712 = c('prop pos no symptoms in 712','total pos no symptoms in 712','total positives (symptoms+no symptoms) in 712')
    stat.asympt = c(prop.no, tot.pos.no.t,tot.pos.t+tot.pos.no.t)
    round.stat = round(stat.asympt, digits=3)
    df.no.712 = data.frame(labels.712,round.stat)
    return(df.no.712)
    
  }
  
  