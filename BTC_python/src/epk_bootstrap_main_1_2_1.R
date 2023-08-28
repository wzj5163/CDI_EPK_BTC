# we use modify the formula of Q
# setwd(wd)

# output Q-density of log return, P-density of log return, EPK of log return

source("EPK_library.r")



# Mapping Table for Input

# Column ... X
# 1 - date
# 2 - IV
# 3 - 1 for Call, 0 for Put
# 4 - Tau 
# 5 - Strike
# 6 - assigned meanwhile as put2call[,6]+ put2call[,7] -  put2call[,5]*exp(- mean(put2call[,8])* tau);
# 7 - Spot
# 8 - Probably Interest Rate
# 9 - assigned meanwhile Moneyness: Spot/Strike


bootstrap_epk = function(f, ts, currdate, out_dir, bandwidth, mon_low, mon_up){
  
  # f = "tmp/confidence_band_input_17.csv"
  # ts = 'data/BTC_USD_Quandl.csv'
  #currdate = as.Date("2021-04-06")
  #out_dir = 'out/'
  
  # Create directory for output if it doesnt exist
  dir.create(out_dir, showWarnings = FALSE)
  
  ngrid = 200;
  #bandwidth = 0.08;
  
  print(paste0('bandwidth: ', bandwidth))
  
  XX = read.csv(f)
  XX[,1] = as.Date(XX[,1])
  
  if(nrow(XX) < 100){
    stop('Too few data points!')
  }
  
  #currdate = as.Date(unique(XX[,1]))
  #if(length(currdate) > 1) stop('too many dates')
  
  #for(currdate in unique_dates){
  
  iday = which(XX[,1] == currdate)
  
  day1 = XX[XX[,1] == currdate,] #XX[XX[,1]==XX[iday,1],]; 
  day1 = day1[day1[,2]>0,];
  tau = day1[1,4];
  
  day1.mat = day1[day1[,4]== tau,];
  day1.call = day1.mat[day1.mat[,3]==1,];
  day1.put = day1.mat[day1.mat[,3]==0,];
  
  # compute the moneyness
  day1.call[,9]=day1.call[,5]/day1.call[,7];
  day1.put[,9]=day1.put[,5]/day1.put[,7];
  
  # Filter
  # only out and at the money options
  #day1.call = day1.call[day1.call[,9]>=1,];
  #day1.put = day1.put[day1.put[,9]<=1,];
  
  # put to calls
  put2call = day1.put;
  put2call[,6] = put2call[,6]+ put2call[,7] -  put2call[,5]*exp(- mean(put2call[,8])* tau);
  
  put2call = put2call[order(put2call[,5]),];
  day1.call = day1.call[order(day1.call[,5]),];
  data = rbind(put2call,day1.call);
  
  # Subset:  IV between 0.05 and 2, Tau > 0
  data = data[(data[,2]>0.05) & (data[,2] < 2) & (data[,4] > 0),];
  
  # no - arbitrage condition
  #data = data[ ((data[,6]<data[,7]) && (data[,6]>data[,7]-data[,5]*exp(-data[,8]*data[,4])) ),]
  print('test no arbitrage condition')
  #write.table(data, file=paste(name,"_cleaned.txt", sep=""), row.names=F, col.names=F, quote=F)
  
  n = dim(data)[1];
  
  price.median = median(data[,7]);
  
  ## regression for volatility 
  
  volas = data[,c(2,9)];
  nsample = dim(volas)[1]; 
  
  # mon.min = min(volas[,2]);mon.max = max(volas[,2]);#fixed moneynes range
  mon.min = mon_low;mon.max = mon_up;
  volas[,2] = (volas[,2]-mon.min)/(mon.max-mon.min);
  mon.grid = seq(1/ngrid, 1, length=ngrid);
  mon.grid.scaled =  (mon.min+mon.grid*(mon.max-mon.min));
  
  if(length(mon.grid.scaled) > 400) stop('Moneyness Grid is too long!')
  
  # For the critical files, the domain is extremely long... how come that?!
  
  for(i in c(1:ngrid))
  {
    print(i)
    dist = volas[,2] - mon.grid[i]
    X = cbind(rep(1, length=nsample), dist, dist^2, dist^3);
    W = diag(K.h(dist, bandwidth));
    beta = t(solve(t(X) %*% W %*% X) %*% t(X) %*% W %*% volas[,1]);
    if (i==1) {sigmas=beta[c(1:3)];} else {sigmas=rbind(sigmas, beta[c(1:3)]);}
  }
  
  sigmas[,3] = 2*sigmas[,3];
  
  #### bands for the derivatives
  bands.init = bands(2, bandwidth, sigmas, volas[,2], volas[,1], mon.grid, n);
  # bands.init = bands(2, bandwidth, sigmas, volas[,2], volas[,1], mon.grid);
  
  #### applying rookley
  
  fder = rookley(mon.grid.scaled, sigmas[,1], sigmas[,2]/(mon.max-mon.min), sigmas[,3]/(mon.max-mon.min)^2, mean(data[,8]), tau);
  
  #### final computations
  
  strike.grid  =  price.median / mon.grid.scaled;
  d2fdX2 = (mon.grid.scaled^2*fder[,3]+2*mon.grid.scaled*fder[,2])/strike.grid^2;
  
  #### bands for SPD
  
  d1=(log(mon.grid.scaled)+tau*(mean(data[,8])+0.5*(sigmas[,1])^2)) / (sqrt(sigmas[,1])*sqrt(tau));
  d2=d1-sqrt(sigmas[,1])*sqrt(tau);
  dgds= exp(-mean(data[,8])*tau) * ( mon.grid.scaled^2 * dnorm(d1) / strike.grid^2 - exp(-mean(data[,8])*tau)*dnorm(d2)/mon.grid.scaled);
  
  band.limit = bands.init$L[3] * sqrt(bands.init$V[,3]) * dgds ; #* exp(-mean(data[,8])*tau);
  
  SPD = new.env();
  SPD$SPD = price.median^2 * exp(mean(data[,8])*tau) * d2fdX2  ;
  SPD$lo = SPD$SPD - abs(price.median^2 * exp(mean(data[,8])*tau) * d2fdX2 * band.limit );
  SPD$up = SPD$SPD + abs(price.median^2 * exp(mean(data[,8])*tau) * d2fdX2 * band.limit );
  SPD=as.list(SPD);
  
  SPD_logreturn = new.env();
  SPD_logreturn$SPD = price.median^2 * exp(mean(data[,8])*tau) * d2fdX2 * mon.grid.scaled;
  SPD_logreturn$lo = SPD_logreturn$SPD - abs(price.median^2 * exp(mean(data[,8])*tau) * d2fdX2 * band.limit * mon.grid.scaled);
  SPD_logreturn$up = SPD_logreturn$SPD + abs(price.median^2 * exp(mean(data[,8])*tau) * d2fdX2 * band.limit * mon.grid.scaled);
  SPD_logreturn=as.list(SPD_logreturn);
  
  #dax=read.table("dax_index.dat"); dax=dax[,2];
  #dax=dax[c((length(dax)-500):length(dax))];
  
  dax = read.csv(ts)
  dax$Date = as.Date(dax$Date)
  dax = dax[dax$Date <= currdate,]
  dax = dax[order(dax[,1], decreasing=FALSE),2]
  
  dax.scaled = 1+log(dax[c(ceiling(tau*31/0.0833):length(dax))]/dax[c(1:(length(dax)+1-ceiling(tau*31/0.0833)))]);
  dax.scaled=dax.scaled[c((length(dax.scaled)-500):length(dax.scaled))];
  kern=density(dax.scaled, bw="nrd0", from=mon.grid.scaled[1], to=mon.grid.scaled[ngrid], n=ngrid);
  
  dax.scaled_logreturn = log(dax[c(ceiling(tau*31/0.0833):length(dax))]/dax[c(1:(length(dax)+1-ceiling(tau*31/0.0833)))]);
  dax.scaled_logreturn = dax.scaled_logreturn[c((length(dax.scaled_logreturn)-500):length(dax.scaled_logreturn))];
  kern_logreturn = density(dax.scaled_logreturn, bw="nrd0", from=log(mon.grid.scaled[1]), to=log(mon.grid.scaled[ngrid]), n=ngrid);
  
  ########### Black-Scholes
  
  sigma.BS = mean((volas[volas[,1]<quantile(volas[,1], probs=0.75),])[,1]);
  q.BS = exp(-(log(mon.grid.scaled) - (mean(data[,8])-sigma.BS/2)*tau)^2/(2*tau*sigma.BS))/(mon.grid.scaled*sqrt(2*3.1415926*sigma.BS*tau));
  p.BS = dnorm(log(mon.grid.scaled), mean=mean(log(dax.scaled)), sd = sd(log(dax.scaled)))/mon.grid.scaled;
  
  d1 = (log(mon.grid.scaled) + (mean(data[,8])+sigmas[,1]/2)*tau)/sqrt(tau)/sigmas[,1]
  d2 = d1 - sqrt(tau)*sigmas[,1]
  C.BS = pnorm(d1)*price.median - mon.grid.scaled * price.median * exp(-mean(data[,8])*tau) * pnorm(d2)
  
  ########### plotting
  
  #main_txt = paste(currdate, " tau = ", tau, "\n")
  
  # Reverse for Moneyness
  #SPD$SPD = rev(SPD$SPD)
  #SPD$lo = rev(SPD$lo)
  #SPD$up = rev(SPD$up)
  #kern$y = rev(kern$y)
  #p.BS = rev(p.BS)
  #mon.grid.scaled = rev(mon.grid.scaled)
  
  pdf(paste(out_dir,"q_",currdate,'_',tau,".pdf", sep=""), width=10, height=6, onefile=F);
  matplot(mon.grid.scaled, cbind(SPD$SPD, SPD$lo, SPD$up), type="l",lty=1, lwd=c(2,1,1,2),col=c(2, "blue3", "blue3", "green3"), xlab="Moneyness", ylab="q(K)", cex=1.5, xlim=c(.85, 1.15), ylim=c(0,5)); # paste(date.dif[iday],
  dev.off()
  
  pdf(paste(out_dir,"EPK_",currdate,'_',tau,".pdf", sep=""), width=10, height=6, onefile=F);
  matplot(mon.grid.scaled, cbind(cbind(SPD$SPD, SPD$lo, SPD$up)/kern$y), type="l",lty=1, lwd=c(2,1,1,2),col=c(2, "blue3", "blue3", "green3"), xlab="Moneyness", ylab="EPK", cex=1.5, xlim=c(.85, 1.15), ylim=c(0,5)); #
  dev.off()
  
  pdf(paste(out_dir,"p_",currdate,'_',tau,".pdf", sep=""), width=10, height=6, onefile=F);
  matplot(mon.grid.scaled, cbind(kern$y, p.BS), type="l",lty=1, lwd=c(2,2),col=c(2, "green3"), xlab="Moneyness", ylab="q(K)", cex=1.5, xlim=c(.85, 1.15), ylim=c(0,5));
  dev.off()
  
  mod = lm(log(SPD$SPD)~ log(mon.grid.scaled));
  util.coef = mod$coef;
  
  if (exists("util.param")==0) {util.param = c(exp(util.coef[1]), -util.coef[2], cor(log(SPD$SPD), log(mon.grid.scaled))^2);
  } else {util.coef = cbind(util.coef,c(exp(util.coef[1]), -util.coef[2], cor(log(SPD$SPD), log(mon.grid.scaled))^2));}
  
  #} # end of the days
  
  #out = list(cbind(cbind(SPD$SPD, SPD$lo, SPD$up)/kern$y, q.BS/p.BS))
  out = list(mon.grid.scaled,
             SPD$SPD,
             SPD$SPD/kern$y,
             SPD$lo/kern$y,
             SPD$up/kern$y,
             log(mon.grid.scaled),
             SPD_logreturn$SPD,
             SPD_logreturn$SPD/kern_logreturn$y,
             SPD_logreturn$lo/kern_logreturn$y,
             SPD_logreturn$up/kern_logreturn$y,
             q.BS/p.BS,
             tau,
             sigmas[,1],
             C.BS,
             rep(price.median,ngrid),
             data[,1],
             data[,2],
             data[,3],
             data[,4],
             data[,5],
             data[,6],
             data[,7],
             data[,8],
             data[,9])
  # out=p.BS
  return(out)
  
}

#test = bootstrap_epk("tmp/confidence_band_input_17.csv")
#print(test)
