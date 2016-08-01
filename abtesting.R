library("pwr")
library("ggplot2")

#generating sample data for two versions
control_visits = 22936
control_clicks = 11553
challenger_visits = 24427 
challenger_clicks = 11694

#assuming the sample size per week
sample_per_week = control_visits + challenger_visits

#calculating the proportions
control_ctr = control_clicks/control_visits
challenger_ctr = challenger_clicks/challenger_visits

#This function returns the sample size needed to perform the test with power (80% , 90%) for various 
#effect sizes we would like to see. 
power_analysis <- function(control_ctr, effect_size, alpha = 0.05, power)
{
  power_list = c()
  effect_size_list = c()
  sample_size_list = c()
  num_rows = 1
  c1 = c()
  c2 = c()
  c3 = c()
  for (p in 1:length(power))
  {
    for (es in 1:length(effect_size))
    {
      pa = control_ctr
      pb = (sin((effect_size[es] + 2*asin(sqrt(pa)))/2))^2
      pd = pb - pa
      power_cal = power.prop.test(n = NULL, p1 = pa, p2 = pb, sig.level = alpha,
                                  alternative = "two.sided",power = power[p],strict = TRUE)
      sample_size = round (power_cal$n*2, digits = 2)
      num_rows <- num_rows + 1
      power_list <- append(x = power_list, values = power[p]*100)
      effect_size_list <- append(x = effect_size_list, values = effect_size[es])
      sample_size_list <- append(x = sample_size_list, values = sample_size)
      c1 <- append(x = c1, values = round(x = pa*100, digits = 2))
      c2 <- append(x = c2, values = round(x = pb*100, digits = 2))
      c3 <- append(x = c3, values = round(x = pd*100, digits = 2))
    } 
  }
  power_df <- cbind.data.frame(power_list,effect_size_list,c1, c2, c3, sample_size_list)
  colnames(power_df)<- c('Power','Effect_Size','Control_CTR', 'Challenger_CTR','Difference_CTR' ,'Sample_Size')
  return (power_df)
}


#this function plots the power analysis graph for the above analysis
power_analysis_plot <-function(df)
{
  
  df$Power <- as.factor(df$Power)
  p<- ggplot(data = df, aes(x = Effect_Size, y=weeks, group = Power,colour = Power))+
    geom_line(size = 1.2)  + geom_point()
  xmarks<-unique(df$Effect_Size)
  p<-p + scale_y_continuous(name = "number of weeks",
                            breaks = seq(from = 1, to = max(df$weeks), by = 2))
  xlabels <- paste0(round(xmarks, 3)*100, "%",sep = "")
  p <- p + scale_x_continuous(breaks = xmarks, labels = xlabels, name = "Effect Size")
  return (p)
}

pow_analysis_df <- power_analysis (control_ctr = control_ctr, effect_size = seq(from = 0.01, to = 0.1, by = 0.005),
                                   alpha = 0.05, power = c(0.8,0.9))
pow_analysis_df$weeks <- round(x = pow_analysis_df$Sample_Size/sample_per_week , digits = 2)
pow_analysis_df
power_analysis_plot(pow_analysis_df)


#Two sample difference in the proprtion test
N1 = control_visits
N2 = challenger_visits
P1 = control_clicks/control_visits
P2 = challenger_clicks/challenger_visits
P0  = (P1*N1 + P2*N2)/(N1+N2)
SE = sqrt(P0*(1-P0)*(1/N1 + 1/N2))
z = (P1 - P2 - 0)/SE
pvalue = 1 - 2*(pnorm(q = z, mean = 0, sd = 1) - 0.5)
#Margin of error
ME = 1.96*SE
statistic =  P1 - P2 
CI1 = statistic -ME
CI2 = statistic +ME
#95% Confidence Interval
c(CI1 , CI2)
#(0.01596437, 0.03398246)

#to check for the power of the test
power_test <- power.prop.test(p1 = P1, p2 = P2, sig.level = 0.05, n = N1 + N2, 
                              alternative = "two.sided")
power_test
#power = 100%