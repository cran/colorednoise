## ----preliminaries, echo = F, message = F--------------------------------
library("colorednoise")
library("dplyr")
library("ggplot2")
library("purrr")

## ----comparing red and blue noise, echo = F------------------------------
blue <- raw_noise(timesteps = 100, mu = 0.5, sigma = 0.2, phi = -0.5)
red <- raw_noise(timesteps = 100, mu = 0.5, sigma = 0.2, phi = 0.5)
ggplot(data = NULL, aes(x = c(1:100), y = blue)) + geom_line(color="blue") + theme_minimal() + theme(axis.title = element_blank()) + ggtitle("Blue Noise")
ggplot(data = NULL, aes(x = c(1:100), y = red)) + geom_line(color="red") + theme_minimal() + theme(axis.title = element_blank()) + ggtitle("Red Noise")

## ----generate noise------------------------------------------------------
red <- raw_noise(timesteps = 100, mu = 0.3, sigma = 1.2, phi = 0.5)
red[1:10]
blue <- raw_noise(timesteps = 100, mu = 0.3, sigma = 1.2, phi = -0.5)
blue[1:10]
ggplot(data = NULL, aes(x = c(1:100), y = blue)) + geom_line(color="blue") + theme_minimal() + theme(axis.title = element_blank()) + ggtitle("Blue Noise")
ggplot(data = NULL, aes(x = c(1:100), y = red)) + geom_line(color="red") + theme_minimal() + theme(axis.title = element_blank()) + ggtitle("Red Noise")

## ----estimate noise------------------------------------------------------
raw_estim(red)
raw_estim(blue)

## ----generate replicates-------------------------------------------------
sd_range <- raw_estim_loop(timesteps = 20, mu = 0.3, sigma = c(0.5, 0.7, 0.9, 1.1, 1.3, 1.5), phi = c(-0.5, 0, 0.5), replicates = 30)
head(sd_range)

## ----plot CIs------------------------------------------------------------
sd_range %>% group_by(phi, sigma) %>% summarize_at(funs(lower.ci = ((function(bar){quantile(bar, probs=c(0.05, 0.95))[[1]]})(.)),
           upper.ci = ((function(bar){quantile(bar, probs=c(0.05, 0.95))[[2]]})(.)),
           mean = mean(.)), .vars = vars(autocorrelation)) -> summ
ggplot(summ, aes(x = sigma, y = mean)) +
  geom_pointrange(aes(ymin = lower.ci, ymax = upper.ci, color = factor(phi)), size = 0.8) + 
  geom_hline(yintercept = 0, linetype = 2, color = "#C0C0C0") +
  geom_hline(yintercept = -0.5, linetype = 2, color = "#0033FF") +
  geom_hline(yintercept = 0.5, linetype = 2, color = "#CC0000") +
  theme(text=element_text(size=20)) + xlab("Standard Deviation") + ylab("Estimated Autocorrelation") + scale_colour_manual(values = c("#0033FF", "#C0C0C0", "#CC0000")) + labs(color="Noise color") + theme_light()

## ----model single population---------------------------------------------
set.seed(3935)
series1 <- timeseries(start=20, timesteps=20, survPhi=0.4, fecundPhi=0.4, survMean=0.5, survSd=0.2, fecundMean=1, fecundSd=0.7)
ggplot(series1, aes(x=timestep, y=population)) + geom_line()

## ----simulate many populations-------------------------------------------
sims <- autocorr_sim(timesteps = seq(5, 60, 5), start = 200, survPhi = c(-0.5, -0.25, -0.2, -0.1, 0, 0.1, 0.2, 0.25, 0.5), fecundPhi = 0, survMean = 0.4, survSd = 0.05, fecundMean = 1.5, fecundSd = 0.2, replicates = 100)
ggplot(sims[[6]], aes(x=timestep, y=population)) + geom_line()

## ----estimate autocorrelation--------------------------------------------
sims %>% map(~group_by(., survPhi, timesteps)) %>% map(~summarize(., acf.surv = autocorrelation(est_surv))) %>% bind_rows -> estimates

## ----plotting estimates--------------------------------------------------
estimates %>% group_by(survPhi, timesteps) %>% summarize_at(funs(lower.ci = ((function(bar){quantile(bar, probs=c(0.05, 0.95), na.rm = T)[[1]]})(.)),
           upper.ci = ((function(bar){quantile(bar, probs=c(0.05, 0.95), na.rm = T)[[2]]})(.)),
           mean = mean(., na.rm = T)), .vars = vars(acf.surv)) -> summ2
# Noise color values for the graph in hexadecimal
noise = c("#0033FF", "#3366FF", "#6699FF", "#99CCFF", "#FFFFFF", "#FF9999", "#FF6666","#FF0000", "#CC0000")
ggplot(summ2, aes(x=timesteps, y=mean)) + geom_point(size=8, aes(color=factor(survPhi))) +
# This creates confidence intervals around the autocorrelations  
geom_pointrange(aes(ymin=lower.ci, ymax=upper.ci), size=0.8) + 
  # Adds a line for the true autocorrelation value
  geom_hline(aes(yintercept=survPhi), linetype=2) +
# This facets the plots by true autocorrelation value  
facet_wrap( ~ survPhi) + 
# This increases the font size and labels everything nicely  
  theme(text=element_text(size=13)) + xlab("Time series lengths") + ylab("Estimated Autocorrelation") + scale_colour_manual(values=noise) + labs(color="Noise color") + ggtitle("Bias in estimates of autocorrelation of survival") + scale_y_continuous(limits=c(-0.85, 0.75))

