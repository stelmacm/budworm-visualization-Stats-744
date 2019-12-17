library(tidyverse)
library(RColorBrewer)
library(colorspace)
## Read necessary data files ##
allfits <- read.csv('prov_fits.csv', header = TRUE)
ttm.df <- read.csv('ttm_df.csv', header = TRUE)

## Input temp vector, province and stage, and return dev rate
pred.mod.prov <- function(temp, pr, st) {
  fit <- subset(allfits, prov == pr & stage == st)
  params <- as.numeric(fit[,1:4])
  tau <- sapply(temp, function(x) {max(0, (x-4.4)/(38-4.4))})
  params <- as.numeric(params)
  pred <- params[1]*(1/(1 + exp(params[2] - params[3]*tau)) - exp((tau - 1)/params[4]))
  pred <- sapply(pred, function(x) {max(x, 0)})
  return(pred)
}

## Input parameters and quantile, and output respective variation multiplier
pred.var <- function(params, quantile) {
  params <- as.numeric(params)
  delta <- 1-(1/params[2])*log((quantile^(-params[1]) - 1)/(0.5^(-params[1]) - 1))
  return(delta)
}

stages <- c('L2', 'L3', 'L4', 'L5', 'L6')
temps <- seq(4.5, 38, by = 0.25)
plev <- c('in', 'qc', 'on', 'nb','ip')

## Generate curves for ribbons
dev.lst <- lapply(1:nrow(allfits), function(x) {
  af <- allfits[x,]
  prov <- as.character(af$prov)
  stage <- as.character(af$stage)
  p <- pred.mod.prov(temp = temps, prov, stage)
  data <- data.frame('temp' = temps, 'rate' = p, prov, stage)
  q <- af$q
  k <- af$k
  sq <- seq(0.01, 0.99, by = 0.01)
  sq <- c(0.001, sq, 0.999)
  pr <- pred.var(c(q, k), sq)
  p.mat <- matrix(p, nr = length(p))
  pr.mat <- matrix(pr, nr = 1)
  ribbons <- p.mat %*% pr.mat
  colnames(ribbons) <- sapply(sq, function(x) {sprintf('q%s', x*100)})
  data <- cbind(data, ribbons)
  return(data)
})
dev <- do.call('rbind', dev.lst)
dev <- subset(dev, stage %in% c('L2', 'L3', 'L4', 'L5', 'L6'))

## Re-order province factor levels
plev <- c('in', 'qc', 'on', 'nb','ip')
dev$prov <- factor(dev$prov, levels = plev)
ttm.df$prov <- factor(ttm.df$prov, levels = plev)

## Create facet labels
prov.labs <- c('Northwest Territories', 'Quebec', 
               'Ontario', 'New Brunswick', 'Lab-Reared')
names(prov.labs) <- levels(dev$prov)

## Remove lab-reared data
dev2 <- dev
dev2 <- subset(dev2, prov != 'ip')
dev2$prov <- factor(dev2$prov)
ttm.df2 <- ttm.df
ttm.df2 <- subset(ttm.df, prov != 'ip')
ttm.df2$prov <- factor(ttm.df2$prov)

## Plot 2 Code
s <- 'L3'
ggplot(data = subset(dev2, stage == s), 
       aes(x = temp, y = rate)) +
  scale_y_continuous(limits = c(0, 1), 
                     sec.axis = sec_axis(~1/., breaks = round(1/c(seq(1, 0.25, by = -0.25), 0.125, 1/64)), 
                                         name = 'Days in Stage'))+
  theme_minimal() +
  facet_wrap(~prov, labeller = labeller(prov = prov.labs)) +
  geom_ribbon(aes(ymin = q0.1, ymax = q99.9), alpha = 0.1, fill = 'gray60') +
  geom_ribbon(aes(ymin = q1, ymax = q99), alpha = 0.15, fill = 'gray60') +
  geom_ribbon(aes(ymin = q2, ymax = q98), alpha = 0.175, fill = 'gray60') +
  geom_ribbon(aes(ymin = q6, ymax = q94), alpha = 0.25, fill = 'gray60') +
  geom_ribbon(aes(ymin = q10, ymax = q90), alpha = 0.325, fill = 'gray60') +
  geom_ribbon(aes(ymin = q14, ymax = q86), alpha = 0.4, fill = 'gray60') +
  geom_ribbon(aes(ymin = q18, ymax = q82), alpha = 0.475, fill = 'gray60') +
  geom_ribbon(aes(ymin = q22, ymax = q78), alpha = 0.55, fill = 'gray60') +
  geom_ribbon(aes(ymin = q26, ymax = q74), alpha = 0.625, fill = 'gray60') +
  geom_ribbon(aes(ymin = q30, ymax = q70), alpha = 0.7, fill = 'gray60') +
  geom_ribbon(aes(ymin = q34, ymax = q66), alpha = 0.775, fill = 'gray60') +
  geom_ribbon(aes(ymin = q38, ymax = q62), alpha = 0.85, fill = 'gray60') +
  geom_ribbon(aes(ymin = q42, ymax = q58), alpha = 0.925, fill = 'gray60') +
  geom_ribbon(aes(ymin = q46, ymax = q54), alpha = 1, fill = 'gray60') +
  geom_line() +
  geom_point(data = subset(ttm.df2, stage == s), 
             aes(x = temp, y = rate, 
                 size = count), shape = 19, alpha = 0.6)+
  guides(color = FALSE, fill = FALSE) +
  labs(x = expression("Temperature " ( degree~C)), 
       y = 'Development Rate (% of Stage/Day)', 
       size = '# Obs') +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0), size = 13),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0), size = 13),
        axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 20)),
        legend.position = 'top', 
        title = element_text(size = 15),
        legend.title = element_text(size = 13),
        strip.text.x = element_text(size = 12),
        axis.text = element_text(size = 11),
        legend.text = element_text(size = 11))  


## Set colour scheme
cl <- sequential_hcl(7, "ag_Sunset")

## Static Plot 3 Code
ggplot(data=subset(sims.all.df, year == 2018), 
            aes(x=date, y = proportion, group=stage, fill=stage)) +
  scale_fill_manual(values = cl) +
  geom_density(position="fill", stat = 'identity', lwd = 0.05) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
  facet_wrap(~prov, scales = 'free_x', ncol = 1, 
             labeller = labeller(prov = prov.labs)) +
  labs(y = 'Proportion of Population in Stage', 
       fill = 'Larval Stage', x = 'Date', 
       title = '2018 Spruce Budworm Development by Location') +
  guides(alpha = FALSE)