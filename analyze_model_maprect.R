# analyze model fitted by cmdstan on NYU-ABU DHABI cluster

require(dplyr)
require(tidyr)
require(rstan)
require(bayesplot)
require(ggplot2)
require(stringr)
require(forcats)
require(lubridate)
require(tidybayes)


# load and combine fitted posteriors

fit_mod1 <- rstan::read_stan_csv("data/arab_full_1.csv")
fit_mod2 <- rstan::read_stan_csv("data/arab_full_2.csv")
fit_mod3 <- rstan::read_stan_csv("data/arab_full_3.csv")
#fit_mod4 <- rstan::read_stan_csv("data/arab_full_4.csv")
fit_mod5 <- rstan::read_stan_csv("data/arab_full_5.csv")
combine_fit <- rstan::sflist2stanfit(list(fit_mod1,
                                          fit_mod2,
                                          fit_mod3,
                                          fit_mod5))

# need the original indices

orig_data <- rstan::read_rdump("data/to_maprect_cluster.R")
alldata <- orig_data$alldata
N <- (ncol(alldata)-4)/5
J <- orig_data$J
Tee <- orig_data$T

jj <- c(rep(1:J,times=Tee),
        rep((J+1):(2*J),times=Tee))
tt <- c(rep(1:Tee,each=J),
        rep(1:Tee,each=J))

combine_id <- tibble(jj=jj,tt=tt) %>% 
  mutate(index=1:n())

# let's plot the results...

time_series <- as.data.frame(combine_fit,pars="dparams") %>% 
  mutate(iter=1:n()) %>% 
  gather(key="variable",value="value",-iter) %>% 
  mutate(index=as.numeric(str_extract(variable,'[0-9]+')))

# merge in IDs

time_series <- left_join(time_series,
                         combine_id,by="index")

# add factor labels

time_series <- mutate(time_series,
                      label_id=factor(jj,
                                      levels=c(1,2,3,4,5,6,7,8),
                                      labels=c("Islamist Egypt",
                                               "Islamist Tunisia",
                                               "Secularist Egypt",
                                               "Secularist Tunisia",
                                               "Anti-Democratic Egypt",
                                               "Anti-Democratic Tunisia",
                                               "Democratic Egypt",
                                               "Democratic Tunisia"))) %>% 
  separate(label_id,into=c("Identity","Country"),sep=" ")

# fix time values

times <- readRDS('data/times.rds')
coup_day_new <- times$time_three[times$time_date==as.Date('2013-07-03')]

time_series <- left_join(time_series,times,by=c("tt"="time_three"))

# plot the buggers 2-D plot



d1 <- time_series %>% 
  mutate(Dimension=if_else(jj<5,"Religion","Democracy")) %>% 
  filter(Dimension=="Religion") %>% 
  group_by(time_date,
           Identity,
           Country,
           Dimension) %>% 
  summarize(Median_Religion=median(value),
            High_Religion=quantile(value,0.95),
            Low_Religion=quantile(value,0.05))

d1 %>% 
  ggplot(aes(y=Median_Religion,x=time_date)) +
  geom_ribbon(aes(ymin=Low_Religion,
                  ymax=High_Religion,
                  fill=paste(Identity,Country)),
              alpha=0.5) +
  scale_fill_brewer(type="qual",name="") +
  scale_y_continuous(breaks=c(-2.5,-2,-1.5,
                              -1,-0.5),
                     labels=c("More\nIslamist",
                              "-2",
                              "-1.5",
                              "-1",
                              "More\nSecular")) +
  geom_line(aes(linetype=paste(Identity,Country)),
            alpha=0.8) +
  scale_linetype(name="") +
    geom_vline(aes(xintercept=as.numeric(as.Date('2013-07-03'))),linetype=3) +
  geom_vline(aes(xintercept=as.numeric(as.Date('2013-07-25'))),linetype=3) +
  theme(panel.background = element_blank(),
        panel.grid = element_blank()) + 
  xlab("") +
  ylab("Islamist-Secular Axis") +
  annotate('text',x=as.Date(c('2013-07-03','2013-07-25')),
           y=c(-0.5,-1.3),label=c('Morsi Coup',
                                        'Assassination of\nMohamed\nBrahmi'),
           size=3,
           fontface="bold")

ggsave("d1_plot.png")
 
d2 <- time_series %>% 
  mutate(Dimension=if_else(jj<5,"Religion","Democracy")) %>% 
  filter(Dimension=="Democracy") %>% 
  group_by(time_date,
           Identity,
           Country,
           Dimension) %>% 
  summarize(Median_Democracy=median(value),
            High_Democracy=quantile(value,0.95),
            Low_Democracy=quantile(value,0.05)) 

d2 %>% 
  ggplot(aes(y=Median_Democracy,x=time_date)) +
  geom_ribbon(aes(ymin=Low_Democracy,
                  ymax=High_Democracy,
                  fill=paste(Identity,Country)),
              alpha=0.5) +
  scale_fill_brewer(type="qual",name="") +
  scale_y_continuous(breaks=c(1,0.5,0,
                              -0.5,-1,-1.5),
                     labels=c("Anti-\nDemocratic",
                              "0.5",
                              "0.0",
                              "-0.5",
                              "-1",
                              "Pro-\nDemocratic")) +
  geom_line(aes(linetype=paste(Identity,Country)),
            alpha=0.8) +
  scale_linetype(name="") +
  geom_vline(aes(xintercept=as.numeric(as.Date('2013-07-03'))),linetype=3) +
  geom_vline(aes(xintercept=as.numeric(as.Date('2013-07-25'))),linetype=3) +
  theme(panel.background = element_blank(),
        panel.grid = element_blank()) + 
  xlab("") +
  ylab("Democracy Axis") +
  annotate('text',x=as.Date(c('2013-07-03','2013-07-25')),
           y=c(0.7,-0.5),label=c('Morsi Coup',
                                 'Assassination of\nMohamed\nBrahmi'),
           size=3,
           fontface="bold")

ggsave("d2_plot.png")

# need original IDs to recombine D1 and D2

orig_ids <- tibble(tt=alldata[1,((2*N)+5):(3*N+4)],
                   jj1=alldata[1,((3*N)+5):(4*N+4)],
                   jj2=alldata[1,((4*N)+5):(5*N+4)]) %>% 
  mutate(jj1=factor(jj1,levels=c(1,2,3,4),
                    labels=c("Islamist Egypt",
                              "Islamist Tunisia",
                              "Secularist Egypt",
                              "Secularist Tunisia")),
         jj2=factor(jj2,levels=c(1,2,3,4),
                    labels=c("Anti-Democratic Egypt",
                             "Anti-Democratic Tunisia",
                             "Democratic Egypt",
                             "Democratic Tunisia"))) %>% 
  left_join(times,by=c("tt"="time_three")) %>% 
  separate(jj1,into=c("Sect","Country"),sep=" ") %>% 
  separate(jj2,into=c("Democracy","Country2"),sep=" ") %>% 
  select(-Country2) %>% 
  left_join(select(d2,Median_Democracy,
                   High_Democracy,
                   Low_Democracy,
                   Identity,
                   Country,
                   time_date),
            by=c("time_date",
                 "Democracy"="Identity",
                 "Country"="Country")) %>% 
  left_join(select(d1,Median_Religion,
                   High_Religion,
                   Low_Religion,
                   Identity,
                   Country,
                   time_date),
            by=c("time_date",
                 "Sect"="Identity",
                 "Country"="Country"))

# let's calculate average names

avg_by_group <- group_by(orig_ids,Country,Sect,Democracy) %>% 
  summarize(avg_dem=mean(Median_Democracy),
            avg_rel=mean(Median_Religion))

require(ggrepel)

orig_ids %>% 
  ggplot(aes(y=Median_Religion,
             x=Median_Democracy)) +
  geom_point(aes(shape=Sect,colour=Democracy)) +
  scale_colour_brewer(type="qual") +
  #guides(colour="none") +
  # geom_errorbar(aes(ymin=Low_Religion,
  #                   ymax=High_Religion)) +
  # geom_errorbarh(aes(xmin=Low_Religion,
  #                    xmax=High_Religion)) +
  # geom_line(aes(linetype=paste(Sect,Democracy,Country),colour=paste(Sect,Democracy,Country)),
  #           size=.5) +
  theme(panel.background = element_blank(),
        panel.grid = element_blank()) +
  geom_text_repel(data=avg_by_group,
                  aes(y=avg_rel,
                      x=avg_dem,
                      label=paste(Sect, Democracy,Country,sep="\n")),
                  size=4) +
  scale_x_continuous(breaks=c(-1.5,-1,-0.5,0,0.5),
                     labels=c("Pro-\nDemocratic",
                              "-1",
                              "-0.5",
                              "0.0",
                              "Anti-\nDemocratic")) +
  scale_y_continuous(breaks=c(-2.5,-2.0,-1.5,-1.0,-0.5),
                     labels=c("More\nIslamist",
                              "-2.0",
                              "-1.5",
                              "-1.0",
                              "More\nSecular")) +
  ylab("Religion Dimension") +
  xlab("Democracy Dimension")

ggsave("2d_plot.png")

# now need to calculate our ideal point marginal effects!!
  
sigmas <- as.data.frame(combine_fit,"varparams") %>% 
  mutate(iter=1:n()) %>% 
  gather(key = "variable",
        value="value",-iter)

# need to separate indices and figure out what the parameters are

sigmas <- mutate(sigmas,ind1=as.numeric(str_extract(variable,"[0-9]+")),
                 ind2=as.numeric(str_extract(variable,"(?<=,)[0-9]+"))) %>% 
  mutate(ind2=factor(ind2,levels=c(1:6),
                     labels=c("d1_obs_discrim",
                              "d1_abs_discrim",
                              "d2_obs_discrim",
                              "d2_abs_discrim",
                              "int_obs",
                              "int_abs")))

sigmas_d1 <- filter(sigmas,ind2=="d1_obs_discrim")

sigmas_d2 <- filter(sigmas,ind2=="d2_obs_discrim")

sigmas_d1_sum <- group_by(sigmas_d1,ind1) %>% 
  summarize(med=median(value),
            high=quantile(value,0.95),
            low=quantile(value,0.05))

sigmas_d2_sum <- group_by(sigmas_d2,ind1) %>% 
  summarize(med=median(value),
            high=quantile(value,0.95),
            low=quantile(value,0.05))

# need to get betas

betax1 <- as.data.frame(combine_fit,"betax1") %>% 
  mutate(iter=1:n()) %>% 
  gather(key="variable",value="value",
         -iter) %>% 
  mutate(variable=factor(variable,
                         levels=c("betax1[1]",
                                  "betax1[2]",
                                  "betax1[3]",
                                  "betax1[4]"),
                         labels=c("Islamist Egypt",
                                  "Islamist Tunisia",
                                  "Secularist Egypt",
                                  "Secularist Tunisia")))

# iterate over posterior draws and calculate effect conditional on pos/neg discrimination
# for all params in to_plot

neg_eff_d1 <- purrr::map2(split(sigmas_d1,sigmas_d1$iter),
                          split(betax1,betax1$iter),
                          function(dis,cov) {
  
  neg_discrim <- dis$value[dis$value<0]
    
  #calculate marginal changes in probability
  
  cov <- mutate(cov,
                type="Islamist Tweets",
                marg_effect= sapply(value,function(v) exp(mean(v*neg_discrim))))
  return(cov)
}) %>% bind_rows

pos_eff_d1 <- purrr::map2(split(sigmas_d1,sigmas_d1$iter),
                          split(betax1,betax1$iter),
                          function(dis,cov) {
                            
                            pos_discrim <- dis$value[dis$value>0]
                            
                            #calculate marginal changes in probability
                            
                            cov <- mutate(cov,
                                          type="Secularist Tweets",
                                          marg_effect= sapply(value,function(v) exp(mean(v*pos_discrim))))
                            return(cov)
                          }) %>% bind_rows

bind_rows(neg_eff_d1,
          pos_eff_d1) %>% 
  ggplot(aes(y=marg_effect-1,x=variable)) +
  stat_summary(fun.data = "median_hilow") +
  facet_wrap(~type) +
  scale_y_continuous(labels=scales::percent) +
  ylab("Marginal Percentage Change\nin Average Tweet Counts Due to Coup") +
  xlab("") +
  geom_hline(yintercept=0,linetype=2) +
  theme(panel.grid=element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face="bold"),
        axis.ticks.y=element_blank()) +
  coord_flip()

ggsave("marg_d1.png")

# now D2 marginal effects

betax2 <- as.data.frame(combine_fit,"betax2") %>% 
  mutate(iter=1:n()) %>% 
  gather(key="variable",value="value",
         -iter) %>% 
  mutate(variable=factor(variable,
                         levels=c("betax2[1]",
                                  "betax2[2]",
                                  "betax2[3]",
                                  "betax2[4]"),
                         labels=c("Anti-Democratic Egypt",
                                  "Anti-Democratic Tunisia",
                                  "Democratic Egypt",
                                  "Democratic Tunisia")))




neg_eff_d2 <- purrr::map2(split(sigmas_d2,sigmas_d2$iter),
                          split(betax2,betax2$iter),
                          function(dis,cov) {
                            
                            neg_discrim <- dis$value[dis$value<0]
                            
                            #calculate marginal changes in probability
                            
                            cov <- mutate(cov,
                                          type="Pro-Democracy Tweets",
                                          marg_effect= sapply(value,function(v) exp(mean(v*neg_discrim))))
                            return(cov)
                          }) %>% bind_rows

pos_eff_d2 <- purrr::map2(split(sigmas_d2,sigmas_d2$iter),
                          split(betax2,betax2$iter),
                          function(dis,cov) {
                            
                            pos_discrim <- dis$value[dis$value>0]
                            
                            #calculate marginal changes in probability
                            
                            cov <- mutate(cov,
                                          type="Anti-Democracy Tweets",
                                          marg_effect= sapply(value,function(v) exp(mean(v*pos_discrim))))
                            return(cov)
                          }) %>% bind_rows

bind_rows(neg_eff_d2,
          pos_eff_d2) %>% 
  ggplot(aes(y=marg_effect-1,x=variable)) +
  stat_summary(fun.data = "median_hilow") +
  facet_wrap(~type) +
  scale_y_continuous(labels=scales::percent) +
  ylab("Marginal Percentage Change\nin Average Tweet Counts Due to Coup") +
  xlab("") +
  geom_hline(yintercept=0,linetype=2) +
  theme(panel.grid=element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face="bold"),
        axis.ticks.y=element_blank()) +
  coord_flip()

ggsave("marg_d2.png")

# Now we need to do the IRFs, and then all done!!

source("helper_func.R")

adj_in1 <- rstan::extract(combine_fit,pars='adj_in1')$adj_in1
adj_out1 <- rstan::extract(combine_fit,pars='adj_out1')$adj_out1
sigma_time1 <- rstan::extract(combine_fit,pars='sigma_time1')$sigma_time1
beta_adj1 <- rstan::extract(combine_fit,pars='betax1')$betax1

adj_in2 <- rstan::extract(combine_fit,pars='adj_in2')$adj_in2
adj_out2 <- rstan::extract(combine_fit,pars='adj_out2')$adj_out2
sigma_time2 <- rstan::extract(combine_fit,pars='sigma_time2')$sigma_time2
beta_adj2 <- rstan::extract(combine_fit,pars='betax2')$betax2


islamists_eg <- irf(shock=beta_adj1[,1],
                    xshock = F,
                    adj_in=adj_in1[,1:2],
                    adj_out=adj_out1[,1:2])
islamists_tun <- irf(shock=beta_adj1[,2],
                     xshock = F,
                     adj_in=adj_in1[,2:1],
                     adj_out=adj_out1[,2:1])
secularist_eg  <- irf(shock=beta_adj1[,3],
                      xshock = F,
                      adj_in=adj_in1[,3:4],
                      adj_out=adj_out1[,3:4])
secularist_tun  <- irf(shock=beta_adj1[,4],
                       xshock = F,
                       adj_in=adj_in1[,4:3],
                       adj_out=adj_out1[,4:3])


all_irfs2 <- bind_rows(list(`Islamists\nEgypt`=islamists_eg,
                            `Islamists\nTunisia`=islamists_tun,
                            `Secularists\nEgypt`=secularist_eg,
                            `Secularists\nTunisia`=secularist_tun),
                       .id='Series')


all_irfs2 %>% 
  ggplot(aes(y=y_shock,x=time))   +
  stat_summary(geom='ribbon',fun.data = hdr,fill='grey80') + theme_minimal() +
  stat_summary(fun.y='median',geom='path',linetype=2) +
  theme(panel.grid=element_blank()) + xlab('Days Since Shock') + ylab('Change in Inter-Group Distance') + 
  scale_colour_brewer(palette='paired',name='') + 
  facet_wrap(~Series,scales='free_y') +
  scale_linetype(name='') +
  geom_hline(yintercept = 0,linetype=2)

ggsave('irf_betax_within_d1.png')

c("Anti-Democratic Egypt",
  "Anti-Democratic Tunisia",
  "Democratic Egypt",
  "Democratic Tunisia")

antidem_eg <- irf(shock=beta_adj2[,1],
                    xshock = F,
                    adj_in=adj_in2[,1:2],
                    adj_out=adj_out2[,1:2])
antidem_tun <- irf(shock=beta_adj2[,2],
                     xshock = F,
                     adj_in=adj_in2[,2:1],
                     adj_out=adj_out2[,2:1])
dem_eg  <- irf(shock=beta_adj2[,3],
                      xshock = F,
                      adj_in=adj_in2[,3:4],
                      adj_out=adj_out2[,3:4])
dem_tun  <- irf(shock=beta_adj2[,4],
                       xshock = F,
                       adj_in=adj_in2[,4:3],
                       adj_out=adj_out2[,4:3])


all_irfs3 <- bind_rows(list(`Anti-Democracy\nEgypt`=antidem_eg,
                            `Anti-Democracy\nTunisia`=antidem_tun,
                            `Pro-Democracy\nEgypt`=dem_eg,
                            `Pro-Democracy\nTunisia`=dem_tun),
                       .id='Series')


all_irfs3 %>% 
  ggplot(aes(y=y_shock,x=time))   +
  stat_summary(geom='ribbon',fun.data = hdr,fill='grey80') + theme_minimal() +
  stat_summary(fun.y='median',geom='path',linetype=2) +
  theme(panel.grid=element_blank()) + xlab('Days Since Shock') + ylab('Change in Inter-Group Distance') + 
  scale_colour_brewer(palette='paired',name='') + 
  facet_wrap(~Series,scales='free_y') +
  scale_linetype(name='') +
  geom_hline(yintercept = 0,linetype=2)

ggsave('irf_betax_within_d2.png')

# finish with combined IRFs

islamists_eg <- irf_2shock(shock=cbind(rep(0,nrow(beta_adj1)),beta_adj1[,2]),
                           adj_in=adj_in1[,1:2],
                           adj_out=adj_out1[,1:2])
islamists_tun <- irf_2shock(shock=cbind(rep(0,nrow(beta_adj1)),beta_adj1[,1]),
                            adj_in=adj_in1[,2:1],
                            adj_out=adj_out1[,2:1])
secularist_eg  <- irf_2shock(shock=cbind(rep(0,nrow(beta_adj1)),beta_adj1[,4]),
                             adj_in=adj_in1[,3:4],
                             adj_out=adj_out1[,3:4])
secularist_tun  <- irf_2shock(shock=cbind(rep(0,nrow(beta_adj1)),beta_adj1[,3]),
                              adj_in=adj_in1[,4:3],
                              adj_out=adj_out1[,4:3])


all_irfs4 <- bind_rows(list(`Islamists\nEgypt`=islamists_eg,
                            `Islamists\nTunisia`=islamists_tun,
                            `Secularists\nEgypt`=secularist_eg,
                            `Secularists\nTunisia`=secularist_tun),
                       .id='Series')


all_irfs4 %>% 
  ggplot(aes(y=y_shock,x=time))   +
  stat_summary(geom='ribbon',fun.data = hdr,fill='grey80') + theme_minimal() +
  stat_summary(fun.y='median',geom='path',linetype=2) +
  theme(panel.grid=element_blank()) + xlab('Days Since Shock') + ylab('Change in Inter-Group Distance') + 
  scale_colour_brewer(palette='paired',name='') + 
  facet_wrap(~Series,scales='free_y') +
  scale_linetype(name='') +
  geom_hline(yintercept = 0,linetype=2)

ggsave('irf_betax_both_d1.png')

# compare combined versus direct effect

bind_rows(list(Indirect=all_irfs4,
               Direct=all_irfs2),.id='Type') %>% 
  ggplot(aes(y=y_shock,x=time))   +
  stat_summary(geom='ribbon',fun.data = hdr,aes(fill=Type),
               alpha=0.5) + theme_minimal() +
  stat_summary(fun.y='median',geom='path',aes(linetype=Type),size=1) +
  scale_fill_brewer(name='',palette='Paired') + 
  theme(panel.grid=element_blank()) + xlab('Days Since Shock') + ylab('Change in Inter-Group Distance') + 
  scale_colour_brewer(palette='RdYlBu',name='') + 
  facet_wrap(~Series,scales='free_y') +
  scale_linetype(name='') +
  geom_hline(yintercept = 0,linetype=2)

ggsave('irf_betax_compare_d1.png')

# now D2 combined

antidem_eg <- irf_2shock(shock=cbind(rep(0,nrow(beta_adj2)),beta_adj2[,2]),
                           adj_in=adj_in2[,1:2],
                           adj_out=adj_out2[,1:2])
antidem_tun <- irf_2shock(shock=cbind(rep(0,nrow(beta_adj2)),beta_adj2[,1]),
                            adj_in=adj_in2[,2:1],
                            adj_out=adj_out2[,2:1])
dem_eg  <- irf_2shock(shock=cbind(rep(0,nrow(beta_adj2)),beta_adj2[,4]),
                             adj_in=adj_in2[,3:4],
                             adj_out=adj_out2[,3:4])
dem_tun  <- irf_2shock(shock=cbind(rep(0,nrow(beta_adj2)),beta_adj2[,3]),
                              adj_in=adj_in2[,4:3],
                              adj_out=adj_out2[,4:3])


all_irfs5 <- bind_rows(list(`Anti-Democracy\nEgypt`=antidem_eg,
                            `Anti-Democracy\nTunisia`=antidem_tun,
                            `Pro-Democracy\nEgypt`=dem_eg,
                            `Pro-Democracy\nTunisia`=dem_tun),
                       .id='Series')


all_irfs5 %>% 
  ggplot(aes(y=y_shock,x=time))   +
  stat_summary(geom='ribbon',fun.data = hdr,fill='grey80') + theme_minimal() +
  stat_summary(fun.y='median',geom='path',linetype=2) +
  theme(panel.grid=element_blank()) + xlab('Days Since Shock') + ylab('Change in Inter-Group Distance') + 
  scale_colour_brewer(palette='paired',name='') + 
  facet_wrap(~Series,scales='free_y') +
  scale_linetype(name='') +
  geom_hline(yintercept = 0,linetype=2)

ggsave('irf_betax_both_d1.png')

# compare combined versus direct effect

bind_rows(list(Indirect=all_irfs5,
               Direct=all_irfs3),.id='Type') %>% 
  ggplot(aes(y=y_shock,x=time))   +
  stat_summary(geom='ribbon',fun.data = hdr,aes(fill=Type),
               alpha=0.5) + theme_minimal() +
  stat_summary(fun.y='median',geom='path',aes(linetype=Type),size=1) +
  scale_fill_brewer(name='',palette='Paired') + 
  theme(panel.grid=element_blank()) + xlab('Days Since Shock') + ylab('Change in Inter-Group Distance') + 
  scale_colour_brewer(palette='RdYlBu',name='') + 
  facet_wrap(~Series,scales='free_y') +
  scale_linetype(name='') +
  geom_hline(yintercept = 0,linetype=2)

ggsave('irf_betax_compare_d2.png')

# look at citizen discrimination parameters

cit_params <- as.data.frame(combine_fit,"varparams")

# restructure

cit_params <- mutate(cit_params, iter=1:n()) %>% 
  gather(key="variable",
         value="estimate",-iter) %>% 
  mutate(cit=as.numeric(str_extract(variable,"[0-9]+")),
         param_type=as.numeric(str_extract(variable,'(?<=,)[0-9]')))

# summarize

cit_params <- group_by(cit_params,cit,param_type) %>% 
  summarize(med_est=median(estimate),
            low_est=quantile(estimate,0.05),
            high_est=quantile(estimate,0.95),
            sd_est=sd(estimate))

# recode everything

cit_params <- mutate(cit_params, param_type=factor(param_type, labels=c("D1_Discrimination_Observed",
                                                             "D1_Discrimination_Unobserved",
                                                             "D2_Discrimination_Observed",
                                                             "D2_Discrimination_Unobserved",
                                                             "None_Intercept_Observed",
                                                             "None_Intercept_Unobserved")))

# need to separate param_type

cit_params <- separate(cit_params,param_type,into=c("Dimension",
                                                    "Parameter",
                                                    "Observed?"),
                                                    sep="_")

# histogram of D1 discrimination obs/missing

# plot overlay of average of ideological groups

d1_sum <- group_by(d1,Identity, Country) %>% 
  summarize(avg=median(Median_Religion))

cit_params %>% 
  filter(Dimension %in% c("D1","None")) %>% 
  group_by(cit) %>% 
  summarize(Observed=-(med_est[Parameter=="Intercept" & `Observed?`=="Observed"])/(med_est[Parameter=="Discrimination" & `Observed?`=="Observed"]),
            Missing=-(med_est[Parameter=="Intercept" & `Observed?`=="Unobserved"])/(med_est[Parameter=="Discrimination" & `Observed?`=="Unobserved"])) %>% 
  gather("Observed","Estimate",-cit) %>% 
  filter(Estimate<10,Estimate>-10) %>% 
  ggplot(aes(x=Estimate)) +
  geom_density(fill="blue",alpha=0.5,colour=NA) +
  # geom_vline(data=d1_sum,aes(xintercept=avg,linetype=paste0(Country, " ", Identity))) +
  scale_linetype(name="Ideal Point\nGroup") +
  facet_wrap(~Observed,scales="free") +
  theme(panel.grid=element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face="bold")) + 
  ylab("Median Posterior Density") +
  xlab("Dimension 1: More Islamist to More Secular")

ggsave("d1_discrim.png")

d2_sum <- group_by(d2,Identity, Country) %>% 
  summarize(avg=median(Median_Democracy))

cit_params %>% 
  filter(Dimension %in% c("D2","None")) %>% 
  group_by(cit) %>% 
  summarize(Observed=-(med_est[Parameter=="Intercept" & `Observed?`=="Observed"])/(med_est[Parameter=="Discrimination" & `Observed?`=="Observed"]),
            Missing=-(med_est[Parameter=="Intercept" & `Observed?`=="Unobserved"])/(med_est[Parameter=="Discrimination" & `Observed?`=="Unobserved"])) %>% 
  gather("Observed","Estimate",-cit) %>% 
  filter(Estimate<10,Estimate>-10) %>% 
  ggplot(aes(x=Estimate)) +
  geom_density(fill="blue",alpha=0.5,colour=NA) +
#  geom_vline(data=d2_sum,aes(xintercept=avg,linetype=paste0(Country, " ", Identity))) +
  scale_linetype(name="Ideal Point\nGroup") +
  facet_wrap(~Observed,scales="free") +
  theme(panel.grid=element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face="bold")) + 
  ylab("Median Posterior Density") +
  xlab("Dimension 2: More Democratic (Negative) to More Authoritarian (Positive)")

ggsave("d2_discrim.png")

