# Run Summary Stats on fitted Bayesian VAR IRT model
# Assumes that a model has already been fit by run_model_IRT_final.R

require(dplyr)
require(ggplot2)
require(rstan)
require(shinystan)
require(ggridges)
require(tidyr)
require(bayesplot)
require(purrr)
require(forcats)


source('helper_func.R')

# load fitted rstan model from run_model_IRT_final.R

out_fit_id <- readRDS('out_fit_id_std_VAR_betax_2018-05-31.rds')
to_plot <- as.array(out_fit_id)

# load times data and check for the coup day
times <- readRDS('times.rds')
coup_day_new <- times$time_three[times$time_date==as.Date('2013-07-03')]

# currently using elite ideal points with country fixed effects included
get_time <- rstan::extract(out_fit_id,pars='alpha_country',permute=T)$alpha

get_time <- lapply(1:dim(get_time)[3],function(x) get_time[,,x]) %>% 
  lapply(as_data_frame) %>% 
  bind_rows(.id='Series') %>% 
  mutate(Series=factor(Series),
         Series=fct_recode(Series,`Tunisia Islamists`='2',
                           `Egypt Islamists`='1',
                           `Tunisia Secularists`='4',
                           `Egypt Secularists`='3')) %>% 
  gather(time_pts,out_vals,-Series) %>% 
  mutate(time_pts=as.numeric(factor(time_pts))) %>% 
  separate(Series,into=c('Country','Religion'),remove=F)  %>% 
  left_join(times,c(time_pts='time_three'))


get_time %>% 
  filter(time_pts>0) %>% 
  ggplot(aes(y=out_vals,x=time_date)) +
  stat_summary(geom='ribbon',fun.data = 'median_hilow',aes(fill=Series),
               alpha=0.5) + theme_minimal() +
  stat_summary(fun.y='median',geom='path',aes(linetype=Series),alpha=0.5) +
  theme(panel.grid=element_blank()) + xlab('') + ylab('Inter-Group Distance') + 
  scale_fill_brewer(palette='RdBu',name='') + 
  scale_linetype(name='') + 
  geom_vline(aes(xintercept=as.numeric(as.Date('2013-07-03'))),linetype=3) +
  scale_x_date(breaks=as.Date(c('2013-05-01','2013-07-03','2013-08-14'))) +
  annotate('text',x=as.Date(c('2013-05-22','2013-07-03','2013-07-28','2013-08-14')),
           y=c(3.5,3.1,3.5,0.3),label=c('New Tunisian\nConstitution',
                                  'Morsi Coup',
                                  'Mohamed Brahmi\nAssassination',
                                  'Rabaa Massacre'),
           size=3)

ggsave('arab_ideology.png',width=8,height=4,units='in')

get_time %>% 
  filter(time_pts>0) %>% 
  ggplot(aes(y=out_vals,x=time_date)) +
  stat_summary(geom='path',fun.y= 'median',aes(linetype=Country)) + theme_minimal() +
  facet_wrap(~Religion,ncol=1,scales='free_y') +
  theme(panel.grid=element_blank()) + xlab('')  + ylab('Inter-Group Distance') + 
  geom_vline(aes(xintercept=as.numeric(as.Date('2013-07-03'))),linetype=3) +
  scale_x_date(breaks=as.Date(c('2013-05-01','2013-07-03','2013-08-14'))) 
ggsave('religion_coint.png')

get_time %>% 
  filter(time_pts>1) %>% 
  ggplot(aes(y=out_vals,x=time_date)) +
  stat_summary(geom='path',fun.y= 'median',aes(linetype=Religion)) + theme_minimal() +
  facet_wrap(~Country,ncol=1,scales='free_y') +
  theme(panel.grid=element_blank()) + xlab('')  + ylab('Inter-Group Distance') + 
  geom_vline(aes(xintercept=as.numeric(as.Date('2013-07-03'))),linetype=3) +
  scale_x_date(breaks=as.Date(c('2013-05-01','2013-07-03','2013-08-14'))) 
ggsave('country_coint.png')


# Citizen Ideal Points -----------------------------------------------------

cit_discrim <- as.data.frame(out_fit_id,pars='delta_1') %>% 
  mutate(iter=1:n()) %>% 
  gather(key = param1,value=estimate1,-iter) %>% 
  mutate(cit_id=stringr::str_extract(param1,'\\[[0-9]+'),
         cit_id=stringr::str_replace(cit_id,'\\[',''),
         cit_id=as.numeric(cit_id))
cit_intercepts <- as.data.frame(out_fit_id,pars='beta_1') %>% 
  mutate(iter=1:n()) %>% 
  gather(key = param2,value=estimate2,-iter) %>% 
  mutate(cit_id=stringr::str_extract(param2,'\\[[0-9]+'),
         cit_id=stringr::str_replace(cit_id,'\\[',''),
         cit_id=as.numeric(cit_id))

cit_combined <- left_join(cit_discrim,cit_intercepts,by=c('cit_id','iter'))

cit_combined <- group_by(cit_combined,cit_id) %>% 
  summarize(mid_point=mean((estimate2)/estimate1),
            mid_point_high=quantile((estimate2)/estimate1,.9),
            mid_point_low=quantile((estimate2)/estimate1,.1))

# histogram of citizen mid-points

cit_combined %>% 
  ggplot(aes(x=mid_point)) +
  geom_histogram() +
  theme_minimal() +
  theme(panel.grid=element_blank()) +
  xlim(c(-2,2)) +
  xlab('Mid-points on Ideal Point Scale') +
  ylab('Count of Citizen Twitter Users')

ggsave('cit_midpoints.png')

# just discrimination parameters

cit_discrim %>% group_by(cit_id) %>% 
  summarize(est=mean(estimate1),
            high=quantile(estimate1,.9),
            low=quantile(estimate1,.1)) %>% 
  ggplot(aes(x=est)) +
  geom_histogram() +
  theme_minimal() +
  theme(panel.grid=element_blank()) +
  xlim(c(-2,2))

# just intercepts

cit_intercepts %>% group_by(cit_id) %>% 
  summarize(est=mean(estimate2),
            high=quantile(estimate2,.9),
            low=quantile(estimate2,.1)) %>% 
  ggplot(aes(x=est)) +
  geom_histogram() +
  theme_minimal() +
  theme(panel.grid=element_blank())

# absence discrimination parameters

cit_abs_discrim <- as.data.frame(out_fit_id,pars='delta_0') %>% 
  mutate(iter=1:n()) %>% 
  gather(key = param1,value=estimate1,-iter) %>% 
  mutate(cit_id=stringr::str_extract(param1,'\\[[0-9]+'),
         cit_id=stringr::str_replace(cit_id,'\\[',''),
         cit_id=as.numeric(cit_id))

cit_abs_discrim %>% group_by(cit_id) %>% 
  summarize(est=mean(estimate1),
            high=quantile(estimate1,.9),
            low=quantile(estimate1,.1)) %>% 
  ggplot(aes(x=est)) +
  geom_histogram() +
  theme_minimal() +
  theme(panel.grid=element_blank()) +
  xlim(c(-4,10))

# let's look at absence midpoints

cit_abs_intercepts <- as.data.frame(out_fit_id,pars='beta_0') %>% 
  mutate(iter=1:n()) %>% 
  gather(key = param2,value=estimate2,-iter) %>% 
  mutate(cit_id=stringr::str_extract(param2,'\\[[0-9]+'),
         cit_id=stringr::str_replace(cit_id,'\\[',''),
         cit_id=as.numeric(cit_id))

cit_abs_combined <- left_join(cit_abs_discrim,cit_abs_intercepts,by=c('cit_id','iter'))

cit_abs_combined <- group_by(cit_abs_combined,cit_id) %>% 
  summarize(mid_point=mean((estimate2)/estimate1),
            mid_point_high=quantile((estimate2)/estimate1,.9),
            mid_point_low=quantile((estimate2)/estimate1,.1))

# histogram of citizen mid-points

cit_abs_combined %>% 
  ggplot(aes(x=mid_point)) +
  geom_histogram() +
  theme_minimal() +
  xlim(c(-10,10)) +
  theme(panel.grid=element_blank()) +
  xlab('Mid-points on Ideal Point Scale') +
  ylab('Count of Citizen Twitter Users')

ggsave('cit_abs_midpoints.png')

# Let's do some impulse response functions
adj_in <- rstan::extract(out_fit_id,pars='adj_in')$adj_in
adj_out <- rstan::extract(out_fit_id,pars='adj_out')$adj_out
sigma_time <- rstan::extract(out_fit_id,pars='sigma_time')$sigma_time
beta_adj <- rstan::extract(out_fit_id,pars='betax')$betax

# First set of shocks is unit shocks from one ideological group to another
# shock is negative or positive depending on whether it would push them towards or 
# away from the religionists in the same country

islamists_eg <- irf(shock=1,
                    adj_in=adj_in[,1:2],
                    adj_out=adj_out[,1:2])
islamists_tun <- irf(shock=1,
                    adj_in=adj_in[,2:1],
                    adj_out=adj_out[,2:1])
secularist_eg  <- irf(shock=1,
                      adj_in=adj_in[,3:4],
                      adj_out=adj_out[,3:4])
secularist_tun  <- irf(shock=1,
                      adj_in=adj_in[,4:3],
                      adj_out=adj_out[,4:3])


all_irfs1 <- bind_rows(list(`Islamists\nEgypt`=islamists_eg,
                      `Islamists\nTunisia`=islamists_tun,
                      `Secularists\nEgypt`=secularist_eg,
                      `Secularists\nTunisia`=secularist_tun),
                      .id='Series')
  

all_irfs1 %>% 
  ggplot(aes(y=y_shock,x=time))   +
  stat_summary(geom='ribbon',fun.data = hdr,fill='grey80') + theme_minimal() +
  stat_summary(fun.y='median',geom='path',linetype=2) +
  theme(panel.grid=element_blank()) + xlab('Days Since Shock') + ylab('Change in Inter-Group Distance') + 
  scale_colour_brewer(palette='paired',name='') + 
  facet_wrap(~Series,scales='free_y') +
  scale_linetype(name='') +
  geom_hline(yintercept = 0,linetype=2)

ggsave('irf_egypt_panels.png')

all_irfs1 %>% 
  separate(Series,into=c('Religion','Time Period'),sep = '\n') %>% 
  ggplot(aes(y=y_shock,x=time))   +
  stat_summary(geom='ribbon',fun.data = hdr,fill='grey80',aes(group=`Time Period`),alpha=0.5) + theme_minimal() +
  stat_summary(fun.y='median',geom='path',aes(linetype=`Time Period`)) +
  theme(panel.grid=element_blank()) + xlab('Time') + ylab('Ideological Positions') + 
  scale_colour_brewer(palette='paired',name='') + 
  facet_wrap(~Religion,scales='free_y',ncol = 1) +
  scale_linetype(name='')

ggsave('irf_egypt_overlap.png')

#Now redo where the shock is betax from within the time series

islamists_eg <- irf(shock=beta_adj[,1],
                    xshock = F,
                    adj_in=adj_in[,1:2],
                    adj_out=adj_out[,1:2])
islamists_tun <- irf(shock=beta_adj[,2],
                     xshock = F,
                     adj_in=adj_in[,2:1],
                     adj_out=adj_out[,2:1])
secularist_eg  <- irf(shock=beta_adj[,3],
                      xshock = F,
                      adj_in=adj_in[,3:4],
                      adj_out=adj_out[,3:4])
secularist_tun  <- irf(shock=beta_adj[,4],
                       xshock = F,
                       adj_in=adj_in[,4:3],
                       adj_out=adj_out[,4:3])


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

ggsave('irf_betax_within.png')

#Now redo where the shock is betax from other time series

islamists_eg <- irf(shock=beta_adj[,2],
                    adj_in=adj_in[,1:2],
                    adj_out=adj_out[,1:2])
islamists_tun <- irf(shock=beta_adj[,1],
                     adj_in=adj_in[,2:1],
                     adj_out=adj_out[,2:1])
secularist_eg  <- irf(shock=beta_adj[,4],
                      adj_in=adj_in[,3:4],
                      adj_out=adj_out[,3:4])
secularist_tun  <- irf(shock=beta_adj[,3],
                       adj_in=adj_in[,4:3],
                       adj_out=adj_out[,4:3])


all_irfs3 <- bind_rows(list(`Islamists\nEgypt`=islamists_eg,
                            `Islamists\nTunisia`=islamists_tun,
                            `Secularists\nEgypt`=secularist_eg,
                            `Secularists\nTunisia`=secularist_tun),
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

ggsave('irf_betax_other.png')

#Now shock both at once

islamists_eg <- irf_2shock(shock=beta_adj[,1:2],
                    adj_in=adj_in[,1:2],
                    adj_out=adj_out[,1:2])
islamists_tun <- irf_2shock(shock=beta_adj[,2:1],
                     adj_in=adj_in[,2:1],
                     adj_out=adj_out[,2:1])
secularist_eg  <- irf_2shock(shock=beta_adj[,3:4],
                      adj_in=adj_in[,3:4],
                      adj_out=adj_out[,3:4])
secularist_tun  <- irf_2shock(shock=beta_adj[,4:3],
                       adj_in=adj_in[,4:3],
                       adj_out=adj_out[,4:3])


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

ggsave('irf_betax_both.png')

# compare combined versus direct effect

bind_rows(list(Combined=all_irfs4,
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

ggsave('irf_betax_compare.png')

# now plot some of the raw parameters

beta_pars <- mcmc_dens_chains_data(to_plot,regex_pars='betax') %>% 
  mutate(parameter=factor(parameter,
                          levels=c('betax[1]','betax[2]',
                                   'betax[3]','betax[4]'),
                          labels=c('Egyptian Islamists',
                                   'Tunisian Islamists',
                                   'Egyptian Secularists',
                                   'Tunisian Secularists')))

beta_pars %>% 
  ggplot(aes(x=x)) +
  geom_density(aes(fill=parameter),colour=NA,alpha=0.5)

beta_pars %>% 
  ggplot(aes(x=x)) +
  geom_density_ridges2(aes(y=parameter,fill=parameter,height=density),colour=NA,
                               stat='identity') +
  theme_ridges() +
  scale_fill_brewer(palette='Paired',name=expression(paste("Posterior Density of ",beta[cgx]))) + 
  scale_y_discrete(expand=c(0.01, 0)) +
  scale_x_continuous(expand=c(0.01, 0)) +
  geom_vline(xintercept = 0,linetype=2) +
  ylab('Ideological Groups') +
  xlab(expression(beta[cgx]))

ggsave('betax.png')

adj_in_pars <- mcmc_dens_chains_data(to_plot,regex_pars='adj_in') %>% 
  mutate(parameter=factor(parameter,
                          levels=c('adj_in[1]','adj_in[2]',
                                   'adj_in[3]','adj_in[4]'),
                          labels=c('Egyptian Islamists',
                                   'Tunisian Islamists',
                                   'Egyptian Secularists',
                                   'Tunisian Secularists')))

adj_out_pars <- mcmc_dens_chains_data(to_plot,regex_pars='adj_out') %>% 
  mutate(parameter=factor(parameter,
                          levels=c('adj_out[1]','adj_out[2]',
                                   'adj_out[3]','adj_out[4]'),
                          labels=c('Egyptian Islamists',
                                   'Tunisian Islamists',
                                   'Egyptian Secularists',
                                   'Tunisian Secularists')))

bind_rows(list(`beta[cgIN]`=adj_in_pars,
               `beta[cgOUT]`=adj_out_pars),.id='type') %>% 
  ggplot(aes(x=x)) +
  geom_density_ridges2(aes(y=parameter,fill=type,height=scaled_density),colour=NA,
                       stat='identity',scale=.5) +
  theme_ridges() +
  scale_fill_brewer(palette='Paired',name="HPD of Parameter",
                    labels=c(expression(beta[cgIN]),
                             expression(beta[cgOUT])),
                    guide=guide_legend(label.hjust=0)) + 
  scale_y_discrete(expand=c(0.01, 0)) +
  scale_x_continuous(expand=c(0.01, 0)) +
  geom_vline(xintercept = 0,linetype=2) +
  ylab('Ideological Groups') +
  xlab('Inter-Group Distance')

ggsave('adj_par.png')

# calculate difference between time series

get_spread <- dplyr::select(get_time,Series, out_vals,time_date) %>% 
  group_by(Series,time_date) %>% 
  mutate(iter=1:n()) %>% 
  distinct %>% spread(key=Series,value=out_vals) %>% 
  mutate(diff_country=abs(`Egypt Islamists`-`Egypt Secularists`) - 
           abs(`Tunisia Islamists`-`Tunisia Secularists`))

get_spread %>% 
  ggplot(aes(y=diff_country,x=time_date)) +
  stat_summary(geom='ribbon',fun.data = 'median_hilow',
               alpha=0.5) + theme_minimal() +
  stat_summary(fun.y='median',geom='path',alpha=0.5,colour='red') +
  stat_smooth(se = F) +
  theme(panel.grid=element_blank()) + xlab('') + ylab('') + 
  geom_vline(aes(xintercept=as.numeric(as.Date('2013-07-03'))),linetype=3) +
  scale_x_date(breaks=as.Date(c('2013-04-25','2013-07-03','2013-08-14'))) +
  scale_y_continuous(breaks=c(-1,-0.5,0,0.5,1),
                     labels=c('Higher\nTunisian\nPolarization','-0.5','0.0','0.5',
                              'Higher\nEgyptian\nPolarization')) +
  annotate('text',x=as.Date(c('2013-04-25','2013-07-03','2013-07-25','2013-08-14')),
           y=c(0.5,.8,1,-0.6),label=c('New Tunisian\nConstitution',
                                      'Morsi Coup',
                                      'Mohamed Brahmi\nAssassination',
                                      'Rabaa Massacre'),
           size=3) +
  geom_hline(yintercept = 0,linetype=2)

ggsave('diff_ideal.png')
