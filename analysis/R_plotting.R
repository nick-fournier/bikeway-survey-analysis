# Title     : Bike survey analysis plots
# Objective : Generate nice looking plots using ggplo2
# Created by: nick Fournier
# Created on: 4/14/2021

#### PACKAGES ####
library(ggplot2)
library(data.table)
library(stringr)
library(ggrepel)

#### PRE-PROCESS DATA FOR PLOTTING ####
get_plotdat <- function(path = "./output/") {
  # LOAD DATA
  data <- fread(paste0(path,'cleaned_survey_data_download_latest.csv'))[!is.na(MAX_BUFFER),]
  plotdat <- list()

  # AGE & GENDER
  plotdat[['agesex']] <- data
  plotdat[['agesex']]$AGEBIN <- .bincode(plotdat[['agesex']]$AGE, breaks=seq(0,100,10))
  plotdat[['agesex']] <- plotdat[['agesex']][,.N, by=.(AGEBIN,GENDER)]
  plotdat[['agesex']][ , N := N/sum(N)]

  #CYCLIST TYPE & FREQ
  plotdat[['typefreq']] <- data[,.N, by=.(CYCLIST_TYPE,CYCLIST_FREQ)]
  freq_levels <- plotdat[['typefreq']][ , sum(N), by=CYCLIST_FREQ][order(V1),CYCLIST_FREQ]
  type_levels <- plotdat[['typefreq']][ , sum(N), by=CYCLIST_TYPE][order(V1),CYCLIST_TYPE]
  plotdat[['typefreq']][ , CYCLIST_FREQ := factor(CYCLIST_FREQ, levels = freq_levels)]
  plotdat[['typefreq']][ , CYCLIST_TYPE := factor(CYCLIST_TYPE, levels = type_levels)]
  plotdat[['typefreq']][ , N := N/sum(N)]


  # CRITERIA
  plotdat[['critera']] <- melt(data, measure.vars = colnames(data)[grepl("RANK_CRIT", colnames(data))])

  # MAX BUFFER
  plotdat[['buffer_max']] <- data[ , .N, by=.(9*MAX_BUFFER,CYCLIST_TYPE)]
  plotdat[['buffer_max']][ , N := N/sum(N)]

  # BUFFER DIMENSION DIST
  plotdat[['buffer_dim']] <- fread(paste0(path,'buffer_dim_data.csv'))
  buffer_levels <- plotdat[['buffer_dim']][ , mean(value), by = variable][order(V1), variable]
  plotdat[['buffer_dim']][ , variable := factor(variable, levels = buffer_levels)]

  # BUFFER WIDTH VS HEIGHT
  plotdat[['buffer_dim_xy']] <- plotdat[['buffer_dim']][ , .(meanval=mean(value)),by=.(variable,WIDTH,HEIGHT)]

  # DEBRIS RANK
  plotdat[['debris']] <- melt(data, measure.vars = colnames(data)[grepl("RANK_DEBRIS", colnames(data))])

  # MEASUREMENT PREF
  plotdat[['measurement']] <- melt(data, measure.vars = colnames(data)[grepl("DEBRIS_MEAS", colnames(data))])

  # VISILITY
  plotdat[['visibility']] <- melt(data, measure.vars = colnames(data)[grepl("RANK_VIS", colnames(data))])
  vis_levels <- plotdat[['visibility']][ , mean(value), by = variable][order(V1), variable]
  plotdat[['visibility']][ , variable := factor(variable, levels = vis_levels)]


  return(plotdat)
}

#### FANCY LABELS ####
get_labels <- function() {
  labs <- list()

  labs[['meas_unit']] <- sapply(unique(data$DEBRIS_MEAS_PARTICLES), str_to_sentence)
  labs[['type']] <- sapply(unique(data$CYCLIST_TYPE), str_to_sentence)
  labs[['freq']] <- sapply(unique(data$CYCLIST_FREQ), str_to_sentence)

  labs[['critera']] <- sapply(
    colnames(data)[grepl("RANK_CRIT_", colnames(data))],
    function(x) {
      str_to_sentence(gsub('RANK_CRIT_','',x))
    })

  labs[['visibility']] <- sapply(
    colnames(data)[grepl("RANK_VIS", colnames(data))],
    function(x) {
      out <- str_to_sentence(gsub('RANK_VIS_','',x))
      if(out %in% c('Broadway','Embarcadero','Potrero'))
        out <- paste0(out, '\n(solid green)')
      return(out)
    })

  labs[['debris']] <- c("RANK_DEBRIS_NON_PUNCT"='Non-puncture hazard',
                        "RANK_DEBRIS_PARTICLES"='Particulates (sand/dirt)',
                        "RANK_DEBRIS_PRECIP"='Precipication (snow/rain)',
                        "RANK_DEBRIS_SLIP"='Slip hazard',
                        "RANK_DEBRIS_PUNCT"='Puncture hazard')

  labs[['debris_meas']] <- c("DEBRIS_MEAS_NON_PUNCT"='Non-puncture hazard',
                             "DEBRIS_MEAS_PARTICLES"='Particulates (e.g., sand)',
                             "DEBRIS_MEAS_PRECIP"='Precipication (e.g., snow/rain)',
                             "DEBRIS_MEAS_SLIP"='Slip hazard',
                             "DEBRIS_MEAS_PUNCT"='Puncture hazard')

  labs[['buffertype_nodim']] <- c("POSTPROTECT" = "Post-protected",
                  "BUFFERED" = "Buffered",
                  "RAISED" = "Raised curb",
                  "STANDARD" = "Standard",
                  "PARKINGPROTECT" = "Parking protected",
                  "CURBPROTECT" = "Curb protected")

  labs[['buffertype']] <- unique(fread('./output/buffer_dim_data.csv')[,.(variable,WIDTH,HEIGHT)])
  formatted <- apply(labs[['buffertype']], 1, function(x) {
    paste0(labs[['buffertype_nodim']][x['variable']], ' (W:', x['WIDTH'], ' H: ', x['HEIGHT'],')')
  })
  names(formatted) <- labs[['buffertype']]$variable
  labs[['buffertype']] <- formatted

  return(labs)
}


#### PLOTTING ####
outpath <- "./output/"
dir.create('./output/plots', showWarnings = FALSE)
plotdat <- get_plotdat()
labs <- get_labels()


#### AGE & GENDER DIST ####
ggplot(data, aes(x=AGE, fill=GENDER)) +
  geom_histogram(binwidth = 10, color='black', aes(y = (..count..)/sum(..count..))) +
  annotate('text', x= 93, y=0.01, hjust=0, vjust=0,
           label=paste0('Mean: ', round(mean(data$AGE)),
                        '\nStd Dev: ', round(sd(data$AGE)),
                        paste0('\n',paste(c('Min:','Max:'), range(data$AGE)),collapse = ""))) +
  scale_y_continuous('Response rate (%)', labels = scales::percent, expand=c(0, 0)) +
  scale_x_continuous('Age') +
  scale_fill_brewer('Gender', palette = 'Set2') +
  coord_cartesian(xlim=c(-5,0)+range(data$AGE), ylim=c(0,0.3), clip = 'off') +
  theme_bw()
ggsave(paste0(outpath,'plots/plt_agegender.png'), width = 6, height=4, dpi=300)




#### CYCLIST TYPE & FREQ DIST ####
ggplot(plotdat[['typefreq']], aes(x=CYCLIST_TYPE, y=N, fill=CYCLIST_FREQ)) +
  geom_col(color='black') +
  scale_y_continuous('Response rate (%)', labels = scales::percent, expand=c(0, 0)) +
  scale_x_discrete('Cyclist Type', labels = labs[['type']]) +
  scale_fill_brewer('Cycling Frequency', palette = 'Set2', labels = labs[['freq']]) +
  coord_cartesian(ylim=c(0,0.10*round(max(plotdat[['typefreq']][ , sum(N), by=CYCLIST_TYPE]$V1)/0.10))) +
  theme_bw()
ggsave(paste0(outpath,'plots/plt_typefreq.png'), width = 6, height=4, dpi=300)


#### RANK CRITERIA ####
ggplot(plotdat[['critera']][ , .(MEAN=mean(value), SD=sd(value)), by=.(variable,CYCLIST_TYPE)],
       aes(x=variable, y=MEAN, fill=CYCLIST_TYPE)) +
  geom_col(position = 'dodge') + #, fill='#4daf4a', color='black') +
  geom_point(aes(group=CYCLIST_TYPE), position=position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin=MEAN-SD, ymax=MEAN+SD), width=.5, position = position_dodge(.9)) +
  scale_x_discrete('Criteria', labels = labs[['criteria']]) +
  scale_y_continuous('Average rank (1-3)', expand=c(0, 0)) +
  scale_fill_brewer('Cycling Type', palette = 'Set2', labels = labs[['type']]) +
  coord_cartesian(ylim=c(0, 3.5)) +
  theme_bw()
ggsave(paste0(outpath,'plots/plt_critera.png'), width = 6, height=4, dpi=300)


#### MEASUREMENT PREFERENCE
ggplot(plotdat[['measurement']], aes(x=value, fill=variable)) +
  stat_count(position = 'dodge', color='black', aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous('Response rate (%)', labels = scales::percent, expand=c(0, 0)) +
  scale_x_discrete('Measurement', labels = labs[['meas_unit']]) +
  scale_fill_brewer('Debris type', palette = 'Set2', labels = labs[['debris_meas']]) +
  coord_cartesian(ylim=c(0,0.2)) +
  theme_bw()
ggsave(paste0(outpath,'plots/plt_measurement.png'), width = 6, height=4, dpi=300)


#### RANK DEBRIS ####
ggplot(plotdat[['debris']][ , .(MEAN=mean(value), SD=sd(value)), by=.(variable,CYCLIST_TYPE)],
       aes(x=variable, y=MEAN, fill=CYCLIST_TYPE)) +
  geom_col(position = 'dodge') + #, fill='#4daf4a', color='black') +
  geom_point(aes(group=CYCLIST_TYPE), position=position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin=MEAN-SD, ymax=MEAN+SD), width=.5, position = position_dodge(.9)) +
  scale_y_continuous('Average rank (1-6)', expand=c(0, 0)) +
  scale_x_discrete('Debris type', labels = labs[['debris']]) +
  scale_fill_brewer('Cyclist Type', palette = 'Set2', labels = labs[['type']]) +
  coord_cartesian(ylim=c(0, 6)) +
  theme_bw() +
  theme(legend.position = 'bottom')
ggsave(paste0(outpath,'plots/plt_debris.png'), width = 7.5, height=4, dpi=300)


#### RANK VISIBILITY ####
ggplot(plotdat[['visibility']][ , .(MEAN=mean(value), SD=sd(value)), by=.(variable,CYCLIST_TYPE)],
       aes(x=variable, y=MEAN, fill=CYCLIST_TYPE)) +
  geom_col(position = 'dodge') + #, fill='#4daf4a', color='black') +
  geom_point(aes(group=CYCLIST_TYPE), position=position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin=MEAN-SD, ymax=MEAN+SD), width=.5, position = position_dodge(.9)) +
  scale_y_continuous('Average rank (1-6)', expand=c(0, 0)) +
  scale_x_discrete('Location', labels = labs[['visibility']]) +
  scale_fill_brewer('Cyclist Type', palette = 'Set2', labels = labs[['type']]) +
  coord_cartesian(ylim=c(0, 7)) +
  theme_bw()
ggsave(paste0(outpath,'plots/plt_visibility.png'), width = 7, height=4, dpi=300)


#### MAX BUFFER DISTRIBUTION ####
ggplot(plotdat[['buffer_max']], aes(x=mean(9*data$MAX_BUFFER),y=0.15)) +
  geom_col(aes(x=MAX_BUFFER, y=N, fill=CYCLIST_TYPE), color='black') +
  scale_y_continuous('Response rate (%)', labels = scales::percent, expand=c(0, 0)) +
  scale_x_continuous(paste('Maximum preferred buffer (ft)\n(Average:',round(mean(9*data$MAX_BUFFER),2),'ft)')) +
  scale_fill_brewer('Cyclist Type', palette = 'Set2', labels = labs[['type']]) +
  coord_cartesian(ylim=c(0, .5)) +
  theme_bw()
ggsave(paste0(outpath,'plots/plt_buffermax.png'), width = 6, height=4, dpi=300)


#### BUFFER BAR CHART ####
ggplot(plotdat[['buffer_dim']][ , .N, by=.(value,variable)][ , .(value=value, PCT=N/sum(N)), by=variable],
       aes(x=value, y=PCT, fill=variable)) +
  geom_col(color='black') +
  scale_y_continuous('Response rate (%)', labels = scales::percent, expand=c(0, 0)) +
  scale_x_continuous('Rank score (1-least to 6-most preferred)', breaks = 1:6, expand = c(0,0)) +
  scale_fill_brewer("Buffer type in image", palette = 'RdBu', labels = labs[['buffertype']]) +
  theme_bw()
ggsave(paste0(outpath,'plots/plt_bufferdim.png'), width = 6, height=4, dpi=300)


#### GRID ####
ggplot(melt(plotdat[['buffer_dim']], value.name = 'DIST',
            measure.vars = c('WIDTH','HEIGHT'), variable.name = 'DIM'),
       aes(x = DIST, y = value, color = DIM, fill = DIM)) +
  # geom_boxplot(size = .75, outlier) +
  # geom_violin() +
  geom_jitter(alpha = .5) +
  facet_grid(~variable, labeller = labeller(variable = labs[['buffertype_nodim']])) +
  scale_fill_brewer("Buffer Dimension:", palette = 'Set1', labels = c("WIDTH"="Width","HEIGHT"="Height")) +
  scale_color_brewer("Buffer Dimension:", palette = 'Set1', labels = c("WIDTH"="Width","HEIGHT"="Height")) +
  scale_x_continuous("Buffer distance (ft)") +
  scale_y_continuous("Preference rank (1-6)") +
  theme_bw() +
  theme(legend.position = 'bottom')
ggsave(paste0(outpath,'plots/plt_buffergrid.png'), width = 8, height=3, dpi=300)


#### WIDTH VS HEIGHT SCATTER PLOT ####
ggplot(plotdat[['buffer_dim_xy']], aes(x=WIDTH, y=HEIGHT, color=factor(round(meanval,1)))) +
  geom_point(size = 4) +
  geom_point(color='black') +
  geom_text_repel(# force             = 0.5,
                  nudge_x           = 0.5,
                  direction         = "y",
                  hjust             = 0,
                  segment.size      = 0.5,
                  box.padding       = 0.5,
                  color='black', label = gsub(' \\(','\n\\(', labs[['buffertype']])) +
  # geom_jitter(width=0.75) +
  scale_size("Average rank") +
  scale_color_brewer('Average rank', palette = 'RdYlBu', labels = labs[['buffertype']]) +
  scale_x_continuous('Buffer width (ft)') +
  scale_y_continuous('Buffer height (ft)') +
  coord_fixed() +
  theme_bw()
ggsave(paste0(outpath,'plots/plt_bufferxy.png'), width = 10, height=5, dpi=300)


#### BUFFER AVERAGE VALUE PLOT ####
ggplot(melt(plotdat[['buffer_dim']], value.name = 'DIST',
           measure.vars = c('WIDTH','HEIGHT'), variable.name = 'DIM'),
       aes(x=DIST, y=value, color=DIM)) +
  # geom_point(alpha=0.3) +
  stat_smooth(method='loess', span=1, se=T, fullrange=T) +
  scale_color_brewer('Moving average score (LOESS:',
                     palette = 'Set1', , labels = c("WIDTH"="Width","HEIGHT"="Height")) +
  scale_x_log10('Buffer width and height in ft (log scale)', breaks = 1:12) +
  scale_y_log10('Preference ranking (1-6)', breaks = seq(0,6,0.5)) +
  # scale_y_log10('Preference ranking (1-6) [log scale]') +
  coord_cartesian(xlim = c(0.75, 11), ylim = c(1,6)) +
  theme_bw() +
  theme(legend.position = 'bottom')
ggsave(paste0(outpath,'plots/plt_bufferloess.png'), width = 5, height=4, dpi=300)

#### FITTED BUFFER FUNCTION ####

