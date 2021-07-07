# Title     : Bike survey analysis plots
# Objective : Generate nice looking plots using ggplot2
# Created by: nick Fournier
# Created on: 4/14/2021

#### PACKAGES ####
library(ggplot2)
library(data.table)
library(stringr)
library(ggrepel)
library(extrafont)
library(viridis)

#### PRE-PROCESS DATA FOR PLOTTING ####
# GRABS THE COLNAMES BASED ON PREFIX
get_vars <- function(var, df) colnames(df)[grepl(var, colnames(df))]
# PROCESS THE PLOT DATA INTO A LIST OF DATAFRAMES
get_plotdat <- function(surveydata, dimdata) {
  plotdat <- list()

  # AGE & GENDER
  plotdat[['agesex']] <- surveydata
  plotdat[['agesex']]$AGEBIN <- .bincode(plotdat[['agesex']]$AGE, breaks=seq(0,100,10))
  plotdat[['agesex']] <- plotdat[['agesex']][,.N, by=.(AGEBIN,GENDER)]
  plotdat[['agesex']][ , N := N/sum(N)]

  # CYCLIST TYPE & FREQ
  plotdat[['typefreq']] <- data.table(table(surveydata[ ,.(CYCLIST_TYPE, CYCLIST_FREQ)]) / nrow(surveydata))
  freq_levels <- c('DAILY','WEEKLY','MONTHLY','RARELY')
  type_levels <- c('NONCYCLIST', 'SOCIAL', 'SOCIAL-REC', 'RECREATIONAL', 'COMMUTER')
  # type_levels <- plotdat[['typefreq']][ , sum(value), by=CYCLIST_TYPE][order(V1),CYCLIST_TYPE]
  plotdat[['typefreq']][ , CYCLIST_FREQ := factor(CYCLIST_FREQ, levels = freq_levels)]
  plotdat[['typefreq']][ , CYCLIST_TYPE := factor(CYCLIST_TYPE, levels = type_levels)]

  # GROUPED CATEGORY
  # plotdat[['typefreq']][ ]


  # CRITERIA
  plotdat[['critera']] <- melt(surveydata, measure.vars = get_vars('RANK_CRIT', surveydata))
  # MAX BUFFER
  plotdat[['buffer_max']] <- surveydata[ , .N, by=.(9*MAX_BUFFER,CYCLIST_TYPE)]
  plotdat[['buffer_max']][ , N := N/sum(N)]

  # BUFFER DIMENSION DIST
  plotdat[['buffer_dim']] <- dimdata
  buffer_levels <- plotdat[['buffer_dim']][ , mean(value), by = variable][order(V1), variable]
  plotdat[['buffer_dim']][ , variable := factor(variable, levels = buffer_levels)]

  # BUFFER WIDTH VS HEIGHT
  plotdat[['buffer_dim_xy']] <- plotdat[['buffer_dim']][ , .(meanval=mean(value)),by=.(variable,WIDTH,HEIGHT)]

  # DEBRIS RANK
  plotdat[['debris']] <- melt(surveydata, measure.vars = get_vars('RANK_DEBRIS', surveydata))

  # MEASUREMENT PREF
  plotdat[['measurement']] <- melt(surveydata, measure.vars = get_vars('DEBRIS_MEAS', surveydata))

  # VISILITY
  plotdat[['visibility']] <- melt(surveydata, measure.vars = get_vars('RANK_VIS', surveydata))
  vis_levels <- plotdat[['visibility']][ , mean(value), by = variable][order(V1), variable]
  plotdat[['visibility']][ , variable := factor(variable, levels = vis_levels)]

  return(plotdat)
}
# FANCY LABELS
get_labels <- function(surveydata, dimdata) {
  labs <- list()

  labs[['meas_unit']] <- sapply(unique(surveydata$DEBRIS_MEAS_PARTICLES), str_to_sentence)
  labs[['type']] <- sapply(unique(surveydata$CYCLIST_TYPE), str_to_sentence)
  labs[['freq']] <- sapply(unique(surveydata$CYCLIST_FREQ), str_to_sentence)

  labs[['criteria']] <- sapply(
    get_vars("RANK_CRIT_",surveydata),
    function(x) {
      str_to_sentence(gsub('RANK_CRIT_','',x))
    })

  labs[['visibility']] <- sapply(
    get_vars("RANK_VIS",surveydata),
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

  labs[['buffertype']] <- unique(dimdata[,.(variable,WIDTH,HEIGHT)])
  formatted <- apply(labs[['buffertype']], 1, function(x) {
    paste0(labs[['buffertype_nodim']][x['variable']], ' (W:', x['WIDTH'], ' H: ', x['HEIGHT'],')')
  })
  names(formatted) <- labs[['buffertype']]$variable
  labs[['buffertype']] <- formatted

  return(labs)
}
# EFFECTIVE BUFFER FUNCTION
func_Wbuf <- function(Wbuf, Hbuf) Wmax * (1 - exp(-coefs['WIDTH'] * Wbuf - coefs['HEIGHT'] * Hbuf))
# FANCY BINNED LABELS, PERCENTS
binlabel_percent <- function(xseq) {
  xseq = scales::percent(xseq, accuracy=1)
  xseq = paste(xseq[-length(xseq)], xseq[-1], sep = "-")
  return(xseq)
}
# FANCY BINNED LABELS
binlabel <- function(xseq) paste(xseq[-length(xseq)], xseq[-1], sep = "-")
# CROSS SECTIONAL LOS SCORE ADJUSTMENT
func_Fw <- function(effbuf) -0.005*(5 + effbuf)^2


#### PLOTTING ####
if(length(list.files('../output')) > 0) {
  outpath <- "../output/"
} else {
  outpath <- "./output/"
}

#### LOAD DATA ####
survey_data <- fread(paste0(outpath,'cleaned_survey_data.csv'))[!is.na(MAX_BUFFER),]
dim_data <- fread(paste0(outpath,'buffer_dim_data.csv'))
coef_data <- fread(paste0(outpath,'buffer_coefssimple.csv'))
coef_data <- fread(paste0(outpath,'buffer_coefs_npsimple.csv'))

dir.create('./output/plots', showWarnings = FALSE)
plotdat <- get_plotdat(surveydata=survey_data, dimdata=dim_data)
labs <- get_labels(surveydata=survey_data, dimdata=dim_data)

#### AGE & GENDER DIST ####
ggplot(survey_data, aes(x=AGE, fill=GENDER)) +
  geom_histogram(binwidth = 5, color='black', aes(y = (..count..)/sum(..count..))) +
  annotate('text', x= 93, y=0.01, hjust=0, vjust=0,
           label=paste0('Mean: ', round(mean(survey_data$AGE)),
                        '\nStd Dev: ', round(sd(survey_data$AGE)),
                        paste0('\n',paste(c('Min:','Max:'), range(survey_data$AGE)),collapse = ""))) +
  scale_y_continuous('Response rate (%)', labels = scales::percent, expand=c(0, 0)) +
  scale_x_continuous('Age') +
  scale_fill_brewer('Gender', palette = 'Set2') +
  coord_cartesian(xlim=c(-5,0)+range(survey_data$AGE), ylim=c(0,0.2), clip = 'off') +
  theme_bw() +
  theme(legend.justification = 'top')

ggsave(paste0(outpath,'plots/plt_agegender.png'), width = 9, height=3, dpi=300)


#### CYCLIST TYPE & FREQ DIST ####
# plotdat[['typefreq']][ , .bincode(value, breaks = seq(0,ceiling(10*max(value))/10,0.05), include.lowest = T)]
ggplot(plotdat[['typefreq']], aes(x=CYCLIST_TYPE, y=CYCLIST_FREQ, fill=N)) +
  geom_tile() +
  geom_text(aes(label = paste0(round(100*N),'%')), fontface = "bold", size = 4) +
  scale_y_discrete('Bicycling Frequency', labels = labs[['freq']], expand=c(0, 0)) +
  scale_x_discrete('Bicyclist Type', labels = labs[['type']], expand=c(0, 0)) +
  scale_fill_viridis("Proportion of\nresponses", option = 'mako', direction = -1, alpha=0.8, labels = scales::percent) +
  coord_fixed(ratio = 4/5) +
  theme_bw()
ggsave(paste0(outpath,'plots/plt_typefreq.png'), width = 7, height=4, dpi=300)


#### RANK CRITERIA ####
ggplot(plotdat[['critera']][ , .(MEAN=mean(value), SD=sd(value)), by=.(variable,CYCLIST_TYPE)],
       aes(x=variable, y=MEAN, fill=CYCLIST_TYPE)) +
  geom_col(position = 'dodge') + #, fill='#4daf4a', color='black') +
  geom_point(aes(group=CYCLIST_TYPE), position=position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin=MEAN-SD, ymax=MEAN+SD), width=.5, position = position_dodge(.9)) +
  scale_x_discrete('Pavement Quality Criteria', labels = labs[['criteria']]) +
  scale_y_continuous('Average rank (1-3)', expand=c(0, 0)) +
  scale_fill_brewer('Bicyclist Type', palette = 'Set2', labels = labs[['type']]) +
  coord_cartesian(ylim=c(0, 3.5)) +
  theme_bw() +
  theme(legend.position = 'bottom')
ggsave(paste0(outpath,'plots/plt_critera.png'), width = 7, height=4, dpi=300)


#### MEASUREMENT PREFERENCE
ggplot(plotdat[['measurement']], aes(x=value, fill=variable)) +
  stat_count(position = 'dodge', color='black', aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous('Response rate (%)', labels = scales::percent, expand=c(0, 0)) +
  scale_x_discrete('Measurement', labels = labs[['meas_unit']]) +
  scale_fill_brewer('Debris type', palette = 'Set2', labels = labs[['debris_meas']]) +
  coord_cartesian(ylim=c(0,0.2)) +
  theme_bw()
ggsave(paste0(outpath,'plots/plt_measurement.png'), width = 7, height=3, dpi=300)


#### RANK DEBRIS ####
ggplot(plotdat[['debris']][ , .(MEAN=mean(value), SD=sd(value)), by=.(variable,CYCLIST_TYPE)],
       aes(x=variable, y=MEAN, fill=CYCLIST_TYPE)) +
  geom_col(position = 'dodge') + #, fill='#4daf4a', color='black') +
  geom_point(aes(group=CYCLIST_TYPE), position=position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin=MEAN-SD, ymax=MEAN+SD), width=.5, position = position_dodge(.9)) +
  scale_y_continuous('Average rank (1-6)', expand=c(0, 0)) +
  scale_x_discrete('Debris type', labels = labs[['debris']]) +
  scale_fill_brewer('Bicyclist Type', palette = 'Set2', labels = labs[['type']]) +
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
  scale_fill_brewer('Bicyclist Type', palette = 'Set2', labels = labs[['type']]) +
  coord_cartesian(ylim=c(0, 7)) +
  theme_bw() +
  theme(legend.position = 'bottom')
ggsave(paste0(outpath,'plots/plt_visibility.png'), width = 7.5, height=4, dpi=300)


#### MAX BUFFER DISTRIBUTION ####
ggplot(plotdat[['buffer_max']], aes(x=mean(9*survey_data$MAX_BUFFER),y=0.15)) +
  geom_col(aes(x=MAX_BUFFER, y=N, fill=CYCLIST_TYPE), color='black') +
  scale_y_continuous('Response rate (%)', labels = scales::percent, expand=c(0, 0)) +
  scale_x_continuous(paste('Maximum preferred buffer (ft)\n(Average:',round(mean(9*survey_data$MAX_BUFFER),2),'ft)')) +
  scale_fill_brewer('Bicyclist Type', palette = 'Set2', labels = labs[['type']]) +
  coord_cartesian(ylim=c(0, .5)) +
  theme_bw()
ggsave(paste0(outpath,'plots/plt_buffermax.png'), width = 7, height=3, dpi=300)


#### BUFFER BAR CHART ####
ggplot(plotdat[['buffer_dim']][ , .N, by=.(value,variable)][ , .(value=value, PCT=N/sum(N)), by=variable],
       aes(x=value, y=PCT, fill=variable)) +
  geom_col(color='black') +
  scale_y_continuous('Response rate (%)', labels = scales::percent, expand=c(0, 0)) +
  scale_x_continuous('Rank score (1-least to 6-most preferred)', breaks = 1:6, expand = c(0,0)) +
  scale_fill_brewer("Buffer type in image", palette = 'Accent', labels = labs[['buffertype']]) +
  theme_bw()
ggsave(paste0(outpath,'plots/plt_bufferdim.png'), width = 7, height = 3, dpi=300)

#### BUFFER BAR CHART DODGE ####
ggplot(plotdat[['buffer_dim']][ , .N, by=.(value,variable)][ , .(value=value, PCT=N/sum(N)), by=variable],
       aes(x=value, y=PCT, fill=variable)) +
  geom_col(color='black', position='dodge') +
  scale_y_continuous('Response rate (%)', labels = scales::percent, expand=c(0, 0)) +
  scale_x_continuous('Rank score (1-least to 6-most preferred)', breaks = 1:6, expand = c(0,0)) +
  scale_fill_brewer("Buffer type in image", palette = 'Accent', labels = labs[['buffertype']]) +
  theme_bw()
ggsave(paste0(outpath,'plots/plt_bufferdim.png'), width = 7, height = 3, dpi=300)


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
  scale_color_brewer('Moving average score (LOESS):',
                     palette = 'Set1', , labels = c("WIDTH"="Width","HEIGHT"="Height")) +
  scale_x_log10('Buffer width and height in ft (log scale)', breaks = 1:12) +
  scale_y_log10('Preference ranking (1-6)', breaks = seq(0,6,0.5)) +
  coord_cartesian(xlim = c(0.75, 11), ylim = c(1,6)) +
  theme_bw() +
  theme(legend.position = 'bottom')
ggsave(paste0(outpath,'plots/plt_bufferloess.png'), width = 7, height=4, dpi=300)


#### FITTED BUFFER FUNCTION ####
Wmax <- 9*mean(survey_data$MAX_BUFFER)
coefs <- with(coef_data[term %in% c('WIDTH','HEIGHT'), .(term,estimate)], structure(estimate, names = term))

labs[['coefs']] <- c(as.expression(bquote('Horizontal component:'~beta[W]==.(round(coefs['WIDTH'],4)))),
                     as.expression(bquote('Vertical component:'~beta[H]==.(round(coefs['HEIGHT'],4)))))

# DENSITY GRID
plotdat[['grid']] <- data.table(expand.grid(Wbuf=seq(0,20,0.5), Hbuf=seq(0,6,0.5)))
plotdat[['grid']][ , effWbuf := func_Wbuf(Wbuf, Hbuf)]
plotdat[['grid']][ , Fw := func_Fw(effWbuf)]

labs[['Fw']] <- paste(sprintf('%.2f', seq(floor(min(plotdat[['grid']]$Fw)),-1, length.out=8)),
                      sprintf('%.2f', seq(ceiling(min(plotdat[['grid']]$Fw)),0, length.out=8)), sep=" to ")

#### BUFFER FUNC FORM ####
ggplot(data.frame(x=c(0,20)), aes(x)) +
  stat_function(fun = function(x) func_Wbuf(x,0), aes(linetype = "h", color="h"), size=1) +
  stat_function(fun = function(x) func_Wbuf(0,x), aes(linetype = "v", color="v"), size=1) +
  scale_x_continuous('Physical buffer distance, height or width (ft)', breaks = seq(0,20,2), expand = c(0,0)) +
  scale_y_continuous(expression('Effective buffer'~(W[buf]^"*")), breaks = seq(0,20,2), expand = c(0,0)) +
  scale_linetype(NULL, labels = labs[['coefs']]) +
  scale_color_brewer(NULL, palette = 'Dark2', labels = labs[['coefs']]) +
  theme_classic() +
  coord_cartesian(xlim=c(0,15), ylim=c(0,20)) +
  theme(legend.position = 'bottom', #c(0.8,0.4),
        legend.direction = 'vertical',
        legend.margin = margin(-2.5,0,0,0, unit="pt"),
        legend.key.height = unit(1, "pt"),
        legend.text.align = 0,
        legend.background = element_rect(colour = "transparent", fill = "transparent"),
        legend.key = element_rect(fill = "transparent", colour = "transparent"))
ggsave(paste0(outpath,'plots/plt_effbuffer_coefs.png'), width = 6, height=3, dpi=300)


#### BUFFER FUNC DENSITY FORM ####
binsize <- 2
ggplot(plotdat[['grid']], aes(Wbuf, Hbuf, z=effWbuf)) +
  geom_contour_filled(binwidth = binsize) +
  geom_contour(binwidth=binsize, aes(color=..level..), size=0.05) +
  scale_color_continuous(low="black", high="black", guide=F) +
  scale_x_continuous(expression("Buffer width, "~italic(W[buf])~" (ft)"), breaks=0:20, expand = c(0,0)) +
  scale_y_continuous(expression("Buffer height, "~italic(H[buf])~" (ft)"), expand = c(0,0)) +
  scale_fill_brewer('Effective buffer',
                    labels = paste(binlabel(seq(0,Wmax,by=binsize)),'ft')) +
  coord_fixed(xlim = c(0, 10), ylim = c(0, 5)) +
  theme_bw() +
  theme(legend.key = element_rect(fill = 'transparent', color='black'),
        legend.position = "right",
        legend.margin = margin(-5,0,0,0, unit="pt"),
        legend.text.align = 0)
ggsave(paste0(outpath,'plots/plt_effbuffer_density.png'), width = 6, height=4, dpi=300)


#### CROSS SECTION LOS FACTOR ####
ggplot(plotdat[['grid']], aes(x=Wbuf, y=Hbuf,  z = Fw)) +
  geom_contour_filled(bins = 8) +
  scale_fill_brewer(expression("Cross-section\nadjustment factor,"~F[w]),
                    palette = "YlGnBu", direction = -1, drop = T)+
  scale_x_continuous(expression("Buffer width, "~italic(W[buf])~" (ft)"), expand = c(0,0)) +
  scale_y_continuous(expression("Buffer height, "~italic(H[buf])~" (ft)"), expand = c(0,0)) +
  coord_fixed(xlim = c(0,6), ylim = c(0,3)) +
  theme_bw() +
  theme(legend.position = "right")
ggsave(paste0(outpath,'plots/plt_effbuffer_los.png'), width = 6, height=4, dpi=300)



