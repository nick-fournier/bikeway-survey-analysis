# Title     : TODO
# Objective : TODO
# Created by: nichf
# Created on: 4/14/2021


#### PLOTTING ####
ggplot(bikedata_long, aes(ch, fill=variable)) + geom_bar(position = 'dodge') + theme_bw()
