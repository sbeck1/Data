 
# Getting LCR_oyster_grid_2013_2017.csv into same format as Grid_data_Nov_2012_bp.csv
# Started by Erica Christensen 2/13/18

library(dplyr)

quadrat = read.csv('Oyster_data/QUadrat/Grid_2013_2017/LCR_oyster_grid_2013_2017.csv',stringsAsFactors = F,na.strings = c('',NA))
quadrat$Date = as.Date(quadrat$Date,format='%m/%d/%Y')

# split Live1, ... Live6, Dead1, ... Dead4 into separate data frames, which can then be stacked
Live1 = select(quadrat,Date,Location,Recorder,Transect,Condition,Distance_along,Distance_from,Live1) %>% filter(!is.na(Live1))
Live2 = select(quadrat,Date,Location,Recorder,Transect,Condition,Distance_along,Distance_from,Live2) %>% filter(!is.na(Live2))
Live3 = select(quadrat,Date,Location,Recorder,Transect,Condition,Distance_along,Distance_from,Live3) %>% filter(!is.na(Live3))
Live4 = select(quadrat,Date,Location,Recorder,Transect,Condition,Distance_along,Distance_from,Live4) %>% filter(!is.na(Live4))
Live5 = select(quadrat,Date,Location,Recorder,Transect,Condition,Distance_along,Distance_from,Live5) %>% filter(!is.na(Live5))
Live6 = select(quadrat,Date,Location,Recorder,Transect,Condition,Distance_along,Distance_from,Live6) %>% filter(!is.na(Live6))
Dead1 = select(quadrat,Date,Location,Recorder,Transect,Condition,Distance_along,Distance_from,Dead1) %>% filter(!is.na(Dead1))
Dead2 = select(quadrat,Date,Location,Recorder,Transect,Condition,Distance_along,Distance_from,Dead2) %>% filter(!is.na(Dead2))
Dead3 = select(quadrat,Date,Location,Recorder,Transect,Condition,Distance_along,Distance_from,Dead3) %>% filter(!is.na(Dead3))
Dead4 = select(quadrat,Date,Location,Recorder,Transect,Condition,Distance_along,Distance_from,Dead4) %>% filter(!is.na(Dead4))


# Rename Live and Dead columns to Size; create new column for Live_Dead
names(Live1)[8] = 'Size'
names(Live2)[8] = 'Size'
names(Live3)[8] = 'Size'
names(Live4)[8] = 'Size'
names(Live5)[8] = 'Size'
names(Live6)[8] = 'Size'
names(Dead1)[8] = 'Size'
names(Dead2)[8] = 'Size'
names(Dead3)[8] = 'Size'
names(Dead4)[8] = 'Size'
Live1$Live_Dead = rep('L')
Live2$Live_Dead = rep('L')
Live3$Live_Dead = rep('L')
Live4$Live_Dead = rep('L')
Live5$Live_Dead = rep('L')
Live6$Live_Dead = rep('L')
Dead1$Live_Dead = rep('D')
Dead2$Live_Dead = rep('D')
Dead3$Live_Dead = rep('D')
Dead4$Live_Dead = rep('D')

# Stack all these data frames together
oyster_quadrats = rbind(Live1,Live2,Live3,Live4,Live5,Live6,Dead1,Dead2,Dead3,Dead4)

# Create Count column -- will be 1 when there is a size measurement, 0 when there is not
oyster_quadrats$Count = rep(0)
oyster_quadrats$Count[oyster_quadrats$Size>0] = 1

# Size 0 is meaningless, replace these with NA
oyster_quadrats$Size[oyster_quadrats$Size==0] = NA


# ==========================================================================================================
# the Live_over column is different - it represents counts of live oysters, not sizes
Live_over = select(quadrat,Date,Location,Recorder,Transect,Condition,Distance_along,Distance_from,Live_over) %>% filter(!is.na(Live_over))
names(Live_over)[8] = 'Count'
Live_over$Live_Dead = rep('L')
Live_over$Size = rep(NA)
Live_over = Live_over[,c('Date','Location','Recorder','Transect','Condition','Distance_along','Distance_from','Size','Live_Dead','Count')]

oyster_quadrats1 = rbind(oyster_quadrats,Live_over)


# ========================================================================================================
# Make sure there is a L and D row for each quadrat (fill in zero counts if necessary)
allquadrats_L = quadrat %>% select(Date,Location,Recorder,Transect,Condition,Distance_along,Distance_from) %>% unique()
allquadrats_L$Live_Dead = rep('L')
allquadrats_D = quadrat %>% select(Date,Location,Recorder,Transect,Condition,Distance_along,Distance_from) %>% unique()
allquadrats_D$Live_Dead = rep('D')
allquadrats = rbind(allquadrats_L,allquadrats_D)

nonzeroquads = oyster_quadrats1 %>% select(Date,Location,Recorder,Transect,Condition,Distance_along,Distance_from,Live_Dead) %>% unique()
zeroquads = anti_join(allquadrats,nonzeroquads,by=c('Date','Location','Recorder','Transect','Condition','Distance_along','Distance_from','Live_Dead'))
zeroquads$Size = rep(NA)
zeroquads$Count = rep(0)
zeroquads = zeroquads[,c('Date','Location','Recorder','Transect','Condition','Distance_along','Distance_from','Size','Live_Dead','Count')]

oyster_quadrats2 = rbind(oyster_quadrats1,zeroquads)


# ============================================================================================================
# Do some checking of the data
#      are there NAs in weird places?
filter(oyster_quadrats2,is.na(Distance_from))




# rename columns to match Grid_data_Nov_2012_bp.csv as closely as possible
names(oyster_quadrats2)[names(oyster_quadrats2)=='Location'] = 'Station'
names(oyster_quadrats2)[names(oyster_quadrats2)=='Recorder'] = 'Counter'
names(oyster_quadrats2)[names(oyster_quadrats2)=='Distance_along'] = 'Dist_Alng'
names(oyster_quadrats2)[names(oyster_quadrats2)=='Distance_from'] = 'Dist_frm'



# reorder columns
oyster_quadrats2 = oyster_quadrats2[,c('Date','Station','Counter','Transect','Condition','Dist_Alng','Dist_frm','Live_Dead','Size','Count')]

# put rows in order of date, station, transect, dist_alng, dist_frm
oyster_quadrats2 = oyster_quadrats2[with(oyster_quadrats2,order(Date,Station,Transect,Condition,Dist_Alng,Dist_frm)),]

write.csv(oyster_quadrats2,'Oyster_data/Quadrat/Oyster_grid_2013_2017.csv',row.names=F,quote=F)
