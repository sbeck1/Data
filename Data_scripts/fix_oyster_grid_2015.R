 
# Getting LCR_oyster_grid_2013_2017.csv into same format as Grid_data_Nov_2012_bp.csv
# Started by Erica Christensen 2/13/18

library(dplyr)

quadrat = read.csv('Oyster_data/QUadrat/Grid_2013_2017/2015_quadrat.csv',stringsAsFactors = F,na.strings = c('',NA,'na'))
names(quadrat) <- c('Date','Location','Recorder','Transect','Distance_along','Distance_from','Live1','Live2','Live3','Live4','Live5','Live6',
                    'Dead1','Dead2','Dead3','Dead4','Live_over1','Live_over2','Live_over3','Live_over4','Live_over5','Dead_over1',
                    'Dead_over2','Dead_over3')
quadrat$Date = as.Date(quadrat$Date,format='%m/%d/%Y')


# split Live1, ... Live6, Dead1, ... Dead4 into separate data frames, which can then be stacked
Live1 = select(quadrat,Date,Location,Recorder,Transect,Distance_along,Distance_from,Live1) %>% filter(!is.na(Live1))
Live2 = select(quadrat,Date,Location,Recorder,Transect,Distance_along,Distance_from,Live2) %>% filter(!is.na(Live2))
Live3 = select(quadrat,Date,Location,Recorder,Transect,Distance_along,Distance_from,Live3) %>% filter(!is.na(Live3))
Live4 = select(quadrat,Date,Location,Recorder,Transect,Distance_along,Distance_from,Live4) %>% filter(!is.na(Live4))
Live5 = select(quadrat,Date,Location,Recorder,Transect,Distance_along,Distance_from,Live5) %>% filter(!is.na(Live5))
Live6 = select(quadrat,Date,Location,Recorder,Transect,Distance_along,Distance_from,Live6) %>% filter(!is.na(Live6))
Dead1 = select(quadrat,Date,Location,Recorder,Transect,Distance_along,Distance_from,Dead1) %>% filter(!is.na(Dead1))
Dead2 = select(quadrat,Date,Location,Recorder,Transect,Distance_along,Distance_from,Dead2) %>% filter(!is.na(Dead2))
Dead3 = select(quadrat,Date,Location,Recorder,Transect,Distance_along,Distance_from,Dead3) %>% filter(!is.na(Dead3))
Dead4 = select(quadrat,Date,Location,Recorder,Transect,Distance_along,Distance_from,Dead4) %>% filter(!is.na(Dead4))

# Rename Live and Dead columns to Size; create new column for Live_Dead
names(Live1)[7] = 'Size'
names(Live2)[7] = 'Size'
names(Live3)[7] = 'Size'
names(Live4)[7] = 'Size'
names(Live5)[7] = 'Size'
names(Live6)[7] = 'Size'
names(Dead1)[7] = 'Size'
names(Dead2)[7] = 'Size'
names(Dead3)[7] = 'Size'
names(Dead4)[7] = 'Size'
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

# ============================================================================================================================
# the Live_over and Dead_over columns are different - it represents counts of oysters, not sizes
Live_over1 = select(quadrat,Date,Location,Recorder,Transect,Distance_along,Distance_from,Live_over1) %>% filter(!is.na(Live_over1))
Live_over2 = select(quadrat,Date,Location,Recorder,Transect,Distance_along,Distance_from,Live_over2) %>% filter(!is.na(Live_over2))
Live_over3 = select(quadrat,Date,Location,Recorder,Transect,Distance_along,Distance_from,Live_over3) %>% filter(!is.na(Live_over3))
Live_over4 = select(quadrat,Date,Location,Recorder,Transect,Distance_along,Distance_from,Live_over4) %>% filter(!is.na(Live_over4))
Live_over5 = select(quadrat,Date,Location,Recorder,Transect,Distance_along,Distance_from,Live_over5) %>% filter(!is.na(Live_over5))
Dead_over1 = select(quadrat,Date,Location,Recorder,Transect,Distance_along,Distance_from,Dead_over1) %>% filter(!is.na(Dead_over1))
Dead_over2 = select(quadrat,Date,Location,Recorder,Transect,Distance_along,Distance_from,Dead_over2) %>% filter(!is.na(Dead_over2))
Dead_over3 = select(quadrat,Date,Location,Recorder,Transect,Distance_along,Distance_from,Dead_over3) %>% filter(!is.na(Dead_over3))

names(Live_over1)[7] = 'Count'
names(Live_over2)[7] = 'Count'
names(Live_over3)[7] = 'Count'
names(Live_over4)[7] = 'Count'
names(Live_over5)[7] = 'Count'
names(Dead_over1)[7] = 'Count'
names(Dead_over2)[7] = 'Count'
names(Dead_over3)[7] = 'Count'
Live_over = rbind(Live_over1,Live_over2,Live_over3,Live_over4,Live_over5)
Dead_over = rbind(Dead_over1,Dead_over2,Dead_over3)

Live_over_count = aggregate(Live_over$Count,by=list(Date=Live_over$Date,Location=Live_over$Location,Recorder=Live_over$Recorder,
                                                    Transect=Live_over$Transect,Distance_along=Live_over$Distance_along,Distance_from=Live_over$Distance_from),
                            FUN=sum)
Dead_over_count = aggregate(Dead_over$Count,by=list(Date=Dead_over$Date,Location=Dead_over$Location,Recorder=Dead_over$Recorder,
                                                    Transect=Dead_over$Transect,Distance_along=Dead_over$Distance_along,Distance_from=Dead_over$Distance_from),
                            FUN=sum)

Live_over_count$Live_Dead = rep('L')
Live_over_count$Size = rep(NA)
Live_over_count$Count = Live_over_count$x
Live_over_count = Live_over_count[,c('Date','Location','Recorder','Transect','Distance_along','Distance_from','Size','Live_Dead','Count')]
Dead_over_count$Live_Dead = rep('D')
Dead_over_count$Size = rep(NA)
Dead_over_count$Count = Dead_over_count$x
Dead_over_count = Dead_over_count[,c('Date','Location','Recorder','Transect','Distance_along','Distance_from','Size','Live_Dead','Count')]

oyster_quadrats1 = rbind(oyster_quadrats,Live_over_count,Dead_over_count)

# ========================================================================================================
# Make sure there is a L and D row for each quadrat (fill in zero counts if necessary)
allquadrats_L = quadrat %>% select(Date,Location,Recorder,Transect,Distance_along,Distance_from) %>% unique()
allquadrats_L$Live_Dead = rep('L')
allquadrats_D = quadrat %>% select(Date,Location,Recorder,Transect,Distance_along,Distance_from) %>% unique()
allquadrats_D$Live_Dead = rep('D')
allquadrats = rbind(allquadrats_L,allquadrats_D)

nonzeroquads = oyster_quadrats1 %>% select(Date,Location,Recorder,Transect,Distance_along,Distance_from,Live_Dead) %>% unique()
zeroquads = anti_join(allquadrats,nonzeroquads,by=c('Date','Location','Recorder','Transect','Distance_along','Distance_from','Live_Dead'))
zeroquads$Size = rep(NA)
zeroquads$Count = rep(0)
zeroquads = zeroquads[,c('Date','Location','Recorder','Transect','Distance_along','Distance_from','Size','Live_Dead','Count')]

oyster_quadrats2 = rbind(oyster_quadrats1,zeroquads)


# =========================================================================================================
# Do some checking of the data
#      are there NAs in weird places?
filter(oyster_quadrats2,is.na(Distance_along))




# rename columns to match Grid_data_Nov_2012_bp.csv as closely as possible
names(oyster_quadrats2)[names(oyster_quadrats2)=='Location'] = 'Station'
names(oyster_quadrats2)[names(oyster_quadrats2)=='Recorder'] = 'Counter'
names(oyster_quadrats2)[names(oyster_quadrats2)=='Distance_along'] = 'Dist_Alng'
names(oyster_quadrats2)[names(oyster_quadrats2)=='Distance_from'] = 'Dist_frm'



# reorder columns
oyster_quadrats2 = oyster_quadrats2[,c('Date','Station','Counter','Transect','Dist_Alng','Dist_frm','Live_Dead','Size','Count')]

# put rows in order of date, station, transect, dist_alng, dist_frm
oyster_quadrats2 = oyster_quadrats2[with(oyster_quadrats2,order(Date,Station,Transect,Dist_Alng,Dist_frm)),]

write.csv(oyster_quadrats2,'Oyster_data/Quadrat/Oyster_grid_2015.csv',row.names=F,quote=F)
