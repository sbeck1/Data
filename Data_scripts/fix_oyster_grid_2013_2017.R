 
# Getting LCR_oyster_grid_2013_2017.csv into same format as Grid_data_Nov_2012_bp.csv
# Started by Erica Christensen 2/13/18

library(dplyr)

quadrat = read.csv('Oyster_data/QUadrat/Grid_2013_2017/LCR_oyster_grid_2013_2017.csv',stringsAsFactors = F)

# split Live1, ... Live6, Dead1, ... Dead4 into separate data frames, which can then be stacked
Live1 = select(quadrat,Date,Location,Recorder,Transect,Condition,Distance_along,Distance_from,Live1)
Live2 = select(quadrat,Date,Location,Recorder,Transect,Condition,Distance_along,Distance_from,Live2) %>% filter(!is.na(Live2))
Live3 = select(quadrat,Date,Location,Recorder,Transect,Condition,Distance_along,Distance_from,Live3) %>% filter(!is.na(Live3))
Live4 = select(quadrat,Date,Location,Recorder,Transect,Condition,Distance_along,Distance_from,Live4) %>% filter(!is.na(Live4))
Live5 = select(quadrat,Date,Location,Recorder,Transect,Condition,Distance_along,Distance_from,Live5) %>% filter(!is.na(Live5))
Live6 = select(quadrat,Date,Location,Recorder,Transect,Condition,Distance_along,Distance_from,Live6) %>% filter(!is.na(Live6))
Dead1 = select(quadrat,Date,Location,Recorder,Transect,Condition,Distance_along,Distance_from,Dead1) 
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
Live1$Live_Dead = rep('Li1')
Live2$Live_Dead = rep('Li2')
Live3$Live_Dead = rep('Li3')
Live4$Live_Dead = rep('Li4')
Live5$Live_Dead = rep('Li5')
Live6$Live_Dead = rep('Li6')
Dead1$Live_Dead = rep('D1')
Dead2$Live_Dead = rep('D2')
Dead3$Live_Dead = rep('D3')
Dead4$Live_Dead = rep('D4')

# Stack all these data frames together
oyster_quadrats = rbind(Live1,Live2,Live3,Live4,Live5,Live6,Dead1,Dead2,Dead3,Dead4)

# Create Count column -- will be 1 when there is a size measurement, 0 when there is not
oyster_quadrats$Count = rep(0)
oyster_quadrats$Count[oyster_quadrats$Size>0] = 1

# Size 0 is meaningless, replace these with NA
oyster_quadrats$Size[oyster_quadrats$Size==0] = NA

# the Live_over column is different - it represents counts of live oysters, not sizes
Live_over = select(quadrat,Date,Location,Recorder,Transect,Condition,Distance_along,Distance_from,Live_over) %>% filter(!is.na(Live_over))
names(Live_over)[8] = 'Count'
Live_over$Live_Dead = rep('Li')
Live_over$Size = rep(NA)
Live_over = Live_over[,c('Date','Location','Recorder','Transect','Condition','Distance_along','Distance_from','Size','Live_Dead','Count')]

oyster_quadrats = rbind(oyster_quadrats,Live_over)

# Do some checking of the data
#      are there NAs in weird places?
filter(oyster_quadrats,is.na(Distance_along))




# rename columns to match Grid_data_Nov_2012_bp.csv as closely as possible
names(oyster_quadrats)[names(oyster_quadrats)=='Location'] = 'Station'
names(oyster_quadrats)[names(oyster_quadrats)=='Recorder'] = 'Counter'
names(oyster_quadrats)[names(oyster_quadrats)=='Distance_along'] = 'Dist_Alng'
names(oyster_quadrats)[names(oyster_quadrats)=='Distance_from'] = 'Dist_frm'



# reorder columns
oyster_quadrats = oyster_quadrats[,c('Date','Station','Counter','Transect','Condition','Dist_Alng','Dist_frm','Live_Dead','Size','Count')]

write.csv(oyster_quadrats,'Oyster_data/Quadrat/Oyster_quadrats.csv',row.names=F)
