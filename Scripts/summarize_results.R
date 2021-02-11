summary.time.string <- paste0(format(Sys.time(), "%Y%m%d_%H%M%S_"),as.character(as.numeric(format(Sys.time(), "%OS6"))*1000000))

swr = function(string, nwrap=10) {
     paste(strwrap(string, width=nwrap), collapse="\n")
}

out.path.trunc <- substr(out.path, 1, nchar(out.path)-1)
swr = Vectorize(swr)
files = list.files(path=out.path.trunc, pattern="*vehicle_energy_summary.rds",full.names=TRUE) 
energy.summaries=lapply(files, readRDS)
files <- list.files(path=out.path.trunc, pattern="*vehicle_energy_summary.rds",full.names=FALSE)
for (i in 1:length(energy.summaries)){energy.summaries[[i]]$time.string <- files[i]}
energy.summaries <- rbindlist(energy.summaries)
energy.summaries[,time.string:=substr(time.string,1,str_count(time.string,"[0-9]")+2)]

files = list.files(path=out.path.trunc, pattern="*vehicles.rds",full.names=TRUE)
vehicles=lapply(files, readRDS)
files <- list.files(path=out.path.trunc, pattern="*vehicles.rds",full.names=FALSE)
for (i in 1:length(vehicles)){vehicles[[i]]$time.string <- files[i]}
vehicles <- rbindlist(vehicles, use.names=TRUE, fill=TRUE)
vehicles[,time.string:=substr(time.string,1,str_count(time.string,"[0-9]")+2)]

files = list.files(path=out.path.trunc, pattern="*vehicle_slopes.rds",full.names=TRUE)
vehicle_slopes=lapply(files, readRDS)
files <- list.files(path=out.path.trunc, pattern="*vehicle_slopes.rds",full.names=FALSE)
for (i in 1:length(vehicle_slopes)){vehicle_slopes[[i]]$time.string <- files[i]}
vehicle_slopes <- rbindlist(vehicle_slopes, use.names=TRUE, fill=TRUE)
vehicle_slopes[,time.string:=substr(time.string,1,str_count(time.string,"[0-9]")+2)]
vehicle_slopes <- as.matrix(vehicle_slopes)

files = list.files(path=out.path.trunc, pattern="*vehicle_intercepts.rds",full.names=TRUE)
vehicle_intercepts=lapply(files, readRDS)
files <- list.files(path=out.path.trunc, pattern="*vehicle_intercepts.rds",full.names=FALSE)
for (i in 1:length(vehicle_intercepts)){vehicle_intercepts[[i]]$time.string <- files[i]}
vehicle_intercepts <- rbindlist(vehicle_intercepts, use.names=TRUE, fill=TRUE)
vehicle_intercepts[,time.string:=substr(time.string,1,str_count(time.string,"[0-9]")+2)]
vehicle_intercepts <- as.matrix(vehicle_intercepts)

files = list.files(path=out.path.trunc, pattern="*vehicle_slopes_external.rds",full.names=TRUE)
vehicle_slopes_external=lapply(files, readRDS)
files <- list.files(path=out.path.trunc, pattern="*vehicle_slopes_external.rds",full.names=FALSE)
for (i in 1:length(vehicle_slopes_external)){vehicle_slopes_external[[i]]$time.string <- files[i]}
vehicle_slopes_external <- rbindlist(vehicle_slopes_external, use.names=TRUE, fill=TRUE)
vehicle_slopes_external[,time.string:=substr(time.string,1,str_count(time.string,"[0-9]")+2)]
vehicle_slopes_external <- as.matrix(vehicle_slopes_external)

files = list.files(path=out.path.trunc, pattern="*vehicle_intercepts_external.rds",full.names=TRUE)
vehicle_intercepts_external=lapply(files, readRDS)
files <- list.files(path=out.path.trunc, pattern="*vehicle_intercepts_external.rds",full.names=FALSE)
for (i in 1:length(vehicle_intercepts_external)){vehicle_intercepts_external[[i]]$time.string <- files[i]}
vehicle_intercepts_external <- rbindlist(vehicle_intercepts_external, use.names=TRUE, fill=TRUE)
vehicle_intercepts_external[,time.string:=substr(time.string,1,str_count(time.string,"[0-9]")+2)]
vehicle_intercepts_external <- as.matrix(vehicle_intercepts_external)


files = list.files(path=out.path.trunc, pattern="*run_summary.rds",full.names=TRUE)
run.summaries=lapply(files, readRDS)
files <- list.files(path=out.path.trunc, pattern="*run_summary.rds",full.names=FALSE)
for (i in 1:length(run.summaries)){run.summaries[[i]]$time.string <- files[i]}
run.summaries <- rbindlist(run.summaries,fill=TRUE)
run.summaries[,time.string:=substr(time.string,1,str_count(time.string,"[0-9]")+2)] 
run.summaries[is.na(labor.price),labor.price:=0]

files = list.files(path=out.path.trunc, pattern="*summary_results.rds",full.names=TRUE)
result.summaries=lapply(files, readRDS)
files <- list.files(path=out.path.trunc, pattern="*summary_results.rds",full.names=FALSE)
for (i in 1:length(result.summaries)){result.summaries[[i]]$time.string <- files[i]}
result.summaries <- rbindlist(result.summaries,fill=TRUE)
result.summaries[,time.string:=substr(time.string,1,str_count(time.string,"[0-9]")+2)]
result.summaries$trip.miles <- NULL; result.summaries$empty.miles <- NULL; result.summaries$pct.occupied.vmt <- NULL;
result.summaries$highway.factor <- NULL; 

files = list.files(path=out.path.trunc, pattern="*vehicle_arcs_used.rds",full.names=TRUE)
v.arcs.used=lapply(files, readRDS)
files <- list.files(path=out.path.trunc, pattern="*vehicle_arcs_used.rds",full.names=FALSE)
for (i in 1:length(v.arcs.used)){v.arcs.used[[i]]$time.string <- files[i]}
v.arcs.used <- rbindlist(v.arcs.used,fill=TRUE)
v.arcs.used[,time.string:=substr(time.string,1,str_count(time.string,"[0-9]")+2)]
vehicle.distances <- v.arcs.used[,.(distance=sum(solution*distance*annualized.days,na.rm=TRUE)),by=.(time.string,vehicle.id)]
v.arcs.used[run.summaries,on=.(time.string=time.string),electricity.price:=i.electricity.price]

files = list.files(path=out.path.trunc, pattern="*vehicles.rds",full.names=TRUE)
vehicles=lapply(files, readRDS)
files <- list.files(path=out.path.trunc, pattern="*vehicles.rds",full.names=FALSE)
for (i in 1:length(vehicles)){vehicles[[i]]$time.string <- files[i]}
vehicles <- rbindlist(vehicles,fill=TRUE)
vehicles[,time.string:=substr(time.string,1,str_count(time.string,"[0-9]")+2)]

vehicles <- vehicles[run.summaries, on=.(time.string=time.string)]
vehicles <- vehicles[vehicle.distances,on=.(vehicle.id=vehicle.id,time.string=time.string),distance.traveled:=i.distance]
result.summaries <- result.summaries[run.summaries,on=.(time.string=time.string)]
v.arcs.used[vehicles,on=.(vehicle.id=vehicle.id,time.string=time.string),':='(vehicle.private.costs=capcost.private,
                                                                              vehicle.external.costs=capcost.external)]
v.arcs.used[,total.vehicle.distance:=sum(solution*annualized.days*distance),
                                         by=.(time.string,vehicle.id)][,distance.proportion:=(solution*distance*annualized.days)/total.vehicle.distance]


usd.2017.to.2018 <- 1.02
maintenance.cv <- 0.065*usd.2017.to.2018
maintenance.hev <- maintenance.cv*0.86 
maintenance.bev <- maintenance.cv*0.509
v.arcs.used[,per.mile.maintenance:=ifelse(vehicle.type=='ICE',maintenance.cv,
                                         ifelse(vehicle.type=='HEV',maintenance.hev,maintenance.bev))]
v.arcs.used[,':='(
     result.costs=0,
     result.damages=0,
     result.fuel.costs=0,
     result.maintenance.costs=0,
     result.tailpipe.damages=0,
     result.refining.damages=0)]
v.arcs.used[type=='charge' & vehicle.type=='EV',':='(result.costs=charge.change.solution*annualized.days*electricity.price,
                                                     result.costs.hourly=charge.change.solution*electricity.price,
                                                     result.damages=charge.change.solution*annualized.days*total.damages,
                                                     result.damages.hourly=charge.change.solution*total.damages)]
v.arcs.used[type!='charge',':='(result.costs=solution*annualized.days*private.cost,
                                result.costs.hourly=solution*private.cost+distance.proportion*vehicle.private.costs/annualized.days,
                                result.fuel.costs=solution*annualized.days*(private.cost-per.mile.maintenance*distance),
                                result.maintenance.costs=solution*annualized.days*(per.mile.maintenance*distance),
                                result.damages=solution*annualized.days*total.damages,
                                result.damages.hourly=solution*total.damages+distance.proportion*vehicle.external.costs/annualized.days,
                                result.tailpipe.damages=solution*annualized.days*(tailpipe.ghg.damages+tailpipe.nox.damages+tailpipe.pm25.damages+tailpipe.voc.damages+tailpipe.so2.damages),
                                result.refining.damages=solution*annualized.days*(upstream.ghg.damages+upstream.nox.damages+upstream.pm25.damages+upstream.voc.damages+upstream.so2.damages)
                                )]

v.arcs.used[category=='Summer_0',cat:='Summer Weekday']
v.arcs.used[category=='Summer_1',cat:='Summer Weekend']
v.arcs.used[category=='Trans_0',cat:='Fall/Spring Weekday']
v.arcs.used[category=='Trans_1',cat:='Fall/Spring Weekend']
v.arcs.used[category=='SXSW_0',cat:='SXSW Weekday']
v.arcs.used[category=='SXSW_1',cat:='SXSW Weekend']
v.arcs.used[category=='Winter_0',cat:='Winter Weekday']
v.arcs.used[category=='Winter_1',cat:='Winter Weekend']
categories <- data.frame(category=v.arcs.used$category,cat=v.arcs.used$cat)
categories <- unique(categories)
categories$catnew <- swr(categories$cat)
v.arcs.used[categories,on=.(category=category),catnew:=catnew]
factor.list <- c('Summer Weekday','Summer Weekend',
                 'Fall/Spring Weekday','Fall/Spring Weekend',
                 'Winter Weekday','Winter Weekend',
                 'SXSW Weekday','SXSW Weekend')
factor.list <- swr(factor.list)
v.arcs.used$catnew_f = factor(v.arcs.used$catnew, levels=factor.list)
v.arcs.used[,cat:=NULL] 
v.arcs.used[,catnew:=NULL] 
#rm(categories)

v.arcs.used[type!='charge',':='(result.pollutant.damages=
                                  solution*annualized.days*(
                                       tailpipe.nox.damages+tailpipe.pm25.damages+tailpipe.voc.damages+tailpipe.so2.damages+
                                       upstream.nox.damages+upstream.pm25.damages+upstream.voc.damages+upstream.so2.damages),
                             result.ghg.damages=
                                  solution*annualized.days*(tailpipe.ghg.damages+upstream.ghg.damages)
                             )]
v.arcs.used[type=='charge' & vehicle.type=='EV',':='(result.pollutant.damages=
                                  charge.change.solution*annualized.days*(
                                       tailpipe.nox.damages+tailpipe.pm25.damages+tailpipe.voc.damages+tailpipe.so2.damages+
                                            upstream.nox.damages+upstream.pm25.damages+upstream.voc.damages+upstream.so2.damages),
                             result.ghg.damages=
                                  charge.change.solution*annualized.days*(tailpipe.ghg.damages+upstream.ghg.damages)
)]

v.arcs.summary <- v.arcs.used[,.(
     bought=sum(solution*(type=='dispatch')),
     trips=sum(1.0*solution*annualized.days*ifelse(type=='trip',1.0,0)),
     trips.per.car=sum(solution*annualized.days*ifelse(type=='trip',1.0,0))/sum(solution*ifelse(type=='dispatch',1,0)),
     vmt=sum(solution*annualized.days*distance),
     trip.vmt=sum(ifelse(type=='trip',solution*annualized.days*distance,0)),
     vmt.per.car=sum(solution*annualized.days*distance)/sum(solution*ifelse(type=='dispatch',1,0)),
     dispatch.private.costs=sum(result.costs,na.rm=TRUE),
     dispatch.private.costs.per.car=sum(result.costs,na.rm=TRUE)/sum(solution*ifelse(type=='dispatch',1,0)),
     fuel.costs=sum(result.fuel.costs,na.rm=TRUE),
     maintenance.costs=sum(result.maintenance.costs,na.rm=TRUE),
     grid.costs=sum(ifelse(vehicle.type=='EV' & type=='charge',result.costs,0)),
     dispatch.damages=sum(result.damages,na.rm=TRUE),
     dispatch.damages.per.car=sum(result.damages)/sum(solution*ifelse(type=='dispatch',1,0)),
     tailpipe.damages=sum(result.tailpipe.damages,na.rm=TRUE),
     refining.damages=sum(result.refining.damages,na.rm=TRUE),
     pollutant.damages=sum(result.pollutant.damages,na.rm=TRUE),
     ghg.damages=sum(result.ghg.damages,na.rm=TRUE),
     grid.damages=sum(ifelse(vehicle.type=='EV' & type=='charge',result.damages,0)),
     grid.damages.per.kwh=sum(ifelse(vehicle.type=='EV' & type=='charge',result.damages,0))/sum(ifelse(vehicle.type=='EV' & type=='charge',charge.change.solution*annualized.days,0)),
     grid.pollutant.damages.per.kwh=sum(ifelse(vehicle.type=='EV' & type=='charge',result.pollutant.damages,0))/sum(ifelse(vehicle.type=='EV' & type=='charge',charge.change.solution*annualized.days,0)),
     grid.ghg.damages.per.kwh=sum(ifelse(vehicle.type=='EV' & type=='charge',result.ghg.damages,0))/sum(ifelse(vehicle.type=='EV' & type=='charge',charge.change.solution*annualized.days,0)),
     trip.miles=sum(ifelse(type=='trip',annualized.days*distance*solution,0)),
     empty.miles=sum(ifelse(type!='trip',annualized.days*distance*solution,0)),
     pct.occupied.vmt=sum(ifelse(type=='trip',distance*solution,0))/sum(annualized.days*distance*solution,na.rm=TRUE),
     highway.factor=weighted.mean(highway.factor,annualized.days*distance*solution),
     purchased.cv=sum(ifelse(type=='dispatch'&vehicle.type=='ICE',solution,0)),
     purchased.hev=sum(ifelse(type=='dispatch'&vehicle.type=='HEV',solution,0)),
     purchased.bev=sum(ifelse(type=='dispatch'&vehicle.type=='EV',solution,0)),
     cv.vmt=sum(ifelse(vehicle.type=='ICE',solution*annualized.days*distance,0)),
     hev.vmt=sum(ifelse(vehicle.type=='HEV',solution*annualized.days*distance,0)),
     bev.vmt=sum(ifelse(vehicle.type=='EV',solution*annualized.days*distance,0)),
     cv.trip.vmt=sum(ifelse(vehicle.type=='ICE' & type=='trip',solution*annualized.days*distance,0)),
     hev.trip.vmt=sum(ifelse(vehicle.type=='HEV' & type=='trip',solution*annualized.days*distance,0)),
     bev.trip.vmt=sum(ifelse(vehicle.type=='EV' & type=='trip',solution*annualized.days*distance,0))
     ), by=time.string]
v.arcs.summary[,':='(vmt.ptm=vmt/trip.miles,vmt.per.car.ptm=vmt.per.car/trip.miles,
                     dispatch.private.costs.ptm=dispatch.private.costs/trip.miles,
                     fuel.costs.ptm=fuel.costs/trip.miles,
                     maintenance.costs.ptm=maintenance.costs/trip.miles,
                     grid.costs.ptm=grid.costs/trip.miles,
                     dispatch.private.costs.per.car.ptm=dispatch.private.costs.per.car/trip.miles,
                     dispatch.damages.ptm=dispatch.damages/trip.miles,
                     tailpipe.damages.ptm=tailpipe.damages/trip.miles,
                     refining.damages.ptm=refining.damages/trip.miles,
                     grid.damages.ptm=grid.damages/trip.miles,
                     pollutant.damages.ptm=pollutant.damages/trip.miles,
                     ghg.damages.ptm=ghg.damages/trip.miles)]
v.arcs.summary <- v.arcs.summary[run.summaries,on=.(time.string=time.string)]
vehicles.summary <- vehicles[,.(car.private.cost.total=sum(capcost.private,na.rm=TRUE),
                                car.damages.total=sum(capcost.external,na.rm=TRUE),
                                car.damages.pollutant=sum(capcost.external*(upfront.nox.cost+upfront.so2.cost+upfront.pm25.cost+upfront.voc.cost)/upfront.damage.cost,na.rm=TRUE),
                                car.damages.ghg=sum(capcost.external*upfront.ghg.cost/upfront.damage.cost,na.rm=TRUE)),
                             by=time.string]
						
summary.table <- v.arcs.summary[vehicles.summary,on=.(time.string=time.string)][
     ,':='(private.costs=dispatch.private.costs+car.private.cost.total,
           external.costs=dispatch.damages+car.damages.total,
           total.costs=car.private.cost.total+car.damages.total+dispatch.private.costs+dispatch.damages)]
summary.table[,':='(overall.private.costs=car.private.cost.total+dispatch.private.costs,
                    overall.damages=car.damages.total+dispatch.damages,
                    overall.pollutant.damages=pollutant.damages+car.damages.pollutant,
                    overall.ghg.damages=ghg.damages+car.damages.ghg)]
summary.table[,':='(overall.private.costs.ptm=overall.private.costs/trip.miles,
                    overall.damages.ptm=overall.damages/trip.miles,
                    purchase.private.costs.ptm=car.private.cost.total/trip.miles,
                    purchase.damages.ptm=car.damages.total/trip.miles,
                    overall.pollutant.damages.ptm=overall.pollutant.damages/trip.miles,
                    overall.ghg.damages.ptm=overall.ghg.damages/trip.miles)]
summary.table[costs.to.include=='private',':='(cti="Private")]
summary.table[costs.to.include=='both',':='(cti="Social")]
summary.table[costs.to.include=='social',':='(cti="External")]
summary.table$cti = factor(summary.table$cti, levels=c("Private","Social","External"))
summary.table <- summary.table[order(powertrains,location,discount.rate,costs.to.include)]

v.arcs.used[,':='(hour=hour(start.time),
                  trip.vehicle.id=ifelse(type=='trip',vehicle.id,as.numeric(NA)))]
if(!("include.resale" %in% colnames(summary.table))){
		summary.table[,include.resale:=TRUE]
	}
summary.summary <- summary.table[,.(time.string,powertrains,damage.model,costs.to.include,location,discount.rate,
                                    cr.to.use,carbon.price,grid.damage.multiplier,ev.purchase.price,
                                    gas.price,electricity.price,labor.price,
                                    purchased.cv,purchased.hev,purchased.bev,
                                    cv.vmt,hev.vmt,bev.vmt,
                                    private.costs,external.costs,total.costs,
                                    dispatch.private.costs,dispatch.damages,dispatch.private.costs.ptm,dispatch.damages.ptm,
                                    fuel.costs.ptm,grid.costs.ptm,maintenance.costs.ptm,
                                    car.private.cost.total,car.damages.total,purchase.private.costs.ptm,purchase.damages.ptm,
                                    tailpipe.damages.ptm,refining.damages.ptm,grid.damages.ptm,
                                    grid.damages.per.kwh,grid.pollutant.damages.per.kwh,grid.ghg.damages.per.kwh,
                                    overall.pollutant.damages.ptm,overall.ghg.damages.ptm,
                                    overall.private.costs.ptm,overall.damages.ptm,total.costs.ptm=overall.private.costs.ptm+overall.damages.ptm,
                                    vmt,pct.occupied.vmt,vmt.ptm,gap,final.mipgap, obj, bound, final.bound, bev.model, include.resale, comment=as.character(comment)
                                    )][,':='(discount.rate=round(discount.rate,digits=3),
									carbon.price=round(carbon.price,digits=3),
									grid.damage.multiplier=round(grid.damage.multiplier,digits=3),
									ev.purchase.price=round(ev.purchase.price,digits=3),
									gas.price=round(gas.price,digits=3),
									electricity.price=round(electricity.price,digits=3),
									labor.price=round(labor.price,digits=3)
									)]
summary.summary[,best.objective:=min(obj,na.rm=TRUE), by=.(powertrains,damage.model,discount.rate,location,
                                                         cr.to.use,carbon.price,grid.damage.multiplier,ev.purchase.price,
                                                         gas.price,electricity.price,labor.price,costs.to.include,include.resale,bev.model)][
                                                              ,case.to.use:=(best.objective==obj)
                                                         ] 
summary.summary[, temp1:=ifelse(case.to.use==1 & costs.to.include=='both',bound,NA)]
summary.summary[, temp2:=ifelse(case.to.use==1 & costs.to.include=='both',final.bound,NA)]
summary.summary[, temp3:=pmax(temp1,temp2)]
summary.summary[, temp:=pmax(ifelse(case.to.use==1 & costs.to.include=='both',bound,NA), 
                             ifelse(case.to.use==1 & costs.to.include=='both,',final.bound,NA),na.rm=TRUE)]

write.csv(summary.summary,paste0(out.path.trunc,"/output_list_",summary.time.string,".csv"))

#obj, bound, final.bound
summary.summary.grp <- summary.summary[,.(count=.N,
                                          cv.private=max(ifelse(case.to.use==1 & costs.to.include=='private',purchased.cv,NA),na.rm=TRUE),
                                          hev.private=max(ifelse(case.to.use==1 & costs.to.include=='private',purchased.hev,NA),na.rm=TRUE),
                                          bev.private=max(ifelse(case.to.use==1 & costs.to.include=='private',purchased.bev,NA),na.rm=TRUE),
                                          cv.social=max(ifelse(case.to.use==1 & costs.to.include=='both',purchased.cv,NA),na.rm=TRUE),
                                          hev.social=max(ifelse(case.to.use==1 & costs.to.include=='both',purchased.hev,NA),na.rm=TRUE),
                                          bev.social=max(ifelse(case.to.use==1 & costs.to.include=='both',purchased.bev,NA),na.rm=TRUE),
                                          cv.vmt.private=max(ifelse(case.to.use==1 & costs.to.include=='private',cv.vmt,NA),na.rm=TRUE),
                                          hev.vmt.private=max(ifelse(case.to.use==1 & costs.to.include=='private',hev.vmt,NA),na.rm=TRUE),
                                          bev.vmt.private=max(ifelse(case.to.use==1 & costs.to.include=='private',bev.vmt,NA),na.rm=TRUE),
                                          cv.vmt.social=max(ifelse(case.to.use==1 & costs.to.include=='both',cv.vmt,NA),na.rm=TRUE),
                                          hev.vmt.social=max(ifelse(case.to.use==1 & costs.to.include=='both',hev.vmt,NA),na.rm=TRUE),
                                          bev.vmt.social=max(ifelse(case.to.use==1 & costs.to.include=='both',bev.vmt,NA),na.rm=TRUE),
                                          private.cost.bau=min(ifelse(case.to.use==1 & costs.to.include=='private',overall.private.costs.ptm,NA),na.rm=TRUE),
                                          private.cost.change=(min(ifelse(case.to.use==1 & costs.to.include=='both',overall.private.costs.ptm,NA),na.rm=TRUE)-min(ifelse(costs.to.include=='private',overall.private.costs.ptm,NA),na.rm=TRUE))/min(ifelse(costs.to.include=='private',overall.private.costs.ptm,NA),na.rm=TRUE),
                                          external.cost.bau=min(ifelse(case.to.use==1 & costs.to.include=='private',overall.damages.ptm,NA),na.rm=TRUE),
                                          external.cost.change=(min(ifelse(case.to.use==1 & costs.to.include=='both',overall.damages.ptm,NA),na.rm=TRUE)-min(ifelse(costs.to.include=='private',overall.damages.ptm,NA),na.rm=TRUE))/min(ifelse(costs.to.include=='private',overall.damages.ptm,NA),na.rm=TRUE),
                                          social.cost.bau=min(ifelse(case.to.use==1 & costs.to.include=='private',total.costs.ptm,NA),na.rm=TRUE),
                                          social.cost.change=(min(ifelse(case.to.use==1 & costs.to.include=='both',total.costs.ptm,NA),na.rm=TRUE)-min(ifelse(costs.to.include=='private',total.costs.ptm,NA),na.rm=TRUE))/min(ifelse(costs.to.include=='private',total.costs.ptm,NA),na.rm=TRUE),
                                          pollutant.cost.bau=min(ifelse(case.to.use==1 & costs.to.include=='private',overall.pollutant.damages.ptm,NA),na.rm=TRUE),
                                          pollutant.cost.change=(min(ifelse(case.to.use==1 & costs.to.include=='both',overall.pollutant.damages.ptm,NA),na.rm=TRUE)-min(ifelse(costs.to.include=='private',overall.pollutant.damages.ptm,NA),na.rm=TRUE))/min(ifelse(costs.to.include=='private',overall.pollutant.damages.ptm,NA),na.rm=TRUE),
                                          ghg.cost.bau=min(ifelse(case.to.use==1 & costs.to.include=='private',overall.ghg.damages.ptm,NA),na.rm=TRUE),
                                          ghg.cost.change=(min(ifelse(case.to.use==1 & costs.to.include=='both',overall.ghg.damages.ptm,NA),na.rm=TRUE)-min(ifelse(costs.to.include=='private',overall.ghg.damages.ptm,NA),na.rm=TRUE))/min(ifelse(costs.to.include=='private',overall.ghg.damages.ptm,NA),na.rm=TRUE),
                                          grid.damages.per.kwh.bau=min(ifelse(case.to.use==1 & costs.to.include=='private',grid.damages.per.kwh,NA),na.rm=TRUE),
                                          grid.damages.per.kwh.change=(min(ifelse(case.to.use==1 & costs.to.include=='both',grid.damages.per.kwh,NA),na.rm=TRUE)-min(ifelse(costs.to.include=='private',grid.damages.per.kwh,NA),na.rm=TRUE))/min(ifelse(costs.to.include=='private',grid.damages.per.kwh,NA),na.rm=TRUE),
                                          #min.gap.private=min(ifelse(costs.to.include=='private',gap,NA),na.rm=TRUE),
                                          #min.gap.social=min(ifelse(costs.to.include=='both',gap,NA),na.rm=TRUE),
                                          #min.mipgap.private=min(ifelse(case.to.use==1 & costs.to.include=='private',final.mipgap,NA),na.rm=TRUE),
                                          #min.mipgap.social=min(ifelse(case.to.use==1 & costs.to.include=='both',final.mipgap,NA),na.rm=TRUE),
                                          min.obj.private=min(ifelse(case.to.use==1 & costs.to.include=='private',obj,NA),na.rm=TRUE),
                                          max.bound.private=max(pmax(ifelse(case.to.use==1 & costs.to.include=='private',bound,NA), ifelse(case.to.use==1 & costs.to.include=='private,',final.bound,NA),na.rm=TRUE),na.rm=TRUE),
                                          min.gap.private=(min(ifelse(case.to.use==1 & costs.to.include=='private',obj,NA),na.rm=TRUE) - max(pmax(ifelse(case.to.use==1 & costs.to.include=='private',bound,NA),
                                                                                                              ifelse(case.to.use==1 & costs.to.include=='private,',final.bound,NA),na.rm=TRUE),na.rm=TRUE))/min(ifelse(case.to.use==1 & costs.to.include=='private',obj,NA),na.rm=TRUE),
                                          min.obj.social=min(ifelse(case.to.use==1 & costs.to.include=='both',obj,NA),na.rm=TRUE),
                                          max.bound.social=max(pmax(ifelse(case.to.use==1 & costs.to.include=='both',bound,NA), ifelse(case.to.use==1 & costs.to.include=='both',final.bound,NA),na.rm=TRUE),na.rm=TRUE),
                                          min.gap.social=(min(ifelse(case.to.use==1 & costs.to.include=='both',obj,NA),na.rm=TRUE) - max(pmax(ifelse(case.to.use==1 & costs.to.include=='both',bound,NA),
                                                                   ifelse(case.to.use==1 & costs.to.include=='both',final.bound,NA),na.rm=TRUE),na.rm=TRUE))/min(ifelse(case.to.use==1 & costs.to.include=='both',obj,NA),na.rm=TRUE),
                                          private.string=max(ifelse(case.to.use==1 & costs.to.include=='private',time.string,as.character(NA)),na.rm=TRUE),
                                          social.string=max(ifelse(case.to.use==1 & costs.to.include=='both',time.string,as.character(NA)),na.rm=TRUE),
										  private.comment=max(ifelse(case.to.use==1 & costs.to.include=='private',as.character(comment),as.character(NA)),na.rm=TRUE),
                                          social.comment=max(ifelse(case.to.use==1 & costs.to.include=='both',as.character(comment),as.character(NA)),na.rm=TRUE)
                                          ),by=.(powertrains,damage.model,discount.rate,location,
                                                 cr.to.use,carbon.price,grid.damage.multiplier,ev.purchase.price,
                                                 gas.price,electricity.price,labor.price,include.resale,bev.model)]
												 
write.csv(summary.summary.grp,paste0(out.path.trunc,"/summary_",summary.time.string,".csv")) 
