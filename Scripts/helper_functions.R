rm(list=lsf.str())

usd.2007.to.2018 <- 1.22
vsl.2010.to.2018 <- 1.149855312
usd.2017.to.2018 <- 1.02
fe.ev.city <- 27.2650 #kwh/100mi
fe.ev.hwy <- 36.0674 #kwh/100mi
fe.ice.city <- 26.0867 #mpg
fe.ice.hwy <- 30.9049 #mpg
ev.purchase.price <- 33950#-7500
ice.purchase.price <- 20490
fe.hev.city <- 1/((1/fe.ice.city)*((1/39)/(1/25)))
fe.hev.hwy <- 1/((1/fe.ice.hwy)*((1/45)/(1/35)))
hev.purchase.price <- ice.purchase.price*(27985/22900)
capacity.ev <- 30.0 #kwh
charge.speed <- 35.21739 #kw "CHAdeMO type. Allows for a 90% charge in approximately 46 minutes."
charge.time <- 15.0 #charge arcs have a ___ minute resolution
gas.price <- 2.20 # AAA tax weekend gas price watch may 3 2018
electricity.price <- 0.107*usd.2017.to.2018 
min.to.round <- 5
emissions <- FALSE
discount.rate<-0.09
lifespan.years <- 12
car.lifespan.vmt <- 170001
battery.lifespan.vmt <- 100000
battery.pct.cost <- 0.25
#Idle numbers based on Ford Focus 2012 (27-31-37 mpg); 2019 Kia Soul is 26-28-31
unmet.demand.penalty <- 100
start.hour.of.day <- 6
default.damage.model="AP3"
default.cr="pope"
default.carbon.price=40.8*usd.2007.to.2018
acs.to.pope.factor = -15.1 + 15.2*1.13 
acs.to.h6c.factor = -15.1 + 15.2*1.14 

default.vsl <- 9.028*1.041864815
maintenance.cv <- 0.065*usd.2017.to.2018
maintenance.hev <- maintenance.cv*0.86#0.785 #*0.904#
maintenance.bev <- maintenance.cv*0.509
cores.available <- 16
default.labor.price <- 0.0
default.bev<-"Soul2018"
bev.density.factor<-1.0
#austin <<- readRDS(paste0("/pylon5/dd5fp4p/mbruchon//Scripts\\Scripts\\austin.rds"))
#daily.crf <- cap.recovery.factor/365

CJ.dt = function(X,Y) {
  stopifnot(is.data.table(X),is.data.table(Y))
  k = NULL
  X = X[, c(k=1, .SD)]
  setkey(X, k)
  Y = Y[, c(k=1, .SD)]
  setkey(Y, NULL)
  X[Y, allow.cartesian=TRUE][, k := NULL][]
} 


#vehicles <- buildVehicles(num.ice,num.hev,num.ev,num.ev,costs.to.include,
#                          damage.model,cr.to.use,carbon.price,vsl)
#vehicle.arcs <- buildVehicleArcs(arc.subset, vehicles,costs.to.include,
#                                 damage.model,cr.to.use,vsl,carbon.price)
#model <- buildModel(arc.subset, vehicle.arcs, vehicles, vehicle.energy.states, "MIP", costs.to.include=costs.to.include,
#                    demandMandate=1) 
#heuristic.list <- rotatingEVHeuristic(arc.subset,num.ev,num.hev,num.ice,costs.to.include,
#                                      damage.model,cr.to.use,carbon.price,vsl)
runModel <- function(num.stations, num.ice, num.hev, num.ev,costs.to.include="private",powertrains="Unspecified",location='Austin',
                     damage.model=default.damage.model,cr.to.use=default.cr,carbon.price=default.carbon.price,
                     grid.damage.multiplier=1.0, 
					 folder=out.path, out.comment="", gas.price.to.use=-1, electricity.price.to.use=-1, v.arcs.to.improve=data.table(),
                     labor.price=default.labor.price,num.iterations=5, lp.only=FALSE,reduce.last.ev=TRUE,
					 need.bound=TRUE, bev.model=default.bev,include.resale=TRUE){
  time.string <- paste0(format(Sys.time(), "%Y%m%d_%H%M%S_"),as.character(as.numeric(format(Sys.time(), "%OS6"))*1000000))
  sink(paste0(data.path,"Out/logs_12142020/r_log_",time.string,".txt"))
  cat(paste0("Time string: ",time.string))
  vsl = default.vsl  
  cat(paste0("Costs to include: ",costs.to.include, ".\n"))
  cat(paste0("Damage model: ",damage.model, ".\n"))
  cat(paste0("CR: ",cr.to.use, ".\n"))
  cat(paste0("Carbon price: ",carbon.price, ".\n"))
  cat(paste0("Include Resale: ",include.resale, ".\n"))
  cat(paste0("VSL: ",vsl, ".\n"))
  cat(paste0("Default VSL: ",default.vsl, ".\n"))
  cat(paste0("Grid damage multiplier: ",grid.damage.multiplier, ".\n"))
  cat(paste0("Location: ",location, ".\n"))
  cat(paste0("Folder: ",out.path, ".\n"))
  cat(paste0("Comment: ",out.comment, ".\n"))
  cat(paste0("Gas Price: ",gas.price.to.use, ".\n"))
  cat(paste0("Electricity Price: ",electricity.price.to.use, ".\n"))
  cat(paste0("Arcs to improve:",ifelse(v.arcs.to.improve[,.N]>0,TRUE,FALSE), ".\n"))
  cat(paste0("powertrains: ",powertrains, ".\n"))
  cat(paste0("Labor price: ",labor.price, ".\n"))
  cat(paste0("Iterations: ",num.iterations, ".\n"))
  cat(paste0("LP Only: ",lp.only, ".\n"))
  cat(paste0("Reduce last EV: ",reduce.last.ev, ".\n"))
  cat(paste0("Need bound: ",need.bound, ".\n"))
  cat(paste0("BEV model: ",bev.model, ".\n"))
  cat(paste0("Include resale: ",include.resale, ".\n"))
  random.suffix <- paste0(sample(1:9, 1),sample(1:9, 1))
  if(powertrains=="Unspecified"){
    powertrains <- 'All'
    if(num.ice==0 & num.hev==0){
      powertrains <- 'BEV Only'
    } else if(num.ev==0 & num.hev==0){
      powertrains <- 'ICE Only'
    } else if(num.ice==0 & num.ev==0){
      powertrains <- 'HEV Only'
    }
  } else if(powertrains=='ICE Only'){
    num.ev <- 0
    num.hev <- 0
  } else if(powertrains=='HEV Only'){
    num.ice <- 0
    num.ev <- 0
  } else if(powertrains=='BEV Only'){ 
    num.ice <- 0
    num.hev <- 0
  } 
  
  cat(paste0("Cars: ",powertrains,", ",num.ice," ICE, ",num.hev," HEV, ",num.ev," BEV.\n"))
  
  if(location=='NYC'){
    gas.price <<- 2.767
    electricity.price <<- 0.2534*usd.2017.to.2018
  } else if(location=='LA'){
    gas.price <<- 3.511
    electricity.price <<- 0.1660*usd.2017.to.2018
  } else if(location=='Austin'){
    gas.price <<- 2.20
    electricity.price <<- 0.107*usd.2017.to.2018
  }
  if(gas.price.to.use>=0){
    gas.price <<- gas.price.to.use
  }
  if(electricity.price.to.use>=0){
	electricity.price <<- electricity.price.to.use
  }
  importDamages(grid.damage.multiplier, damage.model, location)
  dir.create(folder, showWarnings = FALSE)
  #sink(paste0("/pylon5/dd5fp4p/mbruchon//Scripts/Out/logs/r_session_log_",time.string,".txt"))
  buildVeh.out <- buildVehicles(num.ice,num.hev,num.ev,num.ev,costs.to.include,
                                damage.model,cr.to.use,carbon.price,vsl,bev.model,include.resale)
  vehicles <-buildVeh.out$vehicles
  charge.stations <- locateStations(case.subset,num.stations=num.stations)
  base.arc.list <- buildArcList(case.subset,clustered.trips, charge.stations, 
                                num.cars=sum(vehicles$vehicle.count))
  arc.subset <- solveMinCostFlow(base.arc.list,vehicles,randomizeCosts = FALSE,threshold=0,costs.to.include,
                                 damage.model,cr.to.use,carbon.price,vsl,v.arcs.to.improve,labor.price)
  vehicle.arcs <- buildVehicleArcs(arc.subset, vehicles,costs.to.include,
                                   damage.model,cr.to.use,vsl,carbon.price,labor.price)
								   
  vehicle.energy.states <- buildVehicleEnergies(arc.subset,vehicles)
  model <- buildModel(arc.subset, vehicle.arcs, vehicles, vehicle.energy.states, "MIP", costs.to.include=costs.to.include,
                      demandMandate=1,
                      slopes = buildVeh.out$slopes,intercepts =buildVeh.out$intercepts,
                      slopes.external = buildVeh.out$slopes.external, intercepts.external = buildVeh.out$intercepts.external)
  old.model <- model; model <- old.model
  lpsoln <- runGurobi(gurobi_relax(model),type="MIP",heuristic=0,meth=2,cross=0)  
  vehicles$lpbought <- lpsoln$x[(nrow(vehicle.arcs)+1):(nrow(vehicle.arcs)+nrow(vehicles))]
  vehicle.arcs$lptraversed <- lpsoln$x[1:nrow(vehicle.arcs)]
  lp.cvs <- sum(vehicles[vehicle.type=='ICE']$lpbought)
  lp.hevs <- sum(vehicles[vehicle.type=='HEV']$lpbought)
  lp.bevs <- sum(vehicles[vehicle.type=='EV']$lpbought)
	lp.cv.vmts <- sum(vehicle.arcs[vehicle.type=='ICE' & type!='charge',distance*annualized.days*lptraversed])
	lp.hev.vmts <- sum(vehicle.arcs[vehicle.type=='HEV' & type!='charge',distance*annualized.days*lptraversed])
	lp.bev.vmts <- sum(vehicle.arcs[vehicle.type=='EV' & type!='charge',distance*annualized.days*lptraversed])
	vehicles$lpbought <- lpsoln$x[(nrow(vehicle.arcs)+1):(nrow(vehicle.arcs)+nrow(vehicles))]
	cat(paste0("\nInitial Full LP:\n",
	sum(vehicles[vehicle.type=='ICE']$lpbought), " ICE for ",lp.cv.vmts," VMT (",lp.cv.vmts/sum(vehicles[vehicle.type=='ICE']$lpbought)," per car), \n",
	sum(vehicles[vehicle.type=='HEV']$lpbought), " HEV for ",lp.hev.vmts," VMT (",lp.hev.vmts/sum(vehicles[vehicle.type=='HEV']$lpbought)," per car), \n",
	sum(vehicles[vehicle.type=='EV']$lpbought), " BEV for ",lp.bev.vmts," VMT (",lp.bev.vmts/sum(vehicles[vehicle.type=='BEV']$lpbought)," per car), \n",
	sum(vehicles[vehicle.type=='EV' & vehicle.count==1]$lpbought), " Single EV.\n\n "))  
	if(lp.only==TRUE){
    time.string <- substr(out.comment,7,23)
    lp.summary <- data.frame(powertrains=powertrains, num.stations=num.stations, lp.cvs=lp.cvs,lp.hevs=lp.hevs, lp.bevs=lp.bevs,
                             lp.hev.vmts=lp.cv.vmts,lp.hev.vmts=lp.hev.vmts,lp.bev.vmts=lp.bev.vmts,
                             costs.to.include=costs.to.include,damage.model=damage.model,vsl=vsl,cr.to.use=cr.to.use,
                             discount.rate=discount.rate,
                             carbon.price=carbon.price,
                             lp.obj=lpsoln$objval,
                             battery.lifespan=battery.lifespan.vmt,car.lifespan=car.lifespan.vmt,
                             lifespan.years=lifespan.years,
                             capacity.ev=capacity.ev,charge.speed=charge.speed,
                             num.stations=num.stations,
                             grid.damage.multiplier=grid.damage.multiplier,
                             location=location,
                             cleanest.marginal.generator=FALSE,
                             ev.purchase.price=ev.purchase.price,
                             gas.price=gas.price,
                             electricity.price=electricity.price,
                             labor.price=labor.price,
                             time=Sys.time(),
                             time.string=time.string,
                             comment=out.comment)
    saveRDS(lp.summary, file= paste0(folder,time.string,"lp_summary.rds"))
    return(list(lpsoln))
  }
  ok.to.add.evs <- FALSE
  if(nrow(v.arcs.to.improve)==0){
    vehicles$lpbought <- lpsoln$x[(nrow(vehicle.arcs)+1):(nrow(vehicle.arcs)+nrow(vehicles))]   
    if(num.ev==0){
      #RUN THIS WHEN THERE ARE NO BEVs!!
      solution <- runGurobi(model,type="MIP_LightPresolve",heuristic=0.8,meth=3,mip.tol=1e-8, mip.tol.abs=20,time.limit = 60*120)
    } else if(sum(vehicles[vehicle.type=='EV']$lpbought) < 0.5){
      buildVeh.out <- buildVehicles(num.ice,num.hev,0,0,costs.to.include,
                                    damage.model,cr.to.use,carbon.price,vsl,bev.model,include.resale)
      vehicles <-buildVeh.out$vehicles
      vehicle.arcs <- buildVehicleArcs(arc.subset, vehicles,costs.to.include,
                                       damage.model,cr.to.use,vsl,carbon.price,labor.price)
      vehicle.energy.states <- buildVehicleEnergies(arc.subset,vehicles)
      model <- buildModel(arc.subset, vehicle.arcs, vehicles, vehicle.energy.states, "MIP", costs.to.include=costs.to.include, 
                          demandMandate=1,
                          slopes = buildVeh.out$slopes,intercepts =buildVeh.out$intercepts,
                          slopes.external=buildVeh.out$slopes.external, intercepts.external = buildVeh.out$intercepts.external)
      solution <- runGurobi(model,type="MIP",time.limit=360,heuristic=0.8, mip.tol=1e-8, mip.tol.abs=0.001)
    } else{
      heuristic.list <- rotatingEVHeuristic(arc.subset,num.ev = sum(vehicles[vehicle.type=='EV']$lpbought)*1.5,num.hev,num.ice,costs.to.include,
                                            damage.model,cr.to.use,carbon.price,vsl,labor.price)
      vehicle.arcs <- heuristic.list[[3]]
      solution <- heuristic.list[[6]]
      vehicles <- heuristic.list[[1]]
      vehicle.energy.states <- heuristic.list[[4]]
      model <- heuristic.list[[5]]
    }
  } else{
    assigned.v.arcs <- vehicle.arcs[v.arcs.to.improve,
                                    on=.(vehicle.id=vehicle.id,vehicle.type=vehicle.type, node1=node1, node2=node2, path.id=path.id, type=type,
                                         start.time=start.time,end.time=end.time),
                                    assigned:=i.solution]
    new.model <- buildModel(arc.subset, assigned.v.arcs, vehicles, vehicle.energy.states, "MIP", costs.to.include=costs.to.include,
                            demandMandate=1,
                            slopes = buildVeh.out$slopes,intercepts =buildVeh.out$intercepts,
                            slopes.external = buildVeh.out$slopes.external, intercepts.external = buildVeh.out$intercepts.external)
    solution <- runGurobi(new.model,type="MIP",time.limit=1500,heuristic=0.8, mip.tol=1e-5, mip.tol.abs=0.001)
    ok.to.add.evs <- TRUE
  }
  
  if(length(solution$x)>0){
    model$start <- solution$x
    
    vehicle.arcs$probablybought <- solution$x[1:nrow(vehicle.arcs)]
    #	print(paste0("EVs used: ",length(unique(vehicle.arcs[vehicle.type=='EV' & type=='dispatch' & probablybought>=1,vehicle.id]))))
    if(length(unique(vehicle.arcs[vehicle.type=='EV' & type=='dispatch' & probablybought>=1,vehicle.id]))>1 & num.iterations>0){
      solution <- destroyRebuildHeuristic(model,vehicle.arcs,vehicles,vehicle.energy.states,ok.to.add.evs=ok.to.add.evs,iterations=num.iterations,reduce.last.ev=reduce.last.ev)
    }
	
    model$start <- solution$x
    final.mipgap <- -999999
    final.bound <- -999999
    
    if(num.iterations>0 & need.bound==TRUE){
		solution <- runGurobi(model,type="MIP",time.limit=60*60*8,heuristic=0.001, mip.tol=1e-1, mip.tol.abs=100,
							  presolve=2,meth=3,cross=0, mip.focus=3)
		if(length(solution$mipgap) > 0) final.mipgap <-solution$mipgap
		if(length(solution$objbound) > 0) final.bound <- solution$objbound
	}
    purchase.start <- 1 + nrow(vehicle.arcs)
    distance.start <- purchase.start+nrow(vehicles)
    capcost.private.start <- distance.start+nrow(vehicles)
    capcost.external.start <- capcost.private.start+nrow(vehicles)
    charge.start <- capcost.external.start+nrow(vehicles)
    charge.change.start <- charge.start + nrow(vehicle.energy.states);
    
    vehicle.arcs$solution <- solution$x[1:nrow(vehicle.arcs)]
    vehicle.arcs$charge.change.solution <- as.numeric(NA)
    vehicle.arcs[type=='charge' & vehicle.type=='EV']$charge.change.solution <- solution$x[charge.change.start:(charge.change.start+nrow(vehicle.arcs[type=='charge' & vehicle.type=='EV'])-1)]
    vehicles$purchased <- solution$x[purchase.start:(purchase.start+nrow(vehicles)-1)]
    vehicles$distance.traveled <- 100*solution$x[distance.start:(distance.start+nrow(vehicles)-1)]
    vehicles$capcost.private <- solution$x[capcost.private.start:(capcost.private.start+nrow(vehicles)-1)]*100
    vehicles$capcost.external <- solution$x[capcost.external.start:(capcost.external.start+nrow(vehicles)-1)]*100
    #vehicles <- computeVehicleImpacts(vehicles)
    
    vehicle.energy.states$assigned <- solution$x[charge.start:(charge.start+nrow(vehicle.energy.states)-1)]
    
    used.vehicle.arcs <- vehicle.arcs[solution!=0]
    used.vehicle.arcs[type=='charge' & vehicle.type=='EV',':='(result.costs=charge.change.solution*annualized.days*electricity.price,
                                                               result.damages=charge.change.solution*annualized.days*total.damages)]
    used.vehicle.arcs[type!='charge',':='(result.costs=solution*annualized.days*private.cost,
                                          result.damages=solution*annualized.days*total.damages)]
    #vehicle.energy.states$assigned <- solution$x[(nrow(vehicle.arcs)+1):(nrow(vehicle.arcs)+nrow(vehicle.energy.states))]
    summary.results <- used.vehicle.arcs[,.(
      bought=sum(solution*(type=='dispatch')),
      trips=sum(1.0*solution*annualized.days*ifelse(type=='trip',1.0,0)),
      trips.per.car=sum(solution*annualized.days*ifelse(type=='trip',1.0,0))/sum(solution*ifelse(type=='dispatch',1,0)),
      vmt=sum(solution*annualized.days*distance),
      vmt.per.car=sum(solution*annualized.days*distance)/sum(solution*ifelse(type=='dispatch',1,0)),
      dispatch.private.costs=sum(result.costs,na.rm=TRUE),
      dispatch.private.costs.per.car=sum(result.costs,na.rm=TRUE)/sum(solution*ifelse(type=='dispatch',1,0)),
      dispatch.damages=sum(result.damages,na.rm=TRUE),
      dispatch.damages.per.car=sum(result.damages)/sum(solution*ifelse(type=='dispatch',1,0)),
      trip.miles=sum(ifelse(type=='trip',annualized.days*distance,0)),
      empty.miles=sum(ifelse(type!='trip',annualized.days*distance,0)),
      pct.occupied.vmt=sum(ifelse(type=='trip',distance,0))/sum(annualized.days*distance,na.rm=TRUE),
      highway.factor=weighted.mean(highway.factor,annualized.days*distance)),
      by=.(vehicle.id,vehicle.type)]
    summary.results[vehicles,on=.(vehicle.id,vehicle.type),':='(
      purchase.private.costs=i.capcost.private,
      purchase.damages=i.capcost.external)]
    print(paste0("Private=",round(sum(summary.results$purchase.private.costs)+sum(summary.results$dispatch.private.costs),1),
                 " External=",round(sum(summary.results$purchase.damages)+sum(summary.results$dispatch.damages),1),
                 " Total=",round(sum(summary.results$purchase.private.costs)+sum(summary.results$dispatch.private.costs)+
                                   sum(summary.results$purchase.damages)+sum(summary.results$dispatch.damages),1)))
    
    vehicle.energy.summary <- vehicle.energy.states[!is.na(time)]
    vehicle.energy.summary <- vehicle.energy.summary[order(vehicle.id,time)]
    vehicle.energy.summary <- vehicle.energy.summary[, list(energy.level=max(assigned,na.rm=TRUE)),by=c("vehicle.id","time")]
    vehicle.energy.summary<- vehicle.energy.summary[, energy.level.lag := shift(energy.level, 1L, fill = NA, type = "lag")]
    vehicle.energy.summary<- vehicle.energy.summary[, energy.level.lead := shift(energy.level, 1L, fill = NA, type = "lead")]
    vehicle.energy.summary<- vehicle.energy.summary[, vehicle.id.lag := shift(vehicle.id, 1L, fill = NA, type = "lag")]
    vehicle.energy.summary$diff1 <- (abs(vehicle.energy.summary$energy.level-vehicle.energy.summary$energy.level.lag))
    vehicle.energy.summary$diff2 <- (abs(vehicle.energy.summary$energy.level-vehicle.energy.summary$energy.level.lead))
    vehicle.energy.summary <- vehicle.energy.summary[(diff1>1e-5) | is.na(diff1) | (diff2>1e-5) | is.na(diff2) | (vehicle.id !=vehicle.id.lag)]
    
    summary.vehicle.type.hour <- used.vehicle.arcs[, list(
      time=min(align.time(start.time,60*60)),
      distance=sum(distance),
      vehicles=length(unique(vehicle.id)),
      dispatches=sum(ifelse(type=='dispatch',1,0)),
      returns=sum(ifelse(type=='return',1,0)),
      trips=sum(ifelse(type=='trip',1,0)),
      trip.miles=sum(ifelse(type=='trip',distance,0)),
      empty.miles=sum(ifelse(type!='trip',distance,0)),
      deadhead.ratio=sum(ifelse(type=='trip',distance,0))/sum(distance,na.rm=TRUE),
      total.cost=sum(cost),
      highway.factor=weighted.mean(highway.factor,distance)
    ),by=.(vehicle.type,index.start)] #
    summary.vehicle.type.hour[deadhead.ratio==1]$deadhead.ratio <- as.numeric(NA)
    cat(paste0(folder,time.string,"vehicle_arcs_used.rds"))
    saveRDS(used.vehicle.arcs, file= paste0(folder,time.string,"vehicle_arcs_used.rds"))
    saveRDS(vehicles, file= paste0(folder,time.string,"vehicles.rds"))
    saveRDS(buildVeh.out$slopes, file = paste0(folder,time.string,"vehicle_slopes.rds"))
    saveRDS(buildVeh.out$intercepts, file = paste0(folder,time.string,"vehicle_intercepts.rds"))
    saveRDS(buildVeh.out$slopes.external, file = paste0(folder,time.string,"vehicle_slopes_external.rds"))
    saveRDS(buildVeh.out$intercepts.external, file = paste0(folder,time.string,"vehicle_intercepts_external.rds"))
    saveRDS(vehicle.energy.summary, file= paste0(folder,time.string,"vehicle_energy_summary.rds"))
    saveRDS(summary.results, file= paste0(folder,time.string,"summary_results.rds"))
    saveRDS(summary.vehicle.type.hour, file= paste0(folder,time.string,"summary_type_hour.rds"))
    objval = ifelse(length(solution$objval)>0,solution$objval,NA)
    objbound = ifelse(length(lpsoln$objval)>0,lpsoln$objval,NA)
    mipgap = ifelse(length(solution$mipgap)>0,(solution$objval-lpsoln$objval)/solution$objval,NA)
  }
  num.ice <- sum(vehicles[vehicle.type=='ICE']$purchased)
  num.ev  <- sum(vehicles[vehicle.type=='EV']$purchased)
  num.hev <- sum(vehicles[vehicle.type=='HEV']$purchased)
  run.summary <- data.frame(powertrains=powertrains, num.stations=num.stations, num.ice=num.ice,num.hev=num.hev, num.ev=num.ev,
                            costs.to.include=costs.to.include,damage.model=damage.model,vsl=vsl,cr.to.use=cr.to.use,
                            discount.rate=discount.rate,
                            carbon.price=carbon.price, obj=objval,bound=objbound,gap=mipgap,
                            final.bound=final.bound,final.mipgap=final.mipgap,
                            battery.lifespan=battery.lifespan.vmt,car.lifespan=car.lifespan.vmt,
                            lifespan.years=lifespan.years,
                            capacity.ev=capacity.ev,charge.speed=charge.speed,
                            num.stations=num.stations,
                            grid.damage.multiplier=grid.damage.multiplier,
                            location=location,
                            cleanest.marginal.generator=FALSE,
                            ev.purchase.price=ev.purchase.price,
                            gas.price=gas.price,
                            electricity.price=electricity.price,
                            labor.price=labor.price,
                            time=Sys.time(),
                            comment=out.comment,
							bev.model=bev.model,
							include.resale=include.resale,
							time.string=time.string)
  saveRDS(run.summary, file= paste0(folder,time.string,"run_summary.rds"))
  return(run.summary)
}


rotatingEVHeuristic <- function(arcs,num.ev,num.hev,num.ice,costs.to.include="private",
                                damage.model=default.damage.model,cr.to.use=default.cr,carbon.price=default.carbon.price,vsl=default.vsl,
									labor.price=default.labor.price,bev.model=default.bev,include.resale=TRUE){
  relevant.arcs <- arcs
  all.used.ev.arcs <- data.table()
  buildVeh.out <- buildVehicles(num.ice,num.hev,total.ev=num.ev,disaggregated.ev=0,costs.to.include,
                                damage.model,cr.to.use,carbon.price,vsl,bev.model,include.resale)
  veh <-buildVeh.out$vehicles
  v.arcs <- buildVehicleArcs(relevant.arcs, veh,costs.to.include,
                             damage.model,cr.to.use,vsl,carbon.price,labor.price)
  veh.energy.states <- buildVehicleEnergies(relevant.arcs,veh)
  model <- buildModel(relevant.arcs, v.arcs, veh, veh.energy.states, "MIP",costs.to.include=costs.to.include,
                      demandMandate=1,
                      slopes = buildVeh.out$slopes,intercepts =buildVeh.out$intercepts,
                      slopes.external = buildVeh.out$slopes.external, intercepts.external = buildVeh.out$intercepts.external)
  solution <- runGurobi(model,type="MIP",time.limit=1080,heuristic=0.8, meth=3, mip.tol=1e-4, mip.tol.abs=0.05)
  veh$distances <- solution[(nrow(v.arcs)+nrow(veh)+1):(nrow(v.arcs)+2*nrow(veh))]
  v.arcs$assigned <- solution$x[1:nrow(v.arcs)]
  veh$bought <- solution$x[(nrow(v.arcs)+1):(nrow(v.arcs)+nrow(veh))]
  single.evs <- veh[vehicle.type=='EV' & vehicle.count==1,vehicle.id]
  big.evs <- veh[vehicle.type=='EV' & vehicle.count>1,vehicle.id]
  cat(paste("\nHeuristic Initial MIP:\n",
            sum(veh[vehicle.type=='ICE']$bought), " ICE (",sum(v.arcs[vehicle.type=='ICE',distance*annualized.days*assigned])," miles, ",sum(v.arcs[vehicle.type=='ICE',distance*annualized.days*assigned])/sum(veh[vehicle.type=='ICE']$bought)," per car),\n",
            sum(veh[vehicle.type=='HEV']$bought), " HEV (",sum(v.arcs[vehicle.type=='HEV',distance*annualized.days*assigned])," miles, ",sum(v.arcs[vehicle.type=='HEV',distance*annualized.days*assigned])/sum(veh[vehicle.type=='HEV']$bought)," per car),\n",
            sum(veh[vehicle.type=='EV' & vehicle.count==1]$bought), " Single EV (",sum(v.arcs[vehicle.id %in% single.evs,distance*annualized.days*assigned])," miles, ",sum(v.arcs[vehicle.id %in% single.evs,distance*annualized.days*assigned])/sum(veh[vehicle.type=='EV' & vehicle.count==1]$bought)," per car),\n",
            sum(veh[vehicle.type=='EV' & vehicle.count>1]$bought), " Grouped EV (",sum(v.arcs[vehicle.id %in% big.evs,distance*annualized.days*assigned])," miles, ",sum(v.arcs[vehicle.id %in% big.evs,distance*annualized.days*assigned])/sum(veh[vehicle.type=='EV' & vehicle.count>1]$bought)," per car).\n\n",sep=""))
  if(ceiling(sum(veh[vehicle.type=='EV',bought]))==0){
    num.ev = 0
  } else{
	#Add more than enough extra padding to the maximum allowed number of EVs
    num.ev <- min(num.ev,max(10,as.integer(min(ceiling(sum(veh[vehicle.type=='EV',bought])*1.3),sum(veh[vehicle.type=='EV',bought])+10))))
  }
  num.ev.remaining=num.ev
  i=1
  done<-FALSE
  all.used.ev.arcs <- data.table()
  adjustment=0
  unfinished.counter=0
  used.by.next.ev <- data.table()
  first.ev.id <- as.integer(num.ice>0)+as.integer(num.hev>0)+1
  start <- as.numeric(NA)
  this.iteration.used.trips <- data.table()
  while(done==FALSE){
    old.adjustment<-adjustment
    if(num.ev==0){
      num.disaggregated.ev <- 0
    } else{
      num.disaggregated.ev <- 1 #min(num.ev.remaining+adjustment,2)
    }
    buildVeh.out <- buildVehicles(num.ice,num.hev,total.ev=num.ev.remaining+adjustment,disaggregated.ev=num.disaggregated.ev,costs.to.include,
                                  damage.model,cr.to.use,carbon.price,vsl,bev.model,include.resale)
    veh <-buildVeh.out$vehicles
   
    to.opt.vehicle.id <- veh[vehicle.type=="EV" & vehicle.count==1,vehicle.id]
    if(i>1 & unfinished.counter==0 & nrow(this.iteration.used.trips)>0){
      relevant.arcs$supply <- 0
      relevant.arcs <- relevant.arcs[this.iteration.used.trips,  on=.(node1=node1,node2=node2,path.id=path.id), supply:=i.supply]
      this.iteration.used.trips <- data.table()
      relevant.arcs[,supply:=pmax(supply,0,na.rm=TRUE)]
      current.trip.count <- sum(relevant.arcs[type=='trip']$capacity)
      relevant.arcs <- relevant.arcs[,new.capacity:=as.integer(ifelse(capacity-supply>0,capacity-supply,0))]
      relevant.arcs[type=='trip']$capacity <- relevant.arcs[type=='trip']$new.capacity
      relevant.arcs <- relevant.arcs[capacity>0]
      new.trip.count <- sum(relevant.arcs[type=='trip']$capacity)

      arcs.are.left<-TRUE
      before.arc.count <- relevant.arcs[,.N]
      while(arcs.are.left){
        current.arc.count <- relevant.arcs[,.N]
        node.incoming.arcs <- relevant.arcs[,.(incoming.arcs=.N),by=node2]
        node.outgoing.arcs <- relevant.arcs[,.(outgoing.arcs=.N),by=node1]
        all.nodes <- data.table(node=unique(c(node.incoming.arcs$node2,node.outgoing.arcs$node1)))
        all.nodes$incoming.arcs <- as.integer(0); all.nodes$outgoing.arcs <- as.integer(0)
        all.nodes[node.incoming.arcs,on=.(node=node2),incoming.arcs:=i.incoming.arcs]
        all.nodes[node.outgoing.arcs,on=.(node=node1),outgoing.arcs:=i.outgoing.arcs]
        nodes.no.incoming <- all.nodes[incoming.arcs==0]$node
        nodes.no.outgoing <- all.nodes[outgoing.arcs==0]$node
        new.arcs <- relevant.arcs[((!(node1 %in% nodes.no.incoming)) & (!(node2 %in% nodes.no.outgoing))) |
                                    ((!(node2 %in% nodes.no.outgoing)) & type=='dispatch') |
                                    ((!(node1 %in% nodes.no.incoming)) & type=='return')]

        relevant.arcs <- new.arcs
        after.arc.count <- relevant.arcs[,.N]
        if(after.arc.count==current.arc.count){
          arcs.are.left<-FALSE
          cat(paste("\nIteration ", i, ": ",before.arc.count-after.arc.count,"orphaned arcs removed.\n"))
        }
      }
      relevant.arcs$new.capacity <- NULL; relevant.arcs$supply <- NULL
    }
    v.arcs <- buildVehicleArcs(relevant.arcs, veh,costs.to.include,
                               damage.model,cr.to.use,vsl,carbon.price,labor.price)
    v.arcs$start <- as.numeric(NA)
    veh.energy.states <- buildVehicleEnergies(relevant.arcs,veh)
    
    purchase.start <- 1 + nrow(v.arcs)
    distance.start <- purchase.start+nrow(veh)
    capcost.private.start <- distance.start+nrow(veh)
    capcost.external.start <- distance.start+nrow(veh)
    charge.start <- capcost.external.start+nrow(veh)
    
    model <- buildModel(relevant.arcs, v.arcs, veh, veh.energy.states, "MIP",costs.to.include=costs.to.include,
                        demandMandate=1,
                        slopes = buildVeh.out$slopes,intercepts =buildVeh.out$intercepts,
                        slopes.external = buildVeh.out$slopes.external,intercepts.external =buildVeh.out$intercepts.external)
    if(num.ice==0 & num.hev==0){
      model$vtype[which(v.arcs$vehicle.type=='EV' & v.arcs$vehicle.count>1)] <- 'C'
    }
    startobjval=0
    if(num.ice>0 | num.hev>0){
      grb.method <- 3
      timez <- 1800
    } else{
      grb.method <- 3
      timez <- 1800
    }
    
    time.allowed=ifelse(unfinished.counter==0,timez,time.allowed+240)
    if(unfinished.counter==0 & num.ice==0 & num.hev==0){
      solution <- gurobi(model,params=list(LogFile=paste0(out.path,"GurobiLogs/",format(Sys.time(), "%Y%m%d_%H%M%S_"), "log.txt"),
                                           LogToConsole=1,TimeLimit=1800,Threads=cores.available,MIPGap=0.001,MIPGapAbs=10, ScaleFlag=2,Presolve=1,Heuristics=0.4,Method=3))
    } else if(attempts.counter > 0){
	  if(length(start)==length(model$ub)){
        model$start <- start
      }
      solution <- gurobi(model,params=list(LogFile=paste0(out.path,"GurobiLogs/",format(Sys.time(), "%Y%m%d_%H%M%S_"), "log.txt"),
                                           LogToConsole=1,TimeLimit=120,Threads=cores.available,NumericFocus=3,
                                           MIPFocus=1,MIPGap=0.001, MIPGapAbs=10, IntFeasTol=1e-8,FeasibilityTol=1e-8,
                                           Cuts=0,Method=3,ScaleFlag=2,Presolve=0,Aggregate=0,AggFill=0))
    } else{
      if(length(start)==length(model$ub)){
        model$start <- start
      }
      solution <- gurobi(model,params=list(LogFile=paste0(out.path,"GurobiLogs/",format(Sys.time(), "%Y%m%d_%H%M%S_"), "log.txt"),
                                           LogToConsole=1,TimeLimit=time.allowed,Threads=cores.available,MIPGap=0.001, MIPGapAbs=10,Method=3))
    }
    
    if(solution$status=='INFEASIBLE'){
      adjustment=adjustment+1
      old.adjustment=adjustment
      unfinished.counter=0
      attempts.counter=0
      cat(paste("\nIteration ", i, ", unfinished attempts: ",unfinished.counter,": INFEASIBLE. ,adjustment = ",adjustment,".\n\n",sep=""))
      next
    } else if(length(solution$x)==0 | length(solution$objval)==0){
      unfinished.counter=unfinished.counter+1
      old.adjustment=adjustment
      adjustment=adjustment+1
      cat(paste("\nIteration ", i, ", unfinished attempts: ",unfinished.counter,": No solution found. adjustment = ",adjustment,".\n\n",sep=""))
      next
    } else if(solution$status=='TIME_LIMIT' & (round(solution$objval)==round(startobjval)) &
              (sum(solution$x[which(v.arcs$type=='dispatch' & v.arcs$vehicle.type=='EV')])==0 & unfinished.counter<2)){
      unfinished.counter=unfinished.counter+1
      cat(paste("\nIteration ", i, ", unfinished attempts: ",unfinished.counter,": Time limit hit, no EVs used.\n\n",sep=""))
      next
    } else if((abs(solution$objbound-solution$objval)/solution$objval)>0.03 & unfinished.counter<1){
      unfinished.counter=unfinished.counter+1
      start <- solution$x
      cat(paste("\nIteration ", i, ", unfinished attempts: ",unfinished.counter,": Solution not good enough yet.\n\n",sep=""))
      next
    }
    
    veh$bought <- solution$x[(nrow(v.arcs)+1):(nrow(v.arcs)+nrow(veh))]
    num.ev.remaining <- max(sum(veh[vehicle.type=='EV',bought])-1,2)
    
    veh.energy.states$energy.level <- solution$x[charge.start:(charge.start+nrow(veh.energy.states)-1)]
    v.arcs$solution <- round(solution$x[1:nrow(v.arcs)])
    
    v.arcs[,.(
      bought=sum(solution*(type=='dispatch')),
      trips=sum(1.0*solution*annualized.days*ifelse(type=='trip',1.0,0)),
      trips.per.car=sum(solution*annualized.days*ifelse(type=='trip',1.0,0))/sum(solution*ifelse(type=='dispatch',1,0)),
      vmt=sum(solution*annualized.days*distance),
      vmt.per.car=sum(solution*annualized.days*distance)/sum(solution*ifelse(type=='dispatch',1,0))),
      by=.(vehicle.type,vehicle.count)]
	  single.evs <- veh[vehicle.type=='EV' & vehicle.count==1,vehicle.id]
	  big.evs <- veh[vehicle.type=='EV' & vehicle.count>1,vehicle.id]
    cat(paste("\nIteration ", i, ": ",
		sum(veh[vehicle.type=='ICE']$bought), " ICE (",sum(v.arcs[vehicle.type=='ICE',distance*annualized.days*assigned])," miles, ",sum(v.arcs[vehicle.type=='ICE',distance*annualized.days*assigned])/sum(veh[vehicle.type=='ICE']$bought)," per car),\n",
		sum(veh[vehicle.type=='HEV']$bought), " HEV (",sum(v.arcs[vehicle.type=='HEV',distance*annualized.days*assigned])," miles, ",sum(v.arcs[vehicle.type=='HEV',distance*annualized.days*assigned])/sum(veh[vehicle.type=='HEV']$bought)," per car),\n",
		sum(veh[vehicle.type=='EV' & vehicle.count==1]$bought), " Single EV (",sum(v.arcs[vehicle.id %in% single.evs,distance*annualized.days*assigned])," miles, ",sum(v.arcs[vehicle.id %in% single.evs,distance*annualized.days*assigned])/sum(veh[vehicle.type=='EV' & vehicle.count==1]$bought)," per car),\n",
		sum(veh[vehicle.type=='EV' & vehicle.count>1]$bought), " Grouped EV (",sum(v.arcs[vehicle.id %in% big.evs,distance*annualized.days*assigned])," miles, ",sum(v.arcs[vehicle.id %in% big.evs,distance*annualized.days*assigned])/sum(veh[vehicle.type=='EV' & vehicle.count>1]$bought)," per car).\n\n",sep=""))
    cat(paste("\nIteration ", i, ": ",
              sum(veh[vehicle.type=='ICE']$bought), " ICE, ",
              sum(veh[vehicle.type=='HEV']$bought), " HEV, ",
              sum(veh[vehicle.type=='EV' & vehicle.count==1]$bought), " Single EV, ",
              sum(veh[vehicle.type=='EV' & vehicle.count>1]$bought), " Grouped EV.\n\n",sep=""))
    
    if(veh[vehicle.type=='EV' & !(vehicle.id %in% to.opt.vehicle.id) & bought>0,.N]==0) {
      done<-TRUE
    }
    
    used.by.current.ev <- v.arcs[solution>0.999 & vehicle.id %in% to.opt.vehicle.id] #ID of disaggregated vehicle
    used.by.other.evs <- v.arcs[solution>0.999 & vehicle.type=='EV' & !(vehicle.id %in% to.opt.vehicle.id)] #ID of disaggregated vehicle
    used.trips.this.round <- sum(used.by.current.ev[type=='trip']$solution)
    used.trips.this.round.by.others <- sum(used.by.other.evs[type=='trip']$solution)
    if(nrow(all.used.ev.arcs) > 0){
      start.ev.id <- max(all.used.ev.arcs$vehicle.id)+1
      previously.handled.trips <- sum(all.used.ev.arcs[type=='trip']$solution)
    } else{
      start.ev.id <- first.ev.id
      previously.handled.trips <- 0
    }
    used.by.current.ev$vehicle.id <- frank(used.by.current.ev$vehicle.id,ties.method="dense")+start.ev.id-1
    all.used.ev.arcs <- rbindlist(list(
      all.used.ev.arcs, 
      used.by.current.ev),fill=TRUE,use.names=TRUE)
    this.iteration.used.trips <- used.by.current.ev[type=='trip',.(supply=sum(solution,na.rm=TRUE)),by=.(node1,node2,path.id)]
    cat(paste0("\nIteration ", i, ": ",
               used.trips.this.round, " trips by current EV with ID ", unique(used.by.current.ev$vehicle.id)," and ",
               used.trips.this.round.by.others, " trips by aggregated EV. ",
               previously.handled.trips, " prior to this iteration, summing to ",used.trips.this.round+used.trips.this.round.by.others+previously.handled.trips," total trips.\n\n",sep=""))
    i<-i+1
    unfinished.counter=0
    start <- as.numeric(NA)
    maxconstraintcount=0
    adjustment=old.adjustment
  }
  
  num.ev.used <- length(unique(all.used.ev.arcs[,vehicle.id]))
  
  buildVeh.out <- buildVehicles(num.ice,num.hev,total.ev=num.ev.used,disaggregated.ev=num.ev.used,costs.to.include,
                                damage.model,cr.to.use,carbon.price,vsl,bev.model,include.resale)
  veh <-buildVeh.out$vehicles
  v.arcs <- buildVehicleArcs(arcs, veh,costs.to.include,
                             damage.model,cr.to.use,vsl,carbon.price,labor.price)
  v.arcs[all.used.ev.arcs,on=.(node1=node1,node2=node2,path.id=path.id,vehicle.id=vehicle.id),
         assigned := solution]
  # v.arcs[assigned%%1!=0, assigned := as.numeric(NA)]
  veh.energy.states <- buildVehicleEnergies(arcs,veh)
  now.string <- format(Sys.time(), "%Y%m%d_%H%M%S_")
  #  save.image(paste0("/pylon5/dd5fp4p/mbruchon//Scripts/Out/",now.string,"out.rdata"))
  
  model <- buildModel(arcs, v.arcs, veh, veh.energy.states, "MIP", costs.to.include=costs.to.include,
                      demandMandate=1,ev.restriction=FALSE, 
                      slopes = buildVeh.out$slopes,intercepts =buildVeh.out$intercepts,
                      slopes.external = buildVeh.out$slopes.external,intercepts.external =buildVeh.out$intercepts.external)
  v.arcs.2 <- v.arcs; v.arcs.2$assigned <- as.numeric(NA)
  model.to.return <- buildModel(arcs, v.arcs.2, veh, veh.energy.states, "MIP", costs.to.include=costs.to.include,
                                demandMandate=1,ev.restriction=FALSE, 
                                slopes = buildVeh.out$slopes,intercepts =buildVeh.out$intercepts,
                                slopes.external = buildVeh.out$slopes.external,intercepts.external =buildVeh.out$intercepts.external)
  
  model$vtype[which(v.arcs$vehicle.id %in% all.used.ev.arcs$vehicle.id)] <- 'C'
  
  solution <- runGurobi(model,type="MIP",time.limit=1800,heuristic=0.8, mip.tol=1e-7, mip.tol.abs=5,int.feas.tol=1e-5,feas.tol=1e-5)
  if(!(solution$status %in% c("OPTIMAL","TIME_LIMIT"))){
    now.string <- format(Sys.time(), "%Y%m%d_%H%M%S_")
    save.image(paste0(out.path,now.string,"out.rdata"))
  }
  
  now.string <- format(Sys.time(), "%Y%m%d_%H%M%S_")
  return(list(veh,arcs,v.arcs,veh.energy.states,model.to.return,solution))
}

destroyRebuildHeuristic <- function(model,vehicle.arcs,vehicles,vehicle.energy.states,iterations=2,ok.to.add.evs=FALSE,
	reduce.last.ev=TRUE,systematic=TRUE){
	vehicle.tracker <- vehicles[,.(vehicle.id,vehicle.type,optimizations=0)]
  vehicle.arcs$temp <- model$start[1:nrow(vehicle.arcs)]
  purchase.start <- 1 + nrow(vehicle.arcs)
  distance.start <- purchase.start+nrow(vehicles)
  capcost.private.start <- distance.start+nrow(vehicles)
  capcost.external.start <- capcost.private.start+nrow(vehicles)
  charge.start <- capcost.external.start+nrow(vehicles)
  charge.change.start <- charge.start + nrow(vehicle.energy.states);
  old.model <- model
  if(length(model$start)>0){
    num.to.optimize=6
    current.objective <- 99999999
    previous.objective <- 99999999
    for(k in 1:iterations){
      vmt <- model$start[(distance.start):(capcost.private.start-1)]
      ev.vmt <- vmt[which(vehicles$vehicle.type=='EV')]
      ev.vmt <- ev.vmt[which(ev.vmt>0)]
      threshold <- max(head(sort(ev.vmt),3))
      lowest <- max(head(sort(ev.vmt),1))
      upper.evs <- which(vmt > 0 & vmt<=threshold & vehicles$vehicle.type=='EV') #Low mileage
      lowest.ev <- which(vmt > 0 & vmt<=lowest+1 & vehicles$vehicle.type=='EV') #Lowest mileage
      distances <- model$start[distance.start-1 + which(vehicles$vehicle.type=='EV')]
      if(ok.to.add.evs==FALSE & reduce.last.ev){
        ev.ids <- unique(vehicle.arcs[vehicle.type=='EV' & temp > 0 & !(vehicle.id %in% upper.evs),vehicle.id])
      } else if(reduce.last.ev){
        ev.ids <- unique(vehicle.arcs[vehicle.type=='EV' & !(vehicle.id %in% upper.evs,vehicle.id)])
      } else{
		ev.ids <- unique(vehicle.arcs[vehicle.type=='EV' & temp > 0,vehicle.id])
	  }
      if(length(ev.ids)>num.to.optimize){
		if(systematic==TRUE){
			fewest.optimizations <- min(vehicle.tracker$optimizations)
			to.consider <- vehicle.tracker[vehicle.id %in% ev.ids & optimizations==fewest.optimizations]$vehicle.id
			shortage <- num.to.optimize - length(to.consider)
			print(paste0(fewest.optimizations))
			print(fewest.optimizations)
			print(paste0("test2"),fewest.optimizations)
			print(paste0(to.consider))
			print(vehicle.tracker)
			if(shortage>0){ 
				ev.ids <- c(to.consider,
							sample(
							vehicle.tracker[vehicle.id %in% ev.ids & (!(vehicle.id %in% to.consider)) & optimizations<=fewest.optimizations+1]$vehicle.id,shortage
							)
							)
			} else if(shortage<0){
				ev.ids <- sample(to.consider, num.to.optimize)
			} else if(shortage==0){
				ev.ids <- to.consider
			}
		} else{
			ev.ids <- sample(ev.ids,num.to.optimize)
		}
      }
      upper.ev.indices <- distance.start -1 + which(vehicles$vehicle.id %in% upper.evs)
      lowest.ev.index <- distance.start -1 + which(vehicles$vehicle.id %in% lowest.ev)
      non.ev.ids <- unique(vehicle.arcs[vehicle.type!='EV']$vehicle.id)
	  if(reduce.last.ev){
		ids <- unique(c(non.ev.ids,upper.evs,ev.ids))
	  } else{
		ids <- unique(c(non.ev.ids,ev.ids))
	  }
	  counts=data.table(v.id=ids)
	  vehicle.tracker[counts,':='(optimizations=optimizations+1),on=.(vehicle.id=v.id)]
      # CHANGE BACK ids <- unique(c(non.ev.ids,ev.ids))
      to.hold <- c(which(!(vehicle.arcs$vehicle.id %in% ids)),
                   purchase.start-1+which(!(vehicles$vehicle.id %in% ids)),
                   distance.start-1+which(!(vehicles$vehicle.id %in% ids)),
                   capcost.private.start-1+which(!(vehicles$vehicle.id %in% ids)),
                   capcost.external.start-1+which(!(vehicles$vehicle.id %in% ids)),
                   charge.start-1+which(!(vehicle.energy.states$vehicle.id %in% ids)),
                   charge.change.start-1+which(!vehicle.arcs[vehicle.type=='EV' & type=='charge']$vehicle.id %in% ids)
      )
      to.optimize <- c(which((vehicle.arcs$vehicle.id %in% ids)),
                       purchase.start-1+which((vehicles$vehicle.id %in% ids)),
                       distance.start-1+which((vehicles$vehicle.id %in% ids)),
                       capcost.private.start-1+which((vehicles$vehicle.id %in% ids)),
                       capcost.external.start-1+which((vehicles$vehicle.id %in% ids)),
                       charge.start-1+which((vehicle.energy.states$vehicle.id %in% ids)),
                       charge.change.start-1+which(vehicle.arcs[vehicle.type=='EV' & type=='charge']$vehicle.id %in% ids)
      )
      if(length(to.hold)>0){
        model$lb[to.hold] <- model$start[to.hold]; model$ub[to.hold]<- model$start[to.hold]
      }
      model$lb[to.optimize] <- old.model$lb[to.optimize]; model$ub[to.optimize] <- old.model$ub[to.optimize]
	  if(reduce.last.ev==TRUE){
		model$ub[lowest.ev.index] <- model$start[lowest.ev.index]
	  }
      solution <- runGurobi(model,type="MIP",time.limit=360,heuristic=0.5, mip.tol=0.00025, mip.tol.abs=40,presolve=1,cross=0) 
      if(length(solution$x)==0){ 
        break
      }
      previous.objective <- current.objective
      current.objective <- solution$objval
      if(previous.objective-current.objective > 100){
        
      }
      model$start <- solution$x
      print(paste("\nIteration ",k))
	  print(vehicle.tracker[vehicle.type=='EV'])
    }
  }
  return(solution)
}
 
fixupSolution <- function(filestring,num.iterations=2,
	in.folder='test_cases', 	out.folder='test_cases',
	reuse.assignments=TRUE,lp.only=FALSE, objective.to.use='',damage.model.to.use='', location.to.use='',
	carbon.price.to.use=-99,grid.damage.to.use=99,reduce.last.ev=FALSE,need.bound=TRUE, reset.gas.electric.prices = FALSE){
  used.v.arcs <- readRDS(paste0(in.folder,'/',filestring,'vehicle_arcs_used.rds'))
  rs <- readRDS(paste0(in.folder,'/',filestring,'run_summary.rds'))
  print("Before fixup: ")
  print(table(used.v.arcs$vehicle.id))
  if(reuse.assignments){
    if(rs$powertrains=='BEV Only'){
      used.v.arcs[,vehicle.id:=frank(vehicle.id,ties.method="dense")]
    } else if(rs$powertrains=='All'){
      used.v.arcs[,new.rank:=frank(vehicle.id,ties.method="dense")]
      offset <- 3-min(used.v.arcs[vehicle.type=='EV',]$new.rank,na.rm=TRUE)
      used.v.arcs[vehicle.type=='EV',]$vehicle.id <- used.v.arcs[vehicle.type=='EV',]$new.rank + offset
    }
    num.of.cv <- rs[1,]$num.ice
    num.of.hev <- rs[1,]$num.hev
    num.of.bev <- length(unique(used.v.arcs[vehicle.type=='EV',]$vehicle.id))
    print("After fixup: ")
    print(table(used.v.arcs$vehicle.id))
  } else{
    if(rs[1,]$powertrains=='All'){
      num.of.cv <- 50
      num.of.hev <- 50
      num.of.bev <- 50
    } else if(rs[1,]$powertrains=='ICE Only'){
      num.of.cv <- 50
      num.of.hev <- 0
      num.of.bev <- 0
    } else if(rs[1,]$powertrains=='HEV Only'){
      num.of.cv <- 0
      num.of.hev <- 50
      num.of.bev <- 0
    } else if(rs[1,]$powertrains=='BEV Only'){
      num.of.cv <- 0
      num.of.hev <- 0
      num.of.bev <- 50
    } 
  }
  ev.purchase.price <<- rs[1,]$ev.purchase.price
  gas.price <<- rs[1,]$gas.price
  electricity.price <<- rs[1,]$electricity.price
  discount.rate <<- rs[1,]$discount.rate
  if(is.null(rs[1,]$labor.price)){
    rs$labor.price <- 0
  }
   if(is.null(rs[1,]$include.resale | is.na(rs[1,]$include.resale))){
    rs$include.resale <- TRUE
  }
  if(objective.to.use==''){
    the.cti<-rs[1,]$costs.to.include
  } else{
    the.cti<-objective.to.use
  }
  if(damage.model.to.use==''){
    dm.to.use<- rs[1,]$damage.model
  } else{
    dm.to.use<-damage.model.to.use
  }
  if(carbon.price.to.use<0){
    cp.to.use <- rs[1,]$carbon.price
  } else{
    cp.to.use <- carbon.price.to.use
  }
  if(grid.damage.to.use==99){
    gd.to.use <- rs[1,]$grid.damage.multiplier
  } else{
    gd.to.use <- grid.damage.to.use
  }
  if(location.to.use==''){
    loc.to.use <- rs[1,]$location
  } else{
    loc.to.use <- location.to.use
  }
  
  
  cr.to.use <- rs[1,]$cr.to.use
  
  if(dm.to.use=='AP2'){
	dm.to.use <- 'AP3'
	cp.to.use <- default.carbon.price
	if(cr.to.use=='acs'){
		cr.to.use <- 'pope'
	}
  }
	if("bev.model" %in% colnames(rs)){
		bev.model <- rs$bev.model
	} else{
		bev.model <- default.bev
	}
	bev.model <- as.character(bev.model)
	rs$out.comment <- rs$comment
	if(reset.gas.electric.prices==TRUE){
		gp.to.use <- -1
		ep.to.use <- -1
	} else{
		gp.to.use <- rs[1,]$gas.price
		ep.to.use <- rs[1,]$electricity.price
	}
  print(
	paste0("Before runModel: bev.model = ",bev.model," ",rs$bev.model, ", ev.purchase.price = ",ev.purchase.price," ",rs$ev.purchase.price,", capacity.ev = ", rs$capacity.ev,"."))
  new.summary <- runModel(num.stations=rs[1,]$num.stations,
           num.ice=num.of.cv,
           num.hev=num.of.hev,
           num.ev=num.of.bev,
           costs.to.include=the.cti,
           damage.model=dm.to.use, 
           cr.to.use=cr.to.use,
           carbon.price=cp.to.use,
           grid.damage.multiplier=gd.to.use,
           location=loc.to.use,
           folder=out.folder, 
           out.comment=paste0("fixup_",objective.to.use,"_",filestring,"_",rs[1,]$out.comment),
           v.arcs.to.improve=used.v.arcs,
           powertrains=rs[1,]$powertrains,
		   	gas.price.to.use=gp.to.use,
			electricity.price.to.use = ep.to.use,	
           labor.price=rs[1,]$labor.price,
           num.iterations=num.iterations,
           lp.only=lp.only,
		   reduce.last.ev=reduce.last.ev,
		   need.bound=need.bound,
		   bev.model=bev.model,
		   include.resale=rs[1,]$include.resale) 
	return(new.summary$time.string)
}

importAustinData <- function(){
  table_a <-fread(paste0(data.path,"Scripts/R/Rides_DataA.csv"))
  table_b <-fread(paste0(data.path,"Scripts/R/Rides_DataB.csv"))
  
  joined.rides <- table_a[table_b,on=.(RIDE_ID=RIDE_ID), nomatch=0]
  joined.rides[, ':='(start.time = as.POSIXct(started_on,format="%Y-%m-%d %H:%M:%OS"),
                      end.time = as.POSIXct(completed_on,format="%Y-%m-%d %H:%M:%OS"),
                      start.weekday = wday(as.Date(substr(started_on, 1, 10), format="%Y-%m-%d"),label=TRUE),
                      distance = distance_travelled*0.000621371192237 #meters to miles
  )][,
     ':='(start.date = date(start.time),
          start.hour = as.numeric(hour(start.time)),
          end.date = date(end.time),
          end.hour = as.numeric(hour(end.time)),
          duration = as.numeric(difftime(end.time,start.time,units="mins"))
     )
     ][,
       ':='(sample.date=as.Date(ifelse(start.hour>=start.hour.of.day,as.Date(start.date),as.Date(start.date-1))),
            speed = distance/(duration/60)) #mph
       ][,':='(season=ifelse(month(sample.date) %in% c(11,12,1,2,3),"Winter",
                             ifelse(month(sample.date) %in% c(5,6,7,8,9),"Summer","Trans")),
               weekend=ifelse(wday(sample.date) %in% c(6,7),TRUE,FALSE))
         ][month(sample.date)==3 & day(sample.date)>=10 & day(sample.date) <=18,":="(season="SXSW")
           ][,':='(category = paste0(season,"_",as.numeric(weekend)))
             ]
  
  rawdata.all <<- joined.rides[(completed_on > started_on) &
                                 (dispatched_on > completed_on) &
                                 !is.na(distance_travelled)]
  return(joined.rides)
}

importDamages <- function(grid.damage.multiplier=1.0, damage.model=default.damage.model, location='Austin'){
  #Update for population sizes here!
  #AP2 grid damages: mult by 1.2323
  #EASIUR grid damages: mult by 1.1203
  #AP2 gas tailpipe: mult by 1.448
  #EASIUR gas tailpipe: mult by 1.2307
  #AP2 gas upstream: mult by 1.2423
  #EASIUR gas upstream: mult by 1.1203
  #AP2 vehicle life cycle upstream: mult by 1.102
  #EASIUR vehicle life cycle upstream: mult by 1.053
  #texas 2017: 28.3M (1.1203 & 1.2423) (https://www.dshs.state.tx.us/chs/popdat/ST2005.shtm)
  #texas 2010: 25.26 million
  #texas 2005: 22.78 million
  #austin 2017: 2,112,172	(1.2307 & 1.4480)
  #austin 2010: 1,716,289	
  #austin 2005: 1,458,641	
  #us 2017: 325.7M (1.053 & 1.102)
  #us 2010: 309.3M
  #us 2005: 295.5M
  
  pop.conversion.2005.manufacture <- 1.0 #1.102
  pop.conversion.2010.manufacture <- 1.0 #1.053
  if(location=='Austin'){
    pop.conversion.2005.grid <- 1.0 #1.2423
    pop.conversion.2010.grid <- 1.0 #1.1203
    pop.conversion.2005.refinery <- pop.conversion.2005.grid #use all of texas
    pop.conversion.2010.refinery <- pop.conversion.2010.grid #use all of texas
    pop.conversion.2005.city <- 1.0 #1.448
    pop.conversion.2010.city <- 1.0 #1.2307
    fips.city <- 48453
  } else if(location=='NYC'){
    #NYC 2017: 8.623M
    #NYC 2005: 7.97M (1.0819 multiplier)
    #NYC 2010: 8.194M (1.0524 multiplier)
    #nyc	42101	Philadelphia	0.278887779	1.467	1.528	1.581
    #nyc	34039	Union	0.214785215	523988	537743	563892
    #nyc	42045	Delaware	0.158175158	554582	558901	564696
    #nyc	10003	New Castle	0.151681652	524043	538831	559793
    #nyc	34015	Gloucester	0.133200133	275946	288994	292206
    #nyc	42123	Warren	0.054112554	41715	41775	39659
    #nyc	42083	McKean	0.009157509	44082	43333	41330
    pop.conversion.2005.grid <- 1.0 #1.0819
    pop.conversion.2010.grid <- 1.0 #1.0524
    pop.conversion.2005.refinery <- 1.0 #1.0559
    pop.conversion.2010.refinery <- 1.0 #1.0260
    pop.conversion.2005.city <- pop.conversion.2005.grid #use all of nyc
    pop.conversion.2010.city <- pop.conversion.2010.grid #use all of nyc
    fips.city <- 36061 #36081=Queens, 36061=NYC
  }  else if(location=='LA'){
    #LA 2017: 10.16M
    #LA 2005: 9.847M
    #LA 2010: 9.824M
    #CA 2017: 39.4M
    #CA 2005: 35.77M
    #CA 2010: 37.27M
    #contra costa	0.363477697	1.147	1.053	1.004
    #kern	0.021664797	893119	841116	750114
    #los angeles	0.53321821	10.16	9.824	9.778
    #santa barbara	0.005019892	448150	424338	401992
    #solano	0.076619404	445458	414076	407162
    pop.conversion.2005.grid <- 1.0 #39.4/35.77
    pop.conversion.2010.grid <- 1.0 #39.4/37.27
    pop.conversion.2005.refinery <- 1.0 #1.058112286
    pop.conversion.2010.refinery <- 1.0 #1.084514803
    pop.conversion.2005.city <- 1.0 #10.16/9.847 #use all of nyc
    pop.conversion.2010.city <- 1.0 #10.16/9.824 #use all of nyc
    fips.city <- 6037
  }
  
  #Tailpipe+Upstream for EVs
  damages.grid <<- fread(paste0(data.path,"Inputs/cedm.egrid.bySeasonalTOD.csv"))
  #kg/mwh, damages are 2010$/mwh, time zone is UTC-5
  #winter nov-mar, summer may-sept, transition apr-oct
  damages.grid[,kg.per.mwh:=ifelse(kg.per.mwh<0,0,kg.per.mwh)]
  damages.grid[,usd.per.mwh.AP3:=ifelse(usd.per.mwh.AP3<0,0,usd.per.mwh.AP3)]
  damages.grid[,usd.per.mwh.EASIUR:=ifelse(usd.per.mwh.EASIUR<0,0,usd.per.mwh.EASIUR)]
  damages.grid[,usd.per.mwh.INMAP:=ifelse(usd.per.mwh.INMAP<0,0,usd.per.mwh.INMAP)]
  damages.grid[,':='(ton.per.kwh=kg.per.mwh/1000/1000,
                     g.per.kwh=kg.per.mwh,
                     usd.per.kwh.AP3=usd.per.mwh.AP3/1000,
                     usd.per.kwh.EASIUR=usd.per.mwh.EASIUR/1000,
                     usd.per.kwh.INMAP=usd.per.mwh.INMAP/1000)]
  if(grid.damage.multiplier %in% c(0,0.5,1.0,1.5,2.0)){
	temp.multiplier <- grid.damage.multiplier
  } else{
	temp.multiplier <- 1.0	
	cat(paste0("Grid Damage Multiplier: ", grid.damage.multiplier, "\n"))
  }
  if(location=='Austin'){
    damages.grid <<- damages.grid[year %in% c(2017,2018) & region=='ERCT']
    damages.grid <<- damages.grid[,.(g.per.kwh=mean(g.per.kwh*temp.multiplier),
                                     ton.per.kwh=mean(ton.per.kwh*temp.multiplier),
                                     usd.per.kwh.AP3=mean(usd.per.kwh.AP3*pop.conversion.2005.grid*temp.multiplier),
                                     usd.per.kwh.EASIUR=mean(usd.per.kwh.EASIUR*pop.conversion.2010.grid*temp.multiplier),
                                     usd.per.kwh.INMAP=mean(usd.per.kwh.INMAP*pop.conversion.2010.grid*temp.multiplier)),
                                  by=.(season,hour,pollutant)]
    
    # Daylight Savings: Austin was UTC-5 until 11/6/2016 2:00am, then UTC-6 starting 3/12/2017 2:00am
    damages.grid[season=="Summer",':='(hour=as.integer(hour-1))][season=="Summer" & hour==-1,':='(hour=23L)]
  } else if(location=='NYC'){
    damages.grid <<- damages.grid[year %in% c(2017,2018) & region=='NYCW']
    damages.grid <<- damages.grid[,.(g.per.kwh=mean(g.per.kwh*temp.multiplier),
                                     ton.per.kwh=mean(ton.per.kwh*temp.multiplier),
                                     usd.per.kwh.AP3=mean(usd.per.kwh.AP3*pop.conversion.2005.grid*temp.multiplier),
                                     usd.per.kwh.EASIUR=mean(usd.per.kwh.EASIUR*pop.conversion.2010.grid*temp.multiplier),
                                     usd.per.kwh.INMAP=mean(usd.per.kwh.INMAP*pop.conversion.2010.grid*temp.multiplier)),
                                  by=.(season,hour,pollutant)]
    
    # Daylight Savings: NYC was UTC-3 until 11/6/2016 2:00am, then UTC-4 starting 3/12/2017 2:00am
    damages.grid[season!="Summer",':='(hour=as.integer(hour-1))][season=="Summer",':='(hour=as.integer(hour-2))][hour==-1,':='(hour=23L)][hour==-2,':='(hour=22L)]
  } else if(location=='LA'){
    damages.grid <<- damages.grid[year %in% c(2017,2018) & region=='CAMX']
    damages.grid <<- damages.grid[,.(g.per.kwh=mean(g.per.kwh*temp.multiplier),
                                     ton.per.kwh=mean(ton.per.kwh*temp.multiplier),
                                     usd.per.kwh.AP3=mean(usd.per.kwh.AP3*pop.conversion.2005.grid*temp.multiplier),
                                     usd.per.kwh.EASIUR=mean(usd.per.kwh.EASIUR*pop.conversion.2010.grid*temp.multiplier),
                                     usd.per.kwh.INMAP=mean(usd.per.kwh.INMAP*pop.conversion.2010.grid*temp.multiplier)),
                                  by=.(season,hour,pollutant)]
    
    # Daylight Savings: NYC was UTC-3 until 11/6/2016 2:00am, then UTC-4 starting 3/12/2017 2:00am
    damages.grid[season!="Summer",':='(hour=as.integer(hour-3))][season=="Summer",':='(hour=as.integer(hour-4))][hour==-1,':='(hour=23L)][hour==-2,':='(hour=22L)][hour==-3,':='(hour=21L)][hour==-4,':='(hour=20L)]
  }
  if(!(grid.damage.multiplier %in% c(0,0.5,1.0,1.5,2.0))){
	if(damage.model!='AP3'){
		cat(paste0("Other damage models not implemented for grid test cases!"))
	}
	this.mult = 1.0
	total.by.hour <- damages.grid[,.(
		usd.AP3=sum(usd.per.kwh.AP3,na.rm=TRUE),
		usd.EASIUR=sum(usd.per.kwh.EASIUR,na.rm=TRUE),
		usd.INMAP=sum(usd.per.kwh.INMAP,na.rm=TRUE)), by=.(season,hour)]
	if(grid.damage.multiplier %in% c(-2.5,-2)){
		min.AP3 <- total.by.hour[,min(usd.AP3,na.rm=TRUE)]
		min.EASIUR <- total.by.hour[,min(usd.EASIUR,na.rm=TRUE)]
		min.INMAP <- total.by.hour[,min(usd.INMAP,na.rm=TRUE)]
		season.to.use <- total.by.hour[usd.AP3==min.AP3,season]
		hour.to.use <- total.by.hour[usd.AP3==min.AP3,hour]
		if(grid.damage.multiplier==-2.5){this.mult = 0.5}
	} else if(grid.damage.multiplier==-1){
		max.AP3 <- total.by.hour[,max(usd.AP3,na.rm=TRUE)]
		max.EASIUR <- total.by.hour[,max(usd.EASIUR,na.rm=TRUE)]
		max.INMAP <- total.by.hour[,max(usd.INMAP,na.rm=TRUE)]
		season.to.use <- total.by.hour[usd.AP3==max.AP3,season]
		hour.to.use <- total.by.hour[usd.AP3==max.AP3,hour]
		damages.grid.to.apply <- damages.grid[season==season.to.use & hour==hour.to.use]
	}
	damages.grid[pollutant=='co2',':='(
		g.per.kwh=damages.grid[season==season.to.use & hour==hour.to.use & pollutant=='co2',g.per.kwh]*this.mult,
		ton.per.kwh=damages.grid[season==season.to.use & hour==hour.to.use & pollutant=='co2',ton.per.kwh]*this.mult,
		usd.per.kwh.AP3=damages.grid[season==season.to.use & hour==hour.to.use & pollutant=='co2',usd.per.kwh.AP3]*this.mult,
		usd.per.kwh.EASIUR=damages.grid[season==season.to.use & hour==hour.to.use & pollutant=='co2',usd.per.kwh.EASIUR]*this.mult,
		usd.per.kwh.INMAP=damages.grid[season==season.to.use & hour==hour.to.use & pollutant=='co2',usd.per.kwh.INMAP]*this.mult
	)]
	damages.grid[pollutant=='nox',':='(
		g.per.kwh=damages.grid[season==season.to.use & hour==hour.to.use & pollutant=='nox',g.per.kwh]*this.mult,
		ton.per.kwh=damages.grid[season==season.to.use & hour==hour.to.use & pollutant=='nox',ton.per.kwh]*this.mult,
		usd.per.kwh.AP3=damages.grid[season==season.to.use & hour==hour.to.use & pollutant=='nox',usd.per.kwh.AP3]*this.mult,
		usd.per.kwh.EASIUR=damages.grid[season==season.to.use & hour==hour.to.use & pollutant=='nox',usd.per.kwh.EASIUR]*this.mult,
		usd.per.kwh.INMAP=damages.grid[season==season.to.use & hour==hour.to.use & pollutant=='nox',usd.per.kwh.INMAP]*this.mult
	)]
	damages.grid[pollutant=='pm25',':='(
		g.per.kwh=damages.grid[season==season.to.use & hour==hour.to.use & pollutant=='pm25',g.per.kwh]*this.mult,
		ton.per.kwh=damages.grid[season==season.to.use & hour==hour.to.use & pollutant=='pm25',ton.per.kwh]*this.mult,
		usd.per.kwh.AP3=damages.grid[season==season.to.use & hour==hour.to.use & pollutant=='pm25',usd.per.kwh.AP3]*this.mult,
		usd.per.kwh.EASIUR=damages.grid[season==season.to.use & hour==hour.to.use & pollutant=='pm25',usd.per.kwh.EASIUR]*this.mult,
		usd.per.kwh.INMAP=damages.grid[season==season.to.use & hour==hour.to.use & pollutant=='pm25',usd.per.kwh.INMAP]*this.mult
	)]
	damages.grid[pollutant=='so2',':='(
		g.per.kwh=damages.grid[season==season.to.use & hour==hour.to.use & pollutant=='so2',g.per.kwh]*this.mult,
		ton.per.kwh=damages.grid[season==season.to.use & hour==hour.to.use & pollutant=='so2',ton.per.kwh]*this.mult,
		usd.per.kwh.AP3=damages.grid[season==season.to.use & hour==hour.to.use & pollutant=='so2',usd.per.kwh.AP3]*this.mult,
		usd.per.kwh.EASIUR=damages.grid[season==season.to.use & hour==hour.to.use & pollutant=='so2',usd.per.kwh.EASIUR]*this.mult,
		usd.per.kwh.INMAP=damages.grid[season==season.to.use & hour==hour.to.use & pollutant=='so2',usd.per.kwh.INMAP]*this.mult
	)]
	
	
  }
  	
  damages.all.counties <- fread(paste0(data.path,"Inputs/caces.damages.all.counties.csv")) 
  #Units in $/ton
  #2010 VSL: 8.066M (the downloaded versions use 2010)
  #2017 VSL: 9.028M
  #2018 vSL: 9.394M (calculated)
  
  refinery.fips <- fread(paste0(data.path,"Inputs/refcap18.csv"))
  refinery.fips <- refinery.fips[case==location,]
  refinery.fips <- refinery.fips[damages.all.counties,on=.(fips=fips),.(fips=fips,
                                                                        damage.acs.stacklevel=damage.acs.stacklevel,
                                                                        pollutant=pollutant,
                                                                        model=model,
                                                                        season=season,
                                                                        percent=percent)][!is.na(percent)] 
  
  damages.gas.upstream <<- refinery.fips[,.(damage.acs.stacklevel=weighted.mean(damage.acs.stacklevel,w=percent)
  ),by=.(model,season,pollutant)]
  damages.gas.upstream[model=="AP3",':='(damage.acs.stacklevel=damage.acs.stacklevel*pop.conversion.2005.refinery)]
  damages.gas.upstream[model=="EASIUR",':='(damage.acs.stacklevel=damage.acs.stacklevel*pop.conversion.2010.refinery)]
  damages.gas.upstream[model=="INMAP",':='(damage.acs.stacklevel=damage.acs.stacklevel*pop.conversion.2010.refinery)]
  
  damages.gas.tailpipe <<- damages.all.counties[fips==fips.city,.(model,season,pollutant,damage.acs.groundlevel)]
  damages.gas.tailpipe[model=="AP3",':='(damage.acs.groundlevel=damage.acs.groundlevel*pop.conversion.2005.city)]
  damages.gas.tailpipe[model=="EASIUR",':='(damage.acs.groundlevel=damage.acs.groundlevel*pop.conversion.2010.city)]
  damages.gas.tailpipe[model=="INMAP",':='(damage.acs.groundlevel=damage.acs.groundlevel*pop.conversion.2010.city)]
  damages.vehicle <<- data.table(read.csv(paste0(data.path,"Inputs/Greet_Vehicle_Emissions.csv")))
}

appendTripDamages <- function(v.a,
                              model.to.use=default.damage.model,cr.to.use=default.cr,vsl=default.vsl,carbon.price=default.carbon.price){
							  
  grid.damages <- copy(damages.grid)
  if(cr.to.use=='acs'){
    tailpipe.damages <- damages.gas.tailpipe[,damages:=damage.acs.groundlevel]
    upstream.damages <- damages.gas.upstream[,damages:=damage.acs.stacklevel]
  } else if(cr.to.use=='pope'){
    tailpipe.damages <- damages.gas.tailpipe[,damages:=damage.acs.groundlevel*acs.to.pope.factor]
    upstream.damages <- damages.gas.upstream[,damages:=damage.acs.stacklevel*acs.to.pope.factor]
    grid.damages[,':='(usd.per.kwh.AP3=usd.per.kwh.AP3*acs.to.pope.factor,
                       usd.per.kwh.EASIUR=usd.per.kwh.EASIUR*acs.to.pope.factor,
                       usd.per.kwh.INMAP=usd.per.kwh.INMAP*acs.to.pope.factor)]
  }
  #write.csv(tailpipe.damages,paste0(out.path,"damages_tailpipe.csv"))
  #write.csv(upstream.damages,paste0(out.path,"damages_upstream.csv"))
  #write.csv(grid.damages,paste0(out.path,"damages_grid.csv"))
  #Convert from $/ton to $/g:
  if(model.to.use=="EASIUR"){
    tailpipe.damages <- tailpipe.damages[model==model.to.use,
                                         .(damages.per.g=sum(damages)*vsl/1000/1000), by=.(season,pollutant)]
    upstream.damages <- upstream.damages[model==model.to.use,
                                         .(damages.per.g=sum(damages)*vsl/1000/1000), by=.(season,pollutant)]
    grid.damages[,damages.per.kwh:=usd.per.kwh.EASIUR*vsl] #THIS USED TO BE 8.066
  } else if(model.to.use=="AP3"){
    tailpipe.damages <- tailpipe.damages[model==model.to.use,
                                         .(damages.per.g=sum(damages)*vsl/1000/1000), by=.(pollutant)]
    upstream.damages <- upstream.damages[model==model.to.use,
                                         .(damages.per.g=sum(damages)*vsl/1000/1000), by=.(pollutant)]
    grid.damages[,damages.per.kwh:=usd.per.kwh.AP3*vsl]
  } else if(model.to.use=="INMAP"){
    tailpipe.damages <- tailpipe.damages[model==model.to.use,
                                         .(damages.per.g=sum(damages)*vsl/1000/1000), by=.(pollutant)]
    upstream.damages <- upstream.damages[model==model.to.use,
                                         .(damages.per.g=sum(damages)*vsl/1000/1000), by=.(pollutant)]
    grid.damages[,damages.per.kwh:=usd.per.kwh.INMAP*vsl]
  } else if(model.to.use=="none"){
    tailpipe.damages <- tailpipe.damages[model=="AP3",
                                         .(damages.per.g=sum(0)/1000/1000), by=.(pollutant)]
    upstream.damages <- upstream.damages[model=="AP3",
                                         .(damages.per.g=sum(0)/1000/1000), by=.(pollutant)]
    grid.damages[,damages.per.kwh:=0]
  }
  
  #These are all in grams
  v.a[,':='(tailpipe.co2=0.0,tailpipe.ghg=0.0,tailpipe.nox=0.0,tailpipe.pm25=0.0,tailpipe.voc=0.0,tailpipe.so2=0.0,
            upstream.co2=0.0,upstream.ghg=0.0,upstream.nox=0.0,upstream.pm25=0.0,upstream.voc=0.0,upstream.so2=0.0,
            tailpipe.co2.damages=0.0,tailpipe.ghg.damages=0.0,tailpipe.nox.damages=0.0,tailpipe.pm25.damages=0.0,tailpipe.voc.damages=0.0,tailpipe.so2.damages=0.0,
            upstream.co2.damages=0.0,upstream.ghg.damages=0.0,upstream.nox.damages=0.0,upstream.pm25.damages=0.0,upstream.voc.damages=0.0,upstream.so2.damages=0.0)]
  v.a[vehicle.type %in% c("ICE","HEV"), ':='(
    tailpipe.ghg=per.gallon.ghg.tailpipe*energy.loss,
    tailpipe.nox=per.gallon.nox.tailpipe*energy.loss,
    tailpipe.so2=per.gallon.so2.tailpipe*energy.loss,
    tailpipe.pm25=per.gallon.pm25.tailpipe*energy.loss,
    tailpipe.voc=per.gallon.voc.tailpipe*energy.loss,
    upstream.ghg=per.gallon.ghg.upstream*energy.loss,
    upstream.nox=per.gallon.nox.upstream*energy.loss,
    upstream.so2=per.gallon.so2.upstream*energy.loss,
    upstream.pm25=per.gallon.pm25.upstream*energy.loss,
    upstream.voc=per.gallon.voc.upstream*energy.loss
  )]
  v.a[, grep("per.gallon", colnames(v.a)):=NULL]
  
  
  if(model.to.use=='EASIUR'){
    v.a[vehicle.type %in% c('ICE','HEV'), ':='(tailpipe.voc.damages=0,
                                               upstream.voc.damages=0)]
    v.a[vehicle.type %in% c('ICE','HEV') & category %in% c('Summer_0','Summer_1'), ':='(
      tailpipe.nox.damages=tailpipe.nox*(0.4*tailpipe.damages[pollutant=='nox' & season=='spring',damages.per.g]+
                                           0.6*tailpipe.damages[pollutant=='nox' & season=='summer',damages.per.g]),
      tailpipe.so2.damages=tailpipe.so2*(0.4*tailpipe.damages[pollutant=='so2' & season=='spring',damages.per.g]+
                                           0.6*tailpipe.damages[pollutant=='so2' & season=='summer',damages.per.g]),
      tailpipe.pm25.damages=tailpipe.pm25*(0.4*tailpipe.damages[pollutant=='pm25' & season=='spring',damages.per.g]+
                                             0.6*tailpipe.damages[pollutant=='pm25' & season=='summer',damages.per.g])
    )][vehicle.type %in% c('ICE','HEV') & category %in% c('Trans_0','Trans_1'), ':='(
      tailpipe.nox.damages=tailpipe.nox*(0.5*tailpipe.damages[pollutant=='nox' & season=='spring',damages.per.g]+
                                           0.5*tailpipe.damages[pollutant=='nox' & season=='fall',damages.per.g]),
      tailpipe.so2.damages=tailpipe.so2*(0.5*tailpipe.damages[pollutant=='so2' & season=='spring',damages.per.g]+
                                           0.5*tailpipe.damages[pollutant=='so2' & season=='fall',damages.per.g]),
      tailpipe.pm25.damages=tailpipe.pm25*(0.5*tailpipe.damages[pollutant=='pm25' & season=='spring',damages.per.g]+
                                             0.5*tailpipe.damages[pollutant=='pm25' & season=='fall',damages.per.g])
    )][vehicle.type %in% c('ICE','HEV') & category %in% c('Winter_0','Winter_1'), ':='(
      tailpipe.nox.damages=tailpipe.nox*(0.6*tailpipe.damages[pollutant=='nox' & season=='winter',damages.per.g]+
                                           0.4*tailpipe.damages[pollutant=='nox' & season=='fall',damages.per.g]),
      tailpipe.so2.damages=tailpipe.so2*(0.6*tailpipe.damages[pollutant=='so2' & season=='winter',damages.per.g]+
                                           0.4*tailpipe.damages[pollutant=='so2' & season=='fall',damages.per.g]),
      tailpipe.pm25.damages=tailpipe.pm25*(0.6*tailpipe.damages[pollutant=='pm25' & season=='winter',damages.per.g]+
                                             0.4*tailpipe.damages[pollutant=='pm25' & season=='fall',damages.per.g])
    )][vehicle.type %in% c('ICE','HEV') & category %in% c('SXSW_0','SXSW_1'), ':='(
      tailpipe.nox.damages=tailpipe.nox*(tailpipe.damages[pollutant=='nox' & season=='winter',damages.per.g]),
      tailpipe.so2.damages=tailpipe.so2*(tailpipe.damages[pollutant=='so2' & season=='winter',damages.per.g]),
      tailpipe.pm25.damages=tailpipe.pm25*(tailpipe.damages[pollutant=='pm25' & season=='winter',damages.per.g])
    )][vehicle.type %in% c('ICE','HEV') & category %in% c('Summer_0','Summer_1'), ':='(
      upstream.nox.damages=upstream.nox*(0.4*upstream.damages[pollutant=='nox' & season=='spring',damages.per.g]+
                                           0.6*upstream.damages[pollutant=='nox' & season=='summer',damages.per.g]),
      upstream.so2.damages=upstream.so2*(0.4*upstream.damages[pollutant=='so2' & season=='spring',damages.per.g]+
                                           0.6*upstream.damages[pollutant=='so2' & season=='summer',damages.per.g]),
      upstream.pm25.damages=upstream.pm25*(0.4*upstream.damages[pollutant=='pm25' & season=='spring',damages.per.g]+
                                             0.6*upstream.damages[pollutant=='pm25' & season=='summer',damages.per.g])
    )][vehicle.type %in% c('ICE','HEV') & category %in% c('Trans_0','Trans_1'), ':='(
      upstream.nox.damages=upstream.nox*(0.5*upstream.damages[pollutant=='nox' & season=='spring',damages.per.g]+
                                           0.5*upstream.damages[pollutant=='nox' & season=='fall',damages.per.g]),
      upstream.so2.damages=upstream.so2*(0.5*upstream.damages[pollutant=='so2' & season=='spring',damages.per.g]+
                                           0.5*upstream.damages[pollutant=='so2' & season=='fall',damages.per.g]),
      upstream.pm25.damages=upstream.pm25*(0.5*upstream.damages[pollutant=='pm25' & season=='spring',damages.per.g]+
                                             0.5*upstream.damages[pollutant=='pm25' & season=='fall',damages.per.g])
    )][vehicle.type %in% c('ICE','HEV') & category %in% c('Winter_0','Winter_1'), ':='(
      upstream.nox.damages=upstream.nox*(0.6*upstream.damages[pollutant=='nox' & season=='winter',damages.per.g]+
                                           0.4*upstream.damages[pollutant=='nox' & season=='fall',damages.per.g]),
      upstream.so2.damages=upstream.so2*(0.6*upstream.damages[pollutant=='so2' & season=='winter',damages.per.g]+
                                           0.4*upstream.damages[pollutant=='so2' & season=='fall',damages.per.g]),
      upstream.pm25.damages=upstream.pm25*(0.6*upstream.damages[pollutant=='pm25' & season=='winter',damages.per.g]+
                                             0.4*upstream.damages[pollutant=='pm25' & season=='fall',damages.per.g])
    )][vehicle.type %in% c('ICE','HEV') & category %in% c('SXSW_0','SXSW_1'), ':='(
      upstream.nox.damages=upstream.nox*(upstream.damages[pollutant=='nox' & season=='winter',damages.per.g]),
      upstream.so2.damages=upstream.so2*(upstream.damages[pollutant=='so2' & season=='winter',damages.per.g]),
      upstream.pm25.damages=upstream.pm25*(upstream.damages[pollutant=='pm25' & season=='winter',damages.per.g])
    )]
  } else if(model.to.use %in% c('AP3','INMAP')){
    v.a[vehicle.type %in% c('ICE','HEV'), ':='(tailpipe.voc.damages=tailpipe.voc*tailpipe.damages[pollutant=='voc',damages.per.g],
                                               tailpipe.nox.damages=tailpipe.nox*tailpipe.damages[pollutant=='nox',damages.per.g],
                                               tailpipe.so2.damages=tailpipe.so2*tailpipe.damages[pollutant=='so2',damages.per.g],
                                               tailpipe.pm25.damages=tailpipe.pm25*tailpipe.damages[pollutant=='pm25',damages.per.g],
                                               upstream.voc.damages=upstream.voc*upstream.damages[pollutant=='voc',damages.per.g],
                                               upstream.nox.damages=upstream.nox*upstream.damages[pollutant=='nox',damages.per.g],
                                               upstream.so2.damages=upstream.so2*upstream.damages[pollutant=='so2',damages.per.g],
                                               upstream.pm25.damages=upstream.pm25*upstream.damages[pollutant=='pm25',damages.per.g])]
  }
  
  #EASIUR months:
  # Winter: 1,2,3
  # Spring: 4,5,6
  # Summer: 7,8,9
  # Fall: 10,11,12
  # CEDM Grid emissions months: 
  # Winter: 1,2,3,11,12 (0.6*Winter + 0.4*Fall); all UTC-5 except 5 days 2 hrs + 19 days 22 hours = 25 days
  # Trans: 4,10 (0.5*Spring + 0.5*Fall); all UTC-5
  # Summer: 5,6,7,8,9 (0.4*Spring + 0.6*Summer); all UTC-6
  # SXSW: 3/10-3/19
  		
  v.a[,':='(join.hour=hour(start.time),
            join.season=ifelse(category %in% c("Summer_0","Summer_1"),"Summer",
                               ifelse(category %in% c("Trans_0","Trans_1"),"Trans","Winter")
            ))]
  v.a[grid.damages[pollutant=='co2'],on=.(join.season=season,join.hour=hour),':='(ev.upstream.ghg=g.per.kwh)]
  v.a[grid.damages[pollutant=='nox'],on=.(join.season=season,join.hour=hour),':='(ev.upstream.nox=g.per.kwh,
                                                                                  ev.upstream.nox.damages=damages.per.kwh)]
  v.a[grid.damages[pollutant=='so2'],on=.(join.season=season,join.hour=hour),':='(ev.upstream.so2=g.per.kwh,
                                                                                  ev.upstream.so2.damages=damages.per.kwh)]
  v.a[grid.damages[pollutant=='pm25'],on=.(join.season=season,join.hour=hour),':='(ev.upstream.pm25=g.per.kwh,
                                                                                   ev.upstream.pm25.damages=damages.per.kwh)]
  
  v.a[vehicle.type=='EV' & type=='charge', ':='(
    upstream.ghg=ev.upstream.ghg,
    upstream.nox=ev.upstream.nox, upstream.nox.damages=ev.upstream.nox.damages,
    upstream.so2=ev.upstream.so2, upstream.so2.damages=ev.upstream.so2.damages,
    upstream.pm25=ev.upstream.pm25, upstream.pm25.damages=ev.upstream.pm25.damages
  )]
  
  v.a[, grep("ev.upstream", colnames(v.a)):=NULL]
  v.a[, grep("join.", colnames(v.a)):=NULL]
  if(model.to.use!='none'){
    v.a[, ':='(
      tailpipe.ghg.damages=tailpipe.ghg*carbon.price/1000/1000, #converting from $/ton to $/g
      upstream.ghg.damages=upstream.ghg*carbon.price/1000/1000
    )]
  }
  
  v.a[,':='(total.damages=
              tailpipe.ghg.damages+upstream.ghg.damages + 
              tailpipe.nox.damages+upstream.nox.damages +
              tailpipe.so2.damages+upstream.so2.damages +
              tailpipe.pm25.damages+upstream.pm25.damages + 
              tailpipe.voc.damages+upstream.voc.damages)
      ]
	temp <- v.a[type=='charge' & vehicle.type=='EV' & vehicle.id==3]
}

appendVehicleDamages <- function(veh,
                                 model.to.use=default.damage.model,cr.to.use=default.cr,vsl=default.vsl,carbon.price=default.carbon.price){
  Greet.ICE.MPG <- 28
  
  #These are all now in grams per gallon
  veh[vehicle.type=='HEV', ':='(
    Car.Cu.Mine.ghg=damages.vehicle[Model=="Kia Soul HEV" & Stage=="Car.Cu.Mine"]$GHG,
    Car.Cu.Mine.nox=damages.vehicle[Model=="Kia Soul HEV" & Stage=="Car.Cu.Mine"]$NOx,
    Car.Cu.Mine.so2=damages.vehicle[Model=="Kia Soul HEV" & Stage=="Car.Cu.Mine"]$SOx,
    Car.Cu.Mine.pm25=damages.vehicle[Model=="Kia Soul HEV" & Stage=="Car.Cu.Mine"]$PM2.5,
    Car.Cu.Mine.voc=damages.vehicle[Model=="Kia Soul HEV" & Stage=="Car.Cu.Mine"]$VOC,
    Car.Cu.Prod.ghg=damages.vehicle[Model=="Kia Soul HEV" & Stage=="Car.Cu.Prod"]$GHG,
    Car.Cu.Prod.nox=damages.vehicle[Model=="Kia Soul HEV" & Stage=="Car.Cu.Prod"]$NOx,
    Car.Cu.Prod.so2=damages.vehicle[Model=="Kia Soul HEV" & Stage=="Car.Cu.Prod"]$SOx,
    Car.Cu.Prod.pm25=damages.vehicle[Model=="Kia Soul HEV" & Stage=="Car.Cu.Prod"]$PM2.5,
    Car.Cu.Prod.voc=damages.vehicle[Model=="Kia Soul HEV" & Stage=="Car.Cu.Prod"]$VOC,
    Car.Remainder.ghg=damages.vehicle[Model=="Kia Soul HEV" & Stage=="Car.Remainder"]$GHG,
    Car.Remainder.nox=damages.vehicle[Model=="Kia Soul HEV" & Stage=="Car.Remainder"]$NOx,
    Car.Remainder.so2=damages.vehicle[Model=="Kia Soul HEV" & Stage=="Car.Remainder"]$SOx,
    Car.Remainder.pm25=damages.vehicle[Model=="Kia Soul HEV" & Stage=="Car.Remainder"]$PM2.5,
    Car.Remainder.voc=damages.vehicle[Model=="Kia Soul HEV" & Stage=="Car.Remainder"]$VOC,
    Battery.NiCu.Mine.ghg=damages.vehicle[Model=='Kia Soul HEV' & Stage %in% c('Battery.Ni.Mine','Battery.Cu.Mine'),sum(GHG)],
    Battery.NiCu.Mine.nox=damages.vehicle[Model=='Kia Soul HEV' & Stage %in% c('Battery.Ni.Mine','Battery.Cu.Mine'),sum(NOx)],
    Battery.NiCu.Mine.so2=damages.vehicle[Model=='Kia Soul HEV' & Stage %in% c('Battery.Ni.Mine','Battery.Cu.Mine'),sum(SOx)],
    Battery.NiCu.Mine.pm25=damages.vehicle[Model=='Kia Soul HEV' & Stage %in% c('Battery.Ni.Mine','Battery.Cu.Mine'),sum(PM2.5)],
    Battery.NiCu.Mine.voc=damages.vehicle[Model=='Kia Soul HEV' & Stage %in% c('Battery.Ni.Mine','Battery.Cu.Mine'),sum(VOC)],
    Battery.CoMn.Mine.ghg=damages.vehicle[Model=='Kia Soul HEV' & Stage %in% c('Battery.Co.Mine','Battery.Mn.Mine'),sum(GHG)],
    Battery.CoMn.Mine.nox=damages.vehicle[Model=='Kia Soul HEV' & Stage %in% c('Battery.Co.Mine','Battery.Mn.Mine'),sum(NOx)],
    Battery.CoMn.Mine.so2=damages.vehicle[Model=='Kia Soul HEV' & Stage %in% c('Battery.Co.Mine','Battery.Mn.Mine'),sum(SOx)],
    Battery.CoMn.Mine.pm25=damages.vehicle[Model=='Kia Soul HEV' & Stage %in% c('Battery.Co.Mine','Battery.Mn.Mine'),sum(PM2.5)],
    Battery.CoMn.Mine.voc=damages.vehicle[Model=='Kia Soul HEV' & Stage %in% c('Battery.Co.Mine','Battery.Mn.Mine'),sum(VOC)],
    Battery.NiCuCo.Prod.ghg=damages.vehicle[Model=='Kia Soul HEV' & Stage %in% c('Battery.Ni.Prod','Battery.Cu.Prod','Battery.Co.Prod'),sum(GHG)],
    Battery.NiCuCo.Prod.nox=damages.vehicle[Model=='Kia Soul HEV' & Stage %in% c('Battery.Ni.Prod','Battery.Cu.Prod','Battery.Co.Prod'),sum(NOx)],
    Battery.NiCuCo.Prod.so2=damages.vehicle[Model=='Kia Soul HEV' & Stage %in% c('Battery.Ni.Prod','Battery.Cu.Prod','Battery.Co.Prod'),sum(SOx)],
    Battery.NiCuCo.Prod.pm25=damages.vehicle[Model=='Kia Soul HEV' & Stage %in% c('Battery.Ni.Prod','Battery.Cu.Prod','Battery.Co.Prod'),sum(PM2.5)],
    Battery.NiCuCo.Prod.voc=damages.vehicle[Model=='Kia Soul HEV' & Stage %in% c('Battery.Ni.Prod','Battery.Cu.Prod','Battery.Co.Prod'),sum(VOC)],
    Battery.Li.Prod.ghg=damages.vehicle[Model=="Kia Soul HEV" & Stage=="Battery.Li.Prod"]$GHG,
    Battery.Li.Prod.nox=damages.vehicle[Model=="Kia Soul HEV" & Stage=="Battery.Li.Prod"]$NOx,
    Battery.Li.Prod.so2=damages.vehicle[Model=="Kia Soul HEV" & Stage=="Battery.Li.Prod"]$SOx,
    Battery.Li.Prod.pm25=damages.vehicle[Model=="Kia Soul HEV" & Stage=="Battery.Li.Prod"]$PM2.5,
    Battery.Li.Prod.voc=damages.vehicle[Model=="Kia Soul HEV" & Stage=="Battery.Li.Prod"]$VOC,
    Battery.Graphite.Prod.ghg=damages.vehicle[Model=="Kia Soul HEV" & Stage=="Battery.Graphite.Prod"]$GHG,
    Battery.Graphite.Prod.nox=damages.vehicle[Model=="Kia Soul HEV" & Stage=="Battery.Graphite.Prod"]$NOx,
    Battery.Graphite.Prod.so2=damages.vehicle[Model=="Kia Soul HEV" & Stage=="Battery.Graphite.Prod"]$SOx,
    Battery.Graphite.Prod.pm25=damages.vehicle[Model=="Kia Soul HEV" & Stage=="Battery.Graphite.Prod"]$PM2.5,
    Battery.Graphite.Prod.voc=damages.vehicle[Model=="Kia Soul HEV" & Stage=="Battery.Graphite.Prod"]$VOC,
    Battery.Remainder.ghg=damages.vehicle[Model=="Kia Soul HEV" & Stage=="Battery.Remainder"]$GHG,
    Battery.Remainder.nox=damages.vehicle[Model=="Kia Soul HEV" & Stage=="Battery.Remainder"]$NOx,
    Battery.Remainder.so2=damages.vehicle[Model=="Kia Soul HEV" & Stage=="Battery.Remainder"]$SOx,
    Battery.Remainder.pm25=damages.vehicle[Model=="Kia Soul HEV" & Stage=="Battery.Remainder"]$PM2.5,
    Battery.Remainder.voc=damages.vehicle[Model=="Kia Soul HEV" & Stage=="Battery.Remainder"]$VOC,
    per.gallon.ghg.tailpipe=damages.vehicle[Model=="Kia Soul CV" & Stage=="Operation"]$GHG*Greet.ICE.MPG,
    per.gallon.nox.tailpipe=damages.vehicle[Model=="Kia Soul CV" & Stage=="Operation"]$NOx*Greet.ICE.MPG,
    per.gallon.so2.tailpipe=damages.vehicle[Model=="Kia Soul CV" & Stage=="Operation"]$SOx*Greet.ICE.MPG,
    per.gallon.pm25.tailpipe=damages.vehicle[Model=="Kia Soul CV" & Stage=="Operation"]$PM2.5*Greet.ICE.MPG,
    per.gallon.voc.tailpipe=damages.vehicle[Model=="Kia Soul CV" & Stage=="Operation"]$VOC*Greet.ICE.MPG,
    per.gallon.ghg.upstream=damages.vehicle[Model=="Kia Soul CV" & Stage=="WTP"]$GHG*Greet.ICE.MPG,
    per.gallon.nox.upstream=damages.vehicle[Model=="Kia Soul CV" & Stage=="WTP"]$NOx*Greet.ICE.MPG,
    per.gallon.so2.upstream=damages.vehicle[Model=="Kia Soul CV" & Stage=="WTP"]$SOx*Greet.ICE.MPG,
    per.gallon.pm25.upstream=damages.vehicle[Model=="Kia Soul CV" & Stage=="WTP"]$PM2.5*Greet.ICE.MPG,
    per.gallon.voc.upstream=damages.vehicle[Model=="Kia Soul CV" & Stage=="WTP"]$VOC*Greet.ICE.MPG
  )]
  veh[vehicle.type=='ICE', ':='(
    Car.Cu.Mine.ghg=damages.vehicle[Model=="Kia Soul CV" & Stage=="Car.Cu.Mine"]$GHG,
    Car.Cu.Mine.nox=damages.vehicle[Model=="Kia Soul CV" & Stage=="Car.Cu.Mine"]$NOx,
    Car.Cu.Mine.so2=damages.vehicle[Model=="Kia Soul CV" & Stage=="Car.Cu.Mine"]$SOx,
    Car.Cu.Mine.pm25=damages.vehicle[Model=="Kia Soul CV" & Stage=="Car.Cu.Mine"]$PM2.5,
    Car.Cu.Mine.voc=damages.vehicle[Model=="Kia Soul CV" & Stage=="Car.Cu.Mine"]$VOC,
    Car.Cu.Prod.ghg=damages.vehicle[Model=="Kia Soul CV" & Stage=="Car.Cu.Prod"]$GHG,
    Car.Cu.Prod.nox=damages.vehicle[Model=="Kia Soul CV" & Stage=="Car.Cu.Prod"]$NOx,
    Car.Cu.Prod.so2=damages.vehicle[Model=="Kia Soul CV" & Stage=="Car.Cu.Prod"]$SOx,
    Car.Cu.Prod.pm25=damages.vehicle[Model=="Kia Soul CV" & Stage=="Car.Cu.Prod"]$PM2.5,
    Car.Cu.Prod.voc=damages.vehicle[Model=="Kia Soul CV" & Stage=="Car.Cu.Prod"]$VOC,
    Car.Remainder.ghg=damages.vehicle[Model=="Kia Soul CV" & Stage=="Car.Remainder"]$GHG,
    Car.Remainder.nox=damages.vehicle[Model=="Kia Soul CV" & Stage=="Car.Remainder"]$NOx,
    Car.Remainder.so2=damages.vehicle[Model=="Kia Soul CV" & Stage=="Car.Remainder"]$SOx,
    Car.Remainder.pm25=damages.vehicle[Model=="Kia Soul CV" & Stage=="Car.Remainder"]$PM2.5,
    Car.Remainder.voc=damages.vehicle[Model=="Kia Soul CV" & Stage=="Car.Remainder"]$VOC,
    Battery.NiCu.Mine.ghg=0,
    Battery.NiCu.Mine.nox=0,
    Battery.NiCu.Mine.so2=0,
    Battery.NiCu.Mine.pm25=0,
    Battery.NiCu.Mine.voc=0,
    Battery.CoMn.Mine.ghg=0,
    Battery.CoMn.Mine.nox=0,
    Battery.CoMn.Mine.so2=0,
    Battery.CoMn.Mine.pm25=0,
    Battery.CoMn.Mine.voc=0,
    Battery.NiCuCo.Prod.ghg=0,
    Battery.NiCuCo.Prod.nox=0,
    Battery.NiCuCo.Prod.so2=0,
    Battery.NiCuCo.Prod.pm25=0,
    Battery.NiCuCo.Prod.voc=0,
    Battery.Li.Prod.ghg=0,
    Battery.Li.Prod.nox=0,
    Battery.Li.Prod.so2=0,
    Battery.Li.Prod.pm25=0,
    Battery.Li.Prod.voc=0,
    Battery.Graphite.Prod.ghg=0,
    Battery.Graphite.Prod.nox=0,
    Battery.Graphite.Prod.so2=0,
    Battery.Graphite.Prod.pm25=0,
    Battery.Graphite.Prod.voc=0,
    Battery.Remainder.ghg=0,
    Battery.Remainder.nox=0,
    Battery.Remainder.so2=0,
    Battery.Remainder.pm25=0,
    Battery.Remainder.voc=0,
    per.gallon.ghg.tailpipe=damages.vehicle[Model=="Kia Soul CV" & Stage=="Operation"]$GHG*Greet.ICE.MPG,
    per.gallon.nox.tailpipe=damages.vehicle[Model=="Kia Soul CV" & Stage=="Operation"]$NOx*Greet.ICE.MPG,
    per.gallon.so2.tailpipe=damages.vehicle[Model=="Kia Soul CV" & Stage=="Operation"]$SOx*Greet.ICE.MPG,
    per.gallon.pm25.tailpipe=damages.vehicle[Model=="Kia Soul CV" & Stage=="Operation"]$PM2.5*Greet.ICE.MPG,
    per.gallon.voc.tailpipe=damages.vehicle[Model=="Kia Soul CV" & Stage=="Operation"]$VOC*Greet.ICE.MPG,
    per.gallon.ghg.upstream=damages.vehicle[Model=="Kia Soul CV" & Stage=="WTP"]$GHG*Greet.ICE.MPG,
    per.gallon.nox.upstream=damages.vehicle[Model=="Kia Soul CV" & Stage=="WTP"]$NOx*Greet.ICE.MPG,
    per.gallon.so2.upstream=damages.vehicle[Model=="Kia Soul CV" & Stage=="WTP"]$SOx*Greet.ICE.MPG,
    per.gallon.pm25.upstream=damages.vehicle[Model=="Kia Soul CV" & Stage=="WTP"]$PM2.5*Greet.ICE.MPG,
    per.gallon.voc.upstream=damages.vehicle[Model=="Kia Soul CV" & Stage=="WTP"]$VOC*Greet.ICE.MPG
  )]
  veh[vehicle.type=='EV', ':='(
    Car.Cu.Mine.ghg=damages.vehicle[Model=="Kia Soul EV" & Stage=="Car.Cu.Mine"]$GHG,
    Car.Cu.Mine.nox=damages.vehicle[Model=="Kia Soul EV" & Stage=="Car.Cu.Mine"]$NOx,
    Car.Cu.Mine.so2=damages.vehicle[Model=="Kia Soul EV" & Stage=="Car.Cu.Mine"]$SOx,
    Car.Cu.Mine.pm25=damages.vehicle[Model=="Kia Soul EV" & Stage=="Car.Cu.Mine"]$PM2.5,
    Car.Cu.Mine.voc=damages.vehicle[Model=="Kia Soul EV" & Stage=="Car.Cu.Mine"]$VOC,
    Car.Cu.Prod.ghg=damages.vehicle[Model=="Kia Soul EV" & Stage=="Car.Cu.Prod"]$GHG,
    Car.Cu.Prod.nox=damages.vehicle[Model=="Kia Soul EV" & Stage=="Car.Cu.Prod"]$NOx,
    Car.Cu.Prod.so2=damages.vehicle[Model=="Kia Soul EV" & Stage=="Car.Cu.Prod"]$SOx,
    Car.Cu.Prod.pm25=damages.vehicle[Model=="Kia Soul EV" & Stage=="Car.Cu.Prod"]$PM2.5,
    Car.Cu.Prod.voc=damages.vehicle[Model=="Kia Soul EV" & Stage=="Car.Cu.Prod"]$VOC,
    Car.Remainder.ghg=damages.vehicle[Model=="Kia Soul EV" & Stage=="Car.Remainder"]$GHG,
    Car.Remainder.nox=damages.vehicle[Model=="Kia Soul EV" & Stage=="Car.Remainder"]$NOx,
    Car.Remainder.so2=damages.vehicle[Model=="Kia Soul EV" & Stage=="Car.Remainder"]$SOx,
    Car.Remainder.pm25=damages.vehicle[Model=="Kia Soul EV" & Stage=="Car.Remainder"]$PM2.5,
    Car.Remainder.voc=damages.vehicle[Model=="Kia Soul EV" & Stage=="Car.Remainder"]$VOC,
    Battery.NiCu.Mine.ghg=damages.vehicle[Model=='Kia Soul EV' & Stage %in% c('Battery.Ni.Mine','Battery.Cu.Mine'),sum(GHG)]*capacity.ev/30.0/bev.density.factor,
    Battery.NiCu.Mine.nox=damages.vehicle[Model=='Kia Soul EV' & Stage %in% c('Battery.Ni.Mine','Battery.Cu.Mine'),sum(NOx)]*capacity.ev/30.0/bev.density.factor,
    Battery.NiCu.Mine.so2=damages.vehicle[Model=='Kia Soul EV' & Stage %in% c('Battery.Ni.Mine','Battery.Cu.Mine'),sum(SOx)]*capacity.ev/30.0/bev.density.factor,
    Battery.NiCu.Mine.pm25=damages.vehicle[Model=='Kia Soul EV' & Stage %in% c('Battery.Ni.Mine','Battery.Cu.Mine'),sum(PM2.5)]*capacity.ev/30.0/bev.density.factor,
    Battery.NiCu.Mine.voc=damages.vehicle[Model=='Kia Soul EV' & Stage %in% c('Battery.Ni.Mine','Battery.Cu.Mine'),sum(VOC)]*capacity.ev/30.0/bev.density.factor,
    Battery.CoMn.Mine.ghg=damages.vehicle[Model=='Kia Soul EV' & Stage %in% c('Battery.Co.Mine','Battery.Mn.Mine'),sum(GHG)]*capacity.ev/30.0/bev.density.factor,
    Battery.CoMn.Mine.nox=damages.vehicle[Model=='Kia Soul EV' & Stage %in% c('Battery.Co.Mine','Battery.Mn.Mine'),sum(NOx)]*capacity.ev/30.0/bev.density.factor,
    Battery.CoMn.Mine.so2=damages.vehicle[Model=='Kia Soul EV' & Stage %in% c('Battery.Co.Mine','Battery.Mn.Mine'),sum(SOx)]*capacity.ev/30.0/bev.density.factor,
    Battery.CoMn.Mine.pm25=damages.vehicle[Model=='Kia Soul EV' & Stage %in% c('Battery.Co.Mine','Battery.Mn.Mine'),sum(PM2.5)]*capacity.ev/30.0/bev.density.factor,
    Battery.CoMn.Mine.voc=damages.vehicle[Model=='Kia Soul EV' & Stage %in% c('Battery.Co.Mine','Battery.Mn.Mine'),sum(VOC)]*capacity.ev/30.0/bev.density.factor,
    Battery.NiCuCo.Prod.ghg=damages.vehicle[Model=='Kia Soul EV' & Stage %in% c('Battery.Ni.Prod','Battery.Cu.Prod','Battery.Co.Prod'),sum(GHG)]*capacity.ev/30.0/bev.density.factor,
    Battery.NiCuCo.Prod.nox=damages.vehicle[Model=='Kia Soul EV' & Stage %in% c('Battery.Ni.Prod','Battery.Cu.Prod','Battery.Co.Prod'),sum(NOx)]*capacity.ev/30.0/bev.density.factor,
    Battery.NiCuCo.Prod.so2=damages.vehicle[Model=='Kia Soul EV' & Stage %in% c('Battery.Ni.Prod','Battery.Cu.Prod','Battery.Co.Prod'),sum(SOx)]*capacity.ev/30.0/bev.density.factor,
    Battery.NiCuCo.Prod.pm25=damages.vehicle[Model=='Kia Soul EV' & Stage %in% c('Battery.Ni.Prod','Battery.Cu.Prod','Battery.Co.Prod'),sum(PM2.5)]*capacity.ev/30.0/bev.density.factor,
    Battery.NiCuCo.Prod.voc=damages.vehicle[Model=='Kia Soul EV' & Stage %in% c('Battery.Ni.Prod','Battery.Cu.Prod','Battery.Co.Prod'),sum(VOC)]*capacity.ev/30.0/bev.density.factor,
    Battery.Li.Prod.ghg=damages.vehicle[Model=="Kia Soul EV" & Stage=="Battery.Li.Prod"]$GHG*capacity.ev/30.0/bev.density.factor,
    Battery.Li.Prod.nox=damages.vehicle[Model=="Kia Soul EV" & Stage=="Battery.Li.Prod"]$NOx*capacity.ev/30.0/bev.density.factor,
    Battery.Li.Prod.so2=damages.vehicle[Model=="Kia Soul EV" & Stage=="Battery.Li.Prod"]$SOx*capacity.ev/30.0/bev.density.factor,
    Battery.Li.Prod.pm25=damages.vehicle[Model=="Kia Soul EV" & Stage=="Battery.Li.Prod"]$PM2.5*capacity.ev/30.0/bev.density.factor,
    Battery.Li.Prod.voc=damages.vehicle[Model=="Kia Soul EV" & Stage=="Battery.Li.Prod"]$VOC*capacity.ev/30.0/bev.density.factor,
    Battery.Graphite.Prod.ghg=damages.vehicle[Model=="Kia Soul EV" & Stage=="Battery.Graphite.Prod"]$GHG*capacity.ev/30.0/bev.density.factor,
    Battery.Graphite.Prod.nox=damages.vehicle[Model=="Kia Soul EV" & Stage=="Battery.Graphite.Prod"]$NOx*capacity.ev/30.0/bev.density.factor,
    Battery.Graphite.Prod.so2=damages.vehicle[Model=="Kia Soul EV" & Stage=="Battery.Graphite.Prod"]$SOx*capacity.ev/30.0/bev.density.factor,
    Battery.Graphite.Prod.pm25=damages.vehicle[Model=="Kia Soul EV" & Stage=="Battery.Graphite.Prod"]$PM2.5*capacity.ev/30.0/bev.density.factor,
    Battery.Graphite.Prod.voc=damages.vehicle[Model=="Kia Soul EV" & Stage=="Battery.Graphite.Prod"]$VOC*capacity.ev/30.0/bev.density.factor,
    Battery.Remainder.ghg=damages.vehicle[Model=="Kia Soul EV" & Stage=="Battery.Remainder"]$GHG*capacity.ev/30.0/bev.density.factor,
    Battery.Remainder.nox=damages.vehicle[Model=="Kia Soul EV" & Stage=="Battery.Remainder"]$NOx*capacity.ev/30.0/bev.density.factor,
    Battery.Remainder.so2=damages.vehicle[Model=="Kia Soul EV" & Stage=="Battery.Remainder"]$SOx*capacity.ev/30.0/bev.density.factor,
    Battery.Remainder.pm25=damages.vehicle[Model=="Kia Soul EV" & Stage=="Battery.Remainder"]$PM2.5*capacity.ev/30.0/bev.density.factor,
    Battery.Remainder.voc=damages.vehicle[Model=="Kia Soul EV" & Stage=="Battery.Remainder"]$VOC*capacity.ev/30.0/bev.density.factor
  )]
  damages.all.counties <- fread(paste0(data.path,"Inputs/caces.damages.all.counties.csv"))
  employment.counties <- fread(paste0(data.path,"Inputs/manufacture_counties.csv"))  
  employment.counties <- employment.counties[damages.all.counties,on=.(county=fips)][!is.na(V1)]
  damages.process <- employment.counties[,.(auto.acs=weighted.mean(damage.acs.stacklevel,w=auto.cbp.pct,na.rm=TRUE),
                                           NiCu.Mine.acs=weighted.mean(damage.acs.stacklevel,w=NiCu.Mine.cbp.pct,na.rm=TRUE),
                                           CoMn.Mine.acs=weighted.mean(damage.acs.stacklevel,w=CoMn.Mine.cbp.pct,na.rm=TRUE),
                                           NiCuCo.Prod.acs=weighted.mean(damage.acs.stacklevel,w=NiCuCo.Prod.cbp.pct,na.rm=TRUE),
                                           Li.Prod.acs=weighted.mean(damage.acs.stacklevel,w=Li.Prod.cbp.pct,na.rm=TRUE),
                                           Graphite.Prod.acs=weighted.mean(damage.acs.stacklevel,w=Graphite.Prod.cbp.pct,na.rm=TRUE)
  ),by=.(model,season,pollutant)]
  
  if(model.to.use=="none"){
    damages.process <- damages.process[model==model.to.use & season=="annual",.(pollutant=pollutant, 
                                                                                auto=0,
                                                                                NiCu.Mine=0,
                                                                                CoMn.Mine=0,
                                                                                NiCuCo.Prod=0,
                                                                                Li.Prod=0,
                                                                                Graphite.Prod=0
    )]
  }else{
    if(cr.to.use=="acs"){
      damages.process <- damages.process[model==model.to.use & season=="annual",.(pollutant=pollutant, 
                                                                                  auto=auto.acs*vsl,
                                                                                  NiCu.Mine=NiCu.Mine.acs*vsl,
                                                                                  CoMn.Mine=CoMn.Mine.acs*vsl,
                                                                                  NiCuCo.Prod=NiCuCo.Prod.acs*vsl,
                                                                                  Li.Prod=Li.Prod.acs*vsl,
                                                                                  Graphite.Prod=Graphite.Prod.acs*vsl
      )]    
    } else if(cr.to.use=='pope'){
      damages.process <- damages.process[model==model.to.use & season=="annual",.(pollutant=pollutant, 
                                                                                  auto=auto.acs*acs.to.pope.factor*vsl,
                                                                                  NiCu.Mine=NiCu.Mine.acs*acs.to.pope.factor*vsl,
                                                                                  CoMn.Mine=CoMn.Mine.acs*acs.to.pope.factor*vsl,
                                                                                  NiCuCo.Prod=NiCuCo.Prod.acs*acs.to.pope.factor*vsl,
                                                                                  Li.Prod=Li.Prod.acs*acs.to.pope.factor*vsl,
                                                                                  Graphite.Prod=Graphite.Prod.acs*acs.to.pope.factor*vsl
      )]
      }
    
  }
  
  veh[,':='(
    upfront.ghg.cost=(Car.Cu.Mine.ghg + Car.Cu.Prod.ghg + Car.Remainder.ghg + Battery.NiCu.Mine.ghg + Battery.CoMn.Mine.ghg + 
                        Battery.NiCuCo.Prod.ghg + Battery.Li.Prod.ghg + Battery.Graphite.Prod.ghg + Battery.Remainder.ghg)*carbon.price/1000/1000,
    upfront.nox.cost=((Car.Cu.Mine.nox + Battery.NiCu.Mine.nox)*damages.process[pollutant=='nox',NiCu.Mine] +
                        (Car.Cu.Prod.nox + Battery.NiCuCo.Prod.nox)*damages.process[pollutant=='nox',NiCuCo.Prod] + 
                        (Car.Remainder.nox + Battery.Remainder.nox)*damages.process[pollutant=='nox',auto] +
                        Battery.CoMn.Mine.nox*damages.process[pollutant=='nox',CoMn.Mine] +
                        Battery.Li.Prod.nox*damages.process[pollutant=='nox',Li.Prod] +
                        Battery.Graphite.Prod.nox*damages.process[pollutant=='nox',Graphite.Prod])/1000/1000,
    upfront.so2.cost=((Car.Cu.Mine.so2 + Battery.NiCu.Mine.so2)*damages.process[pollutant=='so2',NiCu.Mine] +
                        (Car.Cu.Prod.so2 + Battery.NiCuCo.Prod.so2)*damages.process[pollutant=='so2',NiCuCo.Prod] + 
                        (Car.Remainder.so2 + Battery.Remainder.so2)*damages.process[pollutant=='so2',auto] +
                        Battery.CoMn.Mine.so2*damages.process[pollutant=='so2',CoMn.Mine] +
                        Battery.Li.Prod.so2*damages.process[pollutant=='so2',Li.Prod] +
                        Battery.Graphite.Prod.so2*damages.process[pollutant=='so2',Graphite.Prod])/1000/1000,
    upfront.pm25.cost=((Car.Cu.Mine.pm25 + Battery.NiCu.Mine.pm25)*damages.process[pollutant=='pm25',NiCu.Mine] +
                         (Car.Cu.Prod.pm25 + Battery.NiCuCo.Prod.pm25)*damages.process[pollutant=='pm25',NiCuCo.Prod] + 
                         (Car.Remainder.pm25 + Battery.Remainder.pm25)*damages.process[pollutant=='pm25',auto] +
                         Battery.CoMn.Mine.pm25*damages.process[pollutant=='pm25',CoMn.Mine] +
                         Battery.Li.Prod.pm25*damages.process[pollutant=='pm25',Li.Prod] +
                         Battery.Graphite.Prod.pm25*damages.process[pollutant=='pm25',Graphite.Prod])/1000/1000,
    upfront.voc.cost=((Car.Cu.Mine.voc + Battery.NiCu.Mine.voc)*damages.process[pollutant=='voc',NiCu.Mine] +
                        (Car.Cu.Prod.voc + Battery.NiCuCo.Prod.voc)*damages.process[pollutant=='voc',NiCuCo.Prod] + 
                        (Car.Remainder.voc + Battery.Remainder.voc)*damages.process[pollutant=='voc',auto] +
                        Battery.CoMn.Mine.voc*damages.process[pollutant=='voc',CoMn.Mine] +
                        Battery.Li.Prod.voc*damages.process[pollutant=='voc',Li.Prod] +
                        Battery.Graphite.Prod.voc*damages.process[pollutant=='voc',Graphite.Prod])/1000/1000
  )][,':='(
    upfront.ghg.cost=ifelse(is.na(upfront.ghg.cost),0,upfront.ghg.cost),
    upfront.nox.cost=ifelse(is.na(upfront.nox.cost),0,upfront.nox.cost),
    upfront.so2.cost=ifelse(is.na(upfront.so2.cost),0,upfront.so2.cost),
    upfront.pm25.cost=ifelse(is.na(upfront.pm25.cost),0,upfront.pm25.cost), 
    upfront.voc.cost=ifelse(is.na(upfront.voc.cost),0,upfront.voc.cost)
  )] [,':='(
        upfront.damage.cost=upfront.ghg.cost+upfront.nox.cost+upfront.so2.cost+upfront.pm25.cost+upfront.voc.cost
    )]
  
}

filterTrips <- function(trips){
  trips <- trips[distance < 50 &
                   duration < 120 &
                   duration > 1.5 &
                   distance > 0.25 &
                   speed < 100
                 & speed > 1]
  #1 minute, 0.1 miles
  return(trips)
}

sampleTrips <- function(all.trips,sample.size){
  daily.vol.by.bucket <<- 
    all.trips[,.(vol=.N/(1*length(unique(sample.date)))),
              by=.(category, start.hour)][
                , total.vol:=sum(vol)
                ][, proportion:=vol/total.vol
                  ][, number:= proportion*sample.size
                    ][category=="Summer_0",':='(annualized.days=109.2857143,start.date=as.Date("2017-01-01"))
                      ][category=="Summer_1",':='(annualized.days=43.71428571,start.date=as.Date("2017-01-02"))
                        ][category=="Trans_0", ':='(annualized.days=43.57142857,start.date=as.Date("2017-01-03"))
                          ][category=="Trans_1", ':='(annualized.days=17.42857143,start.date=as.Date("2017-01-04"))
                            ][category=="Winter_0",':='(annualized.days=108.0357143-5,start.date=as.Date("2017-01-05"))
                              ][category=="Winter_1",':='(annualized.days=43.21428571-4,start.date=as.Date("2017-01-06"))
                                ][category=="SXSW_0",':='(annualized.days=5,start.date=as.Date("2017-01-07"))
                                  ][category=="SXSW_1",':='(annualized.days=4,start.date=as.Date("2017-01-08"))
                                    ][order(start.date,start.hour)
                                      ]
  all.trips[, sample.category := paste0(category,'_',start.hour)]
  bucket.sizes = c()
  for(i in 1:nrow(daily.vol.by.bucket)){
    sample_category = paste0(daily.vol.by.bucket[i,]$category,'_',daily.vol.by.bucket[i,]$start.hour)
    # bucket.sizes = c(bucket.sizes,assign(sample_category, daily.vol.by.bucket[i,]$number))
    bucket.sizes[[sample_category]] <- ceiling(daily.vol.by.bucket[i,]$number)
	max.volume <- daily.vol.by.bucket[i,]$vol		
	if(max.volume < ceiling(daily.vol.by.bucket[i,]$number)){		
		bucket.sizes[[sample_category]] <- max.volume		
		daily.vol.by.bucket[i,]$annualized.days <- daily.vol.by.bucket[i,]$annualized.days*bucket.sizes[[sample_category]]/max.volume		
	}	
  }
  set.seed(5)
  trips <- stratified(all.trips,"sample.category",bucket.sizes)
  
  trips[,end.date.offset:=date(end.time)-date(start.time)]
  trips[daily.vol.by.bucket,on=.(category=category),':='(start.date=i.start.date+ifelse(start.hour<start.hour.of.day,1,0),
                                                         end.date=i.start.date+ifelse(start.hour<start.hour.of.day,1,0)+end.date.offset)] #TODO what if start.hour<6 but it goes past midnight
  date(trips$start.time) <- trips$start.date
  date(trips$end.time) <- trips$end.date
  trips$end.date.offset <- NULL
  trips <- unique(trips[order(start.time,end.time)])
  min.time <- min(align.time(trips$start.time,60*60)-3600)
  trips[,':='(index.start=as.integer(difftime(start.time,min.time,units="hours")),
              index.end=as.integer(difftime(end.time,min.time,units="hours")))]
  return(trips)
}

addGeoClusters <- function(data, clusters){
  end_latlongs <- data[,.(lat=end_location_lat,long=end_location_long)]
  start_latlongs <-data[,.(lat=start_location_lat,long=start_location_long)]
  all_latlongs <- rbindlist(list(end_latlongs, start_latlongs))
  
  unique_latlongs <- na.omit(unique(all_latlongs))
  if(nrow(unique_latlongs)<=20000){
    hc <- hclust(dist(unique_latlongs), method="ward.D");
    clust <- cutree(hc,k=clusters);
    unique_latlongs <- data.table(cbind(unique_latlongs, clust))
  } else{
    sample <- unique_latlongs[sample(1:nrow(unique_latlongs),20000),]
    hc <- hclust(dist(sample), method="ward.D");
    clust <- cutree(hc,k=clusters);
    sample <- data.table(cbind(sample, clust))
    train <- data.frame(sample[,.(lat,long)])
    test <- data.frame(unique_latlongs)
    cl <- c(sample$clust)
    results <- knn(train, test, cl, k = 25)
    unique_latlongs$clust <- as.integer(results)
  }
  names(unique_latlongs) <- c('lat', 'long', 'Cluster')
  setkey(data,start_location_lat,start_location_long,end_location_lat,end_location_long)
  setkey(unique_latlongs,lat,long)
  
  data[unique_latlongs, on=c(start_location_lat = "lat",
                             start_location_long = "long"),
       ':='(cluster.start=Cluster)] 
  data[unique_latlongs, on=c(end_location_lat = "lat",
                             end_location_long = "long"),
       ':='(cluster.end=Cluster)]
  
  unique.points <- unique(rbindlist(list(data.frame(cbind(data$cluster.start, 
                                                          data$start_location_long, 
                                                          data$start_location_lat)),
                                         data.frame(cbind(data$cluster.end, 
                                                          data$end_location_long, 
                                                          data$end_location_lat))),
                                    use.names=FALSE))
  cluster.centers <<- unique.points[, .(long = mean(X2,na.rm=TRUE), 
                                        lat = mean(X3,na.rm=TRUE)), by=X1]
  
  return(data)
}

prepDailyData <- function(date1 = "2017-04-04", date2 = "2017-04-05", clusters=40){
  #saveRDS(rawdata.all,file="/pylon5/dd5fp4p/mbruchon//Scripts\\Scripts\\Inputs\\rawdata.all.rds")
  rawdata.all <<- readRDS("/pylon5/dd5fp4p/mbruchon//Inputs/rawdata.all.rds")
  rawdata <<- rawdata.all[(start.date > date1 | (start.date==date1 & start.hour >= 3)) & 
                            (start.date < date2 | (start.date==date2 & start.hour < 3)) &
                            (end.date >= date1 & (end.date<date2 | (end.date==date2 & end.hour <3))),]
  rawdata <<- filterTrips(rawdata)
  date.subset <- addGeoClusters(rawdata,clusters)
  date.subset[, cluster.start.freq := .N, by = cluster.start]
  date.subset[, cluster.end.freq := .N, by = cluster.end]
  date.subset$cluster.max.freq <- pmax(date.subset$cluster.start.freq, date.subset$cluster.end.freq)
  rawdata <<- rawdata[order(start.date,start.time,end.date,end.time)]
  date.subset <- date.subset[order(start.date,start.time,end.date,end.time)]
  
  min.time <- min(align.time(date.subset$start.time,60*60)-3600)
  date.subset[,':='(index.start=as.integer(difftime(start.time,min.time,units="hours")),
                    index.end=as.integer(difftime(end.time,min.time,units="hours")))
              ]
  return(date.subset)
}

inferFromTrips <- function(arcs.in){
	speed.metrics <- data.table(read.csv("/pylon5/dd5fp4p/mbruchon//Inputs/speed_weekday_hour.csv"))
	distance.metrics <- data.table(read.csv("/pylon5/dd5fp4p/mbruchon//Inputs/distance_weekday_hour.csv"))
	duration.metrics <- data.table(read.csv("/pylon5/dd5fp4p/mbruchon//Inputs/duration_weekday_hour.csv"))
  trips <- rawdata[, .(start_location_long,
                           start_location_lat,
                           end_location_long,
                           end_location_lat,
                           start.hour,
                           start.weekday,
                           distance,
                           duration
  )]
  trips[, ':='(speed = distance/(duration/60),
               start.weekday = as.integer(start.weekday))]
  arcs.in[,rownum:=.I]
  others <- arcs.in[, .(start_location_long = start.long,
                        start_location_lat = start.lat,
                        end_location_long = end.long,
                        end_location_lat = end.lat,
                        start.hour = ifelse(is.na(hour(start.time)),hour(end.time),hour(start.time)),
                        start.weekday = as.integer(ifelse(is.na(wday(start.time)),wday(end.time),wday(start.time))),
                        rownum = rownum
  )]
  combos <- unique(others, by=c("start.weekday","start.hour"))
  output=data.frame()
  trainsize = 100000000
  for(i in 1:(nrow(combos))){
    weekday <- combos[i,]$start.weekday
    hour <- combos[i,]$start.hour
    train <- trips[start.hour==hour & start.weekday==weekday & speed < 100, .(start_location_long,
                                                                              start_location_lat,
                                                                              end_location_long,
                                                                              end_location_lat,
                                                                              speed,
                                                                              distance,
                                                                              duration)]
    train.values <- train[, .(speed,distance,duration)]
    train <- train[, .(start_location_long,start_location_lat,end_location_long,end_location_lat)]
    trainsize <- min(trainsize,nrow(train))
    train.speeds <- train.values$speed
    train.distances <- train.values$distance
    train.durations <- train.values$duration
    subset <- others[start.hour==hour & start.weekday==weekday]
    test <- subset[, .(start_location_long, 
                       start_location_lat,
                       end_location_long,
                       end_location_lat)]
    speed.results <- knn.reg(train, test, train.speeds, k = which.min(speed.metrics[i,])[1])
    distance.results <- knn.reg(train, test, train.distances, k = which.min(distance.metrics[i,])[1])
    duration.results <- knn.reg(train, test, train.durations, k = which.min(duration.metrics[i,])[1])
    to.join <- data.table(rownum=subset$rownum,
                          estimated.speed=distance.results$pred/(duration.results$pred/60), 
                          estimated.distance=distance.results$pred,
                          estimated.duration=duration.results$pred)
    arcs.in[to.join,on=.(rownum=rownum), ':='(estimated.speed=i.estimated.speed,
                                              estimated.distance=i.estimated.distance,
                                              estimated.duration=i.estimated.duration)]
  }
  
  return(arcs.in)
}

reduceGraph <- function(data,minutesToRound,minClusterSize=0){
  data <- data[data$cluster.max.freq>=minClusterSize,]
  data$start.time.round <- align.time(data$start.time-60*minutesToRound*0.5,60*minutesToRound)
  data$end.time.round <- align.time(data$end.time-60*minutesToRound*0.5,60*minutesToRound)
  
  data$start_grp_raw <- paste(data$start_location_lat,data$start_location_long,
                              data$started_on)
  data$start_group <- paste(data$cluster.start,data$cluster.end,data$start.time.round)
  data$start_group_end_time <- paste(data$cluster.start,data$cluster.end,data$start.time.round)
  data[, start_group_avg_end_time := mean(end.time), by = start_group]
  
  reduced.trips <- unique(rbindlist(list(data.table(geo=data$cluster.start, time=data$start.time.round),
                                         data.table(geo=data$cluster.end, time=data$end.time.round)))) #was start.group.end.time.round
  reduced.trips$node.id <- seq(1:nrow(reduced.trips))
  
  data[reduced.trips, on = c(cluster.start ="geo", start.time.round="time"), ':='(start.node =i.node.id,
                                                                                  start.time=start.time.round)]
  data[reduced.trips, on = c(cluster.end="geo", end.time.round="time"), ':='(end.node=i.node.id,
                                                                             end.time=end.time.round)]
  #  data[reduced.trips, on = c(cluster.end="geo", start.group.end.time.round="time"), end.node :=i.node.id]
  
  rm(reduced.trips)
  
  clustered.arcs <- data[, .(
    start.lat = mean(start_location_lat,na.rm=TRUE),
    start.long = mean(start_location_long,na.rm=TRUE),
    end.lat = mean(end_location_lat,na.rm=TRUE),
    end.long = mean(end_location_long,na.rm=TRUE),
    start.time = mean(start.time,na.rm=TRUE),
    end.time = mean(end.time,na.rm=TRUE),
    distance = mean(distance,na.rm=TRUE),
    duration = mean(duration,na.rm=TRUE),
    speed = mean(speed,na.rm=TRUE),
    fare = mean(total_fare,na.rm=TRUE),
    highway.factor = NA,
    highway.factor.trunc = NA,
    index.start = as.integer(mean(index.start,na.rm=TRUE)),
    index.end = as.integer(mean(index.end,na.rm=TRUE)),
    demand = .N
  ), by = .(start.node,end.node)]
  
  max.node.number <- max(c(clustered.arcs$start.node,clustered.arcs$end.node))
  num.intra.cluster <- nrow(clustered.arcs[start.node==end.node,])
  clustered.arcs[start.node==end.node,]$end.node = seq(max.node.number+1,max.node.number+num.intra.cluster)
  
  clustered.arcs$highway.factor <- (clustered.arcs$speed-21.2)/(48.3-21.2)
  clustered.arcs$highway.factor.trunc <- pmax(0,pmin(clustered.arcs$highway.factor,1))
  return(clustered.arcs)
}


showGraph <- function(arcs, levels="simple"){
  visNodes <- data.frame(id=unique(c(arcs$node1,arcs$node2)),label=unique(c(arcs$node1,arcs$node2)),level=0)
  if(levels=="simple"){
    visNodes[visNodes$id==unique(arcs[type=='dispatch',]$node1),]$level=1; #source
    visNodes[which(visNodes$id %in% unique(arcs[type=='trip',]$node1)),]$level=2; #trip start
    visNodes[which(visNodes$id %in% unique(arcs[type=='trip',]$node2)),]$level=3; #trip end
    visNodes[visNodes$id==unique(arcs[type=='return',]$node2),]$level=4;#sink
  }
  maxTripDemand <- max(arcs[arcs$type=='trip',]$capacity)
  visEdges <- data.frame(from=arcs$node1,to=arcs$node2,arrows="to",
                         width=pmin(arcs$capacity,maxTripDemand),color=arcs$color)
  visNetwork(nodes=visNodes,edges=visEdges,width="100%",height="100%") %>%
    visEdges(smooth=TRUE) %>%
    visHierarchicalLayout(direction="LR",levelSeparation=1200)
  
  #nw <- graph_from_edgelist(as.matrix(visEdges[,1:2]))
  #asdf <- all_simple_paths(nw,1,2,mode="out")
}

computeFeasibleCombos <- function(clustered.trips, offset=2){
  clustered.trip.columns <- clustered.trips[,.(index.start,index.end,start.time,end.time,start.node,
                                               end.node,is.charge,demand,end.long,end.lat,start.long,
                                               start.lat,end.lat)]
  clustered.trip.columns <- clustered.trip.columns[,':='(start.time.numeric=as.numeric(start.time),
                                                         end.time.numeric=as.numeric(end.time))]
  clustered.trip.columns <- clustered.trip.columns[order(index.start,index.end)]
  clustered.trip.columns.copy <- copy(clustered.trip.columns)
  clustered.trip.columns[,prior.end.time:=end.time]
  clustered.trip.columns.copy[,':='(next.start.time=start.time,
                                    next.start.minus.90=start.time-90*60)]
  
  feasible.combos <- clustered.trip.columns[clustered.trip.columns.copy, on=.(prior.end.time<=next.start.time,
                                                                              prior.end.time>=next.start.minus.90), allow.cartesian=TRUE,
                                            .(   is.charge=is.charge, i.is.charge=i.is.charge,
                                                 demand=demand, i.demand=i.demand,
                                                 i.start.long=i.start.long, i.start.lat=i.start.lat,
                                                 end.long=end.long, end.lat=end.lat,
                                                 i.start.time=i.start.time,
                                                 end.time=end.time,
                                                 i.start.node=i.start.node,
                                                 end.node=end.node,
                                                 index.end=index.end,
                                                 i.index.start=i.index.start,
                                                 time_gap=(i.start.time.numeric-end.time.numeric)/60)
                                            ][(end.node != i.start.node) & 
                                                time_gap>0.0 & (!is.charge | !i.is.charge) &
                                                (time_gap < 30 | ((i.is.charge | is.charge) & time_gap < 90)) &
												(i.is.charge | is.charge | 
													(hour(end.time)<=6 & hour(i.start.time)<=6) | 
													(hour(end.time)>6 & hour(i.start.time)>6))
                                              ]
  feasible.combos <- unique(feasible.combos)
  setkeyv(feasible.combos,c("end.node","i.start.node"))
  feasible.combos <- feasible.combos[order(end.node,i.start.node)]
  feasible.combos <- feasible.combos[, 
                                     .(demand = pmin(sum(i.demand,na.rm=TRUE),sum(demand,na.rm=TRUE)),
                                       start.long = mean(end.long, na.rm=TRUE),
                                       start.lat = mean(end.lat, na.rm=TRUE),
                                       end.long = mean(i.start.long, na.rm=TRUE),
                                       end.lat = mean(i.start.lat, na.rm=TRUE),
                                       start.time = mean(end.time, na.rm=TRUE),
                                       end.time = mean(i.start.time, na.rm=TRUE),
                                       index.start = mean(index.end, na.rm=TRUE),
                                       index.end = mean(i.index.start, na.rm=TRUE),
                                       begins.with.charge = max(is.charge,na.rm=TRUE),
                                       ends.with.charge = max(i.is.charge,na.rm=TRUE)
                                     ),
                                     by = .(end.node,i.start.node)]
  feasible.combos[, ':='(start.node=end.node,
                         end.node=i.start.node)]
  feasible.combos$i.start.node <- NULL
  
  feasible.combos <- inferFromTrips(feasible.combos)
  feasible.combos <- feasible.combos[estimated.duration < as.numeric(difftime(end.time,start.time,units="mins"))]
  feasible.combos[, first.following:=min(ifelse(ends.with.charge==TRUE,end.time,NA),na.rm=TRUE), by=start.node]
  feasible.combos[, last.preceding:=max(ifelse(begins.with.charge==TRUE,start.time,NA),na.rm=TRUE), by=end.node]
  feasible.combos <- feasible.combos[(start.time==last.preceding | !begins.with.charge) & (end.time==first.following | !ends.with.charge)]
  feasible.combos$first.following <- NULL; feasible.combos$last.preceding <- NULL
  return(feasible.combos)
}

buildArcList <- function(date.subset, clustered.trips, charge.stations, num.cars){
  
  num.trip.nodes=max(c(clustered.trips$start.node,clustered.trips$end.node))
  current.charge.node <- num.trip.nodes+1
  for(i in 1:nrow(charge.stations)){
    charge.arc.list <- vector("list", length = 100+difftime(align.time(max(clustered.trips$start.time),3600),
                                                            align.time(min(clustered.trips$start.time),3600)-3600,units="mins")/charge.time)
    current.start.time <- align.time(min(clustered.trips$start.time),3600)-3600+charge.time*60
    next.start.time <- current.start.time+charge.time*60
    final.time <- align.time(max(clustered.trips$end.time+3600),3600)
    lat <- charge.stations[i,]$lat
    long <- charge.stations[i,]$long
    j=1
    while(next.start.time < final.time){
      this <- data.table(
        start.node = current.charge.node,
        end.node = current.charge.node + 1,
        start.lat = lat,
        start.long = long,
        end.lat = lat,
        end.long = long,
        start.time = current.start.time,
        end.time=next.start.time
      )
      charge.arc.list[[j]] <- this
      current.charge.node <- current.charge.node + 1
      current.start.time <- next.start.time
      next.start.time <- next.start.time+charge.time*60
      j=j+1
    }
    current.charge.node <- current.charge.node + 1
  }
  
  charge.arcs <- rbindlist(charge.arc.list)
  min.time <- min(align.time(date.subset$start.time,60*60)-3600)
  charge.arcs[,':='(index.start=as.integer(difftime(start.time,min.time,units="hours")),
                    index.end=as.integer(difftime(end.time,min.time,units="hours")),
                    min.15=align.time(current.start.time,60*15),
                    distance = 0,
                    duration = charge.time,
                    speed = NA,
                    fare = 0,
                    demand = num.cars,
                    highway.factor=NA,
                    highway.factor.trunc=NA)
              ]
  clustered.trips <- rbindlist(list(clustered.trips,charge.arcs),use.names=TRUE)
  clustered.trips$is.charge <- clustered.trips$distance==0 & clustered.trips$duration==charge.time & is.na(clustered.trips$speed)
  feasible.combos <- computeFeasibleCombos(clustered.trips)
  charge.arcs <- clustered.trips[clustered.trips$is.charge==TRUE,]
  clustered.trips <- clustered.trips[is.na(clustered.trips$is.charge) | clustered.trips$is.charge!=TRUE,]
  
  dispatch.arcs <- clustered.trips[, j=list(
    demand = sum(demand,na.rm=TRUE),
    end.time = mean(start.time, na.rm=TRUE),
    index.end = as.integer(mean(index.start, na.rm=TRUE)),
    end.lat = mean(start.lat, na.rm=TRUE),
    end.long = mean(start.long, na.rm=TRUE)
  ), by = list(start.node)]
  first.charge <- charge.arcs[start.time==min(charge.arcs$start.time)][1]
  dispatch.arcs <- rbindlist(list(dispatch.arcs,
	data.table(start.node=first.charge$end.node,
	demand=num.cars,
	end.time=first.charge$start.time,
	index.end=as.integer(first.charge$index.start),
	end.lat = first.charge$start.lat,
	end.long = first.charge$start.long
	)))
   return.arcs <- clustered.trips[, j=list(
    demand = sum(demand,na.rm=TRUE),
    start.time = mean(end.time, na.rm=TRUE),
    index.start = as.integer(mean(index.end, na.rm=TRUE)),
    start.lat = mean(end.lat, na.rm=TRUE),
    start.long = mean(end.long, na.rm=TRUE)
  ), by = list(end.node)]
	final.charge <- charge.arcs[start.time==max(charge.arcs$start.time)][1]
		return.arcs <- rbindlist(list(return.arcs,
		data.table(end.node=final.charge$end.node,
		demand=num.cars,
		start.time=final.charge$end.time,
		index.start=as.integer(final.charge$index.end),
		start.lat = final.charge$end.lat,
		start.long = final.charge$end.long
	)))  
  # base.station <- locateStations(case.subset, num.stations=1)
  base.station <- charge.stations[1]
  
  purchase <- data.table(
    node1=-1,
    node2=0,
    path.id=0,
    cost=0,
    distance=0,
    capacity=num.cars,
    type='purchase',
    color = 'black',
    start.lat = NA,
    start.long = NA,
    end.lat = NA,
    end.long = NA,
    start.time = fastPOSIXct(NA),
    end.time = fastPOSIXct(NA),
    index.start = as.integer(NA),
    index.end = as.integer(NA),
    highway.factor=NA,
    highway.factor.trunc=NA)
  dispatch <- data.table(cbind.data.frame(
    node1 = rep(0,times=nrow(dispatch.arcs)),
    node2 = dispatch.arcs$start.node,
    path.id = 0,
    cost = rep(0,times=nrow(dispatch.arcs)),
    distance = as.numeric(NA),
    capacity = dispatch.arcs$demand,
    type = 'dispatch',
    color = 'gray',
    start.lat = base.station$lat[1],
    start.long = base.station$long[1],
    end.lat = dispatch.arcs$end.lat,
    end.long = dispatch.arcs$end.long,
    start.time = fastPOSIXct(NA),
    end.time = dispatch.arcs$end.time,
    index.start = as.integer(NA),
    index.end = dispatch.arcs$index.end,
    highway.factor=as.numeric(NA),
    highway.factor.trunc=as.numeric(NA)))
  trip <- data.table(cbind.data.frame(
    node1 = clustered.trips$start.node,
    node2 = clustered.trips$end.node,
    path.id = 0,
    cost = 0, #should be a combination of fare + (distance+time) costs
    distance = clustered.trips$distance,
    capacity = clustered.trips$demand,
    type = 'trip',
    color='red',
    start.lat = clustered.trips$start.lat,
    start.long = clustered.trips$start.long,
    end.lat = clustered.trips$end.lat,
    end.long = clustered.trips$end.long,
    start.time = clustered.trips$start.time,
    end.time = clustered.trips$end.time,
    index.start = clustered.trips$index.start,
    index.end = clustered.trips$index.end,
    highway.factor=clustered.trips$highway.factor,
    highway.factor.trunc=clustered.trips$highway.factor.trunc
  ))
  charge <- data.table(cbind.data.frame(
    node1 = charge.arcs$start.node,
    node2 = charge.arcs$end.node,
    path.id = 0,
    cost = 0,
    distance = charge.arcs$distance,
    capacity = charge.arcs$demand,
    type = 'charge',
    color='green',
    start.lat = charge.arcs$start.lat,
    start.long = charge.arcs$start.long,
    end.lat = charge.arcs$end.lat,
    end.long = charge.arcs$end.long,
    start.time = charge.arcs$start.time,
    end.time = charge.arcs$end.time,
    index.start = charge.arcs$index.start,
    index.end = charge.arcs$index.end,
    highway.factor=NA,
    highway.factor.trunc=NA
  ))
  relocation <- data.table(cbind.data.frame(
    node1 = feasible.combos$start.node,
    node2 = feasible.combos$end.node,
    path.id = 0,
    cost = 0,
    distance = feasible.combos$estimated.distance,
    capacity = feasible.combos$demand,
    type = 'relocation',
    color = 'blue',
    start.lat = feasible.combos$start.lat,
    start.long = feasible.combos$start.long,
    end.lat = feasible.combos$end.lat,
    end.long = feasible.combos$end.long,
    start.time = feasible.combos$start.time,
    end.time = feasible.combos$end.time,
    index.start = feasible.combos$index.start,
    index.end = feasible.combos$index.end,
    highway.factor=NA,
    highway.factor.trunc=NA,
    estimated.speed=feasible.combos$estimated.speed,
    estimated.distance=feasible.combos$estimated.distance,
    estimated.duration=feasible.combos$estimated.duration))
  
  #Fix up relocation arcs with identical start/end nodes as a trip arc
  setkey(trip,node1,node2)
  setkey(relocation,node1,node2)
  
  relocation.duplicates <- relocation[trip, nomatch=0, 
                                      .(node1,node2,cost,distance,capacity,type,color, start.lat, 
                                        start.long, end.lat, end.long, start.time, end.time, index.start, index.end,
                                        i.capacity, highway.factor, highway.factor.trunc, estimated.distance,estimated.distance,estimated.duration)]
  relocation <- relocation[relocation.duplicates, 
                           on=c(node1="node1", node2="node2"), 
                           path.id := 1]
  
  charge.duplicates <- charge[rbindlist(list(trip,relocation),fill=TRUE,use.names=TRUE), nomatch=0, 
                              .(node1,node2,cost,distance,capacity,type,color, start.lat, 
                                start.long, end.lat, end.long, start.time, end.time, index.start, index.end,
                                i.capacity, highway.factor, highway.factor.trunc),
                              on=.(node1=node1,node2=node2)]
  charge <- charge[charge.duplicates, 
                   on=c(node1="node1", node2="node2"), 
                   path.id := 2]
  
  current.new.node <- max(c(clustered.trips$start.node,clustered.trips$end.node,
                            charge.arcs$start.node,charge.arcs$end.node))
  no.dispatch <- cbind.data.frame(
    node1 = 1,
    node2 = current.new.node +1,
    path.id = 0,
    cost = 0,
    distance = 0,
    capacity = num.cars,
    type= 'no_dispatch',
    color='gray0',
    start.lat = NA,
    start.long = NA,
    end.lat = NA,
    end.long = NA,
    start.time = fastPOSIXct(NA),
    end.time = fastPOSIXct(NA),
    index.start = as.integer(NA),
    index.end = as.integer(NA),
    highway.factor=NA,
    highway.factor.trunc=NA);
  return <- cbind.data.frame(
    node1 = return.arcs$end.node,
    node2 = rep(current.new.node+1,times=nrow(return.arcs)),
    path.id = 0,
    cost = rep(0,times=nrow(return.arcs)),
    distance = NA,
    capacity = return.arcs$demand,   
    type = 'return',
    color = 'gray',
    start.lat = return.arcs$start.lat,
    start.long = return.arcs$start.long,
    end.lat = base.station$lat[1],
    end.long = base.station$long[1],
    start.time=return.arcs$start.time,
    end.time = fastPOSIXct(NA),
    index.start = return.arcs$index.start,
    index.end = as.integer(NA),
    highway.factor=as.numeric(NA),
    highway.factor.trunc=as.numeric(NA));
  
  return.and.dispatch <- rbindlist(list(dispatch,return))
  return.and.dispatch <- inferFromTrips(return.and.dispatch) #TODO we already called this once; streamline
  
  arcs <- rbindlist(list(
    trip,
    relocation,
    #    no.dispatch,
    #    purchase,
    return.and.dispatch, 
    charge
  ),
  fill=TRUE,
  use.names=TRUE)
  
  arcs[,':='(estimated.duration = 60*estimated.distance/estimated.speed)]
  
  arcs[!(type %in% c('trip','charge')), ':='(speed = estimated.speed,
                                             distance = estimated.distance)]
  arcs[type=='dispatch', ':='(duration = estimated.duration,
                              start.time = align.time(end.time - 60*estimated.duration,60*min.to.round))]
  arcs[type=='return', ':='(duration = estimated.duration,
                            end.time = align.time(start.time + 60*estimated.duration,60*min.to.round))]
  arcs[type=='dispatch' & node2==first.charge$start.node,':='(distance=0,duration=0,start.time=end.time)]
  arcs[type=='return' & node1==final.charge$end.node,':='(distance=0,duration=0,start.time=end.time)]

  charge.nodes <- data.table(node=unique(c(arcs[type=='charge']$node1,
                                           arcs[type=='charge']$node2)))
  arcs$charge.node = FALSE
  arcs <- arcs[charge.nodes, on=c(node2="node"), charge.node:=TRUE]
  arcs <- arcs[charge.nodes, on=c(node1="node"), charge.node:=TRUE]
  
  arcs$capacity <- as.integer(arcs$capacity)
  arcs$node1 <- as.integer(arcs$node1+1) #1 for dispatch
  arcs$node2 <- as.integer(arcs$node2+1)
  arcs[, duration:= as.numeric(difftime(end.time,start.time,units="mins"))]
  assumed.loiter.speed <- 21.2/2
  assumed.loiter.highway.factor <- (assumed.loiter.speed-21.2)/(48.3-21.2)
  arcs[, ':='(highway.factor = (speed-21.2)/(48.3-21.2),
              highway.factor.trunc = pmax(0,pmin((speed-21.2)/(48.3-21.2),1),na.rm=TRUE),
              extra.time = pmax(0,duration-estimated.duration,na.rm=TRUE))]
  arcs[duration==0,':='(speed=21.2,highway.factor=0,highway.factor.trunc=0)]
  arcs[, ':='(extra.distance = (extra.time/60)*assumed.loiter.speed)] #hours extra x city mph = miles
  arcs[(type %in% c('trip','dispatch','return')) | (type=='relocation' & charge.node),
    ':='(extra.distance=0)]
  arcs[,extra.distance:=0]
  
  arcs <- arcs[(type %in% c('trip','dispatch','return')) | (type=='relocation' & charge.node), 
               ':='(
                 energy.loss.ev = (distance/100)*(highway.factor.trunc*fe.ev.hwy +
                                                    (1-highway.factor.trunc)*fe.ev.city), #kwh/100mi to kwh
                 energy.loss.hev = distance*(highway.factor.trunc*(1/fe.hev.hwy)+(1-highway.factor.trunc)*(1/fe.hev.city)), #mpg to g
                 energy.loss.ice = distance*(highway.factor.trunc*(1/fe.ice.hwy)+(1-highway.factor.trunc)*(1/fe.ice.city))
               )]
  
  arcs <- arcs[!(type %in% c('trip','dispatch','return')) & type!='charge' & !charge.node, 
               ':='(
                 energy.loss.ev = (distance/100)*(highway.factor.trunc*fe.ev.hwy +
                                                    (1-highway.factor.trunc)*fe.ev.city), #kwh/100mi to kwh
                 energy.loss.hev = distance*(highway.factor.trunc*(1/fe.hev.hwy)+(1-highway.factor.trunc)*(1/fe.hev.city)), #mpg to g
                 energy.loss.ice = distance*(highway.factor.trunc*(1/fe.ice.hwy)+(1-highway.factor.trunc)*(1/fe.ice.city))                 
               )]
  
  arcs$energy.loss.ev <- round(arcs$energy.loss.ev,4)
  arcs$energy.loss.hev <- round(arcs$energy.loss.hev,4)
  arcs$energy.loss.ice <- round(arcs$energy.loss.ice,4)
  arcs <- arcs[, cost.ev := cost + (distance+extra.distance)*maintenance.bev]
  arcs <- arcs[, cost.hev := cost + (distance+extra.distance)*maintenance.hev + energy.loss.hev*gas.price]
  arcs <- arcs[, cost.ice := cost + (distance+extra.distance)*maintenance.cv + energy.loss.ice*gas.price]
  arcs[is.na(cost.ev)]$cost.ev <- 0
  arcs[is.na(cost.hev)]$cost.hev <- 0
  arcs[is.na(cost.ice)]$cost.ice <- 0
  
  
  arcs <- arcs[(estimated.duration < duration) | charge.node | type!='relocation'] #TODO: should get nearest feasible charge arc
  arcs[, ':='(start.date=date(start.time),
              start.hour=hour(start.time),
              end.date=date(end.time),
              end.hour=hour(end.time))]
  arcs$index.start <- frank(arcs, start.date,start.hour,ties.method="dense")-1
  arcs$index.end   <- frank(arcs, end.date,end.hour,ties.method="dense")-1
  arcs[,start.date.to.join:=ifelse((start.hour<6 & start.date!=min(arcs$start.date)) | (start.hour>=6 & start.date==max(arcs$start.date)),start.date-1,start.date)]
  arcs[daily.vol.by.bucket,on=.(start.date.to.join=start.date), ':='(annualized.days=i.annualized.days,category=i.category)]
  arcs$start.date <- NULL; arcs$start.date.to.join <- NULL; arcs$start.hour <- NULL; arcs$end.date <- NULL; arcs$end.hour <- NULL
  if(nrow(arcs) != nrow(unique(cbind(arcs$node1,arcs$node2,arcs$path.id)))) print("DUPLICATED ARCS!!")
  arcs <- arcs[order(node1,node2,path.id)]
  arcs[,distance:=distance+extra.distance]
  return(arcs)
}

buildVehicles <- function(num.ice, num.hev, total.ev, disaggregated.ev, costs.to.include="private",
                          damage.model=default.damage.model,cr.to.use=default.cr,carbon.price=default.carbon.price,vsl=default.vsl,bev.model=default.bev,
						  include.resale=TRUE){
  print(paste0("Inside buildVehicles: bev.model = ",
  bev.model, ", ev.purchase.price = ",ev.purchase.price,", capacity.ev = ", capacity.ev,"include.resale = ",include.resale,".\n"))

  aggregated.ev <- total.ev-disaggregated.ev
  vehicle.type <- c(rep("ICE",times=ifelse(num.ice>0,1,0)),
                    rep("HEV",times=ifelse(num.hev>0,1,0)),
                    rep("EV",times=ifelse(disaggregated.ev>0,disaggregated.ev,0)),
                    rep("EV",times=ifelse(total.ev-disaggregated.ev>0,1,0)))
  vehicle.count <- c(rep(num.ice,times=ifelse(num.ice>0,1,0)),
                     rep(num.hev,times=ifelse(num.hev>0,1,0)),
                     rep(1,times=ifelse(disaggregated.ev>0,disaggregated.ev,0)),
                     rep(total.ev-disaggregated.ev,times=ifelse(total.ev-disaggregated.ev>0,1,0)))
  num.cars <- length(vehicle.type)
  
  if(bev.model=='Bolt2020'){
	fe.ev.city <<- 26.4997 #kwh/100mi
	fe.ev.hwy <<- 31.3069 #kwh/100mi
	ev.purchase.price <<- 36620
	capacity.ev <<- 66.0
	bev.density.factor <<- 1.0
  } else if(bev.model=='Bolt2020_906'){
  	fe.ev.city <<- 26.4997 #kwh/100mi
	fe.ev.hwy <<- 31.3069 #kwh/100mi
	ev.purchase.price <<- 36620
	capacity.ev <<- 66.0
	bev.density.factor <<- (66/906)/(30/605)
  }
  
  energy.capacity <- vehicle.count*capacity.ev
  energy.capacity[vehicle.type %in% c('ICE,HEV')] <- 0
  initial.energy <- energy.capacity/2
  highway.fe <- ifelse(vehicle.type=='EV', fe.ev.hwy, 
                       ifelse(vehicle.type=="HEV", fe.hev.hwy, fe.ice.hwy)) #kwh/100mi and MPG
  city.fe <- ifelse(vehicle.type=='EV', fe.ev.city, 
                    ifelse(vehicle.type=="HEV", fe.hev.city, fe.ice.city)) #kwh/100mi and MPG
  
  vehicles <- data.table(vehicle.id = seq(1,num.cars), vehicle.count, vehicle.type,
                         energy.capacity, initial.energy, highway.fe, city.fe,
                         purchased=as.logical(NA))
  
  appendVehicleDamages(vehicles,damage.model,cr.to.use,vsl,carbon.price)
  vehicles[,':='(sticker.price=ifelse(vehicle.type=="EV", ev.purchase.price,
                                      ifelse(vehicle.type=="HEV", hev.purchase.price, ice.purchase.price)))]
  if(costs.to.include=="both"){
    vehicles[,':='(total.price=sticker.price+upfront.damage.cost)]
  } else if(costs.to.include=="private"){
    vehicles[,':='(total.price=sticker.price)]
  }
  crf.years = (discount.rate*((1+discount.rate)^(lifespan.years-1)))/((1+discount.rate)^lifespan.years - 1)
  
  breaks = c(0, 
             2610,
             #battery.lifespan.vmt/lifespan.years,
             car.lifespan.vmt/lifespan.years,
             car.lifespan.vmt/lifespan.years + (car.lifespan.vmt-car.lifespan.vmt/lifespan.years)*0.75)
  breaks = sort(breaks)
  breaks = unique(breaks)
  avmt=matrix(as.numeric(NA),nrow=length(breaks), ncol=nrow(vehicles))
  annualized.private.costs=matrix(as.numeric(NA),nrow=length(breaks), ncol=nrow(vehicles))
  annualized.externalities=matrix(as.numeric(NA),nrow=length(breaks), ncol=nrow(vehicles))
  for(i in 1:length(breaks)){
    avmt.car <- rep(breaks[i],times=nrow(vehicles))
    resale.year.car       <- ifelse(avmt.car<=car.lifespan.vmt/lifespan.years, 
                                    lifespan.years, 
                                    car.lifespan.vmt/avmt.car)
    crf.car <- discount.rate*((1+discount.rate)^(resale.year.car-1))/(((1+discount.rate)^resale.year.car)-1)
    resale.value.car <- ifelse(avmt.car <= 2610,
                               0.38,
                               ifelse(avmt.car < car.lifespan.vmt/lifespan.years-1,
                                      0.4253-avmt.car*0.00001848+(avmt.car^2)*0.0000000004244,
                                      ifelse(avmt.car<car.lifespan.vmt/lifespan.years+1,
                                             (0.4253-avmt.car*0.00001848+(avmt.car^2)*0.0000000004244 + -0.723952+0.101438*log(avmt.car))/2,
                                             -0.723952+0.101438*log(avmt.car))
                               )
    )
    resale.value.hev <- ifelse(avmt.car <= 2610,
                               0.28,
                               ifelse(avmt.car < car.lifespan.vmt/lifespan.years-1,
                                      0.3131-avmt.car*0.00001296+(avmt.car^2)*0.0000000002977,
                                      ifelse(avmt.car<car.lifespan.vmt/lifespan.years+1,
                                             (0.3131-avmt.car*0.00001296+(avmt.car^2)*0.0000000002977 + -1.688-0.000001603*avmt.car+0.1988*log(avmt.car))/2,
                                             -1.688-0.000001603*avmt.car+0.1988*log(avmt.car))
                               )
    )
    resale.value.bev <- ifelse(avmt.car <= 2610,
                               0.17,
                               ifelse(avmt.car < car.lifespan.vmt/lifespan.years-1,
                                      0.180318-avmt.car*0.000007496923+(avmt.car^2)*0.0000000001722094,
                                      ifelse(avmt.car<car.lifespan.vmt/lifespan.years+1,
                                             (0.180318-avmt.car*0.000007496923+(avmt.car^2)*0.0000000001722094 + -2.226146-0.00000192881*avmt.car+0.2392061*log(avmt.car))/2,
                                             -2.226146-0.00000192881*avmt.car+0.2392061*log(avmt.car))
                               )
    )
    for(j in 1:length(resale.value.car)){
      if(vehicle.type[j]=='HEV'){
        resale.value.car[j] <- resale.value.hev[j]
      } else if(vehicle.type[j]=='EV'){
        resale.value.car[j] <- resale.value.bev[j]
      }
    }
	
	resale.cash.flow.car <- resale.value.car/((1+discount.rate)^resale.year.car)
	
  if(include.resale!=TRUE){
		resale.cash.flow.car <- resale.cash.flow.car*0
  }
	
    annualized.cost.car <- vehicles$sticker.price*(1-resale.cash.flow.car)*crf.car
    
    annualized.private.cost <- annualized.cost.car
    annualized.externality <- vehicles$upfront.damage.cost*crf.car
    avmt[i,] <- avmt.car
    annualized.private.costs[i,] <- annualized.private.cost
    annualized.externalities[i,] <- annualized.externality
  }
  
  slopes=matrix(as.numeric(NA),nrow=length(breaks)-1, ncol=nrow(vehicles))
  intercepts=matrix(as.numeric(NA),nrow=length(breaks)-1, ncol=nrow(vehicles))
  for(i in 1:(length(breaks)-1)){
    x.bend.0 = avmt[i,]
    x.bend.1 = avmt[i+1,]
    y.bend.0 = annualized.private.costs[i,]
    y.bend.1 = annualized.private.costs[i+1,]
    slopes[i,] <- (y.bend.1-y.bend.0)/(x.bend.1-x.bend.0)
    intercepts[i,] <- y.bend.0-slopes[i,]*x.bend.0
  }
  slopes.external=matrix(as.numeric(NA),nrow=2, ncol=nrow(vehicles))
  intercepts.external=matrix(as.numeric(NA),nrow=2, ncol=nrow(vehicles))
  breaks.external <- c(1,3,4)
  for(i in 1:(length(breaks.external)-1)){
    one <- breaks.external[i]
    two <- breaks.external[i+1]
    x.bend.0 = avmt[one,]
    x.bend.1 = avmt[two,]
    y.bend.0 = annualized.externalities[one,]
    y.bend.1 = annualized.externalities[two,]
    slopes.external[i,] <- (y.bend.1-y.bend.0)/(x.bend.1-x.bend.0)
    intercepts.external[i,] <- y.bend.0-slopes.external[i,]*x.bend.0
  }
  vehicles$capcost.fixed.private <- annualized.private.costs[3,]
  vehicles$capcost.fixed.social <- annualized.private.costs[3,] + annualized.externalities[3] 
  
  #  write.csv(data.frame(x=x,y=y),paste0(num.ice,num.hev,num.ev,discount.rate,costs.to.include,".csv"))
  return(list(vehicles=vehicles,slopes=slopes,intercepts=intercepts,slopes.external=slopes.external,intercepts.external=intercepts.external))
}

buildVehicleArcs <- function(arc.list, vehicles, costs.to.include="private",
                             damage.model=default.damage.model,cr.to.use=default.cr,vsl=default.vsl,carbon.price=default.carbon.price, labor.price=default.labor.price){
  v.a<-setkey(arc.list[,c(k=1,.SD)],k)[vehicles[,c(k=1,.SD)],allow.cartesian=TRUE][,k:=NULL]
  
  v.a[, ':='(
    energy.loss = ifelse(vehicle.type=='EV',energy.loss.ev,
                         ifelse(vehicle.type=='HEV',energy.loss.hev, energy.loss.ice)),
    cost = ifelse(vehicle.type=='EV', cost.ev,
                  ifelse(vehicle.type=='HEV', cost.hev, cost.ice))
  )]
  
  v.a[type=='charge' & vehicle.type=='EV']$energy.loss <- -charge.speed*charge.time/60
  v.a[type=='charge' & vehicle.type %in% c('HEV','ICE')]$energy.loss <- 0
  v.a$labor.price.per.kwh <- 0
  v.a[type=='charge' & vehicle.type=='EV' & hour(start.time) != start.hour.of.day-1,labor.price.per.kwh:=labor.price/charge.speed]
  if(costs.to.include=="both"){
    v.a[type=='purchase', cost := capcost.fixed.social]
  } else{
    v.a[type=='purchase', cost := capcost.fixed.private]
  }
  
  v.a[vehicle.type=='EV' & is.na(energy.loss)]$energy.loss <- 0
  v.a[type!='trip' & (cost<0 | is.na(cost)),cost:=0]
  v.a[type!='trip']$cost = pmax(0, v.a[type!='trip']$cost,na.rm=TRUE)
  
  v.a[, grep("upfront", colnames(v.a)):=NULL]
  v.a[, grep("capcost", colnames(v.a)):=NULL]
  v.a[, grep("battery", colnames(v.a)):=NULL]
  v.a[, grep("sticker.price", colnames(v.a)):=NULL]
  v.a[, grep("total.price", colnames(v.a)):=NULL]
  v.a[, grep("energy.loss.", colnames(v.a)):=NULL]
  v.a[, grep("cost.ev", colnames(v.a)):=NULL]
  v.a[, grep("cost.hev", colnames(v.a)):=NULL]
  v.a[, grep("cost.ice", colnames(v.a)):=NULL]
  v.a[, grep("energy.capacity", colnames(v.a)):=NULL]
  v.a[, grep("initial.energy", colnames(v.a)):=NULL]
  v.a[, grep("purchased", colnames(v.a)):=NULL]
  v.a[,private.cost:=cost]
  v.a[type!='charge',private.cost:=private.cost+labor.price*duration/60]
  appendTripDamages(v.a,damage.model,cr.to.use,vsl,carbon.price)
  if(costs.to.include=="both"){
    v.a[type!='charge',':='(cost=private.cost+total.damages)]
  } else if(costs.to.include=="private"){
    v.a[type!='charge',':='(cost=private.cost)]
  } else if(costs.to.include=="social"){
    v.a[type!='charge',':='(cost=total.damages)]
  }
  v.a[,':='(purchased=as.logical(NA),assigned=as.numeric(NA))]
  v.a <- v.a[order(node1,node2,path.id,vehicle.id)]
  return(v.a)
}


buildVehicleEnergies <- function(arcs,veh){
  veh.energy.states <- data.table(expand.grid(unique(c(arcs$start.time,arcs$end.time)),
                                              veh[vehicle.type=='EV']$vehicle.id))
  setnames(veh.energy.states, c("Var1","Var2"), c("time","vehicle.id"))
  veh.energy.states[veh,on=.(vehicle.id=vehicle.id), energy.capacity := energy.capacity]
  veh.energy.states <- veh.energy.states[order(time,vehicle.id)]
  veh.energy.states$energy.level <- as.numeric(NA)
  return(veh.energy.states)
}

buildModel <- function(in.arcs, in.v.arcs = NULL, in.vehicles = NULL, in.vehicle.energies = NULL, 
                       type = "MIP", costs.to.include="private", car.limit=NA, breakSymmetry=0, demandMandate=1, 
                       force.ev.multiplier = as.numeric(NA), ev.restriction=TRUE, slopes= NULL, intercepts = NULL,
                       slopes.external = NULL, intercepts.external = NULL
){
  
  arcs <- copy(in.arcs)
  v.arcs <- copy(in.v.arcs)
  vehicles <- copy(in.vehicles)
  vehicle.energies <- copy(in.vehicle.energies)
  model = list()
  A <- NULL;
  b <- NULL;
  id <- NULL
  vehicle.ids <- NULL;
  sense <- NULL;
  current.row <- 1;
  arcs[type=='dispatch', start.time:=NA];
  arcs[type=='return', end.time:=NA];
  
  id <- data.table(sort(unique(c(arcs$node1,arcs$node2,
                                 v.arcs$node1,v.arcs$node2))))
  id$rank <- frank(id$V1,ties.method="dense")
  arcs$node1 <- id[arcs, on=c(V1 = "node1")]$rank
  arcs$node2 <- id[arcs, on=c(V1 = "node2")]$rank
  v.arcs$node1 <- id[v.arcs, on=c(V1 = "node1")]$rank
  v.arcs$node2 <- id[v.arcs, on=c(V1 = "node2")]$rank
  if(!is.null(vehicle.energies)){
    #    v.arcs[type=='dispatch', start.time:=NA];
    #    v.arcs[type=='return', end.time:=NA];
  }
  
  vehicle.ids <- data.table(sort(unique(c(vehicles$vehicle.id, 
                                          v.arcs$vehicle.id,
                                          vehicle.energies$vehicle.id))))
  vehicle.ids$rank <- frank(vehicle.ids$V1,ties.method="dense")
  vehicles$vehicle.id <- vehicle.ids[vehicles, on=c(V1 = "vehicle.id")]$rank
  v.arcs$vehicle.id <- vehicle.ids[v.arcs, on=c(V1 = "vehicle.id")]$rank
  num.arcs <- nrow(arcs)
  num.cars <- sum(vehicles$vehicle.count)
  num.total.nodes <- length(unique(c(arcs$node1,arcs$node2)))
  if(!is.null(vehicle.energies)){
    vehicle.energies$vehicle.id <- vehicle.ids[vehicle.energies, on=c(V1 = "vehicle.id")]$rank
    v.e.id <- unique(data.table(V1=vehicle.energies$time,V2=vehicle.energies$vehicle.id),by=c('V1','V2'))
    v.e.id$rank <- frank(v.e.id, V1,V2,ties.method="dense")
    vehicle.energies$v.e.rank <- v.e.id[vehicle.energies, on=c(V1="time",V2="vehicle.id")]$rank
    v.arcs$v.e.rank.node1 <- v.e.id[v.arcs, on=c(V1="start.time",V2="vehicle.id")]$rank
    v.arcs$v.e.rank.node2 <- v.e.id[v.arcs, on=c(V1="end.time",V2="vehicle.id")]$rank
    min.time <- min(vehicle.energies$time)
    max.time <- max(vehicle.energies$time)
    sink.time <- max(unique(v.arcs[type=='return']$end.time))
    time.annualizations <- data.table(time=arcs$start.time,annualized.days=arcs$annualized.days)
    time.annualizations <- unique(rbindlist(list(time.annualizations,
                                                 data.table(time=sink.time,
                                                            annualized.days=max(daily.vol.by.bucket[start.date==max(start.date)]$annualized.days)))))
  }
  if(!is.null(v.arcs)){
    sink.node <- unique(v.arcs[type=='return']$node2)
    node.annualizations <- data.table(node=arcs$node1,annualized.days=arcs$annualized.days)
    node.annualizations <- unique(rbindlist(list(node.annualizations,
                                                 data.table(node=sink.node,
                                                            annualized.days=daily.vol.by.bucket[start.date==max(start.date)]$annualized.days))))
  }
  
  # A matrix structure (left to right:)
  # Assignments(i,j,k) | StateOfCharge(j,k) | Purchases(k)
  assign.start <- 1;
  #if(exists("v.arcs")) setkey(v.arcs,node1,node2,vehicle.id)
  getRows <- function(startRow, rows, termsPerRow) {
    return (startRow+sort(rep(seq(1,rows),times=termsPerRow))-1)
  }
  
  if (type %in% c("MIP","MIP_MinimizeFleet","MIP_MaximizeTrips")){
    range.limited.cars <- which(vehicles$vehicle.type=='EV')
    num.range.limited.cars <- sum(vehicles$vehicle.type=='EV')
    purchase.start <- assign.start + nrow(v.arcs)
    distance.start <- purchase.start+nrow(vehicles)
    capcost.private.start <- distance.start+nrow(vehicles)
    capcost.external.start <- capcost.private.start+nrow(vehicles)
    charge.start <- capcost.external.start+nrow(vehicles)
    charge.change.start <- charge.start + nrow(vehicle.energies);
    
    #Constraint Purchase_Outflow {
    #     IndexDomain: k;
    #     Definition: sum((i,j) | i=1 and j<>1, Assignments(i,j,k)) = Purchases(k);
    # assignments(i,j,k) - purchases(k) = 0
    #  A matrix structure (left to right:)
    # Assignments(i,j,k)| StateOfCharge(j,k) | Purchases(k)
    rows <- sort(v.arcs[type=='dispatch' | type=='in.progress']$vehicle.id) +current.row-1
    rows <- c(rows, getRows(current.row,nrow(vehicles),1))
    # #columns with +1 on each row should be: 
    # #e.g. A(12k), A(13k), ..., A(14jk)
    # #columns with a -1 on each row should be:
    # #NOTE: no row for j=0
    columns <- c(
      order(v.arcs[type=='dispatch' | type=='in.progress']$vehicle.id), # +assignments(ijk)
      seq(purchase.start,purchase.start+nrow(vehicles)-1) # -purchases(k)
    )
    entries <- c(
      rep(1,times=nrow(v.arcs[type=='dispatch' | type=='in.progress'])),
      rep(-1,times=nrow(vehicles))
    )
    new.for.b <- rep(0,times=length(unique(rows)))
    new.for.sense <- rep("=",times=length(unique(rows)))
    
    b <- c(b, c(new.for.b));
    sense <- c(sense,new.for.sense)
    new.for.A = data.frame(cbind(rows, columns, entries));
    A <- rbindlist(list(A,new.for.A), use.names=FALSE, fill=FALSE, idcol=NULL)
    new.for.A <- c(); new.for.b <- c(); new.for.sense <- c();
    rows <- c(); columns <- c(); entries <- c();
    current.row <- max(A$rows)+1
    
    #Constraint DefineDistanceTravelled
    #Distance traveled = sum of distances assigned to each car
    columns <- c(
      which(v.arcs$distance>0), 
      distance.start-1 + vehicles$vehicle.id
    )
    rows <- c(
      v.arcs[distance>0]$vehicle.id + current.row-1,
      vehicles$vehicle.id + current.row-1
    )
    entries <- c(
      v.arcs[distance>0,distance*annualized.days/100],
      rep(-1,times=nrow(vehicles))
    )
    
    new.for.b <- rep(0,times=length(unique(rows)))
    new.for.sense <- rep("=",times=length(unique(rows)))
    
    b <- c(b, c(new.for.b));
    sense <- c(sense,new.for.sense)
    new.for.A = data.frame(cbind(rows, columns, entries));
    A <- rbindlist(list(A,new.for.A), use.names=FALSE, fill=FALSE, idcol=NULL)
    new.for.A <- c(); new.for.b <- c(); new.for.sense <- c();
    rows <- c(); columns <- c(); entries <- c();
    current.row <- max(A$rows)+1
    
    #Constraint DistanceLimitPerCar
    #Distance traveled = sum of distances assigned to each car
    columns <- c(
      purchase.start-1 + vehicles$vehicle.id,
      distance.start-1 + vehicles$vehicle.id
    )
    rows <- c(
      vehicles$vehicle.id + current.row-1,
      vehicles$vehicle.id + current.row-1
    )
    entries <- c(
      rep(car.lifespan.vmt/100,times=nrow(vehicles)),
      rep(-1,times=nrow(vehicles))
    )
    
    new.for.b <- rep(0,times=length(unique(rows)))
    new.for.sense <- rep(">=",times=length(unique(rows)))
    
    b <- c(b, c(new.for.b));
    sense <- c(sense,new.for.sense)
    new.for.A = data.frame(cbind(rows, columns, entries));
    A <- rbindlist(list(A,new.for.A), use.names=FALSE, fill=FALSE, idcol=NULL)
    new.for.A <- c(); new.for.b <- c(); new.for.sense <- c();
    rows <- c(); columns <- c(); entries <- c();
    current.row <- max(A$rows)+1
    
    #Constraint CapCost_LowerBound_Variable_1
    #CapCost >= Distance.Traveled*capcost.variable.slope + Purchase.Variable*capcost.variable.intercept
    #Purchase.Variable*capcost.variable.intercept + Distance.Traveled*capcost.variable.slope - CapCost <= 0
    if(is.null(intercepts)) print ("NULL!!!!")
    for(i in 1:nrow(intercepts)){
      veh.ints <- intercepts[i,]
      veh.slopes <- slopes[i,]
      to.use <- which(!is.na(veh.ints) & !is.na(veh.ints))
      columns <- c(
        purchase.start-1 + vehicles$vehicle.id[to.use],
        distance.start-1 + vehicles$vehicle.id[to.use],
        capcost.private.start-1 + vehicles$vehicle.id[to.use]
      )
      rows <- c(
        vehicles$vehicle.id[to.use] + current.row-1,
        vehicles$vehicle.id[to.use] + current.row-1,
        vehicles$vehicle.id[to.use] + current.row-1
      )
      entries <- c(
        veh.ints[to.use]/100, 
        veh.slopes[to.use],
        rep(-1,times=nrow(vehicles[to.use]))
      )
      
      new.for.b <- rep(0,times=length(unique(rows)))
      new.for.sense <- rep("<=",times=length(unique(rows)))
      
      b <- c(b, c(new.for.b));
      sense <- c(sense,new.for.sense)
      new.for.A = data.frame(cbind(rows, columns, entries));
      A <- rbindlist(list(A,new.for.A), use.names=FALSE, fill=FALSE, idcol=NULL)
      new.for.A <- c(); new.for.b <- c(); new.for.sense <- c();
      rows <- c(); columns <- c(); entries <- c();
      current.row <- max(A$rows)+1
    }
    if(is.null(intercepts.external)) print ("NULL!!!!")
    for(i in 1:nrow(intercepts.external)){
      veh.ints <- intercepts.external[i,]
      veh.slopes <- slopes.external[i,]
      to.use <- which(!is.na(veh.ints) & !is.na(veh.ints))
      columns <- c(
        purchase.start-1 + vehicles$vehicle.id[to.use],
        distance.start-1 + vehicles$vehicle.id[to.use],
        capcost.external.start-1 + vehicles$vehicle.id[to.use]
      )
      rows <- c(
        vehicles$vehicle.id[to.use] + current.row-1,
        vehicles$vehicle.id[to.use] + current.row-1,
        vehicles$vehicle.id[to.use] + current.row-1
      )
      entries <- c(
        veh.ints[to.use]/100,
        veh.slopes[to.use],
        rep(-1,times=nrow(vehicles[to.use]))
      )
      
      new.for.b <- rep(0,times=length(unique(rows)))
      new.for.sense <- rep("<=",times=length(unique(rows)))
      
      b <- c(b, c(new.for.b));
      sense <- c(sense,new.for.sense)
      new.for.A = data.frame(cbind(rows, columns, entries));
      A <- rbindlist(list(A,new.for.A), use.names=FALSE, fill=FALSE, idcol=NULL)
      new.for.A <- c(); new.for.b <- c(); new.for.sense <- c();
      rows <- c(); columns <- c(); entries <- c();
      current.row <- max(A$rows)+1
    }
    
    #Constraint ForceSingleEVPurchase
    #For heuristic runs with a single EV + an aggregated huge EV, force the single EV
    #to be used before the aggregated huge EV.
    #Purchase(singleEV)*(countAggregate/countSingle)>= Purchase(aggregateEV)
    #Purchase(singleEV)*(countAggregate/countSingle) - Purchase(aggregateEV) >= 0
    if(ev.restriction==TRUE & (nrow(vehicles[vehicle.type=='EV'])>1 & nrow(vehicles[vehicle.type=='EV'])<=4)){
      if(length(unique(vehicles[vehicle.type=='EV',energy.capacity]))>1){
        little.ev.id <- vehicles[vehicle.type=='EV' & vehicle.count==1,vehicle.id]
        big.ev.id <- vehicles[vehicle.type=='EV' & vehicle.count!=1,vehicle.id]
      } else{
        little.ev.id <- min(vehicles[vehicle.type=='EV',vehicle.id])
        big.ev.id <- max(vehicles[vehicle.type=='EV',vehicle.id])
      }
      
      all.combos <- expand.grid(little.ev.id,big.ev.id)
      for(i in 1:nrow(all.combos)){
        little.id <- all.combos[i,]$Var1;
        big.id <- all.combos[i,]$Var2;
        big.ev.size <- vehicles[vehicle.id==big.id,energy.capacity]/vehicles[vehicle.id==little.id,energy.capacity]
        new.cols <- c(which(v.arcs$type=='trip' & v.arcs$vehicle.id==little.id),
                      which(v.arcs$type=='trip' & v.arcs$vehicle.id==big.id))
        new.rows <- rep(current.row,times=length(new.cols))
        #        new.entries <- c(big.ev.size*v.arcs[vehicle.id==little.id & type=='trip',energy.loss],
        #                         rep(-1*v.arcs[vehicle.id==big.ev.id & type=='trip',energy.loss]))
        new.entries <- c(rep(big.ev.size,times=nrow(v.arcs[type=='trip' & vehicle.id==big.id])),
                         rep(-1,times=nrow(v.arcs[type=='trip' & vehicle.id==big.id])))
        columns <- c(columns,new.cols)
        rows <- c(rows,new.rows)
        entries<- c(entries,new.entries)
        current.row <- current.row+1
      }
      
      new.for.b <- rep(0,times=length(unique(rows)))
      new.for.sense <- rep('>=',times=length(unique(rows)))
      
      b <- c(b, c(new.for.b));
      sense <- c(sense,new.for.sense)
      new.for.A = data.frame(cbind(rows, columns, entries));
      A <- rbindlist(list(A,new.for.A), use.names=FALSE, fill=FALSE, idcol=NULL)
      new.for.A <- c(); new.for.b <- c(); new.for.sense <- c();
      rows <- c(); columns <- c(); entries <- c();
      current.row <- max(A$rows)+1
    }
    
    #Constraint FlowPreservation {
    #     IndexDomain: (j,k) | j <> 1 and j <> numtrips*2+2;
    #     Definition: sum(i,Assignments(i,j,k)) = sum(i,Assignments(j,i,k));
    # A matrix structure (left to right:)
    # Assignments(i,j,k) | StateOfCharge(j,k) | Purchases(k)
    #}
    rows <- rbindlist(list(
      #rows with +1 should be:
      v.arcs[type=='in.progress',j=list(vehicle.id=vehicle.id, node=node2)],
      v.arcs[type=='dispatch',j=list(vehicle.id=vehicle.id, node=node2)],
      v.arcs[type=='trip',j=list(vehicle.id=vehicle.id, node=node2)],
      v.arcs[type=='charge',j=list(vehicle.id=vehicle.id, node=node2)],
      v.arcs[type=='relocation',j=list(vehicle.id=vehicle.id, node=node2)],
      #rows with -1 should be:
      v.arcs[type=='trip',j=list(vehicle.id=vehicle.id, node=node1)],
      v.arcs[type=='charge',j=list(vehicle.id=vehicle.id, node=node1)],
      v.arcs[type=='return',j=list(vehicle.id=vehicle.id, node=node1)],
      v.arcs[type=='relocation',j=list(vehicle.id=vehicle.id, node=node1)]
    ))
    rows <- frank(rows,node,vehicle.id, ties.method="dense") +current.row-1
    positives <- c(
      #columns with +1 should be:
      which(v.arcs$type=='in.progress'),
      which(v.arcs$type=='dispatch'),
      which(v.arcs$type=='trip'),
      which(v.arcs$type=='charge'),
      which(v.arcs$type=='relocation')
    )
    negatives <- c(
      #rows with -1 should be:
      which(v.arcs$type=='trip'),
      which(v.arcs$type=='charge'),
      which(v.arcs$type=='return'),
      which(v.arcs$type=='relocation')
    )
    columns <- c(positives, negatives)
    entries <-  c(
      rep(1,times=length(positives)), # +a(i,j,k)
      rep(-1,times=length(negatives)) # -a(j,i,k)
    )
    
    new.for.b <- rep(0,times=length(unique(rows)))
    new.for.sense <- rep("=",times=length(unique(rows)))
    b <- c(b, c(new.for.b));
    sense <- c(sense,new.for.sense)
    new.for.A = data.frame(cbind(rows, columns, entries));
    A <- rbindlist(list(A,new.for.A), use.names=FALSE, fill=FALSE, idcol=NULL)
    new.for.A <- c(); new.for.b <- c(); new.for.sense <- c();
    rows <- c(); columns <- c(); entries <- c();
    current.row <- max(A$rows)+1
    
    #Constraint CapacityConstraint {
    #     IndexDomain: (i,j);
    #     Definition: sum(k,Assignments(i,j,k)) <= Demand(i,j);
    #}
    rows <- frank(v.arcs[type=='trip'],node1,node2,path.id,ties.method="dense") +current.row-1
    columns <- which(v.arcs$type=='trip')
    entries <- seq(1,1,length.out=length(rows)); # +assignments(ijk)
    new.for.b <- arcs[arcs$type=='trip',]$capacity
    if(demandMandate==1){
      new.for.sense <- rep("=",times=length(unique(rows)))
    } else{
      new.for.sense <- rep("<=",times=length(unique(rows)))
    }
    b <- c(b, c(new.for.b));
    sense <- c(sense,new.for.sense)
    new.for.A = data.frame(cbind(rows, columns, entries));
    A <- rbindlist(list(A,new.for.A), use.names=FALSE, fill=FALSE, idcol=NULL)
    new.for.A <- c(); new.for.b <- c(); new.for.sense <- c();
    rows <- c(); columns <- c(); entries <- c();
    current.row <- max(A$rows)+1
    
    if(!is.null(vehicle.energies)){
      
      if(vehicles[vehicle.type=='EV',.N]>0){
        #SOC(j,k) - energy.capacity*purchased <= 0
        num.ev.ids <- vehicles[vehicle.type=='EV',.N]
        columns <- c(
          charge.start-1 + vehicle.energies[time==min.time,v.e.rank], #SOC(j,k)
          rep(purchase.start-1 + which(vehicles$vehicle.type=='EV'),times=vehicle.energies[time==min.time,.N]/num.ev.ids) # -energy.capacity*Purchased
        )
        rows <- c(
          current.row-1 + frank(vehicle.energies[time==min.time,v.e.rank],ties.method="dense"),
          current.row-1 + frank(vehicle.energies[time==min.time,v.e.rank],ties.method="dense")
        )
        entries <- c(
          rep(1,times=vehicle.energies[time==min.time,.N]),
          rep(-1*capacity.ev,times=vehicle.energies[time==min.time,.N])
        )
        
        new.for.b <- rep(0,times=length(unique(rows)))
        new.for.sense <- rep("<=",times=length(unique(rows)))
        
        b <- c(b, c(new.for.b));
        sense <- c(sense,new.for.sense)
        new.for.A = data.frame(cbind(rows, columns, entries));
        A <- rbindlist(list(A,new.for.A), use.names=FALSE, fill=FALSE, idcol=NULL)
        new.for.A <- c(); new.for.b <- c(); new.for.sense <- c();
        rows <- c(); columns <- c(); entries <- c();
        current.row <- max(A$rows)+1
		
		#times.to.index <- c(min.time)
		#  | time==max.time
		times.to.index <- unique(vehicle.energies[time==min.time| (day(time)>1 & hour(time)==6 & minute(time)==0),time])
		columns <- c(
			charge.start-1 + vehicle.energies[time==min.time,v.e.rank],
			charge.start-1 + vehicle.energies[time==max.time,v.e.rank]
		#          rep(purchase.start-1 + which(vehicles$vehicle.type=='EV'),times=vehicle.energies[time %in% times.to.index,.N]/num.ev.ids) # -energy.capacity*Purchased
		)
		rows <- c(
			current.row-1 + frank(vehicle.energies[time==min.time,v.e.rank],ties.method="dense"),
			current.row-1 + frank(vehicle.energies[time==max.time,v.e.rank],ties.method="dense")
		) 
		entries <- c(
			rep(1,times=vehicle.energies[time==min.time,.N]),
			rep(-1,times=vehicle.energies[time==max.time,.N])
		)
		new.for.b <- rep(0,times=length(unique(rows)))
	    new.for.sense <- rep("=",times=length(unique(rows)))        
		
        b <- c(b, c(new.for.b));
        sense <- c(sense,new.for.sense)
        new.for.A = data.frame(cbind(rows, columns, entries));
        A <- rbindlist(list(A,new.for.A), use.names=FALSE, fill=FALSE, idcol=NULL)
        new.for.A <- c(); new.for.b <- c(); new.for.sense <- c();
        rows <- c(); columns <- c(); entries <- c();
        current.row <- max(A$rows)+1

        if(vehicles[vehicle.type=='EV' & vehicle.count>1,.N]>0){
          charge.times <- unique(arcs[type=='charge']$start.time)
          all.times <- unique(vehicle.energies$time)
          all.times.charge <- which(all.times %in% charge.times)
          all.times.charge.plus.one <- all.times.charge+1
          charge.times.plus.one <- all.times[all.times.charge.plus.one]
          charge.times <- charge.times.plus.one
          big.ev.ids <- unique(vehicles[vehicle.type=='EV' & vehicle.count>1,vehicle.id])
          columns <- c(
            charge.start-1 + vehicle.energies[time %in% charge.times & vehicle.id %in% big.ev.ids,v.e.rank], #SOC(j,k)
            rep(purchase.start-1 + which(vehicles$vehicle.id %in% big.ev.ids),
                times=vehicle.energies[time %in% charge.times & vehicle.id %in% big.ev.ids,.N]/length(big.ev.ids)) # -energy.capacity*Purchased
          )
          rows <- c(
            current.row-1 + frank(vehicle.energies[time %in% charge.times & vehicle.id %in% big.ev.ids,v.e.rank],ties.method="dense"),
            current.row-1 + frank(vehicle.energies[time %in% charge.times & vehicle.id %in% big.ev.ids,v.e.rank],ties.method="dense")
          )
          entries <- c(
            rep(1,times=vehicle.energies[time %in% charge.times & vehicle.id %in% big.ev.ids,.N]),
            rep(-1*capacity.ev,times=vehicle.energies[time %in% charge.times & vehicle.id %in% big.ev.ids,.N])
          )
          
          new.for.b <- rep(0,times=length(unique(rows)))
          new.for.sense <- rep("<=",times=length(unique(rows)))
          
          b <- c(b, c(new.for.b));
          sense <- c(sense,new.for.sense)
          new.for.A = data.frame(cbind(rows, columns, entries));
          A <- rbindlist(list(A,new.for.A), use.names=FALSE, fill=FALSE, idcol=NULL)
          new.for.A <- c(); new.for.b <- c(); new.for.sense <- c();
          rows <- c(); columns <- c(); entries <- c();
          current.row <- max(A$rows)+1
        }
      }
      
      #Constraint Charge_Change_Ceiling {
      #     IndexDomain: (j,k);
      #     Definition: StateOfCharge(j,k) = 
      #       StateOfCharge(sourceNode,k) +sum(i1i2 where time(i2) <= time(j), Assignments(i1,i2)*ArcEnergyChange(i1,i2,k)
      # A matrix structure (left to right:)
      # Assignments(i,j,k) | StateOfCharge(j,k) as.POSIXct(joined.rides$started_on,format="%Y-%m-%d %H:%M:%OS")
      #}
      time.ranks <- data.table(unique(rbindlist(list(data.table(time=v.arcs$start.time),
                                                     data.table(time=v.arcs$end.time)))))
      time.ranks[, ':='(time.rank = frank(time,ties.method="dense"))]
      time.ranks[, ':='(prior.time.rank = time.rank-1)]
      
      vehicle.energies[time.ranks, on=.(time=time), ':='(time.rank=time.rank,
                                                         prior.time.rank=prior.time.rank)]
      vehicle.energies[vehicle.energies, on=.(vehicle.id=vehicle.id,prior.time.rank=time.rank),prior.time.v.e.rank := i.v.e.rank]
      v.arcs[, v.arc.index := .I]
      v.arcs[time.ranks, on=.(start.time=time), ':='(time.rank.start=i.time.rank,
                                                     prior.time.rank=i.prior.time.rank)]
      v.arcs[time.ranks, on=.(end.time=time), ':='(time.rank.end=i.time.rank)]
      
      test <- merge(v.arcs,vehicle.energies,
                    by.x=c("vehicle.id","time.rank.start"),by.y=c("vehicle.id","prior.time.rank"),
                    allow.cartesian=TRUE
      )
      
      test<-test[,c("vehicle.id","start.time","end.time","node1","node2","v.arc.index","type","energy.loss","time.rank.start","time.rank.end",
                    "time","v.e.rank","time.rank")]
      test[,':='(v.e.time=time,
                 v.e.time.rank=time.rank)]
      test$time <- NULL; test$time.rank <- NULL
      
      temp.ranks = data.table(old.rank = unique(c(vehicle.energies[time.rank!=1]$v.e.rank,test$v.e.rank)))
      temp.ranks$new.rank <- frank(temp.ranks$old.rank,ties.method="dense")
      vehicle.energies[temp.ranks, on=c(v.e.rank="old.rank"), ':='(new.v.e.rank = new.rank)]
      test[temp.ranks, on=c(v.e.rank="old.rank"), ':='(new.v.e.rank = new.rank)]
      
      #SOC(j,k) = SOC(prior) + sum(A(ijk)*energychange(ijk))
      #SOC(j,k) - SOC(prior) - sum(A(ijk)*energychange(ijk)) = 0
      columns <- c(
        charge.start-1 + vehicle.energies[time.rank!=1]$v.e.rank, #SOC(j,k)
        charge.start-1 + vehicle.energies[time.rank!=1]$prior.time.v.e.rank, # -SOC(prior)
        test$v.arc.index# -sum(A(ijk)#energychange(ijk))
      )
      rows <- c(
        current.row-1 + vehicle.energies[time.rank!=1]$new.v.e.rank,
        current.row-1 + vehicle.energies[time.rank!=1]$new.v.e.rank,
        current.row-1 + test$new.v.e.rank
      )
      entries <- c(
        rep(1,vehicle.energies[time.rank!=1,.N]),
        rep(-1,vehicle.energies[time.rank!=1,.N]),
        test$energy.loss
      )
      temp <- data.table(cbind(columns,rows,entries))
      test.grouped <- temp[, .(charges=(min(entries*(columns<charge.start))<0)),by=rows]
      new.for.sense <- ifelse(test.grouped$charges,"<=","=")
      new.for.b <- rep(0,times=length(unique(rows)))
      b <- c(b, c(new.for.b));
      sense <- c(sense,new.for.sense)
      new.for.A = data.frame(cbind(rows, columns, entries));
      A <- rbindlist(list(A,new.for.A), use.names=FALSE, fill=FALSE, idcol=NULL)
      new.for.A <- c(); new.for.b <- c(); new.for.sense <- c();
      rows <- c(); columns <- c(); entries <- c();
      current.row <- max(A$rows)+1
      
      #Constraint DefineTheChargeAmounts
      #SOC(j,k) = SOC(i,k) + sum(A(ijk)*AEC(ijk), notcharge) + Charge(ij,k)
      #Charge(ij,k) + sum(A(ijk)*AEC(ijk), notcharge) = SOC(j,k) - SOC(i,k)
      #SOC(i,k) - SOC(j,k) + sum(A(ijk)*AEC(ijk) + Charge(ij,k) = 0 
      #      for all i starting from start of charge node to just before end of charge,
      #      specifically for charge arcs and EVs
      #      Note: these are charge arcs, so change should be positive
      if(!type %in% c("MIP_MinimizeFleet") & v.arcs[vehicle.type=='EV',.N]){
        charge.arcs <- v.arcs[type=='charge' & vehicle.type=='EV']
        charge.arcs$charge.arc.rank <- frank(charge.arcs$v.arc.index, ties.method="dense")
        v.arcs.copy <- v.arcs
        v.arcs.copy[charge.arcs, on=.(vehicle.id=vehicle.id,
                                      time.rank.start>=time.rank.start,
                                      time.rank.start<time.rank.end), allow.cartesian=TRUE,
                    ':='(charge.arc.rank = i.charge.arc.rank,
                         v.e.rank.node1 = v.e.rank.node1,
                         v.e.rank.node2 = v.e.rank.node2,
                         arc.start.time = start.time, 
                         arc.end.time = end.time,
                         arc.start.time.rank = time.rank.start,
                         arc.end.time.rank = time.rank.end,
                         charge.start.time.rank = i.time.rank.start,
                         charge.end.time.rank = i.time.rank.end,
                         charge.start.time = i.start.time,
                         charge.end.time = i.end.time,
                         arc.energy.loss = energy.loss)]
        v.arcs.copy <- v.arcs.copy[type!='charge' & vehicle.type=='EV' &
                                     arc.start.time.rank >= min(charge.arcs$time.rank.start) &
                                     arc.start.time.rank < max(charge.arcs$time.rank.end)]
        v.arcs.copy <- v.arcs.copy[order(charge.arc.rank),]
        
        columns <- c(
          charge.start-1 + charge.arcs$v.e.rank.node1,
          charge.start-1 + charge.arcs$v.e.rank.node2,
          v.arcs.copy$v.arc.index,
          charge.change.start-1 + charge.arcs$charge.arc.rank
        )
        rows <- c(
          current.row-1 + charge.arcs$charge.arc.rank,
          current.row-1 + charge.arcs$charge.arc.rank,
          current.row-1 + v.arcs.copy$charge.arc.rank,
          current.row-1 + charge.arcs$charge.arc.rank
        )
        entries <- c(
          rep( 1,times=nrow(charge.arcs)),
          rep(-1,times=nrow(charge.arcs)),
          -v.arcs.copy$energy.loss,
          rep( 1,times=nrow(charge.arcs))
        )
        new.for.sense <- rep("=",times=length(unique(rows)))
        new.for.b <- rep(0,times=length(unique(rows)))
        sense <- c(sense,new.for.sense)
        b <- c(b, c(new.for.b));
        new.for.A = data.frame(cbind(rows, columns, entries));
        A <- rbindlist(list(A,new.for.A), use.names=FALSE, fill=FALSE, idcol=NULL)
        new.for.A <- c(); new.for.b <- c(); new.for.sense <- c();
        rows <- c(); columns <- c(); entries <- c();
        current.row <- max(A$rows)+1
      }
    }  
    
    if(type=="MIP_MinimizeFleet"){
      columns <- c(which(v.arcs$type=='dispatch'),
                   purchase.start)
      rows <- rep(current.row, times=v.arcs[type=='dispatch',.N]+1)
      entries <- c(rep(1,times=v.arcs[type=='dispatch',.N]),
                   -1
      )
      new.for.sense <- rep("=",times=length(unique(rows)))
      new.for.b <- rep(0,times=length(unique(rows)))
      sense <- c(sense,new.for.sense)
      b <- c(b, c(new.for.b));
      new.for.A = data.frame(cbind(rows, columns, entries));
      A <- rbindlist(list(A,new.for.A), use.names=FALSE, fill=FALSE, idcol=NULL)
      new.for.A <- c(); new.for.b <- c(); new.for.sense <- c();
      rows <- c(); columns <- c(); entries <- c();
      current.row <- max(A$rows)+1
    }
    
    if(breakSymmetry==1){
      #Constraint SymmetryBreak
      #purchase(k) > purchase(k+1)
      min.ev.id <- min(vehicles[vehicle.type=='EV']$vehicle.id)
      max.ev.id <- max(vehicles[vehicle.type=='EV']$vehicle.id)
      rows <- c(
        v.arcs[type=='dispatch' & vehicle.type=='EV' & vehicle.id!=max.ev.id]$vehicle.id-min.ev.id+1 +current.row-1,
        v.arcs[type=='dispatch' & vehicle.type=='EV' & vehicle.id!=max.ev.id]$vehicle.id-min.ev.id+1 +current.row-1
      )
      columns <- c(
        which(v.arcs$type=='dispatch' & v.arcs$vehicle.type=='EV' & v.arcs$vehicle.id!=max.ev.id),
        which(v.arcs$type=='dispatch' & v.arcs$vehicle.type=='EV' & v.arcs$vehicle.id!=min.ev.id)
      )
      entries <- c(
        rep(1, times=length(columns)/2),
        rep(-1, times=length(columns)/2)
      )
      new.for.sense <- rep(">=",times=length(unique(rows)))
      new.for.b <- rep(0,times=length(unique(rows)))
      b <- c(b, c(new.for.b));
      sense <- c(sense,new.for.sense)
      new.for.A = data.frame(cbind(rows, columns, entries));
      A <- rbindlist(list(A,new.for.A), use.names=FALSE, fill=FALSE, idcol=NULL)
      new.for.A <- c(); new.for.b <- c(); new.for.sense <- c();
      rows <- c(); columns <- c(); entries <- c();
      current.row <- max(A$rows)+1
      
      min.ev.id <- min(vehicles[vehicle.type=='EV']$vehicle.id)
      max.ev.id <- max(vehicles[vehicle.type=='EV']$vehicle.id)
      rows <- c(
        v.arcs[vehicle.type=='EV' & vehicle.id!=max.ev.id]$vehicle.id-min.ev.id+1 +current.row-1,
        v.arcs[vehicle.type=='EV' & vehicle.id!=max.ev.id]$vehicle.id-min.ev.id+1 +current.row-1
      )
      columns <- c(
        which(v.arcs$vehicle.type=='EV' & v.arcs$vehicle.id!=max.ev.id),
        which(v.arcs$vehicle.type=='EV' & v.arcs$vehicle.id!=min.ev.id)
      )
      entries <- c(
        v.arcs[vehicle.type=='EV' & vehicle.id!=max.ev.id]$distance,
        v.arcs[vehicle.type=='EV' & vehicle.id!=max.ev.id]$distance
      )
      new.for.sense <- rep(">=",times=length(unique(rows)))
      new.for.b <-  rep(0,times=length(unique(rows)))
      b <- c(b, c(new.for.b));
      sense <- c(sense,new.for.sense)
      new.for.A = data.frame(cbind(rows, columns, entries));
      A <- rbindlist(list(A,new.for.A), use.names=FALSE, fill=FALSE, idcol=NULL)
      new.for.A <- c(); new.for.b <- c(); new.for.sense <- c();
      rows <- c(); columns <- c(); entries <- c();
      current.row <- max(A$rows)+1
    }
    
    model$vtype      <- c(
      ifelse(v.arcs$vehicle.count>1,'I','B'), #assignments
      ifelse(vehicles$vehicle.count>1,'I','B'), #purchases
      rep('C',times=nrow(vehicles)*3), #distances & both capcosts
      rep('C',times=nrow(vehicle.energies)), #charges
      rep('C', times=v.arcs[vehicle.type=='EV' & type=='charge',.N]) #charge changes
    )
    
	temp1 <- ifelse(is.na(vehicle.energies$energy.level),0,vehicle.energies$energy.level)
    model$lb <- c(
      ifelse(is.na(v.arcs$assigned),0,v.arcs$assigned), #assignments
      ifelse(is.na(vehicles$purchased),0,vehicles$purchased), #purchases
      rep(0,times=nrow(vehicles)*3), #distances & both capcosts
	  temp1,
      #ifelse(is.na(vehicle.energies$energy.level),0,vehicle.energies$energy.level), #charges
      rep(0,times=v.arcs[vehicle.type=='EV' & type=='charge',.N]) #charge changes
    )
    
    temp2 <- vehicle.energies$energy.capacity
    temp2[!is.na(vehicle.energies$energy.level)] <- vehicle.energies[!is.na(vehicle.energies$energy.level),]$energy.level
    
    model$ub <- c(
      ifelse(is.na(v.arcs$assigned),v.arcs$vehicle.count,v.arcs$assigned), #assignments
      ifelse(is.na(vehicles$purchased),vehicles$vehicle.count,vehicles$purchased), #purchases
      rep(Inf,times=nrow(vehicles)*3), #distances & both capcosts
      temp2, #charges
      rep(Inf,times=v.arcs[vehicle.type=='EV' & type=='charge',.N]) #charge changes
    )
    
    if(type=="MIP_MinimizeFleet"){
      model$vtype <- c(model$vtype[1:(nrow(v.arcs)+nrow(vehicle.energies))],'I')
      model$lb <- c(model$lb[1:(nrow(v.arcs)+nrow(vehicle.energies))],0)
      model$ub <- c(model$ub[1:(nrow(v.arcs)+nrow(vehicle.energies))], 10000)
    }
    
    if(demandMandate!=1){
      v.arcs[type=='trip']$cost <- v.arcs[type=='trip']$cost-unmet.demand.penalty*30000
    }
    # Objective: sum((ijk), Assignments(ijk)*ArcCost(ijk)) + sum((k),Purchases(k)*InvestmentCost(k))";
    
    if(type=="MIP_MinimizeFleet"){
      model$obj <- c(
        rep(0, times=(nrow(v.arcs)+nrow(vehicle.energies))),
        1)
    } else {
      v.a.temp <- v.arcs
      v.a.temp[,nocharge.damages:=total.damages][type=='charge',nocharge.damages:=0]
      max.charge.time <- max(v.arcs[vehicle.type=='EV' & type=='charge',start.time],na.rm=TRUE)
      final.charge.damages <- max(v.arcs[vehicle.type=='EV' & type=='charge' & start.time==max.charge.time]$total.damages)
      if(costs.to.include=="private"){
        model$obj        <- c(v.a.temp[,private.cost],        
                              rep(0,times=nrow(vehicles)), #purchases
                              rep(0,times=nrow(vehicles)), #distances
                              rep(100, times=nrow(vehicles)), #private capcosts
                              rep(0.0001, times=nrow(vehicles)), #external capcosts
                              rep(0,times=vehicle.energies[time!=max.charge.time,.N]), #charges,
                              rep(-electricity.price,times=vehicle.energies[time==max.charge.time,.N]), #charges
                              rep(electricity.price,times=v.arcs[vehicle.type=='EV' & type=='charge',.N]) + v.arcs[vehicle.type=='EV' & type=='charge']$labor.price.per.kwh #charge changes
        )
      } else if(costs.to.include=="both"){
        model$obj        <- c(v.a.temp[,private.cost+nocharge.damages],
                              rep(0,times=nrow(vehicles)), #purchases
                              rep(0,times=nrow(vehicles)), #distances
                              rep(100, times=nrow(vehicles)), #private capcosts
                              rep(100, times=nrow(vehicles)), #external capcosts
                              rep(0,times=nrow(vehicle.energies[time!=max.charge.time])), #charges,
                              rep(-electricity.price-final.charge.damages,times=nrow(vehicle.energies[time==max.charge.time])), #charges
                              rep(electricity.price,times=v.arcs[vehicle.type=='EV' & type=='charge',.N]) + v.arcs[vehicle.type=='EV' & type=='charge']$labor.price.per.kwh + v.arcs[vehicle.type=='EV' & type=='charge']$total.damages) #charge changes      
      } 
      
      #Annualization
      model$obj[1:nrow(v.arcs)] <- model$obj[1:nrow(v.arcs)]*v.arcs$annualized.days
      if(!is.null(vehicle.energies) & nrow(vehicle.energies)>0){
        vehicle.energies[time.annualizations,on=.(time=time), annualized.days:=i.annualized.days]
        #model$obj[charge.start:(charge.change.start-1)] <- model$obj[charge.start:(charge.change.start-1)]*vehicle.energies$annualized.days
        model$obj[charge.change.start:(charge.change.start+charge.arcs[,.N]-1)] <- 
          model$obj[charge.change.start:(charge.change.start+charge.arcs[,.N]-1)]*charge.arcs$annualized.days
      }
    }
    
  } else if (type=="MinCostFlow" | type=="MinCostFlow_CarLimit"){
    #Constraint CapacityConstraint {
    #     IndexDomain: (i,j);
    #     Definition: sum(k,Assignments(i,j,k)) <= Demand(i,j);
    #}
    num.trip <- sum(arcs$type=='trip')
    not.trip <- sum(arcs$type!='trip')
    
    rows <- getRows(current.row,num.trip, 1)
    columns <- which(arcs$type=='trip')
    entries <- seq(1,1,length.out=length(rows)) # +assignments(ijk)
    new.for.b <- c(arcs[arcs$type=='trip',]$capacity)
    if(type=="MinCostFlow"){
      new.for.sense <- rep("=",times=length(unique(rows)))
    } else{
      new.for.sense <- rep("<=",times=length(unique(rows)))
    }
    b <- c(b, c(new.for.b));
    sense <- c(sense,new.for.sense)
    new.for.A = data.frame(cbind(rows, columns, entries));
    A <- rbindlist(list(A,new.for.A), use.names=FALSE, fill=FALSE, idcol=NULL)
    new.for.A <- c(); new.for.b <- c(); new.for.sense <- c();
    rows <- c(); columns <- c(); entries <- c();
    current.row <- max(A$rows)+1
    
    rows <- getRows(current.row,not.trip, 1)
    columns <- which(arcs$type!='trip')
    entries <- seq(1,1,length.out=length(rows)) # +assignments(ijk)
    new.for.b <- c(arcs[arcs$type!='trip',]$capacity)
    new.for.sense <- rep("<=",times=length(unique(rows)))
    b <- c(b, c(new.for.b));
    sense <- c(sense,new.for.sense)
    new.for.A = data.frame(cbind(rows, columns, entries));
    A <- rbindlist(list(A,new.for.A), use.names=FALSE, fill=FALSE, idcol=NULL)
    new.for.A <- c(); new.for.b <- c(); new.for.sense <- c();
    rows <- c(); columns <- c(); entries <- c();
    current.row <- max(A$rows)+1
    
    #Constraint FlowPreservation {
    #     IndexDomain: (j,k) | j <> 1 and j <> numtrips*2+2;
    #     Definition: sum(i,Assignments(i,j,k)) = sum(i,Assignments(j,i,k));
    # A matrix structure (left to right:)
    # Assignments(i,j,k) | StateOfCharge(j,k) | Purchases(k)
    #}
    
    #NOTE: no row exists for j=1, j=14
    rows <- c(
      #rows with +1 should be:
      arcs[arcs$type=='dispatch',]$node2-1,
      arcs[arcs$type=='trip',]$node2-1,
      arcs[arcs$type=='charge',]$node2-1,
      arcs[arcs$type=='relocation',]$node2-1,
      #rows with -1 should be:
      arcs[arcs$type=='trip',]$node1-1,
      arcs[arcs$type=='charge',]$node1-1,
      arcs[arcs$type=='return',]$node1-1,
      arcs[arcs$type=='relocation',]$node1-1
    ) + current.row -1
    positives <- c(
      #columns with +1 should be:
      which(arcs$type=='dispatch'),
      which(arcs$type=='trip'),
      which(arcs$type=='charge'),
      which(arcs$type=='relocation')
    )
    negatives <- c(
      #rows with -1 should be:
      which(arcs$type=='trip'),
      which(arcs$type=='charge'),
      which(arcs$type=='return'),
      which(arcs$type=='relocation')
    )
    columns <- c(positives, negatives)
    entries <- c(
      rep(1,times=length(positives)), # +a(i,j,k)
      rep(-1,times=length(negatives)) # -a(j,i,k)
    )
    
    new.for.b <- rep(0,times=length(unique(rows)))
    new.for.sense <- rep("=",times=length(unique(rows)))
    b <- c(b, c(new.for.b));
    sense <- c(sense,new.for.sense)
    new.for.A = data.frame(cbind(rows, columns, entries));
    A <- rbindlist(list(A,new.for.A), use.names=FALSE, fill=FALSE, idcol=NULL)
    new.for.A <- c(); new.for.b <- c(); new.for.sense <- c();
    rows <- c(); columns <- c(); entries <- c();
    current.row <- max(A$rows)+1
    
    if (type=="MinCostFlow_CarLimit" & car.limit < num.cars){
      columns <- c(
        #columns with +1 should be:
        which(arcs$type=='dispatch')
      )
      rows <- rep(current.row,times=length(columns))
      entries <- c(
        rep(1,times=length(columns)) # +a(i,j,k)
      )
      new.for.b <- min(num.cars,car.limit)
      new.for.sense <- "<="
      b <- c(b, c(new.for.b));
      sense <- c(sense,new.for.sense)
      new.for.A = data.frame(cbind(rows, columns, entries));
      A <- rbindlist(list(A,new.for.A), use.names=FALSE, fill=FALSE, idcol=NULL)
      new.for.A <- c(); new.for.b <- c(); new.for.sense <- c();
      rows <- c(); columns <- c(); entries <- c();
      current.row <- max(A$rows)+1
    }
    
    model$vtype      <- c(
      rep('C',times=num.arcs)
    )
    model$lb <- c(
      rep(0,times=num.arcs)
    )
    model$obj <- arcs$cost
    model$obj <- model$obj*arcs$annualized.days
  }
  
  model$A          <- sparseMatrix(i=A$rows,j=A$columns,x=A$entries)
  model$modelsense <- 'min'
  model$rhs        <- b
  model$sense      <- sense

  return(model)
}

runGurobi <- function(model, type="MIP", time.limit=3600*31, heuristic=0.05, meth=3, cross=-1, crossbasis=0, mip.tol=1e-3, mip.tol.abs=5,
                      log.to.console=1, scale.flag=2,int.feas.tol=1e-6,aggregate=1,feas.tol=1e-6,presolve=2,mip.focus=1){
  start.time <- Sys.time()
  if(type=="MIP"){
    gc();
    solution <- gurobi(model, params = list(
      #LogFile=paste0("/pylon5/dd5fp4p/mbruchon//Out/",format(Sys.time(), "%Y%m%d_%H%M%S_"), "log.txt"),
      LogToConsole=log.to.console,
      Method=meth, #3=automatic concurrent, 4=deterministic concurrent, 2=barrier, 5=deterministic simplex
      Heuristics=heuristic, #%of time target
      Threads=cores.available,
      PreDepRow=1,
      Presolve=presolve,
      PreDual=2,
      PrePasses=20,
      PreSparsify=1,
      MIPGap=mip.tol,
      MIPGapAbs=mip.tol.abs,
      #      ConcurrentMIP=cores.available,
      DisplayInterval=1,
      MIPFocus=mip.focus,
      MarkowitzTol=0.6,
      Cuts=2,
      CutPasses=500,
      CutAggPasses=1,
      Symmetry=2,
      Aggregate=aggregate,
      NumericFocus=2,
      ScaleFlag=2,
      Crossover=cross,
      CrossoverBasis=crossbasis,
      SimplexPricing=1,
      TimeLimit=time.limit,
      IntFeasTol=int.feas.tol,
      FeasibilityTol=feas.tol,
	  NodefileStart=100
    ))
  }
  else if(type=="MIP_LightPresolve"){
    gc();
    solution <- gurobi(model, params = list(
      #LogFile=paste0("/pylon5/dd5fp4p/mbruchon//Out/",format(Sys.time(), "%Y%m%d_%H%M%S_"), "log.txt"),
      Method=meth, #3=automatic concurrent, 4=deterministic concurrent, 2=barrier, 5=deterministic simplex
      Heuristics=heuristic, #%of time target
      Threads=cores.available,
      Presolve=1,
      Presolve=0,
      MIPGap=mip.tol,
      MIPGapAbs=mip.tol.abs,
      DisplayInterval=1,
      MIPFocus=1,
      MarkowitzTol=0.2,
      Symmetry=2,
      Aggregate=1,
      NumericFocus=2,
      Crossover=cross,
      CrossoverBasis=crossbasis,
      SimplexPricing=1,
      TimeLimit=time.limit
    ))
  } else {
    solution <- gurobi(model,params = list(
      #DualReductions=0, #only use 0 if debugging INF_OR_UNBOUND; this makes presolve useless
      Method=3, #automatic concurrent
      Threads=cores.available, #,
      PreDepRow=1,
      PreDual=2,
      #      Presolve=2#,
      OutputFlag=0,
      Crossover=0,
      NodeMethod=2
    ))
  }
  
  return(solution)
  Sys.time()-start.time
}


mcf_Deterministic <- function(arc.list,vehicles, v.type, weighted=TRUE, costs.to.include="private",
                              damage.model=default.damage.model,cr.to.use=default.cr,carbon.price=default.carbon.price,vsl=default.vsl,labor.price=default.labor.price){
  
  v.arcs.temp <- buildVehicleArcs(arc.list,vehicles[min(which(vehicles$vehicle.type==v.type))],costs.to.include,
                                  damage.model, cr.to.use,vsl,carbon.price,labor.price)
  if(v.type=='EV'){
    new.costs <- v.arcs.temp$energy.loss
  } else{
    
    if(costs.to.include=="private"){
      new.costs        <- v.arcs.temp$private.cost
      new.costs[arc.list$type=='dispatch'] <- new.costs[arc.list$type=='dispatch'] + max(vehicles[vehicle.type==v.type]$capcost.fixed.private)
    } else if(costs.to.include=="both"){
      new.costs        <- v.arcs.temp[,private.cost+total.damages]
      new.costs[arc.list$type=='dispatch'] <- new.costs[arc.list$type=='dispatch'] + max(vehicles[vehicle.type==v.type]$capcost.fixed.social)
    } else if(costs.to.include=="social"){
      new.costs        <- v.arcs.temp[,total.damages]
    }
  }
  
  model <- buildModel(arc.list, in.v.arcs = NULL, in.vehicles=vehicles, costs.to.include=costs.to.include, type="MinCostFlow")
  
  if(weighted==TRUE){
    new.costs[arc.list$type=='trip'] <- rescale(-new.costs[arc.list$type=='trip'])*-5000-5000
    new.costs[arc.list$type=='dispatch'] <- rescale(new.costs[arc.list$type=='dispatch'])*20000+20000
    new.costs[arc.list$type=='relocation'] <- rescale(new.costs[arc.list$type=='relocation'])*50+50
  }
  
  model$obj <- new.costs
  result <- runGurobi(model,"MinCostFlow")
  return(matrix(result$x,nrow=1))
}

mcf_VaryingFleetSize <- function(arc.list, vehicles, min.fleet.size=8, weighted=TRUE, costs.to.include="private",
                                 damage.model=default.damage.model,cr.to.use=default.cr,carbon.price=default.carbon.price,vsl=default.vsl,labor.price=default.labor.price){
  num.cars <- max(vehicles$vehicle.count)
  num.simulations <- num.cars
  simulations = c()
  results = data.table()
  num.iterations=4
  for(k in 1:num.iterations){
    for(i in (floor(log2(num.cars))+1):(floor(log2(min.fleet.size)))){
      arc.list.temp <- arc.list
      for(j in 1:num.simulations){
        model <- buildModel(in.arcs=arc.list.temp, in.v.arcs=NULL, in.vehicles=vehicles, 
                            in.vehicle.energies=NULL, type="MinCostFlow_CarLimit", costs.to.include=costs.to.include,car.limit=min(2^i,num.cars))
        set.seed(k+i+j)
        rand.type <- sample(unique(vehicles$vehicle.type),1)
        
        v.arcs.temp <- buildVehicleArcs(arc.list,vehicles[min(which(vehicles$vehicle.type==rand.type))],costs.to.include,
                                        damage.model, cr.to.use,vsl,carbon.price,labor.price)
        if(rand.type=='EV'){
          new.costs <- v.arcs.temp$energy.loss
        } else{
          if(costs.to.include=="private"){
            new.costs        <- v.arcs.temp$private.cost
          } else if(costs.to.include=="both"){
            new.costs        <- v.arcs.temp[,private.cost+total.damages]
          } else if(costs.to.include=="social"){
            new.costs        <- v.arcs.temp[,total.damages]
          }
        }
        
        if(weighted==TRUE){
          new.costs[arc.list.temp$type=='trip'] <- rescale(new.costs[arc.list.temp$type=='trip'])*-50000-50000
          new.costs[arc.list.temp$type=='purchase'] <- rescale(new.costs[arc.list.temp$type=='purchase'])*500+500
          new.costs[arc.list.temp$type=='relocation'] <- rescale(new.costs[arc.list.temp$type=='relocation'])*50+50
        }
        model$obj <- new.costs
        result <- runGurobi(model,"MinCostFlow_CarLimit")
        simulations <- rbind(simulations,result$x)
        
        cat(paste("VaryingFleetSize_Weighted (round ",k,", fleet size=",min(2^i,num.cars),
                  ", iteration ",j,")\n", sep=""))
        results <- rbind(results, data.frame(
          simultaneous.cars = min(2^i,num.cars),
          iter = j, 
          obj = result$objval,
          runtime = result$runtime, 
          trip.arcs = sum(colSums(simulations>0)/nrow(simulations) > 0 & arc.list$type=='trip'),
          reloc.arcs = sum(colSums(simulations>0)/nrow(simulations) > 0 & arc.list$type=='relocation')
        ))
        #      print(results)
        if(sum(result$x>0)==0) break
        arc.list.temp$result <- result$x
        
        arc.list.temp[type!='no_dispatch' & result>0, ':='(capacity=as.integer(capacity-result))]
      }
    }
  }
  
  sims <<- simulations
  
  #  write.csv(results, paste0(format(Sys.time(), "%Y%m%d_%H%M_"), "mcf_summary.csv"))
  #  write.csv(simulations, paste0(format(Sys.time(), "%Y%m%d_%H%M_"), "mcf_sims.csv"))
  return(simulations)
}

mcf_RandomArcCosts <- function(arc.list, vehicles, bounds=0.25, weighted=TRUE,costs.to.include="private",
                               damage.model=default.damage.model,cr.to.use=default.cr,carbon.price=default.carbon.price,vsl=default.vsl,labor.price=default.labor.price){
  model <- buildModel(arc.list, in.vehicles=vehicles, type="MinCostFlow")
  num.EVs <- length(unique(vehicles[vehicles$vehicle.type=="EV",]$vehicle.id))
  num.cars <- length(unique(vehicles$vehicle.id))
  fraction.EVs <- num.EVs/num.cars
  
  simulations <- c()
  for(i in 1:num.cars){
    rand <- runif(1,0,1)
    if(rand>(1/2)){
      new.costs <- arc.list.temp$cost.ice
    } else {
      new.costs <- arc.list.temp$cost.hev
    }
    if(weighted==TRUE){
      new.costs[arc.list$type=='trip'] <- rescale(-new.costs[arc.list$type=='trip'])*-5000-5000
      new.costs[arc.list$type=='purchase'] <- rescale(new.costs[arc.list$type=='purchase'])*500+500
      new.costs[arc.list$type=='relocation'] <- rescale(new.costs[arc.list$type=='relocation'])*50+50
    }
    model$obj <- new.costs + new.costs*runif(length(new.costs),-bounds,bounds)
    result <- runGurobi(model,"MinCostFlow")
    simulations <- rbind(simulations,result$x)
    
    #if(i%%10==0 | i==num.cars) cat(paste("..", i, "..", sep=""))
  }
  return(simulations) 
}

improveSolution <- function(model, vehicle.arcs, vehicle.energy.states, start, iterations){
  v.arcs <- copy(vehicle.arcs)
  v.e.states <- copy(vehicle.energy.states)
  for(i in 1:iterations){
    this.model <- model
    v.arcs$this.solution <- as.numeric(NA)
    v.arcs$this.solution <- start[1:nrow(v.arcs)]
    used.cars <- unique(v.arcs[v.arcs$this.solution>0 & v.arcs$vehicle.type=='EV']$vehicle.id)
    cars.to.reuse <- used.cars[sample(1:length(used.cars),floor(0.9*length(used.cars)))]
    v.arcs[!data.table(V1=cars.to.reuse),on=c(vehicle.id="V1"), this.solution := as.numeric(NA)]
    #    v.arcs[sample(which(v.arcs$this.solution>0 & vehicle.type=='EV'),floor((0.5)*length(which(v.arcs$this.solution>0 & vehicle.type=='EV')))),
    #           ]$this.solution <- as.numeric(NA)
    #   v.arcs[this.solution==0]$this.solution <- as.numeric(NA)
    #    v.arcs[vehicle.type!='EV']$this.solution <- as.numeric(NA)
    #    v.arcs[sample(which(v.arcs$this.solution>0),floor((1-0.25)*length(which(v.arcs$this.solution>0)))),
    #           ]$this.solution <- as.numeric(NA)
    this.model$lb[which(!is.na(v.arcs$this.solution))] <- v.arcs[!is.na(this.solution)]$this.solution
    this.model$ub[which(!is.na(v.arcs$this.solution))] <- v.arcs[!is.na(this.solution)]$this.solution
    this.model$start <- c(start[1:nrow(v.arcs)],
                          rep(as.numeric(NA),times=nrow(v.e.states))#,
                          #rep(as.numeric(NA),times=length(unique(v.arcs$vehicle.id)))
    )
    start <- (runGurobi(this.model,type="MIP",time.limit=180,heuristic=0.5, mip.tol=1e-3))$x
  }
  return(start)
}

computeMinFeasibleFleet <- function(arcs, type){
  trips <- arcs[type=='trip']
  trips$min.15 <- align.time(trips$start.time,60*15)
  max.15min.volume <- max(trips[, 
                                j=list(demand = sum(capacity,na.rm=TRUE)), 
                                by = list(day(start.time),hour(start.time),minute(min.15))]$demand)
  done <- FALSE
  within.5 <- FALSE
  fleet.size <- max.15min.volume*2
  while(done==FALSE){
    if(type=='EV'){
      buildVeh.out <- buildVehicles(0,0,fleet.size,fleet.size)
    } else if(type=='HEV'){
      buildVeh.out <- buildVehicles(0,fleet.size,0,0)
    } else {
      buildVeh.out <- buildVehicles(fleet.size,0,0,0)
    }
    vehicles <-buildVeh.out$vehicles
    v.arcs <- buildVehicleArcs(arcs,vehicles)
    vehicle.energy.states <- buildVehicleEnergies(arcs,vehicles)
    
    model <- buildModel(arcs,v.arcs, vehicles, vehicle.energy.states, "MIP_MinimizeFleet")
    result <- runGurobi(model,heuristic=0.9,mip.tol=1.0)
    if(result$status=="INFEASIBLE"){
      fleet.size <- fleet.size + ifelse(within.5,1,5)
    } else{
      if(within.5){
        done <- TRUE
      } else{
        within.5 <- TRUE
        fleet.size <- fleet.size - 4
      }
    }
  }
}

tripChainHeuristic <- function(arcs, veh.arcs, vehicles, veh.e.states){
  
  arcs$mcf.det.used <- c(mcf_Deterministic(arcs, vehicles, "EV"))
  #  mcf.1 <- c(mcf_Deterministic(arcs, vehicles, "EV",weighted=TRUE))
  #  mcf.2 <- c(mcf_Deterministic(arcs, vehicles, "ICE",weighted=TRUE))
  #  mcf.3 <- c(mcf_Deterministic(arcs, vehicles, "EV",weighted=FALSE))
  #  mcf.4 <- c(mcf_Deterministic(arcs, vehicles, "ICE",weighted=FALSE))
  #  arcs$mcf.det.used <- pmax(mcf.1,mcf.2,mcf.3,mcf.4)
  used.arcs <- arcs[arcs$mcf.det.used>0,]
  used.arcs$start.count <- used.arcs[, start.count := .N, by = node1]$start.count
  used.arcs$end.count <- used.arcs[, end.count := .N, by = node2]$end.count
  used.arcs$new.node1 = used.arcs$node1; used.arcs$new.node2 = used.arcs$node2; 
  new.id = max(c(arcs$node1,arcs$node2))+1
  used.arcs$superpath.id = used.arcs$path.id
  i=max(used.arcs$superpath.id)+1
  while(nrow(used.arcs[type!='dispatch' & end.count==1])!=0 & 
        nrow(used.arcs[type!='dispatch' & start.count==1])!=0 &
        nrow(used.arcs[type!='return' & start.count==1])!=0 & 
        nrow(used.arcs[type!='purchase' & end.count==1])!=0){
    #Get nodes which have both end.count == 1 AND start.count ==1 
    not.start.end <- used.arcs[type!='dispatch' & type !='return' & type !='purchase']
    end.counts <- not.start.end[, j=list(end.count = .N), by=new.node2]
    start.counts <- not.start.end[, j=list(start.count = .N), by=new.node1]
    
    is.start.end <- used.arcs[type=='dispatch' | type =='return' | type =='purchase']
    start.end.nodes <- data.table(node=unique(c(is.start.end$new.node1,is.start.end$new.node2)))
    
    all.counts <- data.table(node=unique(c(not.start.end$new.node1,not.start.end$new.node2)))
    all.counts <- all.counts[!start.end.nodes, on=c(node="node")]
    all.counts <- end.counts[all.counts, on=c(new.node2="node")]
    all.counts <- start.counts[all.counts, on=c(new.node1="new.node2")]
    all.counts <- all.counts[start.count==1 & end.count==1]
    all.counts <- all.counts[order(new.node1)]
    if(nrow(all.counts)==0){
      break;
    } else{
      node.to.fix <- all.counts[1]$new.node1; start <- node.to.fix; end <- node.to.fix
      indices <- c()
      while(length(which(used.arcs$type!='dispatch' & used.arcs$type!='return' & used.arcs$type!='purchase' & 
                         (used.arcs$node1==start | used.arcs$node2==start)))==2 &
            length(which(used.arcs$type!='dispatch' & used.arcs$type!='return' & used.arcs$type!='purchase' & used.arcs$node2==start))==1 &
            length(which((used.arcs$type=='dispatch' | used.arcs$type=='return' | used.arcs$type=='purchase') & 
                         (used.arcs$node1==start | used.arcs$node2==start)))==0){
        indices <- c(indices,which(used.arcs$type!='dispatch' & used.arcs$type!='return' & used.arcs$type!='purchase' & used.arcs$node2==start))
        start <- used.arcs[type!='dispatch' & type!='return' & type !='purchase' & node2==start]$node1
      }
      while(length(which(used.arcs$type!='dispatch' & used.arcs$type!='return' & used.arcs$type!='purchase' & 
                         (used.arcs$node1==end | used.arcs$node2==end)))==2 &
            length(which(used.arcs$type!='dispatch' & used.arcs$type!='return' & used.arcs$type!='purchase' & used.arcs$node1==end))==1 & 
            length(which((used.arcs$type=='dispatch' | used.arcs$type=='return' | used.arcs$type=='purchase') & 
                         (used.arcs$node1==end | used.arcs$node2==end)))==0){
        indices <- c(indices,which(used.arcs$type!='dispatch' & used.arcs$type!='return' & used.arcs$type!='purchase' & used.arcs$node1==end))
        end <- used.arcs[type!='dispatch' & type!='return' & type!='purchase' & node1==end]$node2
      }
      if(length(indices)>0){
        indices <- unique(indices)
        used.arcs[indices,]$new.node1 <- start
        used.arcs[indices,]$new.node2 <- end
        used.arcs[indices,]$superpath.id <- i
        used.arcs$start.count = NULL; used.arcs$end.count = NULL;
        used.arcs$start.count <- used.arcs[, start.count := .N, by = new.node1]$start.count
        used.arcs$end.count <- used.arcs[, end.count := .N, by = new.node2]$end.count
        i <- i+1
      }
    }
  }
  
  used.arcs2 <- NULL
  used.arcs2 <- used.arcs[, j=list(
    node1 = as.integer(mean(new.node1, na.rm=TRUE)),
    node2 = as.integer(mean(new.node2, na.rm=TRUE)),
    path.id = as.integer(mean(superpath.id, na.rm=TRUE)),
    cost = sum(cost, na.rm=TRUE),
    distance = sum(distance, na.rm=TRUE),
    capacity = mean(mcf.det.used, na.rm=TRUE),
    start.time = mean(start.time, na.rm=TRUE),
    end.time = mean(end.time, na.rm=TRUE),
    type = ifelse(sum(type=='trip')>0,'trip', names(sort(-table(type)))[1])
  ), by = list(new.node1, new.node2, superpath.id)]
  
  veh.arcs2 <- NULL; veh.arcs3 <- NULL
  veh.arcs2 <- veh.arcs[used.arcs,on=c(node1="node1",node2="node2",path.id="path.id")]
  veh.arcs3 <- veh.arcs2[, has.charge := sum(charge.node=='TRUE')>0, by = list(new.node1, new.node2, superpath.id)]
  veh.arcs3 <- veh.arcs3[, j=list(
    node1 = as.integer(mean(new.node1, na.rm=TRUE)),
    node2 = as.integer(mean(new.node2, na.rm=TRUE)),
    path.id = as.integer(mean(superpath.id,na.rm=TRUE)),
    cost = sum(cost, na.rm=TRUE),
    distance = sum(distance, na.rm=TRUE),
    capacity = as.integer(mean(mcf.det.used, na.rm=TRUE)),
    type = as.character(ifelse(sum(type=='trip')>0,'trip', names(sort(summary(as.factor(type)), decreasing=T)[1]))),
    vehicle.type = names(sort(summary(as.factor(vehicle.type)), decreasing=T)[1]),
    vehicle.count = as.integer(mean(vehicle.count, na.rm=TRUE)),
    has.charge = sum(has.charge=='TRUE')>0,
    energy.loss = round(sum(energy.loss, na.rm=TRUE),4),
    energy.capacity = mean(energy.capacity,na.rm=TRUE),
    start.time = mean(start.time,na.rm=TRUE),
    end.time = mean(end.time,na.rm=TRUE),
    assigned = NA
  ), by = list(new.node1, new.node2, superpath.id, vehicle.id)]
  nodes <- data.table(unique(c(veh.arcs3$node1, veh.arcs3$node2)))
  
  used.arcs2 <- used.arcs2[order(node1,node2,path.id)]
  veh.e.states2 <- veh.e.states[nodes, on=c(node="V1")]
  veh.arcs3 <- veh.arcs3[order(node1,node2,path.id)]
  veh.e.states2 <- veh.e.states2[order(node,vehicle.id)]
  
  this.model <- buildModel(used.arcs2, veh.arcs3, vehicles, veh.e.states2, "MIP", 
                           slopes = buildVeh.out$slopes,intercepts =buildVeh.out$intercepts,
                           slopes.external = buildVeh.out$slopes.external,intercepts.external =buildVeh.out$intercepts.external)
  
  gurobi_write(this.model, 'test2.mps')
  #  solution.lp <-runGurobi(gurobi_relax(this.model), type="NOTMIP",time.limit=1800, heuristic=0.2)
  #  this.model$start <- ifelse(round(solution.lp$x)==solution.lp$x, solution.lp$x, as.numeric(NA))
  solution <- runGurobi(this.model,type="MIP",time.limit=3600*0.2,heuristic=0.3,mip.tol=1e-3)
  
  
  #kept <- veh.arcs3[solution!=0]
  #  nodes.to.check <- data.table(rbind(unique(cbind(kept$new.node1,kept$vehicle.id)),unique(cbind(kept$new.node2,kept$vehicle.id))))
  #  node.energies.to.check <- veh.e.states2[nodes.to.check,on=c(node="V1",vehicle.id="V2")]
  
  veh.arcs3$solution <- solution$x[1:nrow(veh.arcs3)]
  veh.e.states2$solution <- solution$x[(nrow(veh.arcs3)+1):(nrow(veh.arcs3)+nrow(veh.e.states2))]
  vehicles$solution <- solution$x[(length(solution$x)-nrow(vehicles)+1):length(solution$x)]
  
  veh.arcs4 <- veh.arcs2[veh.arcs3, 
                         on=c(new.node1="new.node1",new.node2="new.node2",superpath.id="superpath.id",vehicle.id="vehicle.id"), 
                         start:=i.solution]
  
  
  #  veh.arcs3 <<- veh.arcs3
  #  veh.arcs2 <<- veh.arcs2
  veh.e.states4 <<- veh.e.states2
  veh.arcs5 <- veh.arcs
  veh.e.states5 <- veh.e.states
  veh.arcs5$start <- 0
  veh.arcs5 <- veh.arcs5[veh.arcs4, 
                         on=c(node1="node1",node2="node2",path.id="path.id",vehicle.id="vehicle.id"), 
                         start := i.start]
  veh.e.states5 <- veh.e.states5[veh.e.states4, 
                                 on=c(node="node",vehicle.id="vehicle.id"),
                                 start := i.solution]
  
  #    inflows <- veh.arcs[, j=list(inflow = sum(start,na.rm=TRUE)), by=list(node2,vehicle.id)]
  #    outflows <- veh.arcs[, j=list(outflow = sum(start,na.rm=TRUE)), by=list(node1,vehicle.id)]
  #    inout <- merge(inflows,outflows, by.x=c("vehicle.id","node2"), by.y=c("vehicle.id","node1"), all=TRUE)
  #   inout$diff <- inout$outflow - inout$inflow
  #    View(inout[diff!=0])
  return(c(veh.arcs5$start, 
           veh.e.states5$start #,
           #vehicles$solution
  ))
}

describeArcs <- function(sims, arc.list, prefix="New: "){
  cat(paste(prefix, sum(colSums(sims>0)/nrow(sims) > 0), " arcs (",
            sum(colSums(sims>0)/nrow(sims) > 0 & arc.list$type=='dispatch'), " dispatch, ",
            sum(colSums(sims>0)/nrow(sims) > 0 & arc.list$type=='trip'), " trip, ",
            sum(colSums(sims>0)/nrow(sims) > 0 & arc.list$type=='relocation'), " reloc, ",
            sum(colSums(sims>0)/nrow(sims) > 0 & arc.list$type=='return'), " return).\n",sep=""))
}

plotClusterMap <- function(trips){
  unique.points <- unique(rbindlist(list(data.frame(cbind(trips$cluster.start, trips$start_location_long, trips$start_location_lat)),
                                         data.frame(cbind(trips$cluster.end, trips$end_location_long, trips$end_location_lat))),
                                    use.names=FALSE))
  #  austin <<- get_map(location = 'austin',zoom=11);
  cluster.centers <<- unique.points[, .(long=mean(X2,na.rm=TRUE), lat = mean(X3,na.rm=TRUE)), by=X1]
  voronoi <- deldir(cluster.centers$long,cluster.centers$lat)
  #  ggmap(austin, extent = "device") + 
  #    geom_point(data =unique.points, alpha=0.9, size=1.0, aes(x=X2, y=X3, colour = factor(X1)))
  
  ggmap(austin, extent = "device") + 
    geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), size = 2,
                 data = voronoi$dirsgs,
                 linetype = 1,
                 color= "#FFB958") + 
    #Plot the points
    geom_point(data=cluster.centers,aes(x=long,y=lat,colour=factor(X1)),
               size = 4)
  
}


plotSelectedRoutes <- function(arc.list){
  #  austin <- get_map(location = 'austin',zoom=11);
  to.plot <- finalized.results[assigned==1 & vehicle.id < 100,]
  to.plot <- to.plot[order(vehicle.id, start.time)]
  ggmap(get_map(location = 'austin',zoom=11)) + 
    geom_segment(aes(x=start.long, y=start.lat, xend = end.long, yend = end.lat), 
                 data=to.plot, size = 0.5, colour=as.factor(to.plot$vehicle.id)) +
    labs(colour = "id")
  
  geom_path(aes(x = start.long, y = st, colour = as.factor(vehicle.id)), data = movement_example) + 
    labs(colour = "id")
  
}

plotAllArcs <- function(toPlot){
  #  austin <<- get_map(location = 'austin',zoom=11);
  toPlot <- toPlot[start.long != end.long | start.lat != end.lat]
  toPlot[type=='trip', type := "Trips"]
  toPlot[type=='relocation', type := "Connections"]
  toPlot[type=='dispatch' | type=='return', type := "Dispatches & Returns"]
  ggmap(austin) + 
    geom_curve(aes(x=start.long, y=start.lat, 
                   xend = end.long, yend = end.lat,
                   color=type), 
               data=toPlot[type!='charge' & type !='return'], size = 0.5,
               curvature=0.1,
               alpha=0.1
    ) + 
    coord_cartesian() +
    scale_color_manual(values=c("#ff5c33","#4d79ff","#999999")) +
    theme(text = element_text(size=20),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.x=element_blank(),
          axis.ticks.y=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(), 
          legend.position="none",) +
    facet_grid(~type)
}

locateStations <- function(trips, num.stations){
  trips$cluster.start <- NULL
  trips$cluster.end <- NULL
  trips <- addGeoClusters(trips,num.stations)
  unique.points <- unique(rbindlist(list(data.frame(cbind(trips$cluster.start, trips$start_location_long, trips$start_location_lat)),
                                         data.frame(cbind(trips$cluster.end, trips$end_location_long, trips$end_location_lat))),
                                    use.names=FALSE))
  stations <- unique.points[, j=list(long=mean(X2), lat = mean(X3)), by=X1]
  setnames(stations,"X1", "station.id")
  return(stations)
}

fixupRemovedNodes <- function(arcs){
  max.node <- max(unique(c(arcs$node1, arcs$node2)))
  max.trip.node <- max(unique(c(arcs[arcs$type=='trip']$node1,arcs[arcs$type=='trip']$node2)))
  num.split.relocations <- nrow(arcs[arcs$type=='relocation.1',])
  arcs[arcs$type=='relocation.1',]$node2 <- seq(max.trip.node+1,max.trip.node+num.split.relocations)
  arcs[arcs$type=='relocation.2',]$node1 <- seq(max.trip.node+1,max.trip.node+num.split.relocations)
  new.max <- max(unique(c(arcs[arcs$type!='return']$node1,arcs[arcs$type!='return']$node2)))
  arcs[arcs$node2==max.node,]$node2 <- max.node
  return(arcs)
}

solveMinCostFlow <- function(arc.list, vehicles, randomizeCosts=FALSE, threshold=0,costs.to.include="private",
                             damage.model=default.damage.model,cr.to.use=default.cr,carbon.price=default.carbon.price,vsl=default.vsl,v.arcs.to.improve=data.table(),labor.price=default.labor.price){
  
  mcf.charge.nodes <- c(arc.list[type=='charge']$node1,arc.list[type=='charge']$node2)
  arc.list$keep.for.charge<-FALSE
  arc.list[data.frame(id=mcf.charge.nodes), on=c(node1="id"), keep.for.charge := TRUE]
  arc.list[data.frame(id=mcf.charge.nodes), on=c(node2="id"), keep.for.charge := TRUE]
  if(nrow(v.arcs.to.improve)>0){
    keep.from.existing.solution <- data.table(unique(v.arcs.to.improve[, .(node1,node2,path.id)]))
    arc.list[keep.from.existing.solution, on=.(node1=node1,node2=node2,path.id=path.id), keep.for.solution := TRUE]
  }
  sims <- c()
  if(nrow(vehicles[vehicle.type=='EV'])>0){
    new.sims <- mcf_Deterministic(arc.list, vehicles, v.type="EV", weighted=TRUE, costs.to.include=costs.to.include, damage.model,cr.to.use,carbon.price,vsl,labor.price)
    describeArcs(new.sims,arc.list,"From mcf_Deterministic_weighted_EV: ")
    sims <- rbind(sims,new.sims)
    describeArcs(sims,arc.list,"Total: ")
  }
  if(nrow(vehicles[vehicle.type=='ICE'])>0){
    new.sims <- mcf_Deterministic(arc.list, vehicles, v.type="ICE", weighted=TRUE, costs.to.include=costs.to.include, damage.model,cr.to.use,carbon.price,vsl,labor.price)
    describeArcs(new.sims,arc.list,"From mcf_Deterministic_weighted_ICE: ")
    sims <- rbind(sims,new.sims)
    describeArcs(sims,arc.list,"Total: ")
  }
  if(nrow(vehicles[vehicle.type=='HEV'])>0){
    new.sims <- mcf_Deterministic(arc.list,vehicles,v.type="HEV", weighted=TRUE, costs.to.include=costs.to.include, damage.model,cr.to.use,carbon.price,vsl,labor.price)
    describeArcs(new.sims,arc.list,"From mcf_Deterministic_weighted_HEV: ")
    sims <- rbind(sims,new.sims)
    describeArcs(sims,arc.list,"Total: ")
  }
  if(sum(vehicles$vehicle.count<400) & length(unique(vehicles$vehicle.type))>1){
    new.sims <- mcf_VaryingFleetSize(arc.list, vehicles, min.fleet.size=16, weighted=TRUE, costs.to.include=costs.to.include, damage.model,cr.to.use,carbon.price,vsl,labor.price)
    describeArcs(new.sims,arc.list,"From VaryingFleetSize_Weighted: ")
    sims <- rbind(sims,new.sims)
    describeArcs(sims,arc.list,"Total: ")
  }
  
  if(randomizeCosts==TRUE){
    new.sims <- mcf_RandomArcCosts(arc.list, vehicles, bounds=0.1, weighted=TRUE, costs.to.include, damage.model,cr.to.use,carbon.price,vsl,labor.price)
    describeArcs(new.sims,arc.list,"From RandomArcCosts_Weighted: ")
    sims <- rbind(sims,new.sims)
    describeArcs(sims,arc.list,"Total: ")
    new.sims <- mcf_RandomArcCosts(arc.list, vehicles, bounds=0.1, weighted=FALSE, costs.to.include, damage.model,cr.to.use,carbon.price,vsl,labor.price)
    describeArcs(new.sims,arc.list,"From RandomArcCosts_Unweighted: ")
    sims <- rbind(sims,new.sims)
    describeArcs(sims,arc.list,"Total: ")
  }
  
  
  binUsage <- sims>0
  totalUsage <- colSums(binUsage)/nrow(sims)
  
  tripTotal <- sum(arc.list$type=='trip')
  tripKeep <- sum(arc.list$type=='trip' & totalUsage>threshold)
  relocTotal <- sum(arc.list$type=='relocation')
  relocKeep <- sum(arc.list$type=='relocation' & totalUsage>threshold)
  cat(paste("\n",tripKeep,"/",tripTotal," trips and ",relocKeep,"/",relocTotal," relocations kept.\n", sep=""))
  arc.list$used <- arc.list$type!='relocation' | totalUsage>threshold
  
  if(nrow(v.arcs.to.improve)>0){
    arc.list$kept <- arc.list$used==1 | 
      (arc.list$type!='relocation' & 
         arc.list$type!='relocation.1' &
         arc.list$type!='relocation.2') |
      arc.list$keep.for.charge == TRUE |
      arc.list$keep.for.solution == TRUE
  } else{
    arc.list$kept <- arc.list$used==1 | 
      (arc.list$type!='relocation' & 
         arc.list$type!='relocation.1' &
         arc.list$type!='relocation.2') |
      arc.list$keep.for.charge == TRUE
  }
  to.keep <- unique(arc.list[arc.list$kept==1, c("node1","node2","path.id")])
  to.keep$keep = TRUE
  arc.subset <- data.table(to.keep[arc.list, on=.(node1=node1,node2=node2,path.id=path.id)])
  arc.subset <- arc.subset[keep==TRUE,]
  return(arc.subset)
}
