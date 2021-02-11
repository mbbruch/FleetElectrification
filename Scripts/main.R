rm(list=ls());
data.path <- "/pylon5/dd5fp4p/mbruchon/"
out.path <<- paste0(data.path,"Out/this_test/")
code.path <- "/home/mbruchon/"
source(paste0(code.path,"Scripts/setup.R"))
source(paste0(code.path,"Scripts/helper_functions.R"))

ev.purchase.price <<- 33950
discount.rate <<- 0.09
result <- runModel(num.stations=1,num.ice=50,num.hev=50,num.ev=50,
	costs.to.include="private", location="Austin",
	folder=out.path, num.iterations=10) 	
this.string <- result$time.string

while(!is.na(this.string)){
	this.string <- fixupSolution(filestring=this.string,in.folder=out.path, out.folder=out.path,num.iterations=10)	
	source(paste0(code.path,"Scripts/summarize_results.R"))
}
	
source(paste0(code.path,"Scripts/summarize_results.R"))
q("no")
