
# Analysis of wings from Africa along the elevation gradient
rm(list=ls())
setwd( "/Users/Will/Desktop/African_Dryad" )
source( "PitchersPoolDworkinFunctions.R" )

wings <- read.csv( "Afro_Main_ProcCoords.csv", colClasses=c( rep("factor", 5), rep("numeric", 100) ))

names(wings)
# LineID, Pop, Line, Ind(ividual #), Sex,
# followed by 96 Procrustes coordinates,
# followed by Csize(mm), Elevation(altitude (m)), Lat(itude - degrees), Lon(gitude - degrees)

wings$absLat <- abs(wings$Lat) # create a new vector of absolute values of latitude, i.e. distance from equator

# sample sizes
table(wings$LineID, wings$Sex)
 
 
 
# modelling Cline related Size variation #

require(MCMCglmm)

size.mcmc <- MCMCglmm(Csize ~ Elevation + Sex + absLat + Lon,
				random=~ Line,
				prior=  list( R=list(V=1, nu=0.05), G=list(G1=list(V=1, nu=0.05))),
				data= wings,
				nitt= 200000,
				burnin= 2000,
				thin=10)
# NB: including interactions is deprecated by ~dDIC 5
	
summary( size.mcmc )
	acf( size.mcmc$VCV )		#plots to check for autocorrelation along the Markov chain...
		plot( size.mcmc$VCV )
	acf( size.mcmc$Sol )
		plot( size.mcmc$Sol )

summary(size.mcmc)$solutions[2,1:3] * 1000	# size per 1km altitude

# refitting the same model as a line-means anova ONLY for variance-accounted-for stat.s

wings.means <- aggregate( c(wings["Csize"], wings["Elevation"], wings["absLat"], wings["Lon"]), c(wings["Sex"], wings["Pop"], wings["Line"]), mean )

wings.cs.means.lm <- lm(Csize ~ Elevation + Sex + absLat + Lon, data=wings.means)
	PRsq(wings.cs.means.lm)
	Rsq( lm(Csize ~ Elevation, data= wings.means) )
	Rsq( lm(Csize ~ Sex, data= wings.means) )
	Rsq( lm(Csize ~ absLat, data=wings.means) )
	Rsq( lm(Csize ~ Lon, data=wings.means) )

###

# modelling Cline related Shape variation #

wings.PC <- prcomp( wings[,6:101] )
wings <- cbind( wings, wings.PC$x[,1:58] )

require(MCMCglmm)

# here we are expressing altitude in 100's of metres...
wings$elev <- wings$Elevation / 100

# ...here and scaling the principal componenet scores by a factor of 10,000 - this was necessary in order to prevent floating-point errors leading to nonsensical results
PCmultiplied <- wings[,107:164] * 10000
colnames(PCmultiplied) <- sub("PC", "PCm", colnames(PCmultiplied))

wings <- data.frame(wings, PCmultiplied)

fmla.LHS <- paste("PCm", 1:58, sep="", collapse=" , ")

fmla <- as.formula(paste("cbind(", fmla.LHS, ")" ,"~", "trait + trait:Sex + trait:Csize + trait:elev + trait:absLat + trait:Lon - 1"))
# '-1' suppresses intercept because otherwise the interpretation would unnecessarily complicated...

fam.test <- rep("gaussian", 58)

prior.model.1 <- list( R=list(V=diag(58), nu=0.1),  
                                 G=list(G1=list(V=diag(58), nu=0.1)))

model.wings.African <- MCMCglmm(fmla,
					random=~ idh(trait):LineID,
					rcov=~ idh(trait):units,
					prior=  prior.model.1,
					data= wings,
					family= fam.test,
					nitt= 200000,
					burnin= 2000,
					thin=10)

# this will take quite some time to run!

summary( model.wings.African )

# calculating vector correlations and HPD's for same:
sexM_effect <- summary( model.wings.African )$solutions[59:116, 1]
Csize_effect <- summary( model.wings.African )$solutions[117:174, 1]
elev_effect <- summary( model.wings.African )$solutions[175:232, 1]
lat_effect <- summary( model.wings.African )$solutions[233:290, 1]
lon_effect <- summary( model.wings.African )$solutions[291:348, 1]

ang.vec.abs( elev_effect, sexM_effect )
ang.vec.abs( elev_effect, Csize_effect )
ang.vec.abs( elev_effect, lat_effect )
	ang.vec.abs( sexM_effect, Csize_effect )
	ang.vec.abs( sexM_effect, lat_effect )
		ang.vec.abs( Csize_effect, lat_effect )

coef.sexM  <- model.wings.African$Sol[,59:116]
coef.Csize <- model.wings.African$Sol[,117:174]
coef.elev <- model.wings.African$Sol[,175:232]
coef.lat <- model.wings.African$Sol[,233:290]

elev.sex <- rep(NA, 19800)
elev.size <- rep(NA, 19800)
elev.lat <- rep(NA, 19800)
	sex.size <- rep(NA, 19800)
	sex.lat <- rep(NA, 19800)
		size.lat <- rep(NA, 19800)

for (i in 1:19800){
	sexM <- coef.sexM[i,]
	elev <- coef.elev[i,]
	size <- coef.Csize[i,]
	lat <- coef.lat[i,]
		elev.sex[i] <- ang.vec.abs(sexM, elev)[1]
		elev.size[i] <- ang.vec.abs(size, elev)[1]
		elev.lat[i] <- ang.vec.abs(elev, lat)[1]
			sex.size[i] <- ang.vec.abs(sexM, size)[1]
			sex.lat[i] <- ang.vec.abs(sexM, lat)[1]
				size.lat[i] <- ang.vec.abs(size, lat)[1]
}

HPDinterval( as.mcmc( elev.sex ))
HPDinterval( as.mcmc( elev.size ))
HPDinterval( as.mcmc( elev.lat ))
	HPDinterval( as.mcmc( sex.size ))
	HPDinterval( as.mcmc( sex.lat ))
		HPDinterval( as.mcmc( size.lat ))


# refitting the same model as a line-means manova ONLY for variance-accounted-for stat.s

shape.line.means <- aggregate(wings[,c(102:164)], c(wings["LineID"], wings["Sex"], wings["Pop"] ), mean)
names(shape.line.means)

shape.model.means <- lm(as.matrix(shape.line.means[,9:66]) ~ Csize + Elevation + Sex + absLat + Lon, data=shape.line.means )
	shapePRsq( shape.model.means )
	shapeRsq( lm(as.matrix(shape.line.means[,9:66]) ~ shape.line.means$Csize ) )
	shapeRsq( lm(as.matrix(shape.line.means[,9:66]) ~ shape.line.means$Elevation ) )
	shapeRsq( lm(as.matrix(shape.line.means[,9:66]) ~ shape.line.means$Sex ) )
	shapeRsq( lm(as.matrix(shape.line.means[,9:66]) ~ shape.line.means$absLat ) )
	shapeRsq( lm(as.matrix(shape.line.means[,9:66]) ~ shape.line.means$Lon ) )

###############################

# East vs West Analysis

# MCMCglmm by subset...

EAST_wings <- wings[wings$Pop=="ED"|wings$Pop=="EZ"|wings$Pop=="RG"|wings$Pop=="KO"|wings$Pop=="MW",]

WEST_wings <- wings[wings$Pop=="CN"|wings$Pop=="CO"|wings$Pop=="GA"|wings$Pop=="NG",]

#...but otherwise the model remains the same:

E.wings.African <- MCMCglmm(fmla,
				random=~ idh(trait):LineID,
				rcov=~ idh(trait):units,
				prior=  prior.model.1,
				data= EAST_wings,
				family= fam.test,
				nitt= 200000,
				burnin= 2000,
				thin=10)

W.wings.African <- MCMCglmm(fmla,
				random=~ idh(trait):LineID,
				rcov=~ idh(trait):units,
				prior=  prior.model.1,
				data= WEST_wings,
				family= fam.test,
				nitt= 200000,
				burnin= 2000,
				thin=10)

# comparing the fitted altitude effects:

E.coef.elev <- E.wings.African$Sol[,175:232]
	E.elev_effect <- summary( E.wings.African )$solutions[175:232, 1]

W.coef.elev <- W.wings.African$Sol[,175:232]
	W.elev_effect <- summary( W.wings.African )$solutions[175:232, 1]

E_W_elev <- rep(NA, 19800)
	for (i in 1:19800) {
		E_W_elev[i] <- ang.vec.abs( W.coef.elev[i,], E.coef.elev[i,] )[1]
	}
		HPDinterval( as.mcmc( E_W_elev ))
		ang.vec.abs( W.elev_effect, E.elev_effect )


# allometry for comparison

E.coef.size <- E.wings.African$Sol[,117:174]
	E.size_effect <- summary( E.wings.African )$solutions[117:174, 1]

W.coef.size <- W.wings.African$Sol[,117:174]
	W.size_effect <- summary( W.wings.African )$solutions[117:174, 1]

E_W_size <- rep(NA, 19800)
	for (i in 1:19800) {
		E_W_size[i] <- ang.vec.abs( W.coef.size[i,], E.coef.size[i,] )[1]
	}
		HPDinterval( as.mcmc( E_W_size ))
		ang.vec.abs( W.size_effect, E.size_effect )

# dimorphism for comparison

E.coef.sex <- E.wings.African$Sol[,59:116]
	E.sex_effect <- summary( E.wings.African )$solutions[59:116, 1]

W.coef.sex <- W.wings.African$Sol[,59:116]
	W.sex_effect <- summary( W.wings.African )$solutions[59:116, 1]

E_W_sex <- rep(NA, 19800)
	for (i in 1:19800) {
		E_W_sex[i] <- ang.vec.abs( W.coef.sex[i,], E.coef.sex[i,] )[1]
	}
		HPDinterval( as.mcmc( E_W_sex ))
		ang.vec.abs( W.sex_effect, E.sex_effect )

###############################

# Cameroon vs Ethiopia Analysis

# MCMCglmm by subset...

Ethiopia <- wings[ wings$Pop=="ED" | wings$Pop=="EZ" ,]

Cameroon <- wings[ wings$Pop=="CN" | wings$Pop=="CO" ,]

# ...here however, the differences ranges in Latitude and Longitude are not fitted
fmla.2 <- as.formula(paste("cbind(", fmla.LHS, ")" ,"~", "trait + trait:Sex + trait:Csize + trait:elev  - 1")) 

Eth.wings.African <- MCMCglmm(fmla.2,
					random=~ idh(trait):LineID,
					rcov=~ idh(trait):units,
					prior=  prior.model.1,
					data= Ethiopia,
					family= fam.test,
					nitt= 200000,
					burnin= 2000,
					thin=10)


Cam.wings.African <- MCMCglmm(fmla.2,
					random=~ idh(trait):LineID,
					rcov=~ idh(trait):units,
					prior=  prior.model.1,
					data= Cameroon,
					family= fam.test,
					nitt= 200000,
					burnin= 2000,
					thin=10)

# comparing fitted Altitude effects...

E.coef.elev <- Eth.wings.African$Sol[,175:232]
	E.elev_effect <- summary( Eth.wings.African )$solutions[175:232, 1]

C.coef.elev <- Cam.wings.African$Sol[,175:232]
	C.elev_effect <- summary( Cam.wings.African )$solutions[175:232, 1]

Cam_Eth <- rep(NA, 19800)
	for (i in 1:19800) {
		eth <- E.coef.elev[i,]
		cam <- C.coef.elev[i,]
		Cam_Eth[i] <- ang.vec.abs( eth, cam )[1]
	}

	HPDinterval( as.mcmc( Cam_Eth ))
		ang.vec.abs( E.elev_effect, C.elev_effect )

# Dimorphism effects for comparison

E.coef.sex <- Eth.wings.African$Sol[,59:116]
	E.sex_effect <- summary( Eth.wings.African )$solutions[59:116, 1]
	
C.coef.sex <- Cam.wings.African$Sol[,59:116]
	C.sex_effect <- summary( Cam.wings.African )$solutions[59:116, 1]

C_E_sex <- rep(NA, 19800)
	for (i in 1:19800) {
		C_E_sex[i] <- ang.vec.abs( E.coef.sex[i,], C.coef.sex[i,] )[1]
	}
	HPDinterval( as.mcmc( C_E_sex ))
		ang.vec.abs( E.sex_effect, C.sex_effect )

# Allometry effects for comparison

E.coef.allometry <- Eth.wings.African$Sol[,117:176]
	E.allometry_effect <- summary( Eth.wings.African )$solutions[117:176, 1]
	
C.coef.allometry <- Cam.wings.African$Sol[,117:176]
	C.allometry_effect <- summary( Cam.wings.African )$solutions[117:176, 1]

C_E_allom <- rep(NA, 19800)
	for (i in 1:19800) {
		C_E_allom[i] <- ang.vec.abs(E.coef.allometry[i,], C.coef.allometry[i,] )[1]
	}
	HPDinterval( as.mcmc( C_E_allom ))
		ang.vec.abs( E.allometry_effect, C.allometry_effect )

###################################################################################

# ANALYSES FOR TEMPERATURE MANIPULATION EXPERIMENT

rm(list=ls())
setwd( "/Users/Will/Desktop/African_Dryad" )
source( "PitchersPoolDworkinFunctions.R" )

wings <- read.csv( "Afro_Temp_ProcCoords.csv", colClasses=c( rep("factor", 5), rep("numeric", 100) ))

names(wings)
# Line, Temp(erature), Sex, Ind(ividual #), Pop
# followed by 96 Procrustes coordinates,
# followed by Csize(mm), Elevation(altitude (m)), Lat(itude - degrees), Lon(gitude - degrees)

wings$absLat <- abs(wings$Lat)

require(MCMCglmm)

mcmc.fmla <- as.formula(paste("Csize ~ Elevation * Temp * Sex + absLat + Lon"))

prior.1 <- list( R=list(V=1, nu=0.05),  
                                 G=list(G1=list(V=1, nu=0.05)))

size.temp.mcmc <- MCMCglmm(mcmc.fmla,
					random=~ Line,
					prior=  prior.1,
					data= wings,
					nitt= 200000,
					burnin= 2000,
					thin=10)

mcmc.summary <- summary( size.temp.mcmc )
	mc.coefs <- mcmc.summary$solutions[,1]

#plots to check for autocorrelation along the Markov chain...
	acf( size.temp.mcmc$VCV )
		plot( size.temp.mcmc$VCV )
	acf( size.temp.mcmc$Sol )
		plot( size.temp.mcmc$Sol )

# refitting the same model as a line-means anova ONLY for variance-accounted-for stat.s

wings.linemeans <- aggregate( c(wings["Csize"], wings["Elevation"]), c(wings["Sex"], wings["Line"], wings["Temp"], wings["Pop"], wings["absLat"], wings["Lon"]), mean )

wings.cs.lm <- lm(Csize ~ Elevation * Temp * Sex + absLat + Lon, data=wings.linemeans)	
	PRsq( wings.cs.lm )
	Rsq( lm(Csize ~ Elevation, data=wings.linemeans) )
	Rsq( lm(Csize ~ Temp, data=wings.linemeans) )
	Rsq( lm(Csize ~ Sex, data=wings.linemeans) )
	Rsq( lm(Csize ~ absLat, data=wings.linemeans) )
	Rsq( lm(Csize ~ Lon, data=wings.linemeans) )
	Rsq( lm(Csize ~ Elevation:Temp, data=wings.linemeans) )
	Rsq( lm(Csize ~ Elevation:Sex, data=wings.linemeans) )
	Rsq( lm(Csize ~ Temp:Sex, data=wings.linemeans) )
	Rsq( lm(Csize ~ Elevation:Temp:Sex, data=wings.linemeans) )


### Shape model

PC.wings <- prcomp(wings[,6:101])

wings <- data.frame(wings, PC.wings$x[,1:58])

# as before, we're scaling the altitude and the princiapal components in order that the model behaves computationally
wings$Elev100 <- wings$Elevation / 100

PCmultiplied <- wings[,107:164] * 10000
colnames(PCmultiplied) <- sub("PC", "PCm", colnames(PCmultiplied))

wings <- data.frame(wings, PCmultiplied)

# Multivariate mixed model


fmla.LHS <- paste("PCm", 1:58, sep="", collapse=" , ")
fmla.temp <- as.formula(paste("cbind(", fmla.LHS, ")" ,"~", "trait + trait:Sex + trait:Temp + trait:Csize + trait:Elev100 + trait:absLat + trait:Lon - 1"))

fam.test <- rep("gaussian", 58)

prior.model.1 <- list( R=list(V=diag(58), nu=0.01),  
                                G=list(G1=list(V=diag(58), nu=0.01)))

# prior.model.1 <- list( R=list(V=diag(26), nu=0.05))                                 

model.wings.African <- MCMCglmm(fmla.temp,
                       random=~ idh(trait):Line,
                       rcov=~ idh(trait):units, 
                       prior=  prior.model.1,
                       data= wings,
                       family= fam.test,
                       nitt= 200000,       
                       burnin= 2000,
                       thin=10)

summary( model.wings.African )


# vector correlations and HPD's for same:

sexM_effect <- summary( model.wings.African )$solutions[59:116, 1]
temp_effect <- summary( model.wings.African )$solutions[117:174, 1]
Csize_effect <- summary( model.wings.African )$solutions[175:232, 1]
elev_effect <- summary( model.wings.African )$solutions[233:290, 1]
lat_effect <- summary( model.wings.African )$solutions[291:348, 1]
lon_effect <- summary( model.wings.African )$solutions[349:406, 1]

ang.vec.abs( elev_effect, sexM_effect )
ang.vec.abs( elev_effect, Csize_effect )
ang.vec.abs( elev_effect, temp_effect )
ang.vec.abs( elev_effect, lat_effect )
	ang.vec.abs( sexM_effect, Csize_effect )
	ang.vec.abs( sexM_effect, temp_effect )
	ang.vec.abs( sexM_effect, lat_effect )
		ang.vec.abs( Csize_effect, temp_effect )
		ang.vec.abs( temp_effect, lat_effect )
			ang.vec.abs( Csize_effect, lat_effect )


coef.sexM  <- model.wings.African$Sol[,59:116]
coef.temp <- model.wings.African$Sol[,117:174]
coef.Csize <- model.wings.African$Sol[,175:232]
coef.elev <- model.wings.African$Sol[,233:290]
coef.lat <- model.wings.African$Sol[,291:348]
coef.lon <- model.wings.African$Sol[,349:406]

elev.sex <- rep(NA, 19800)
elev.size <- rep(NA, 19800)
elev.temp <- rep(NA, 19800)
elev.lat <- rep(NA, 19800)
	sex.size <- rep(NA, 19800)
	sex.temp <- rep(NA, 19800)
	sex.lat <- rep(NA, 19800)
		temp.size <- rep(NA, 19800)
		temp.lat <- rep(NA, 19800)
			size.lat <- rep(NA, 19800)

for (i in 1:19800){
	sexM <- coef.sexM[i,]
	temp <- coef.temp[i,]
	size <- coef.Csize[i,]
	elev <- coef.elev[i,]
	lat <- coef.lat[i,]
		elev.sex[i] <- ang.vec.abs(elev, sexM)[1]
		elev.size[i] <- ang.vec.abs(elev, size)[1]
		elev.temp[i] <- ang.vec.abs(elev, temp)[1]
		elev.lat[i] <- ang.vec.abs(elev, lat)[1]
		sex.size[i] <- ang.vec.abs(sexM, size)[1]
		sex.temp[i] <- ang.vec.abs(sexM, temp)[1]
		sex.lat[i] <- ang.vec.abs(sexM, lat)[1]
		temp.size[i] <- ang.vec.abs(temp, size)[1]
		temp.lat[i] <- ang.vec.abs(temp, lat)[1]
		size.lat[i] <- ang.vec.abs(size, lat)[1]
}

HPDinterval( as.mcmc( elev.sex ))
HPDinterval( as.mcmc( elev.size ))
HPDinterval( as.mcmc( elev.temp ))
HPDinterval( as.mcmc( elev.lat ))
	HPDinterval( as.mcmc( sex.size ))
	HPDinterval( as.mcmc( sex.temp ))
	HPDinterval( as.mcmc( sex.lat ))
		HPDinterval( as.mcmc( temp.size ))
		HPDinterval( as.mcmc( temp.lat ))
			HPDinterval( as.mcmc( size.lat ))


# line means model for R^2 calculation

wing.means <- aggregate( wings[,c(7:164)], c(wings["Line"], wings["Pop"], wings["Sex"], wings["Temp"]), mean )

shape.model <- lm(as.matrix(wing.means[,107:162]) ~ Csize+Elevation+Sex+Temp+absLat+Lon, data= wing.means )
	shapePRsq( shape.model )
	shapeRsq( lm(as.matrix(wing.means[,107:162]) ~ Csize, data= wing.means ) )
	shapeRsq( lm(as.matrix(wing.means[,107:162]) ~ Elevation, data= wing.means ) )
	shapeRsq( lm(as.matrix(wing.means[,107:162]) ~ Sex, data= wing.means ) )
	shapeRsq( lm(as.matrix(wing.means[,107:162]) ~ Temp, data= wing.means ) )
	shapeRsq( lm(as.matrix(wing.means[,107:162]) ~ absLat, data= wing.means ) )
	shapeRsq( lm(as.matrix(wing.means[,107:162]) ~ Lon, data= wing.means ) )


