# univariate Rsquared and partial Rsquared calculators

Rsq <- function( model ){
	fitted.variance <- var(model$fitted)
	total.variance	<- var(model$fitted) + var(model$resid)
	fitted.variance / total.variance
}


PRsq <- function( model ){
	residual.variance <- var(model$resid)
	variables <- attr(terms(model), "term.labels")
		model.length <- length(variables)
		variable.name <- rep(NA, model.length )
		partial.Rsq <- rep(NA, model.length )
			
	for (i in 1:model.length){
		variable.name[i] <- variables[i]
		drop <- parse( text=variables[i] )
		new.formula <- as.formula( paste( ".~.-", variables[i], sep=""))
		new.model <- update(model, new.formula )
		partial.Rsq[i] <- (var(new.model$resid) - residual.variance)/ var(new.model$resid)
		}
	R2 <- Rsq( model )
	list(Rsquared=R2, partials=data.frame( cbind( variable.name, partial.Rsq ))	)
}


# multivariate Rsquared and partial Rsquared calculators

shapeRsq <- function( model ){
	fitted.variance <- sum(diag(var(model$fitted)))
	total.variance	<- sum(diag(var(model$fitted + model$resid)))
	fitted.variance / total.variance
}

comment(shapeRsq) <- "this function takes a miultivariate model object and returns the fitted variance / total variance: eqivalent to an Rsquared"


shapePRsq <- function( model ){
# Based on the derivation from page 269 of Kutner et. al. (Applied linear statistical models edition 5.)
	residual.variance <- var(model$resid)
	variables <- attr(terms(model), "term.labels")
		model.length <- length(variables)
		variable.name <- rep(NA, model.length )
		partial.Rsq <- rep(NA, model.length )
			
	for (i in 1:model.length){
		variable.name[i] <- variables[i]
		drop <- parse( text=variables[i] )
		new.formula <- as.formula( paste( ".~.-", variables[i], sep=""))
		new.model <- update(model, new.formula )
		partial.Rsq[i] <- (sum ( diag( var(new.model$resid))) - sum( diag( residual.variance)) ) / sum( diag( var(new.model$resid)))
		}
	R2 <- shapeRsq( model )
	list(Rsquared=R2, partials=data.frame( cbind( variable.name, partial.Rsq ))	)
}


# calculate tangent approximaton for tangent approximates Procrustes Distance (Euclidean Distance) 
# This is just the magnitude of the vector!
PD <- function(x) { 
	sqrt(t(x)%*%x)}
comment(PD) <- c("This just computes the Euclidean Distance (norm) for a vector")


###################################


# This function will compute the angle between two vectors.	BEWARE if the vectors can be of arbitrary sign, use the function below this one.

ang.vec <- function(vec1, vec2){
	vec.cor <- (t(vec1) %*% vec2)/(PD(vec1)*PD(vec2))
	vec.angle <- acos(vec.cor)*(180/pi)
	return(c(vector.cor=vec.cor, vec.angle=vec.angle))}	
comment(ang.vec) <- c(" This computes both the vector correlation, and angle, between two vectors.", " to compare to the Pearson correlation coefficient make sure to center and standardize vectors", "DO NOT USE THIS IF VECTORS CAN BE OF ARBITRARY SIGN")



####### When the vectors can be of arbitrary sign, use this which computes the magnitude of the vector correlation, and then computes the angle.

ang.vec.abs <- function(vec1, vec2){
	vec.cor <- abs((t(vec1) %*% vec2)/(PD(vec1)*PD(vec2)))
	vec.angle <- acos(vec.cor)*(180/pi)
	return(c(vector.cor=vec.cor, vec.angle=vec.angle))}	
comment(ang.vec) <- c(" This computes both the vector correlation, and angle, between two vectors.", " to compare to the Pearson correlation coefficient make sure to center and standardize vectors", "set it up to compute the absolute values of the vector correlation")



#################################

# Klingenberg's "shape score"
# now let us compute Klingenberg's "shape score" as per Drake and Klingenberg

ShapeScore <- function(Beta, Y) {
	## This computes the "shape score" of Drake and Klingenberg 2008
	## Beta is the coefficients for each procrustes coordinate from a multivariate multiple regression. 
	## i.e. shape.model.1 <- lm(cbind(Y1, Y2) ~ X1 + X2, data=data.set )
	## Beta ...   beta.model <- t(coef(shape.model.1)[k,]) # where k is the covariate of focus
	## 
	## Y is the raw procrustes coordinates (Y1, Y2, ...)
	# Since Klingenberg uses the generalized inverses, we have tested this for the 2p-4 PCs and have gotten identical results.
	s = Y %*% t(Beta) %*% ((Beta %*% t(Beta))^-0.5)
	return(shapeScore=s)
}

comment(ShapeScore) <- c("This computes the 'shape score' of Drake and Klingenberg 2008", 
  "Beta is the coefficients for each procrustes coordinate from a multivariate multiple regression. ",
  "i.e. shape.model.1 <- lm(cbind(Y1, Y2) ~ X1 + X2, data=data.set )",
  "Beta ...   beta.model <- t(coef(shape.model.1)[k,]) # where k is the covariate of focus",
  "Y is the raw procrustes coordinates (Y1, Y2, ...)",
  "Klingenberg uses the generalized inverses, we have tested this for the 2p-4 PCs and have gotten identical results.")


#################################