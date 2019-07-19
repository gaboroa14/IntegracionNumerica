
###
### INTEGRALES
### Regla del Trapecio Simple
###

TrapecioSimple<-function(fx,a,b){
	aprox = (fx(b)+fx(a))*(b - a) / 2
	return(aprox)
}

###
### Regla del Trapecio Compuesto
###

TrapecioCompuesto<-function(fx,a,b,n){
	h = (b-a) / n
	aprox = h * (fx(a) + fx(b)) / 2 
	suma = 0
	x = numeric(n)
	x[1] = a

	for (i in 2 : n){
		x[i]=x[i-1]+h
	}

	for (i in 1 : n){
		suma = suma + fx(x[i]) * h
	}

	return(aprox+suma)
}

###
### Regla de Simpson Simple
###

SimpsonSimple<-function(fx,a,b){
	h = b-a
	return(h*(fx(a)+4*fx((a+b)/2)+fx(b))/3)
}

###
### Regla de Simpson Compuesta
###

SimpsonCompuesto<-function(fx,a,b,n){
	if (n%%2!=0){
		return("ERROR: El n introducido debe ser un número par (2, 4, 6, ...)")
	}

	h = (b-a)/n
	x = numeric(n)
	x[1]=a

	for (i in 2 : n){
		x[i]=x[i-1]+h
	}

	sumapar = 0
	sumaimpar = 0

	for (i in 2 : n){
		if (i%%2==0){
			sumapar = sumapar + fx(x[i])
		} else {
			sumaimpar = sumaimpar + fx(x[i])
		}
	}

	aprox = (h / 3)*(fx(a)+2*sumaimpar+4*sumapar+fx(b))
	return(aprox)
}
