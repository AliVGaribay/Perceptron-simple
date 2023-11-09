sigmoide <- function(y){
    return(1 / (1 + exp(-y)))
}

perceptronSimple <- function(datos, B, funcion, Lr, epochs) {
    X <- cbind(1, datos[, -ncol(datos)])  # Agregar una columna de sesgo
    Y <- datos[, ncol(datos)]
    W <- rep(1, ncol(X))

    for (x in 1:epochs) {
        for (y in 1:nrow(datos)) {
            predic <- funcion(sum(X[y,] * W))
            error <- Y[y] - predic  # Corregir la posición del paréntesis
            delta <- Lr * error * X[y,]
            W <- W + delta
			
			if(x == epochs){
				print(predic)
			}
        }
    }

    return(W)
}

or <- data.frame(
    x1 = c(0, 0, 1, 1),
    x2 = c(0, 1, 0, 1),
    y = c(0, 1, 1, 1)
)

and <- data.frame(
	x1 = c(0,0,1,1),
	x2 = c(0,1,0,1),
	 y = c(0,0,0,1)
)

resultado <- perceptronSimple(or, 1, sigmoide, 0.1, 5000)  # Aumenté las épocas para una mejor convergencia
print(resultado)

resultado <- perceptronSimple(and, 1, sigmoide, 0.1, 5000)  # Aumenté las épocas para una mejor convergencia
print(resultado)