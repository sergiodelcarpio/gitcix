
x <- "HOLA"
y <- "UNTRM"


paste(x, y, sep = "-")


area_cir <- function(radio){
  
  pi*radio*radio
  
  
}



area_cir(radio=10)



calcular_IMC <- function(peso, altura) {
  # Calcula el IMC
  imc <- peso / (altura^2)
  
  # Genera un mensaje de alerta según el valor del IMC
  if (imc < 18.5) {
    mensaje <- "Bajo peso. Se recomienda una evaluación médica."
  } else if (imc >= 18.5 && imc < 24.9) {
    mensaje <- "Peso normal. ¡Sigue manteniéndote saludable!"
  } else if (imc >= 25 && imc < 29.9) {
    mensaje <- "Sobrepeso. Considera mejorar tu alimentación y actividad física."
  } else if (imc >= 30 && imc < 34.9) {
    mensaje <- "Obesidad grado 1. Consulta con un profesional de salud."
  } else if (imc >= 35 && imc < 39.9) {
    mensaje <- "Obesidad grado 2. Se recomienda atención médica urgente."
  } else {
    mensaje <- "Obesidad grado 3. Atención médica inmediata es necesaria."
  }
  return(paste("Tu IMC es", round(imc, 2), ":", mensaje))
}

peso <-100
altura <- 1.78
calcular_IMC(peso, altura)

