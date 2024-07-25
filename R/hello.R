#Realice una o varias funciones recursivas para (x^y)^z

# Función para pedir un número entero al usuario
read_integer <- function(prompt) {
  repeat {
    input <- readline(prompt = prompt)
    value <- as.integer(input)

    if (!is.na(value) && value == as.numeric(input)) {
      return(value)
    } else {
      cat("Por favor, ingrese un número entero válido.\n")
    }
  }
}

# Función recursiva para calcular x^y
power_xy <- function(x, y) {
  if (y == 0) {
    return(1) # Cualquier número elevado a la potencia 0 es 1
  } else {
    return(x * power_xy(x, y - 1))
  }
}

# Función recursiva para calcular (x^y)^z
power_xyz <- function(x, y, z) {
  # Primero calcular x^y
  xy_result <- power_xy(x, y)
  # Luego calcular (x^y)^z
  if (z == 0) {
    return(1) # Cualquier número elevado a la potencia 0 es 1
  } else {
    return(xy_result * power_xyz(xy_result, 1, z - 1))
  }
}

# Pedir valores enteros al usuario
x <- read_integer("Introduce el valor de x (entero): ")
y <- read_integer("Introduce el valor de y (entero): ")
z <- read_integer("Introduce el valor de z (entero): ")

# Llamar a la función y mostrar el resultado
result <- power_xyz(x, y, z)
cat(sprintf("El resultado de (%d^%d)^%d es: %d\n", x, y, z, result))
