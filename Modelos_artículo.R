# Packages
library(INLA)
library(mgcv)
library(stats)

# Transformación de datos
datos$visitors2 <- log(datos$visitors)
datos$density2 <- sqrt(datos$density)
datos$meanFID2 <- sqrt(datos$meanFID)

# Modelos 

## Inferencia clásica

### Modelo sin transformar los datos
M1.C <-
  glm(meanFID ~ density + visitors,
      family = Gamma(link = "log"),
      data = FID)

### Modelo con las variables transformadas
M2.C <-
  glm(meanFID ~ density2 + visitors2,
      family = Gamma(link = "identity"),
      data = FID)

### Modelo suavizado
M3.C <-
  gam(
    meanFID ~ s(density) + s(visitors, k = 8),
    family = Gamma(link = "log"),
    data = FID
  )

### Modelo con densidad transformada y polinomio de grado 3
M4.C <-
  glm(
    meanFID ~ density2 + visitors + I(visitors ^ 2) + I(visitors ^ 3),
    family = Gamma(link = "inverse"),
    data = FID
  )

## Inferencia bayesiana

### Modelo sin transformar los datos
M1.B <-
  inla(
    meanFID ~ visitors + density,
    data = datos,
    family = "gamma",
    control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
  )

### Modelo con log y raiz cuadrada
M2.B <-
  inla(
    meanFID ~ visitors2 + density2,
    data = datos,
    family = "gamma",
    control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
  )

### Modelo suavizado
M3.B <-
  inla(
    meanFID ~ f(visitors, model = "rw2") + f(density, model = "rw2"),
    data = datos,
    family = "gamma",
    control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
  )

### Modelo polinomio y raiz cuadrada
M4.B <-
  inla(
    meanFID ~ visitors + I(visitors ^ 2) + I(visitors ^ 3) + density2,
    data = datos,
    family = "gamma",
    control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
  )

