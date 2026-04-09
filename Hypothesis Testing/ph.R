#Taller en R prueba de hipotesis

#HERNANDEZ MATIAS JHAN CARLOS
#PEÑA ORTEGA SAMUEL NISSI
#RUZ TERÁN ANDRÉS FELIPE
#VEGA ROJANO JUAN CARLOS

base1=Estudiantes #Cambiar el nombre de la base
head(base1,4) #Encabezado de la base
datos=base1[1:35,] #Filtrar filas
dim(datos) #Dimension de la base
str(datos) #Caracterización de las variables
names(datos) #Nombres de las variables
class(datos$P3) #Identificar tipo de variable en una sola

p3=as.numeric(datos$P3) #Convertir variable a numerica

"H0 la media de la calificación P3 es 3.5
H1: La media de la calificación P3 es diferente de 3.5"

#Media Muestras Grandes
mu=3.5
xbarra=mean(p3) #Media Muestral
s=sd(p3) #Desviación Muestral
s2=var(p3) #Varianza Muestral
n=length(p3) #Tamaño de la Muestra
sx=s/sqrt(n) #Desviación de la Media Muestral
alfa=0.05 #Nivel de Significancia (Error)

#Valor de Prueba
z=(xbarra-mu)/sx;z

#Valor Critico
z_critico=qnorm(alfa/2);z_critico
z_criticopo=qnorm(1-alfa/2);z_criticopo

#Prueba Bilateral
if(z > z_critico & z < z_criticopo){
  resultado="Se acepta H0"
} else {
  resultado="No se acepta H0"
}
cat("Resultado de la prueba:", resultado, "\n")

#Prueba Unilateral Izquierda
z_critico=qnorm(alfa)

if(z > z_critico){
  resultado="Se acepta H0"
} else {
  resultado="No se acepta H0"
}
cat("Resultado de la prueba:", resultado, "\n")

#Prueba Unilateral Derecha
z_critico=qnorm(1-alfa)

if(z < z_critico){
  resultado="Se acepta H0"
} else {
  resultado="No se acepta H0"
}
cat("Resultado de la prueba:", resultado, "\n")

#Bilateral, cola_izquierda, cola_derecha
# =, <, >
phmediag=function(x,mu,alfa,tipo_prueba="bilateral"){
  xbarra=mean(x) #Media Muestral
  s=sd(x) #Desviación Muestral
  s2=var(x) #Varianza Muestral
  n=length(x) #Tamaño de la Muestra
  sx=s/sqrt(n) #Desviación de la Media Muestral
  
  #Valor de Prueba
  z=(xbarra-mu)/sx;z
  
  if(tipo_prueba=="bilateral"){
    #Valor Critico
    z_critico=qnorm(alfa/2)
    z_criticopo=qnorm(1-alfa/2)
    
    #Prueba Bilateral
    if(z > z_critico & z < z_criticopo){
      resultado="Se acepta H0"
    } else {
      resultado="No se acepta H0"
    }
    
  }else if (tipo_prueba=="cola_izquierda"){
    z_critico=qnorm(alfa)
    
    if(z > z_critico){
      resultado="Se acepta H0"
    } else {
      resultado="No se acepta H0"
    }
    
  }else if(tipo_prueba=="cola_derecha"){
    z_critico=qnorm(1-alfa)
    
    if(z < z_critico){
      resultado="Se acepta H0"
    } else {
      resultado="No se acepta H0"
    }
  }
  return(data.frame(valor_z=z,valor_critico=z_critico,decision=resultado))
  
}

phmediag(p3,mu=3.5,alfa=0.05,tipo_prueba="bilateral")


#==============================================================================#


#Prueba de Hipotesis Simplificada
phmediag2=function(xbarra,s,n,mu,alfa,tipo_prueba="bilateral"){
  sx=s/sqrt(n) #Desviación de la Media Muestral
  
  #Valor de Prueba
  z=(xbarra-mu)/sx;z
  
  if(tipo_prueba=="bilateral"){
    #Valor Critico
    z_critico=qnorm(alfa/2)
    z_criticopo=qnorm(1-alfa/2)
    
    #Prueba Bilateral
    if(z > z_critico & z < z_criticopo){
      resultado="Se acepta H0"
    } else {
      resultado="No se acepta H0"
    }
    
  }else if (tipo_prueba=="cola_izquierda"){
    z_critico=qnorm(alfa)
    
    if(z > z_critico){
      resultado="Se acepta H0"
    } else {
      resultado="No se acepta H0"
    }
    
  }else if(tipo_prueba=="cola_derecha"){
    z_critico=qnorm(1-alfa)
    
    if(z < z_critico){
      resultado="Se acepta H0"
    } else {
      resultado="No se acepta H0"
    }
  }
  return(data.frame(valor_z=z,valor_critico=z_critico,decision=resultado))
  
}

phmediag2(51.8,5.9,250,50,0.03,tipo_prueba="cola_derecha")


#==============================================================================#


#Prueba de Hipotesis Simplificada
phmediag2=function(xbarra,s,n,mu,alfa,tipo_prueba="bilateral"){
  sx=s/sqrt(n) #Desviación de la Media Muestral
  
  #Valor de Prueba
  z=(xbarra-mu)/sx;z
  
  if(tipo_prueba=="bilateral"){
    #Valor Critico
    z_critico=qnorm(alfa/2)
    z_criticopo=qnorm(1-alfa/2)
    
    #Prueba Bilateral
    if(z > z_critico & z < z_criticopo){
      resultado="Se acepta H0"
    } else {
      resultado="No se acepta H0"
    }
    
  }else if (tipo_prueba=="cola_izquierda"){
    z_critico=qnorm(alfa)
    
    if(z > z_critico){
      resultado="Se acepta H0"
    } else {
      resultado="No se acepta H0"
    }
    
  }else if(tipo_prueba=="cola_derecha"){
    z_critico=qnorm(1-alfa)
    
    if(z < z_critico){
      resultado="Se acepta H0"
    } else {
      resultado="No se acepta H0"
    }
  }
  return(data.frame(valor_z=z,valor_critico=z_critico,decision=resultado))
  
}

phmediag2(51.8,5.9,250,50,0.03,tipo_prueba="cola_derecha")


#==============================================================================#


#Prueba de Hipotesis Simplificada Pequeñas
phmediap=function(xbarra,s,n,mu,alfa,tipo_prueba="bilateral"){
  sx=s/sqrt(n) #Desviación de la Media Muestral
  
  #Valor de Prueba
  t=(xbarra-mu)/sx
  v=n-1 #Grados de Libertad
  
  if(tipo_prueba=="bilateral"){
    #Valor Critico
    t_critico=qt(alfa/2,v)
    t_criticopo=qt(1-alfa/2,v)
    
    #Prueba Bilateral
    if(t > t_critico & t < t_criticopo){
      resultado="Se acepta H0"
    } else {
      resultado="No se acepta H0"
    }
    
  }else if (tipo_prueba=="cola_izquierda"){
    t_critico=qt(alfa,v)
    
    if(t > t_critico){
      resultado="Se acepta H0"
    } else {
      resultado="No se acepta H0"
    }
    
  }else if(tipo_prueba=="cola_derecha"){
    t_critico=qt(1-alfa,v)
    
    if(t < t_critico){
      resultado="Se acepta H0"
    } else {
      resultado="No se acepta H0"
    }
  }
  return(data.frame(valor_t=t,valor_critico=t_critico,decision=resultado))
  
}

phmediap(51.8,5.9,250,50,0.03,tipo_prueba="cola_derecha")


#==============================================================================#


# Prueba de Hipótesis para Proporciones (una muestra)
phproporcion = function(X, N, ppo, alfa, tipo_prueba = "bilateral") {
  
  # Validación de entradas
  if (N <= 0 || ppo <= 0 || ppo >= 1 || alfa <= 0 || alfa >= 1) {
    stop("Entradas no válidas. Revise el tamaño de la muestra, proporción hipotética y alfa.")
  }
  
  # Cálculo de la proporción muestral
  pmu = X / N
  
  # Error estándar bajo la hipótesis nula
  se_p = sqrt((ppo * (1 - ppo)) / N)
  
  # Estadístico Z
  z_valor = (pmu - ppo) / se_p
  
  # Inicialización de los valores críticos y la decisión
  z_critico_inf = NA
  z_critico_sup = NA
  decision = ""
  
  # Pruebas unilaterales y bilaterales
  switch(tipo_prueba,
         "bilateral" = {
           z_critico_inf = qnorm(alfa / 2)
           z_critico_sup = qnorm(1 - alfa / 2)
           if (z_valor > z_critico_inf & z_valor < z_critico_sup) {
             decision = "Se acepta H0"
           } else {
             decision = "No se acepta H0"
           }
         },
         "cola_izquierda" = {
           z_critico_inf = qnorm(alfa)
           if (z_valor > z_critico_inf) {
             decision = "Se acepta H0"
           } else {
             decision = "No se acepta H0"
           }
         },
         "cola_derecha" = {
           z_critico_sup = qnorm(1 - alfa)
           if (z_valor < z_critico_sup) {
             decision = "Se acepta H0"
           } else {
             decision = "No se acepta H0"
           }
         },
         stop("Tipo de prueba no válido. Use 'bilateral', 'cola_izquierda' o 'cola_derecha'.")
  )
  
  # Retornamos los resultados
  return(data.frame(
    pmu = round(pmu, 4),
    ppo = ppo,
    z_valor = round(z_valor, 4),
    z_critico_inferior = round(z_critico_inf, 4),
    z_critico_superior = round(z_critico_sup, 4),
    decision = decision
  ))
}


# phproporcion (X, N, ppo, alfa, tipo_prueba = "")
print(phproporcion(378, 802, 0.50, 0.10, tipo_prueba = "cola_izquierda"))


#==============================================================================#


# Prueba de Hipótesis para Diferencia de Proporciones (dos muestras)
phdiffp = function(x1, x2, n1, n2, alfa, tipo_prueba = "bilateral") {
  
  # Validación de entradas
  if (n1 <= 0 || n2 <= 0 || alfa <= 0 || alfa >= 1) {
    stop("Entradas no válidas. Revise los tamaños de las muestras y alfa.")
  }
  
  # Proporciones de éxito en cada muestra
  p1 = x1 / n1
  p2 = x2 / n2
  
  # Proporción combinada (ponderada)
  p_comb = (x1 + x2) / (n1 + n2)
  
  # Error estándar combinado
  se_comb = sqrt(p_comb * (1 - p_comb) * ((1 / n1) + (1 / n2)))
  
  # Estadístico Z
  z_valor = (p1 - p2) / se_comb
  
  # Inicialización de valores críticos y decisión
  z_critico_inf = NA
  z_critico_sup = NA
  decision = ""
  
  # Pruebas unilaterales y bilaterales
  switch(tipo_prueba,
         "bilateral" = {
           z_critico_inf = qnorm(alfa / 2)
           z_critico_sup = qnorm(1 - alfa / 2)
           if (z_valor > z_critico_inf & z_valor < z_critico_sup) {
             decision = "Se acepta H0"
           } else {
             decision = "No se acepta H0"
           }
         },
         "cola_izquierda" = {
           z_critico_inf = qnorm(alfa)
           if (z_valor > z_critico_inf) {
             decision = "Se acepta H0"
           } else {
             decision = "No se acepta H0"
           }
         },
         "cola_derecha" = {
           z_critico_sup = qnorm(1 - alfa)
           if (z_valor < z_critico_sup) {
             decision = "Se acepta H0"
           } else {
             decision = "No se acepta H0"
           }
         },
         stop("Tipo de prueba no válido. Use 'bilateral', 'cola_izquierda' o 'cola_derecha'.")
  )
  
  # Retornamos los resultados
  return(data.frame(
    z_valor = round(z_valor, 4),
    z_critico_inferior = round(z_critico_inf, 4),
    z_critico_superior = round(z_critico_sup, 4),
    decision = decision
  ))
}


# phdiffp (x1, x2, n1, n2, alfa, tipo_prueba = "")
print(phdiffp(52, 56, 500, 470, 0.02, tipo_prueba = "bilateral"))


#==============================================================================#


#Diferencia de medias (3 casos)
#Diferencia de medias - Muestras grandes (Varianzas conocidas)
phdiffmedg = function(xbarra1, xbarra2, s1, s2, n1, n2, alfa, tipo_prueba = "bilateral") {
  
  # Validación de entradas
  if (n1 <= 0 || n2 <= 0 || s1 <= 0 || s2 <= 0 || alfa <= 0 || alfa >= 1) {
    stop("Entradas no válidas. Revise tamaños de las muestras, desviaciones estándar y alfa.")
  }
  
  # Cálculo del error estándar de la diferencia de medias
  se_dif = sqrt((s1^2 / n1) + (s2^2 / n2))
  
  # Cálculo del estadístico z
  z_valor = (xbarra1 - xbarra2) / se_dif
  
  # Inicialización de valores críticos y decisión
  z_critico_inf = NA
  z_critico_sup = NA
  decision = ""
  
  # Pruebas unilaterales y bilaterales
  switch(tipo_prueba,
         "bilateral" = {
           z_critico_inf = qnorm(alfa / 2)
           z_critico_sup = qnorm(1 - alfa / 2)
           if (z_valor > z_critico_inf & z_valor < z_critico_sup) {
             decision = "Se acepta H0"
           } else {
             decision = "No se acepta H0"
           }
         },
         "cola_izquierda" = {
           z_critico_inf = qnorm(alfa)
           if (z_valor > z_critico_inf) {
             decision = "Se acepta H0"
           } else {
             decision = "No se acepta H0"
           }
         },
         "cola_derecha" = {
           z_critico_sup = qnorm(1 - alfa)
           if (z_valor < z_critico_sup) {
             decision = "Se acepta H0"
           } else {
             decision = "No se acepta H0"
           }
         },
         stop("Tipo de prueba no válido. Use 'bilateral', 'cola_izquierda' o 'cola_derecha'.")
  )
  
  # Retornamos los resultados
  return(data.frame(
    z_valor = round(z_valor, 4),
    z_critico_inferior = round(z_critico_inf, 4),
    z_critico_superior = round(z_critico_sup, 4),
    decision = decision
  ))
}

# phdiffmedg(xbarra1, xbarra2, s1, s2, n1, n2, alfa, tipo_prueba="")
print(phdiffmedg(3.56, 3.680, 0.89, 0.966, 86, 72, 0.03, tipo_prueba = "bilateral"))


#==============================================================================#

#Prueba de Hipótesis para Diferencia de Medias - Muestras pequeñas (Varianzas desconocidas e iguales)
phdiffmedp = function(xbarra1, xbarra2, s1, s2, n1, n2, alfa, tipo_prueba = "bilateral") {
  
  # Validación de entradas
  if (n1 <= 1 || n2 <= 1 || s1 <= 0 || s2 <= 0 || alfa <= 0 || alfa >= 1) {
    stop("Entradas no válidas. Revise los tamaños de las muestras, desviaciones estándar y alfa.")
  }
  
  # Cálculo de la desviación estándar combinada
  sp = sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2))
  
  # Error estándar combinado
  sx = sp * sqrt(1/n1 + 1/n2)
  
  # Valor de prueba t
  t_valor = (xbarra1 - xbarra2) / sx
  
  # Grados de libertad
  v = n1 + n2 - 2
  
  # Inicialización de los valores críticos y la decisión
  t_critico_inf = NA
  t_critico_sup = NA
  decision = ""
  
  # Pruebas unilaterales y bilaterales
  switch(tipo_prueba,
         "bilateral" = {
           t_critico_inf = qt(alfa / 2, v)
           t_critico_sup = qt(1 - alfa / 2, v)
           if (t_valor > t_critico_inf & t_valor < t_critico_sup) {
             decision = "Se acepta la hipótesis nula"
           } else {
             decision = "No se acepta H0"
           }
         },
         "cola_izquierda" = {
           t_critico_inf = qt(alfa, v)
           if (t_valor > t_critico_inf) {
             decision = "Se acepta la hipótesis nula"
           } else {
             decision = "No se acepta H0"
           }
         },
         "cola_derecha" = {
           t_critico_sup = qt(1 - alfa, v)
           if (t_valor < t_critico_sup) {
             decision = "Se acepta la hipótesis nula"
           } else {
             decision = "No se acepta H0"
           }
         },
         stop("Tipo de prueba no válido. Use 'bilateral', 'cola_izquierda' o 'cola_derecha'.")
  )
  
  # Retornamos los resultados
  return(data.frame(
    t_valor = round(t_valor, 4),
    t_critico_inferior = round(t_critico_inf, 4),
    t_critico_superior = round(t_critico_sup, 4),
    decision = decision
  ))
}


# phdiffmedp(xbarra1, xbarra2, s1, s2, n1, n2, alfa, tipo_prueba = "")
print(phdiffmedp(12.3, 10.1, 2.1, 2.4, 10, 12, 0.05, tipo_prueba = "bilateral"))


#==============================================================================#


# Prueba de Hipótesis para Diferencia de Medias - Muestras Independientes (Varianzas desconocidas y diferentes)
phdiffmedi = function(xbarra1, xbarra2, s1, s2, n1, n2, alfa, tipo_prueba = "bilateral") {
  
  # Validación de entradas
  if (n1 <= 1 || n2 <= 1 || s1 <= 0 || s2 <= 0 || alfa <= 0 || alfa >= 1) {
    stop("Entradas no válidas. Revise los tamaños de las muestras, desviaciones estándar y alfa.")
  }
  
  # Cálculo del error estándar combinado
  se = sqrt((s1^2 / n1) + (s2^2 / n2))
  
  # Valor de prueba t
  t_valor = (xbarra1 - xbarra2) / se
  
  # Cálculo de los grados de libertad con la fórmula de Welch
  num = (s1^2 / n1 + s2^2 / n2)^2
  denom = ((s1^2 / n1)^2 / (n1 - 1)) + ((s2^2 / n2)^2 / (n2 - 1))
  v = num / denom
  
  # Inicialización de los valores críticos y la decisión
  t_critico_inf = NA
  t_critico_sup = NA
  decision = ""
  
  # Pruebas unilaterales y bilaterales
  switch(tipo_prueba,
         "bilateral" = {
           t_critico_inf = qt(alfa / 2, v)
           t_critico_sup = qt(1 - alfa / 2, v)
           if (t_valor > t_critico_inf & t_valor < t_critico_sup) {
             decision = "Se acepta H0"
           } else {
             decision = "No se acepta H0"
           }
         },
         "cola_izquierda" = {
           t_critico_inf = qt(alfa, v)
           if (t_valor > t_critico_inf) {
             decision = "Se acepta H0"
           } else {
             decision = "No se acepta H0"
           }
         },
         "cola_derecha" = {
           t_critico_sup = qt(1 - alfa, v)
           if (t_valor < t_critico_sup) {
             decision = "Se acepta H0"
           } else {
             decision = "No se acepta H0"
           }
         },
         stop("Tipo de prueba no válido. Use 'bilateral', 'cola_izquierda' o 'cola_derecha'.")
  )
  
  # Retornamos los resultados
  return(data.frame(
    t_valor = round(t_valor, 4),
    t_critico_inferior = round(t_critico_inf, 4),
    t_critico_superior = round(t_critico_sup, 4),
    grados_de_libertad = round(v, 4),
    decision = decision
  ))
}
# phdiffmedi(xbarra1, xbarra2, s1, s2, n1, n2, alfa, tipo_prueba = "")
print(phdiffmedi(7.2, 6.1, 1.8, 2.1, 15, 12, 0.05, tipo_prueba = "bilateral"))


#==============================================================================#


#Prueba para varianza (1 muestra)
phvarianza = function(s2, n, sigma0_2, alfa, tipo_prueba = "bilateral") {
  
  # Validación de entradas
  if (n <= 1 || s2 <= 0 || sigma0_2 <= 0 || alfa <= 0 || alfa >= 1) {
    stop("Entradas no válidas. Revise el tamaño de la muestra, varianza muestral, varianza hipotética y alfa.")
  }
  
  # Cálculo del estadístico chi-cuadrado
  chi2_valor = (n - 1) * s2 / sigma0_2
  
  # Inicialización de los valores críticos y la decisión
  chi2_critico_inf = NA
  chi2_critico_sup = NA
  decision = ""
  
  # Pruebas unilaterales y bilaterales
  switch(tipo_prueba,
         "bilateral" = {
           chi2_critico_inf = qchisq(alfa / 2, n - 1)
           chi2_critico_sup = qchisq(1 - alfa / 2, n - 1)
           if (chi2_valor > chi2_critico_inf & chi2_valor < chi2_critico_sup) {
             decision = "Se acepta H0"
           } else {
             decision = "No se acepta H0"
           }
         },
         "cola_izquierda" = {
           chi2_critico_inf = qchisq(alfa, n - 1)
           if (chi2_valor > chi2_critico_inf) {
             decision = "Se acepta H0"
           } else {
             decision = "No se acepta H0"
           }
         },
         "cola_derecha" = {
           chi2_critico_sup = qchisq(1 - alfa, n - 1)
           if (chi2_valor < chi2_critico_sup) {
             decision = "Se acepta H0"
           } else {
             decision = "No se acepta H0"
           }
         },
         stop("Tipo de prueba no válido. Use 'bilateral', 'cola_izquierda' o 'cola_derecha'.")
  )
  
  # Retornamos los resultados
  return(data.frame(
    chi2_valor = round(chi2_valor, 4),
    chi2_critico_inferior = round(chi2_critico_inf, 4),
    chi2_critico_superior = round(chi2_critico_sup, 4),
    grados_de_libertad = n - 1,
    decision = decision
  ))
}

#phvarianza(s2, n, sigma0_2, alfa, tipo_prueba = "")
print(phvarianza(0.0003, 10, 0.0002, 0.05, tipo_prueba = "cola_derecha"))


#==============================================================================#


#Prueba para razón de varianzas (2 muestras)
phrazonv = function(s1_2, s2_2, n1, n2, alfa, tipo_prueba = "bilateral") {
  
  # Validación de entradas
  if (n1 <= 1 || n2 <= 1 || s1_2 <= 0 || s2_2 <= 0 || alfa <= 0 || alfa >= 1) {
    stop("Entradas no válidas. Revise los tamaños de las muestras, varianzas muestrales y alfa.")
  }
  
  # Cálculo del estadístico F
  F_valor = s1_2 / s2_2
  
  # Inicialización de los valores críticos y la decisión
  F_critico_inf = NA
  F_critico_sup = NA
  decision = ""
  
  # Pruebas unilaterales y bilaterales
  switch(tipo_prueba,
         "bilateral" = {
           F_critico_inf = qf(alfa / 2, n1 - 1, n2 - 1)
           F_critico_sup = qf(1 - alfa / 2, n1 - 1, n2 - 1)
           if (F_valor > F_critico_inf & F_valor < F_critico_sup) {
             decision = "Se acepta H0"
           } else {
             decision = "No se acepta H0"
           }
         },
         "cola_izquierda" = {
           F_critico_inf = qf(alfa, n1 - 1, n2 - 1)
           if (F_valor > F_critico_inf) {
             decision = "Se acepta H0"
           } else {
             decision = "No se acepta H0"
           }
         },
         "cola_derecha" = {
           F_critico_sup = qf(1 - alfa, n1 - 1, n2 - 1)
           if (F_valor < F_critico_sup) {
             decision = "Se acepta H0"
           } else {
             decision = "No se acepta H0"
           }
         },
         stop("Tipo de prueba no válido. Use 'bilateral', 'cola_izquierda' o 'cola_derecha'.")
  )
  
  # Retornamos los resultados
  return(data.frame(
    F_valor = round(F_valor, 4),
    F_critico_inferior = round(F_critico_inf, 4),
    F_critico_superior = round(F_critico_sup, 4),
    grados_de_libertad_n1 = n1 - 1,
    grados_de_libertad_n2 = n2 - 1,
    decision = decision
  ))
}

#phrazonv (s1_2, s2_2, n1, n2, alfa, tipo_prueba = "")
print(phrazonv(0.0003, 0.0001, 10, 20, 0.05, tipo_prueba = "cola_derecha"))


#HERNANDEZ MATIAS JHAN CARLOS
#PEÑA ORTEGA SAMUEL NISSI
#RUZ TERÁN ANDRÉS FELIPE
#VEGA ROJANO JUAN CARLOS
