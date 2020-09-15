library(DBI)
library(dbplyr)
library(dplyr)
library(reshape)
library(tidyr)
library(janitor)
library(Rfast)
library(DescTools)
library(lubridate)
library(hms)
library(stringr)
library(ggplot2)
library(lpSolve)

#la funcion objetivo va a ser la cantidad de meseros "x" por cada horario de 7 am. a 23pm. (9 horarios)

# 07:00am - 15:00pm
# 09:00am - 16:00pm
# 10:00am - 17:00pm
# 11:00am - 18:00pm
# 13:00pm - 20:00pm
# 12:00pm - 17:00pm
# 15:00pm - 22:00pm
# 19:00pm - 23:00pm
# 16:00pm - 23:00pm

#RESTRICCIONES

#la funcion a continuacion crea todos los posibles horarios (de 8 horas matutino y de 7 horas vespertino)
#posibles en el horario establecido por la hora inicial y final

restriccion_horarios <- function(hora_ini, hora_final){
  
  n <- 0
  
  h_activas <- hora_final - hora_ini + 1
  
  df <- matrix(ncol = h_activas)
  
  lista <- c()
  
  while (n < 9) {
    
    var <-  c(rep(0,n),rep(1, ifelse(n<4, 8, 7)), rep(0, h_activas - ifelse(n<4, 8, 7) - n))
    
    nombre <- paste0("h", n)
    
    lista[n+1] <- nombre
    
    df <- rbind(df, var)
    
    n <- n + 1
    
  }
  
  final <- t(df[-1,])
  
  rownames(final) <- c(hora_ini:hora_final) 
  colnames(final) <- lista
  
  return(final)
  
}

restricciones <- restriccion_horarios(7,21)

horarios <- colnames(restricciones)

codh <- readxl::read_excel("cod_horarios.xlsx")%>%
  mutate(Hora_entrada = format(Hora_entrada, "%I:%M %p"),
         Hora_salida = format(Hora_salida, "%I:%M %p"))


lista_unidades <- #ingresa aqui el listado de unidades de negocio 

  
#funcion de horario a partir de las 7
  
#el el caso de que existan formatos diferentes de unidades de negocio 
  #y tengan distintos parametros, los puedes cambiar

#la base de datos de ventas debe de estar estructurada de la siguiente manera:
  
#Codigo_Unidad | Fecha | Hora_aprestura_cuanta | Hora_cierre_cuenta | canal_venta | 
  

optimizador <- function(lista, formato){
  
  df2 <- data.frame(matrix(ncol = 5, nrow = 0))
  
  
  if(formato=="Café"){ 
    
    restricciones <- restriccion_horarios(7,21)[3:15,3:9]
    
    objetivo <- c(rep(1,7))
    
    dir <- c(rep(">=", 13))
    
    hora_ini <- 9
    
    perc.estadia <- 0.74

  }
  else{
    
    restricciones <- restriccion_horarios(7,21)
    
    objetivo <- c(rep(1,9))
    
    dir <- c(rep(">=", 15))
    
    hora_ini <- 7
    
    perc.estadia <- 0.84

  }
  
  horarios <- colnames(restricciones)
  
  for(a in lista){


    db_vta <- db #base de datos de ventas 
      filter(Codigo_Unidad == a)%>%
      mutate(Hora = format(C_chk_clsd_tm,"%h"),
             Hora_entrada = format(C_chk_open_tm,"%h"),
             mes = sql("DATEPART(MONTH, MicrosBsnzDate)"),
             año = sql("DATEPART(YEAR, MicrosBsnzDate)"),
             Canal_master = ifelse(!is.na(Mesa) && Propina != 0 && Propina != 7.75, "MESAS", "DELIVERY-PICKUP"),
             fac = 1)%>%
      group_by(Codigo_Unidad, MicrosBsnzDate, Hora_entrada, Hora, mes, año)%>%
      summarise(Venta_Piso = sum(VentaMicros_SINIVA[Canal_master=="MESAS"], na.rm = T),
                Venta = sum(VentaMicros_SINIVA, na.rm = T))%>%
      mutate(Platos = ceiling((Venta)/ifelse(Hora_entrada < 12, 85, 130)),
             Personas_s = ceiling((Venta_Piso)/ifelse(Hora_entrada < 12, 85, 130)))%>%
      collect()
    
    
    hra_c <- db_vta %>%
      ungroup()%>%
      select(Hora = Hora_entrada, Personas_entrada = Personas_s, Platos, MicrosBsnzDate, Codigo_Unidad,
             mes, año)%>%
      full_join(.,db_vta[,-3])%>%
      replace(., is.na(.), 0)%>%
      mutate(
        Personas = ceiling((Personas_entrada*perc.estadia) + Personas_s),
        Meseros = ifelse(Personas < 15, 1, ceiling(Personas / 15)),
        Cocineros = round((((Platos*perc.estadia) + (Platos*(1-perc.estadia)))/4)^(1/1.7)))
    
    
    necesidad_prom <- hra_c %>%
      ungroup()%>%
      mutate(Hora = as.numeric(Hora))%>%
      filter(between(Hora, hora_ini, 22), año == 2019)%>%
      group_by(Hora)%>%
      summarise(Meseros_prom = ceiling(mean(Meseros)))
    
    
    for(i in c(1:12)){
      
      proyeccion <- hra_c %>%
        ungroup()%>%
        mutate(Hora = as.numeric(Hora))%>%
        filter(between(Hora, hora_ini,21), mes == i)%>%
        left_join(., necesidad_prom)%>%
        mutate(Meseros_proy = Meseros)%>%
        group_by(Hora)%>%
        summarise(Venta = sum(Venta),
                  Meseros = ceiling(max(quantile(Meseros_proy, 0.95, na.rm = T))),
                  Cocineros = ceiling(max(quantile(Cocineros, 0.95, na.rm = T))),
                  Meseros_promedio = ceiling(max(quantile(Meseros_prom,0.95, na.rm = T))))%>%
        full_join(., data.frame(Hora = c(hora_ini:21)))%>%
        replace_na(list(Meseros = 0, Cocineros = 0))%>%
        arrange(Hora)
      
      #solucion de restricciones
      
      puestos <- c("Cocineros", "Meseros")
      
      
          for(p in puestos){
            

            puestos <- function(df, name){
              
              get(name, df)
              
            }
            
            sol <- puestos(proyeccion, p)
      
            
            
            Solucion <- lpSolve::lp(direction = "min", 
                                    objective.in = objetivo, 
                                    const.mat = restricciones, 
                                    const.dir = dir, 
                                    const.rhs = sol,
                                    all.int=T)
            
            
            total <- Solucion$objval
            
            
            m_x_horario <- Solucion$solution
            
            
            df2 <- rbind(df2, data.frame(Unidad = a, Horario = horarios, Cantidad = m_x_horario, Puesto = p, Mes = i))
            
            
          }
          
    
      solucion_final <- df2 %>%
        left_join(., codh)%>%
        group_by(Puesto, Mes)%>%
        summarise(Cantidad = sum(Cantidad))
        
      
      sol_unidad <- df2 %>%
        left_join(., codh)%>%
        group_by(Unidad, Puesto, Hora_entrada, Hora_salida, Mes)%>%
        summarise(Cantidad = sum(Cantidad))
      
    }
    
  }
  
  return(list(resumen = solucion_final, unidades = sol_unidad))
  
}

RH_Bistros <- optimizador(lista_bistro, "Bistro")
RH_Cafe <- optimizador(lista_cafe, "Café")


optimizador_semanal <- function(lista, formato, semana, crecimiento){
  
  df <- data.frame(matrix(ncol = 5, nrow = 0))
  df2 <- data.frame(matrix(ncol = 4, nrow = 0))
  
  
  if(formato=="Café"){
    
    restricciones <- restriccion_horarios(7,21)[4:12,4:5]
    
    restricciones[9,1] <- 1
    
    objetivo <- c(rep(1,2))
    
    dir <- c(rep(">=", 9))
    
    hora_ini <- 10
    
    hora_final <- 18
    
    perc.estadia <- 0.74
    
  }
  else{
    
    restricciones <- restriccion_horarios(7,21)[2:14,2:8]
    
    objetivo <- c(rep(1,7))
    
    dir <- c(rep(">=", 13))
    
    hora_ini <- 8
    
    hora_final <- 20
    
    perc.estadia <- 0.84
    
  }
  
  horarios <- colnames(restricciones)
  
  for(a in lista){
    
    semana_actual <- semana
    
    db_vta_entrada <- db %>%
      filter(Codigo_Unidad == a)%>%
      mutate(Hora = format(C_chk_open_tm,"%h"),
             mes = sql("DATEPART(MONTH, MicrosBsnzDate)"),
             año = sql("DATEPART(YEAR, MicrosBsnzDate)"),
             semana = sql("DATEPART(ISO_WEEK, MicrosBsnzDate)"),
             Canal_master = ifelse(!is.na(Mesa) && Propina != 0 && Propina != 7.75, "MESAS", "DELIVERY-PICKUP"),
             fac = 1)%>%
      filter(semana == semana_actual,
             año==2020)%>%
      group_by(Codigo_Unidad, MicrosBsnzDate, Hora, semana, año)%>%
      summarise(Venta_Piso = sum(VentaMicros_SINIVA[Canal_master=="MESAS"], na.rm = T)*crecimiento,
                Venta = sum(VentaMicros_SINIVA, na.rm = T)*crecimiento)%>%
      mutate(Platos = ceiling((Venta)/ifelse(Hora < 12, 85, 130)),
             Personas_entrada = ceiling((Venta_Piso)/ifelse(Hora < 12, 85, 130)))%>%
      collect()%>%
      replace(., is.na(.),0)%>%
      select(-Venta, -Venta_Piso)
    
    db_vta_salida <- db %>%
      filter(Codigo_Unidad == a)%>%
      mutate(Hora = format(C_chk_clsd_tm,"%h"),
             Hora_entrada = format(C_chk_open_tm,"%h"),
             mes = sql("DATEPART(MONTH, MicrosBsnzDate)"),
             año = sql("DATEPART(YEAR, MicrosBsnzDate)"),
             semana = sql("DATEPART(ISO_WEEK, MicrosBsnzDate)"),
             Canal_master = ifelse(!is.na(Mesa) && Propina != 0 && Propina != 7.75, "MESAS", "DELIVERY-PICKUP"),
             fac = 1,
             vper = sum(VentaMicros_SINIVA[(Canal_master=="MESAS")], na.rm = T)*crecimiento/ifelse(Hora_entrada < 12, 85,130))%>%
      filter(semana == semana_actual,
             año==2020)%>%
      ungroup()%>%
      group_by(Codigo_Unidad, MicrosBsnzDate, Hora, semana, año)%>%
      summarise(
        Personas_salida = ceiling(sum(vper, na.rm = TRUE))
                )%>%
      collect()%>%
      replace(., is.na(.),0)
    
    
    final <- left_join(db_vta_entrada,db_vta_salida)%>%
      mutate(Hora = factor(Hora, levels = c(1:24)))%>%
      arrange(MicrosBsnzDate,Hora)
    
    
    hra_c <- db_vta %>%
      ungroup()%>%
      select(Hora = Hora_entrada, Personas_entrada = Personas_s, Platos, MicrosBsnzDate, Codigo_Unidad,
             semana, año)%>%
      full_join(.,db_vta[,-3])%>%
      replace(., is.na(.), 0)%>%
      mutate(
        Personas = ceiling((Personas_entrada*perc.estadia) + Personas_s),
        Meseros = ifelse(Personas < 15, 1, ceiling(Personas / 15)),
        Cocineros = round((((Platos*perc.estadia) + (Platos*(1-perc.estadia)))/4)^(1/1.7)))
    
    necesidad_prom <- hra_c %>%
      ungroup()%>%
      mutate(Hora = as.numeric(Hora),
             Dia = weekdays(as.Date(MicrosBsnzDate)))%>%
      filter(between(Hora, hora_ini, hora_final), año == 2020)%>%
      group_by(Hora)%>%
      summarise(Meseros_prom = ceiling(mean(Meseros)))
    
      proyeccion <- hra_c %>%
        ungroup()%>%
        mutate(Hora = as.numeric(Hora))%>%
        filter(between(Hora, hora_ini, hora_final))%>%
        left_join(., necesidad_prom)%>%
        mutate(Meseros_proy = Meseros)%>%
        group_by(Hora)%>%
        summarise(Venta = sum(Venta),
                  Meseros = ceiling(max(quantile(Meseros_proy, 0.95, na.rm = T))),
                  Cocineros = ceiling(max(quantile(Cocineros, 0.95, na.rm = T))),
                  Meseros_promedio = ceiling(max(quantile(Meseros_prom,0.95, na.rm = T))))%>%
        full_join(., data.frame(Hora = c(hora_ini:hora_final)))%>%
        replace_na(list(Meseros = 0, Cocineros = 0))%>%
        arrange(Hora)
      
      proyeccion_dia <- hra_c %>%
        ungroup()%>%
        mutate(Hora = as.numeric(Hora),
               Dia = format(as.Date(MicrosBsnzDate), "%A"))%>%
        filter(between(Hora, hora_ini,hora_final))%>%
        left_join(., necesidad_prom)%>%
        mutate(Meseros_proy = Meseros)%>%
        group_by(Hora, Dia)%>%
        summarise(Venta = sum(Venta),
                  Meseros = ceiling(max(quantile(Meseros_proy, 0.95, na.rm = T))),
                  Cocineros = ceiling(max(quantile(Cocineros, 0.95, na.rm = T))),
                  Meseros_promedio = ceiling(max(quantile(Meseros_prom,0.95, na.rm = T))))%>%
        full_join(., data.frame(Hora = c(hora_ini:hora_final)))%>%
        replace_na(list(Meseros = 0, Cocineros = 0))%>%
        arrange(Hora)
      
      #solucion de restricciones
      
      puestos <- c("Cocineros", "Meseros")
      
      proyecciones <- c("proyeccion", "proyeccion_dia")
      
      for(p in puestos){
        
        
        puestos <- function(df, name){
          
          get(name, df)
          
        }
        
        sol <- puestos(proyeccion, p)
        
        
        
        Solucion <- lpSolve::lp(direction = "min", 
                                objective.in = objetivo, 
                                const.mat = restricciones, 
                                const.dir = dir, 
                                const.rhs = sol,
                                all.int=T)
        
        
        total <- Solucion$objval
        
        
        m_x_horario <- Solucion$solution
        
        
        df2 <- rbind(df2, data.frame(Unidad = a, Horario = horarios, Cantidad = m_x_horario, Puesto = p))
        
        
        #---proyeccion x dia
        
        lista_dias <- unique(proyeccion_dia$Dia)
        
        for(d in lista_dias){
          
          
          proyeccion_2 <- proyeccion_dia %>%
            filter(Dia == d)
          
          
          sol <- puestos(proyeccion_2, p)
          
          
          Solucion <- lpSolve::lp(direction = "min", 
                                  objective.in = objetivo, 
                                  const.mat = restricciones, 
                                  const.dir = dir, 
                                  const.rhs = sol,
                                  all.int=T)
          
          
          total <- Solucion$objval
          
          
          m_x_horario <- Solucion$solution
          
          
          df <- rbind(df, data.frame(Unidad = a, Horario = horarios, Cantidad = m_x_horario, Puesto = p, Dia = d))
          
          
        }
      
      
      solucion_final <- df2 %>%
        left_join(., codh)%>%
        group_by(Puesto)%>%
        summarise(Cantidad = sum(Cantidad))
      
      
      sol_unidad <- df2 %>%
        left_join(., codh)%>%
        group_by(Unidad, Puesto, Hora_entrada, Hora_salida)%>%
        summarise(Cantidad = sum(Cantidad))
      
      
      sol_dia_unidad <- df %>%
        left_join(., codh)%>%
        group_by(Unidad, Puesto, Dia, Hora_entrada, Hora_salida)%>%
        summarise(Cantidad = sum(Cantidad))
          
    }
    
  }
  
  return(list(resumen = solucion_final, unidades = sol_unidad, requerimientoDia = sol_dia_unidad))
  
}


RH_bist_25 <- optimizador_semanal(lista_bistro, "Bistro", 36, 1)
RH_cafe_25 <- optimizador_semanal(lista_cafe, "Café", 36, 1)

RH_cafe_25$requerimientoDia

tfinal <- rbind(RH_bist_25$unidades, RH_cafe_25$unidades) %>%
  dplyr::rename(Codigo_Unidad=Unidad)%>%
  mutate(Horario = paste(Hora_entrada, Hora_salida, sep="-"))%>%
  left_join(., info_unidades[,c(1,3,6)])

tdia <-  rbind(RH_bist_25$requerimientoDia, RH_cafe_25$requerimientoDia) %>%
  dplyr::rename(Codigo_Unidad=Unidad)%>%
  mutate(Horario = paste(Hora_entrada, Hora_salida, sep="-"))%>%
  left_join(., info_unidades[,c(1,3,6)])

openxlsx::write.xlsx(list("RESUMEN_DATA"=tfinal, "REQUERIMIENTO X DIA"=tdia), "RH_necesidad_semana37.xlsx", row.names=F)




#------- tabla final -------


tabla <- rbind(RH_Bistros$unidades, RH_Cafe$unidades)%>%
  group_by(Unidad, Puesto, Mes)%>%
  summarise(Cantidad = sum(Cantidad))%>%
  dplyr::rename(Codigo_Unidad=Unidad)%>%
  left_join(., venta2019)%>%
  filter(año == 2019)%>%
  left_join(., info_unidades[,c(1,3,6)])%>%
  filter(!is.na(Venta))


plot(tabla)

openxlsx::write.xlsx(tabla, "RH_necesidad.xlsx", row.names=F)


x <- tabla$Venta[tabla$Puesto== "Cocineros" & tabla$Nombre %like% "%Caya%"]/4.33/1000
y <- tabla$Cantidad[tabla$Puesto== "Cocineros" & tabla$Nombre %like% "%Caya%"]

#& tabla$Detalle_formato == "Bistro"

x <- tabla$Venta_Piso[tabla$Puesto== "Meseros"]/1000
y <- tabla$Cantidad[tabla$Puesto== "Meseros"]


DF <- data.frame(x, y)

ggplot(DF, aes(x = x, y = y)) +
  geom_point() +
  stat_smooth(method = 'lm', aes(colour = 'linear'), se = FALSE) +
  stat_smooth(method = 'lm', formula = y ~ poly(x,2), aes(colour = 'polynomial'), se= FALSE) +
  #stat_smooth(method = 'nls', formula = y ~ a * log(x) + b, aes(colour = 'logarithmic'), se = FALSE, method.args = list(start = list(a = 1, b = 1))) +
  #stat_smooth(method = 'nls', formula = y ~ a * exp(b * x), aes(colour = 'Exponential'), se = FALSE, method.args = list(start = list(a = 1, b = 1))) +
  theme_bw() +
  scale_colour_brewer(name = 'Trendline', palette = 'Set2') +
  scale_x_continuous(breaks = seq(0,10000,by=100))+
  scale_y_continuous(breaks = seq(0,20,by=1))+
  xlab("Ventas (EN MILES)")+
  ylab("Cantidad de Meseros")+
  labs(title = "CANTIDAD DE MESEROS BISTROS")
