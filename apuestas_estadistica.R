#paga mayor-----
#sacar resumen estadistico de la paga
apuestas %>%
  group_by(pais, 
           fecha) %>%
  filter(servia_mayor == "Si") %>%
  summarise(min= min(pago_mayor),
            mean= mean(pago_mayor),
            max= max(pago_mayor)) %>%
  #grafico boxplot
  pivot_longer(-c(pais, fecha),
               names_to = NULL) %>%
  ggplot() +
  geom_boxplot(aes(pais,
                   value)) +
  scale_y_continuous(n.breaks = 7) +
  facet_grid(. ~ fecha)


#ganancia neta
apuestas %>%
  group_by(pais, fecha) %>% 
  summarize(neto= sum((servia_mayor=="Si") * pago_mayor,
                      na.rm = T) - n()) %>%
  #promedio acierto pais
  #filter(neto>0) %>% summarize(prom_acierto= n()/max(apuestas$fecha))
  #grafico
  ggplot(aes(fecha, neto, color= pais)) + 
  geom_smooth()
  geom_line()



#ganancia neta
apuestas %>%
  group_by(pais, fecha) %>% 
  summarize(neto= sum((servia_mayor=="Si") * pago_mayor,
                      na.rm = T) - n()) %>%
  summarise(min= min(neto),
            mean= mean(neto),
            max= max(neto)) %>%
  #grafico
  pivot_longer(-pais,
             names_to = NULL) %>%
  ggplot() +
  geom_boxplot(aes(pais,
                   value)) +
  scale_y_continuous(n.breaks = 7)

apuestas %>%
  group_by(pais, fecha) %>% 
  summarize(neto= sum((servia_mayor=="Si") * pago_mayor,
                      na.rm = T) - n()) %>%
  group_by(pais) %>%
  summarize(ganancia= sum(neto))

#paga menor----

#sacar resumen estadistico de la paga
apuestas %>%
  group_by(pais, 
           fecha) %>%
  filter(servia_mayor == "Si") %>%
  summarise(min= min(pago_menor),
            mean= mean(pago_menor),
            max= max(pago_menor)) %>%
  #grafico boxplot
  pivot_longer(-c(pais, fecha),
               names_to = NULL) %>%
  ggplot() +
  geom_boxplot(aes(pais,
                   value)) +
  scale_y_continuous(n.breaks = 7) +
  facet_grid(. ~ fecha)


#ganancia neta
apuestas %>%
  group_by(pais, fecha) %>% 
  summarize(neto= sum((servia_menor=="Si") * pago_menor,
                      na.rm = T) - n()) %>%
  #promedio acierto pais
  #filter(neto>0) %>% summarize(prom_acierto= n()/max(apuestas$fecha))
  #grafico
  ggplot(aes(fecha, neto, color= pais)) + geom_smooth()


#ganancia neta
apuestas %>%
  group_by(pais, fecha) %>% 
  summarize(neto= sum((servia_menor=="Si") * pago_menor,
                      na.rm = T) - n()) %>%
  
  summarise(min= min(neto),
            mean= mean(neto),
            max= max(neto)) %>%
  #grafico
  pivot_longer(-pais,
               names_to = NULL) %>%
  ggplot() +
  geom_boxplot(aes(pais,
                   value)) +
  scale_y_continuous(n.breaks = 7)

apuestas %>%
  group_by(pais, fecha) %>% 
  summarize(neto= sum((servia_menor=="Si") * pago_menor,
                      na.rm = T) - n()) %>%
  group_by(pais) %>%
  summarize(ganancia= sum(neto))
