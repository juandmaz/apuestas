library(readxl)
library(writexl)
library(purrr)
library(gtsummary)
library(janitor)
library(tidyverse)

apuestas<- read_excel("C:/Users/CEDES SALUD notebook/Desktop/apuestas.xlsx")

#preprocesamiento
names(apuestas)

#limpieza base-----
apuestas<- apuestas %>%
  clean_names()

names(apuestas)


apuestas<-
  apuestas %>%
  pivot_longer(cols= names(apuestas[str_detect(names(apuestas),
                                               "(codere|betsson|bet365)(_mayor)")]),
               names_to = "empresa_pago",
               values_to = "pago") %>% 
  group_by(partido) %>%
  slice(which.max(pago)) %>%  
  mutate(empresa_pago= str_remove(empresa_pago,
                                  "_mayor")) %>% 
  pivot_longer(cols = contains("paga_mayor"),
               values_to = "pronostico_mayor",
               names_to = "empresa_pronostico_mayor") %>% 
  mutate(empresa_pronostico_mayor= str_remove(empresa_pronostico_mayor,
                                              "_paga_mayor")) %>% 
  filter(empresa_pronostico_mayor==empresa_pago) %>% 
  mutate(servia_mayor= if_else(pronostico_mayor==ganador,
                               "Si",
                               "No")) %>% 
  ungroup() %>%
  rename_with(~str_c(., "_mayor"), c(empresa_pago, pago))

apuestas<-
  apuestas %>% 
  pivot_longer(cols= names(apuestas[str_detect(names(apuestas),
                                               "(codere|betsson|bet365)(_menor)")]),
               names_to = "empresa_pago_menor",
               values_to = "pago_menor") %>% 
  group_by(partido) %>%
  slice(which.max(pago_menor)) %>%  
  mutate(empresa_pago_menor= str_remove(empresa_pago_menor,
                                        "_menor")) %>% 
  pivot_longer(cols = contains("paga_menor"),
               values_to = "pronostico_menor",
               names_to = "empresa_pronostico_menor") %>% 
  mutate(empresa_pronostico_menor= str_remove(empresa_pronostico_menor,
                                              "_paga_menor")) %>% 
  filter(empresa_pronostico_menor==empresa_pago_menor) %>% 
  mutate(servia_menor= if_else(pronostico_menor==ganador,
                               "Si",
                               "No")) %>%  
  ungroup()

apuestas<-
  apuestas %>%
  select(c("partido", "pais", "fecha",
           "pronostico_mayor", "pago_mayor", "empresa_pronostico_mayor",
           "pronostico_menor", "pago_menor", "empresa_pronostico_menor",
           "ganador", "servia_mayor", "servia_menor"))


view(apuestas)


#diferencias-----
nombres<- apuestas %>%
 select(c("pronostico_mayor",  
         "pronostico_menor",  
         "ganador")) %>%
unlist() |> 
unique() |> 
sort() #|> 
paste0(collapse = "\n")

clipr::write_last_clip()


indice<- read_excel("C:/Users/CEDES SALUD notebook/Desktop/apuestas.xlsx",
                    sheet = 2)

setdiff(nombres, indice$Indice)

apuestas %>%
  filter_all(any_vars(str_detect(.,
                                 "|"))) %>% view()

apuestas_pre<- read_excel("C:/Users/CEDES SALUD notebook/Desktop/apuestas.xlsx")

apuestas_pre<- apuestas_pre %>%
  clean_names()

apuestas_pre %>%
  filter(fecha!=1) %>%
  filter(if_all(c(bet365_paga_mayor,
                  codere_paga_mayor), 
                ~ betsson_paga_mayor != .x)) %>%
  select(partido,
         fecha,
         bet365_paga_mayor,
         codere_paga_mayor,
         betsson_paga_mayor)


apuestas_pre %>%
  filter(fecha!=1) %>%
  filter(if_all(c(bet365_paga_menor,
                  codere_paga_menor), 
                ~ betsson_paga_menor != .x)) %>%
  select(partido,
         fecha,
         bet365_paga_menor,
         codere_paga_menor,
         betsson_paga_menor)


