library(tidyverse)
library(ggrepel)
library(knitr)
library(kableExtra)

library(DT)
library(janitor)

#Importação do BD
disciplinas_2018_1 <- read_delim("BD/SABE - Disciplinas 2018.1 e 2018.2/disciplinas_2018_1.csv", delim = ";")
disciplinas_2018_2 <- read_csv2("BD/SABE - Disciplinas 2018.1 e 2018.2/disciplinas_2018_2.csv")

# ----Limpeza e manipulação do BD ----
#Renomeando a variavel Comentarios para manter todos os BDs com as mesmas 
#variáveis e criando a variavel Semestre para identificar os BDs.
disciplinas_2018_1 <- disciplinas_2018_1 %>% 
  mutate(SEMESTRE = 20181) %>% 
  rename(COMENTARIOS_DISC = comentariodisc)

#Quantidade de respondentes do semestre 2018.1
disciplinas_2018_1$MATRICULA %>% n_distinct()


disciplinas_2018_2 <- disciplinas_2018_2 %>% 
  mutate(SEMESTRE = 20182) %>% 
  rename(COMENTARIOS_DISC = comentdisc)

#Quantidade de respondentes do semestre 2018.2
disciplinas_2018_2$MATRICULA %>% n_distinct()

#Unindo o banco de dados.
SABEdisc_2018 <- full_join(disciplinas_2018_1, disciplinas_2018_2)

#Removendo toda a formatação dos nomes das variáveis.
install.packages("abjutils") #Pacote necessário para remover acentos de strings
#read://https_blog.curso-r.com/?url=https%3A%2F%2Fblog.curso-r.com%2Fposts%2F2017-07-24-janitor%2F

nomes <- SABEdisc_2018 %>% clean_names() %>%  names() %>% abjutils::rm_accent()
names(SABEdisc_2018) <- nomes

SABEdisc_2018 %>% names()

#Contando o número de respondentes totais
SABEdisc_2018$matricula %>% n_distinct()

SABEdisc_2018 %>% group_by(matricula) %>% 
  count() %>% arrange(matricula)

#Criando a VA Departamento
SABEdisc_2018 <- SABEdisc_2018 %>% 
  mutate(
    departamento = stringr::str_sub(disciplina, end = 3)
  )

SABEdisc_2018 <- SABEdisc_2018 %>% 
  mutate(
    departamento = ifelse(departamento != "MAT", departamento, 
                    case_when(
                      departamento == "MAT" ~ case_when(
                        stringr::str_count(disciplina, "MAT045") > 0 ~ "DCC",
                        stringr::str_count(disciplina, "MAT174") > 0 ~ "DCC",
                        stringr::str_count(disciplina, "MATA37") > 0 ~ "DCC",
                        stringr::str_count(disciplina, "MAT191") > 0 ~ "DMat",
                        stringr::str_count(disciplina, "MAT198") > 0 ~ "DMat",
                        stringr::str_count(disciplina, "MATA0") > 0 ~ "DMat",
                        TRUE ~ "DEst"
                      )
                    )
    )
  )

glimpse(SABEdisc_2018)

#-----------------Copiado da professora----

# Recategorizando variavel SITUACAO em um nova variavel SITUACAO1
SABEdisc_2018 <- SABEdisc_2018 %>% 
  mutate(
    situacao = case_when(
      situacao == "aprovado" ~ "Aprovado",
      situacao == "reprovadoNota" ~ "Reprovado Nota",
      situacao == "reprovadoFalta" ~ "Reprovado Falta",
      situacao == "trancou" ~ "Trancou"
    )
  )















{
#Analisando a variavel comentários para verificar a relevância.
SABEdisc_2018 %>% 
  filter(!is.na(comentarios_disc)) %>% 
  select(comentarios_disc) %>% 
  view()

} #Dispensável...

table(disciplinas_2018$q1)

anyNA(disciplinas_2018$DISCIPLINA)

disciplinas_2018 %>%
  count(DISCIPLINA) %>%
  # top_n(10, n) %>% 
  mutate(DISCIPLINA = forcats::fct_reorder(DISCIPLINA, n)) %>%
  ggplot(aes(x = DISCIPLINA)) +
  geom_col(aes(y = n), show.legend = FALSE) +
  geom_text(aes(y = n/2, label = n)) +
  coord_flip()

disciplinas_2018 %>%  
  group_by(SEMESTRE) %>% 
  count(SITUACAO) %>% 
  mutate(Fr = n/sum(n)*100) %>%
  ggplot(aes(x = SEMESTRE, y = Fr, fill = SITUACAO)) +
  geom_col() +
  geom_text(aes(label = paste0(sprintf("%1.0f", pct), "%\n")),
              size = 3,
              position=position_stack(vjust=.5)) +
  # geom_text_repel(aes(label = round(Fr,0))) +
  #scale_fill_manual(values = c("green", "red", "pink", "blue")) + #Mudei as cores
  #scale_fill_discrete(labels = c("Aprovado", "Reprovado_Falta", "Reprovado_Nota", "Trancou"))+
  labs(x = "Semestre", y = "Freqência (%)")

  # geom_label(aes(x = SEMESTRE, y = Fr, label = round(Fr,0)))
# install.packages("patchwork")


disciplinas_2018 %>% count(q1) %>% 
  mutate("fr(%)" = n/sum(n)*100) %>% view()

levels(disciplinas_2018$q1) #Verificando os levels da VA.

#Questão 1 ----
#Construindo uma tabela de Frequência da questão 1:
disciplinas_2018 %>%
  # filter(DEPARTAMENTO == "DEst") %>% #Filtrando ficam 126 de 154 observações
  count(q1) %>% 
  mutate(Fr = n/sum(n)*100) %>% 
  mutate(q1 = forcats::lvls_revalue(q1, c("Outros", "Discordo Totalmente", 
                              "Discordo", "Não Sei", "Concordo", 
                              "Concordo Toralmente")))
#Gráfico
disciplinas_2018 %>%  
  # group_by(SEMESTRE) %>% 
  count(q1) %>% 
  mutate(Fr = n/sum(n)*100, 
         q1 = forcats::lvls_revalue(q1, c("Outros", "Discordo Totalmente", 
                                          "Discordo", "Não Sei", "Concordo", 
                                          "Concordo Toralmente"))) %>%
  ggplot(aes(x = q1, y = Fr, fill = q1)) +
  geom_col() +
  geom_text_repel(aes(label = round(Fr,1))) +
  #scale_fill_manual(values = c("green", "red", "pink", "blue")) + #Mudei as cores
  #scale_fill_discrete(labels = c("Aprovado", "Reprovado_Falta", "Reprovado_Nota", "Trancou"))+
  labs(x = "Q1", y = "Freqência (%)")

#----

disciplinas_2018 %>% filter(DEPARTAMENTO=="DEst")
  