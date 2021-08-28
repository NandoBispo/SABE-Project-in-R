library(tidyverse)
library(ggrepel)
library(knitr)
library(kableExtra)

#Importação do BD
disciplinas_2018_1 <- read_csv2("BD/SABE - Disciplinas 2018.1 e 2018.2/disciplinas_2018_1.csv")
disciplinas_2018_2 <- read_csv2("BD/SABE - Disciplinas 2018.1 e 2018.2/disciplinas_2018_2.csv")

#Avaliando a estrutura dos dados:
str(disciplinas_2018_1)
str(disciplinas_2018_2)

# Limpeza e manipulação do BD ----
#Renomeando a variavel Comentarios para manter todos os BDs com as mesmas variáveis.
disciplinas_2018_1 <- disciplinas_2018_1 %>% 
  rename(COMENTARIOS_DISC = comentariodisc)

disciplinas_2018_2 <- disciplinas_2018_2 %>% 
  rename(COMENTARIOS_DISC = comentdisc)

#Criando uma nova variável para distinguir entre os bancos de dados.
disciplinas_2018_1 <- disciplinas_2018_1 %>% 
  mutate(SEMESTRE = 2018.1)

disciplinas_2018_2 <- disciplinas_2018_2 %>% 
  mutate(SEMESTRE = 2018.2)

#Unindo o banco de dados.
disciplinas_2018 <- full_join(disciplinas_2018_1, disciplinas_2018_2)

#Transformando as variaveis do banco de dados em fator.
disciplinas_2018 <- disciplinas_2018 %>% 
  mutate_all(as_factor)

#Transformando algumas variaveis do BD em caractere e alterando o formato de strings.
disciplinas_2018 <- disciplinas_2018 %>% 
  mutate(COMENTARIOS_DISC = as.character(COMENTARIOS_DISC),
         DISCIPLINA = as.character(DISCIPLINA),
         SITUACAO = str_to_title(SITUACAO))

#Excluindo VAs que não trarão informação.
disciplinas_2018 <- disciplinas_2018 %>% 
  select(-ID, -TURMA)

#A ideia aqui é categorizar as disciplinas por departamento
disciplinas_2018 %>% 
  group_by(DISCIPLINA) %>% 
  distinct(DISCIPLINA) %>%
  select(DISCIPLINA) %>% 
  view()

#Criando uma variável para categorizar as disciplinas do Departamento de Estatística
disciplinas_2018 <- disciplinas_2018 %>% 
  mutate(DEPARTAMENTO = ifelse(DISCIPLINA %in% c("ADM001", "ISCA82", "FCHC45", 
                                                 "LETA09", "MAT045", "MAT174", 
                                                 "MATE56"), "Não_DEst", "DEst"))

glimpse(disciplinas_2018)

#----
#Verificando o número de alunos pelo números distintos de matrículas.
disciplinas_2018 %>% 
  # pull(MATRICULA) %>% 
  distinct(MATRICULA) %>% 
  view()

disciplinas_2018 %>% 
  mutate(SITUACAO = str_to_title(SITUACAO)) %>% select(SITUACAO)

{
#Analisando a variavel comentários para verificar a relevância.
disciplinas_2018 %>% 
  filter(!is.na(COMENTARIOS_DISC)) %>% 
  select(COMENTARIOS_DISC) %>% 
  view()

disciplinas_2018_2 %>% 
  filter(!is.na(COMENTARIOS_DISC)) %>% 
  select(COMENTARIOS_DISC) %>% 
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
  