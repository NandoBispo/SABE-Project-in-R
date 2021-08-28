#Carregando pacotes
library(knitr)
install.packages("kableExtra")
library(kableExtra)
#Carregando pacote para ajudar na manipulação dos dados:
library(dplyr)

mtcars[1:10, 1:2] %>%
  mutate(
    car = row.names(.),
    # Você não precisa de formato = "html" se você já definiu opções (knitr.table.format)
    mpg = cell_spec(mpg, "html", color = ifelse(mpg > 20, "red", "blue")),
    cyl = cell_spec(cyl, "html", color = "white", align = "c", angle = 45, 
                    background = factor(cyl, c(4, 6, 8), 
                                        c("#666666", "#999999", "#BBBBBB")))) %>%
  select(car, mpg, cyl) %>% 
  kable("html", escape = F) %>% 
  kable_styling("striped", full_width = F)

?cell_spec
