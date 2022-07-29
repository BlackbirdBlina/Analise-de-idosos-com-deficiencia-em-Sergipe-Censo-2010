#Author Sabrina Barbosa
#26-07-2022
#Universidade Federal do Rio Grande do Norte
#Departamento de Demografia - CCET

#Pacotes necessários
library(pacman)
p_load(tidyverse,data.table,readr,readxl,vroom, tableone, gtsummary, ggplot2)

#Importando dados em estudo
layoutdados <- read_excel("Layout_microdados_Amostra.xls", sheet = "PESS",
                          range = cell_limits(c(2, 1), c(NA, 12)))

#Exclusão de colunas desnecessárias
layoutdados <- layoutdados[ ,-c(2, 3, 4, 5, 6, 7, 11, 12)]

#Convertendo os dados importados em um novo formato (data frame) para armazenar tabelas
#Em seguida é utilizado o "rownames" para alterar o nome das linhas utilizando os códigos das
#variáveis para serem os índices/nomes dessas linhas
#Isso foi feito para que as linhas não tivessem referência numérica.
layoutdados <- as.data.frame(layoutdados)
rownames(layoutdados) <- layoutdados[ , 1]
layoutdados <- layoutdados[ ,-c(1)]

#Colocando nome nas colunas e identificando quais variáveis serão utilizadas para este algoritmo
colnames(layoutdados) <- c("Inicio", "Final", "INT")
layoutdados
#De acordo com a planilha do IBGE do código V0614 até o código V0617 se refere às dificuldades/deficiência
varalvo <- c("V0601", "V6036", "V6531", "V1006", "V0606", "V6400", "V0637", 
             "V0502", "V5030", "V0657", "V0614", "V0615", "V0616", "V0617")
              
layoutdados <- layoutdados[varalvo, ]
view(layoutdados)

#Lendo e guardando o arquivo .txt na variável serg2010
serg2010 <- vroom_fwf(file = 'Amostra_Pessoas_28.txt', fwf_positions(layoutdados$Inicio,
                                                                    layoutdados$Final,
                                                        col_names = rownames(layoutdados)))

#------x------x------x------Alterando os valores de cada variável------x------x------x------# 
serg2010$V0601[serg2010$V0601 == 1] <- "Masculino"
serg2010$V0601[serg2010$V0601 == 2] <- "Feminino"

serg2010 <- transform(serg2010,V6036 = as.numeric(V6036))
serg2010 <- transform(serg2010,V6531 = as.numeric(V6531))

serg2010$V6531 <- serg2010$V6531/100

serg2010$V1006[serg2010$V1006 == 1] <- "Urbana"
serg2010$V1006[serg2010$V1006 == 2] <- "Rural"

serg2010$V0606[serg2010$V0606 == 1] <- "Branca"
serg2010$V0606[serg2010$V0606 == 2] <- "Preta"
serg2010$V0606[serg2010$V0606 == 3] <- "Amarela"
serg2010$V0606[serg2010$V0606 == 4] <- "Parda"
serg2010$V0606[serg2010$V0606 == 5] <- "Indígena"
serg2010$V0606[serg2010$V0606 == 9] <- "Ignorado"

serg2010$V6400[serg2010$V6400 == 1] <- "Sem instrução e fundamental incompleto"
serg2010$V6400[serg2010$V6400 == 2] <- "Fundamental completo e médio incompleto"
serg2010$V6400[serg2010$V6400 == 3] <- "Médio completo e superior incompleto"
serg2010$V6400[serg2010$V6400 == 4] <- "Superior completo"
serg2010$V6400[serg2010$V6400 == 5] <- "Não determinado"

serg2010$V0637[serg2010$V0637 == 1] <- "Sim"
serg2010$V0637[serg2010$V0637 == 2] <- "Não, mas viveu"
serg2010$V0637[serg2010$V0637 == 3] <- "Não, nunca viveu"

serg2010$V0502[serg2010$V0502 == "01"] <- "Pessoa responsável pelo domicílio"
serg2010$V0502[serg2010$V0502 == "02"] <- "Cônjuge ou companheiro(a) de sexo diferente"
serg2010$V0502[serg2010$V0502 == "03"] <- "Cônjuge ou companheiro(a) do mesmo sexo"
serg2010$V0502[serg2010$V0502 == "04"] <- "Filho(a) do responsável e do cônjuge"
serg2010$V0502[serg2010$V0502 == "05"] <- "Filho(a) somente do responsável"
serg2010$V0502[serg2010$V0502 == "06"] <- "Enteado(a)"
serg2010$V0502[serg2010$V0502 == "07"] <- "Genro ou nora"
serg2010$V0502[serg2010$V0502 == "08"] <- "Pai, mãe, padrasto ou madrasta"
serg2010$V0502[serg2010$V0502 == "09"] <- "Sogro(a)"
serg2010$V0502[serg2010$V0502 == 10] <- "Neto(a)"
serg2010$V0502[serg2010$V0502 == 11] <- "Bisneto(a)"
serg2010$V0502[serg2010$V0502 == 12] <- "Irmão ou irmã"
serg2010$V0502[serg2010$V0502 == 13] <- "Avô ou avó"
serg2010$V0502[serg2010$V0502 == 14] <- "Outro parente"
serg2010$V0502[serg2010$V0502 == 15] <- "Agregado(a)"
serg2010$V0502[serg2010$V0502 == 16] <- "Convivente"
serg2010$V0502[serg2010$V0502 == 17] <- "Pensionista"
serg2010$V0502[serg2010$V0502 == 18] <- "Empregado(a) doméstico(a)"
serg2010$V0502[serg2010$V0502 == 19] <- "Parente do(a) empregado(a)  doméstico(a)"
serg2010$V0502[serg2010$V0502 == 20] <- "Individual em domicílio coletivo"

serg2010$V5030[serg2010$V5030 == 1] <- "Unipessoal"
serg2010$V5030[serg2010$V5030 == 2] <- "Duas pessoas ou mais sem parentesco"
serg2010$V5030[serg2010$V5030 == 3] <- "Duas pessoas ou mais com parentesco"

serg2010$V0657[serg2010$V0657 == 1] <- "Sim"
serg2010$V0657[serg2010$V0657 == 0] <- "Não"
serg2010$V0657[serg2010$V0657 == 9] <- "Ignorado"

#Variáveis de pessoas com deficiência/dificuldade
serg2010$V0614[serg2010$V0614 == 1] <- "Sim, não consegue de modo algum"
serg2010$V0614[serg2010$V0614 == 2] <- "Sim, grande dificuldade"
serg2010$V0614[serg2010$V0614 == 3] <- "Sim, alguma dificuldade"
serg2010$V0614[serg2010$V0614 == 4] <- "Não, nenhuma dificuldade"
serg2010$V0614[serg2010$V0614 == 9] <- "Ignorado"

serg2010$V0615[serg2010$V0615 == 1] <- "Sim, não consegue de modo algum"
serg2010$V0615[serg2010$V0615 == 2] <- "Sim, grande dificuldade"
serg2010$V0615[serg2010$V0615 == 3] <- "Sim, alguma dificuldade"
serg2010$V0615[serg2010$V0615 == 4] <- "Não, nenhuma dificuldade"
serg2010$V0615[serg2010$V0615 == 9] <- "Ignorado"

serg2010$V0616[serg2010$V0616 == 1] <- "Sim, não consegue de modo algum"
serg2010$V0616[serg2010$V0616 == 2] <- "Sim, grande dificuldade"
serg2010$V0616[serg2010$V0616 == 3] <- "Sim, alguma dificuldade"
serg2010$V0616[serg2010$V0616 == 4] <- "Não, nenhuma dificuldade"
serg2010$V0616[serg2010$V0616 == 9] <- "Ignorado"

serg2010$V0617[serg2010$V0617 == 1] <- "Sim"
serg2010$V0617[serg2010$V0617 == 2] <- "Não"
serg2010$V0617[serg2010$V0617 == 9] <- "Ignorado"

#Criando a variável de "pelo menos uma das dificuldades anteriores"
serg2010["Pelo menos uma das dificuldades anteriores"] <- with(serg2010,
                                                               ifelse(V0614 == "Não, nenhuma dificuldade" & 
                                                                      V0615 == "Não, nenhuma dificuldade" &
                                                                      V0616 == "Não, nenhuma dificuldade" &
                                                                      V0617 == "Não", "Não", "Sim"))
#------x------x------x------x------Fim da alteração dos valores------x------x------x------x------#



#Alterando o nome das colunas, substituindo os códigos por suas respectivas descrições/categorias
colnames(serg2010) <- c("Sexo",
                        "Idade",
                        "Renda domiciliar per capita",
                        "Situação do Domicílio",
                        "Raça/Cor",
                        "Nível de Instrução",
                        "Vive com cônjuge ou companheiro(a)",
                        "Relação de parentesco com a pessoa responsável pelo domicílio",
                        "Tipo de unidade doméstica",
                        "Recebe benefício programa social",
                        "Dificuldade visual",
                        "Dificuldade auditiva",
                        "Dificuldade motora",
                        "Dificuldade mental ou intelectual",
                        "Pelo menos uma das dificuldades anteriores")

#Criando variáveis categóricas para Idade (grupo etário) e Renda (categoria de renda)
serg2010["Grupo etario"] <- serg2010["Idade"]
serg2010["Grupo etario"][serg2010["Idade"] >= 0 & serg2010["Idade"] < 5 ] <- "0 a 4 anos"
serg2010["Grupo etario"][serg2010["Idade"] > 4 & serg2010["Idade"] < 10 ] <- "5 a 9 anos"
serg2010["Grupo etario"][serg2010["Idade"] > 9 & serg2010["Idade"] < 15 ] <- "10 a 14 anos"
serg2010["Grupo etario"][serg2010["Idade"] > 14 & serg2010["Idade"] < 20 ] <- "15 a 19 anos"
serg2010["Grupo etario"][serg2010["Idade"] > 19 & serg2010["Idade"] < 25 ] <- "20 a 24 anos"
serg2010["Grupo etario"][serg2010["Idade"] > 24 & serg2010["Idade"] < 30 ] <- "25 a 29 anos"
serg2010["Grupo etario"][serg2010["Idade"] > 29 & serg2010["Idade"] < 35 ] <- "30 a 34 anos"
serg2010["Grupo etario"][serg2010["Idade"] > 34 & serg2010["Idade"] < 40 ] <- "35 a 39 anos"
serg2010["Grupo etario"][serg2010["Idade"] > 39 & serg2010["Idade"] < 45 ] <- "40 a 44 anos"
serg2010["Grupo etario"][serg2010["Idade"] > 44 & serg2010["Idade"] < 50 ] <- "45 a 49 anos"
serg2010["Grupo etario"][serg2010["Idade"] > 49 & serg2010["Idade"] < 55 ] <- "50 a 54 anos"
serg2010["Grupo etario"][serg2010["Idade"] > 54 & serg2010["Idade"] < 60 ] <- "55 a 59 anos"
serg2010["Grupo etario"][serg2010["Idade"] > 59 & serg2010["Idade"] < 65 ] <- "60 a 64 anos"
serg2010["Grupo etario"][serg2010["Idade"] > 64 & serg2010["Idade"] < 70 ] <- "65 a 69 anos"
serg2010["Grupo etario"][serg2010["Idade"] > 69 & serg2010["Idade"] < 75 ] <- "70 a 74 anos"
serg2010["Grupo etario"][serg2010["Idade"] > 74 & serg2010["Idade"] < 80 ] <- "75 a 79 anos"
serg2010["Grupo etario"][serg2010["Idade"] > 79 & serg2010["Idade"] < 85 ] <- "80 a 84 anos"
serg2010["Grupo etario"][serg2010["Idade"] > 84 & serg2010["Idade"] < 90 ] <- "85 a 89 anos"
serg2010["Grupo etario"][serg2010["Idade"] >= 90 ] <- "90 anos ou mais"
serg2010["Categoria de renda"] <- serg2010["Renda domiciliar per capita"]
serg2010["Categoria de renda"][serg2010["Renda domiciliar per capita"] <= 205 ] <- "Até meio salário mínimo"
serg2010["Categoria de renda"][serg2010["Renda domiciliar per capita"] > 205 & serg2010["Renda domiciliar per capita"] <= 510] <- "Entre meio e 1 salário mínimo"
serg2010["Categoria de renda"][serg2010["Renda domiciliar per capita"] > 510 & serg2010["Renda domiciliar per capita"] <= 715] <- "Entre 1 e 1,5 salários mínimos"
serg2010["Categoria de renda"][serg2010["Renda domiciliar per capita"] > 715 & serg2010["Renda domiciliar per capita"] <= 1020] <- "Entre 1,5 e 2 salários mínimos"
serg2010["Categoria de renda"][serg2010["Renda domiciliar per capita"] > 1020 ] <- "mais de 2 salários mínimos"


#Reordenando as colunas para incluir o "Grupo etário" após a coluna de "Idade" e "Categoria de renda" após
#a coluna "Renda domiciliar per capita"
serg2010 <- serg2010[c("Sexo",
                       "Idade",
                       "Grupo etario",
                       "Renda domiciliar per capita",
                       "Categoria de renda",
                       "Situação do Domicílio",
                       "Raça/Cor",
                       "Nível de Instrução",
                       "Vive com cônjuge ou companheiro(a)",
                       "Relação de parentesco com a pessoa responsável pelo domicílio",
                       "Tipo de unidade doméstica",
                       "Recebe benefício programa social",
                       "Dificuldade visual",
                       "Dificuldade auditiva",
                       "Dificuldade motora",
                       "Dificuldade mental ou intelectual",
                       "Pelo menos uma das dificuldades anteriores")]

#Criando as variáveis para agrupar os idosos e, em seguida, para agrupar os idosos com alguma dificuldade/deficiência
idososerg2010 <- serg2010[serg2010["Idade"] >= 60, ]
idosodific <- idososerg2010[idososerg2010["Pelo menos uma das dificuldades anteriores"] == "Sim", ]
view(idosodific)

#------x------x------x------Criando as tabelas com as análises descritivas das variáveis------x------x------x------#
theme_gtsummary_language(language = "pt", big.mark = ".",decimal.mark =",")

#Visão estatística geral
tbl_summary(serg2010,
            digits = list(everything() ~ c(0,1)),
            type = all_continuous() ~ "continuous2",
            statistic = list(all_continuous() ~ c("{mean} ({sd})",
                                                  "{median} ({p25} - {p75})",
                                                  "{min} - {max}"),
                             all_categorical() ~ "{n} / {N} ({p}%)"))

#Visão estatística de idosos
tbl_summary(idososerg2010,
            digits = list(everything() ~ c(0,1)),
            type = all_continuous() ~ "continuous2",
            statistic = list(all_continuous() ~ c("{mean} ({sd})",
                                                  "{median} ({p25} - {p75})",
                                                  "{min} - {max}"),
                             all_categorical() ~ "{n} / {N} ({p}%)"))

#Visão estatística organizada por presença ou não de alguma dificuldade/deficiência
tbl_summary(idososerg2010, by = "Pelo menos uma das dificuldades anteriores",
            digits = list(everything() ~ c(0,1)), percent = "row",
            type = all_continuous() ~ "continuous2",
            statistic = list(all_continuous() ~ c("{mean} ({sd})",
                                                  "{median} ({p25} - {p75})",
                                                  "{min} - {max}"),
                             all_categorical() ~ "{n} / {N} ({p}%)")) %>% add_p()


#------x------x------x------x------Testes qui-quadrado de Pearson------x------x------x------x------#

#Teste qui-quadrado para variáveis categóricas
#Considerando que as duas variáveis comparadas possuem uma distribuição estatística idêntica (H0)
#É verificado a seguir se rejeita ou não essa hipótese, por meio do teste qui-quadrado:
chisq.test(x = idososerg2010$`Pelo menos uma das dificuldades anteriores`,
           y = idososerg2010$Sexo)
chisq.test(x = idososerg2010$`Pelo menos uma das dificuldades anteriores`,
           y = idososerg2010$`Grupo etario`)
chisq.test(x = idososerg2010$`Pelo menos uma das dificuldades anteriores`,
           y = idososerg2010$`Categoria de renda`)
chisq.test(x = idososerg2010$`Pelo menos uma das dificuldades anteriores`,
           y = idososerg2010$`Situação do Domicílio`)
chisq.test(x = idososerg2010$`Pelo menos uma das dificuldades anteriores`,
           y = idososerg2010$`Raça/Cor`)
chisq.test(x = idososerg2010$`Pelo menos uma das dificuldades anteriores`,
           y = idososerg2010$`Nível de Instrução`)
chisq.test(x = idososerg2010$`Pelo menos uma das dificuldades anteriores`,
           y = idososerg2010$`Vive com cônjuge ou companheiro(a)`)
chisq.test(x = idososerg2010$`Pelo menos uma das dificuldades anteriores`,
           y = idososerg2010$`Relação de parentesco com a pessoa responsável pelo domicílio`)
chisq.test(x = idososerg2010$`Pelo menos uma das dificuldades anteriores`,
           y = idososerg2010$`Tipo de unidade doméstica`)
chisq.test(x = idososerg2010$`Pelo menos uma das dificuldades anteriores`,
           y = idososerg2010$`Recebe benefício programa social`)


#------x------x------x------x------x------Gráficos------x------x------x------x------x------#
#------Gráficos comparando com variável "Pelo menos uma das dificuldades anteriores"------#


#Gráfico Boxplot relacionando homens e mulheres idosos, por idade, com e sem dificuldade/deficiência
ggplot(idososerg2010, aes(x = `Pelo menos uma das dificuldades anteriores`, y = Idade, fill=`Pelo menos uma das dificuldades anteriores`)) +
  labs(x = "Idosos sem e com dificuldade/deficiência", y = "Idade dos Idosos") + 
  geom_boxplot(show.legend = F) + facet_wrap(.~as.factor(Sexo)) + 
  scale_fill_manual(values = c("#84D959","#D9574A"))

#Gráfico Boxplot sem filtro, mostrando outliers e as diferenças entre as variáveis para idosos com e sem deficiência
ggplot(idososerg2010, aes(x = `Pelo menos uma das dificuldades anteriores`, y = `Renda domiciliar per capita`, fill=`Pelo menos uma das dificuldades anteriores`)) +
  labs(x = "Idosos sem e com dificuldade/deficiência", y = "Renda per capita") + 
  geom_boxplot(show.legend = F) + facet_wrap(.~as.factor(Sexo)) + 
  scale_fill_manual(values = c("#84D959","#D9574A"))

#Gráfico Box plot com filtro, mostrando principalmente se há ou não diferenças entre as variáveis para idosos com e sem deficiência
idososerg2010 %>% filter(`Renda domiciliar per capita` < 1000) %>% ggplot(aes(x = `Pelo menos uma das dificuldades anteriores`, y = `Renda domiciliar per capita`, fill=`Pelo menos uma das dificuldades anteriores`)) +
  labs(x = "Idosos sem e com dificuldade/deficiência", y = "Renda per capita abaixo de mil reais") + 
  geom_boxplot(show.legend = F) + facet_wrap(.~as.factor(Sexo)) + 
  scale_fill_manual(values = c("#84D959","#D9574A"))

#Gráfico de barras relacionando homens e mulheres idosos com e sem dificuldade/deficiência
ggplot(idososerg2010, aes(x = `Pelo menos uma das dificuldades anteriores`, fill = `Pelo menos uma das dificuldades anteriores`)) +
  labs(x = "Idosos sem e com dificuldade/deficiência", y = "Quantidade de Idosos") + 
  geom_bar(show.legend = F) + facet_wrap(.~as.factor(Sexo)) + 
  scale_fill_manual(values = c("#84D959","#D9574A"))

#Gráfico de barras relacionando situação do domicílio com idosos que têm ou não algum tipo de dificuldade/deficiência
ggplot(idososerg2010, aes(x = `Pelo menos uma das dificuldades anteriores`, fill = `Pelo menos uma das dificuldades anteriores`)) +
  labs(x = "Idosos sem e com dificuldade/deficiência", y = "Quantidade de Idosos") + 
  geom_bar(show.legend = F) + facet_wrap(.~as.factor(`Situação do Domicílio`)) + 
  scale_fill_manual(values = c("#84D959","#D9574A"))

#Gráfico apresentando a porcentagem da relação acima
freq <- idososerg2010 %>%
  group_by(`Situação do Domicílio`, `Pelo menos uma das dificuldades anteriores`) %>%
  summarise(n = n()) %>% 
  mutate(freq = n / sum(n) * 100) %>% 
  ungroup()

ggplot(freq, aes(x = `Pelo menos uma das dificuldades anteriores`, y = freq, fill = `Pelo menos uma das dificuldades anteriores`, label = round(freq, 1))) +
  geom_col(position = 'dodge', show.legend = F) +
  facet_wrap(~`Situação do Domicílio`) + 
  geom_text(position = position_nudge(x = 0, y = 2)) +
  theme_classic() + scale_fill_manual(values = c("#84D959","#D9574A"))

#Gráfico de barras relacionando Raça/cor com idosos que têm ou não alguma dificuldade/deficiência
ggplot(idososerg2010, aes(x = `Pelo menos uma das dificuldades anteriores`, fill = `Pelo menos uma das dificuldades anteriores`)) +
  labs(x = "Idosos sem e com dificuldade/deficiência", y = "Quantidade de Idosos") + 
  geom_bar(show.legend = F) + facet_wrap(.~as.factor(`Raça/Cor`)) + 
  scale_fill_manual(values = c("#84D959","#D9574A"))

#Gráfico apresentando a porcentagem da relação acima
freq <- idososerg2010 %>%
  group_by(`Raça/Cor`, `Pelo menos uma das dificuldades anteriores`) %>%
  summarise(n = n()) %>% 
  mutate(freq = n / sum(n) * 100) %>% 
  ungroup()

ggplot(freq, aes(x = `Pelo menos uma das dificuldades anteriores`, y = freq, fill = `Pelo menos uma das dificuldades anteriores`, label = round(freq, 1))) +
  geom_col(position = 'dodge', show.legend = F) +
  facet_wrap(~`Raça/Cor`) + 
  geom_text(size = 2, position = position_nudge(x = 0, y = 2)) +
  theme_classic() + scale_fill_manual(values = c("#84D959","#D9574A"))

#Gráfico de barras relacionando Nível de instrução com idosos que têm ou não alguma dificuldade/deficiência
idososerg2010 %>% filter(`Nível de Instrução` != "Não determinado") %>% ggplot(aes(x = `Pelo menos uma das dificuldades anteriores`, fill = `Pelo menos uma das dificuldades anteriores`)) +
  labs(x = "Idosos sem e com dificuldade/deficiência", y = "Quantidade de Idosos") + 
  geom_bar(show.legend = F) + facet_wrap(.~as.factor(`Nível de Instrução`)) + 
  scale_fill_manual(values = c("#84D959","#D9574A"))

#Gráfico apresentando a porcentagem da relação acima
freq <- idososerg2010 %>% filter(`Nível de Instrução` != "Não determinado") %>%
  group_by(`Nível de Instrução`, `Pelo menos uma das dificuldades anteriores`) %>%
  summarise(n = n()) %>% 
  mutate(freq = n / sum(n) * 100) %>% 
  ungroup()

ggplot(freq, aes(x = `Pelo menos uma das dificuldades anteriores`, y = freq, fill = `Pelo menos uma das dificuldades anteriores`, label = round(freq, 1))) +
  geom_col(position ='dodge',show.legend = F) +
  facet_wrap(~`Nível de Instrução`) + 
  geom_text(size = 2, position = position_nudge(x = 0, y = 2)) +
  theme_classic() + scale_fill_manual(values = c("#84D959","#D9574A"))

#Gráfico de barras relacionando a variável Vive com cônjuge ou companheiro(a) com idosos que têm ou não alguma dificuldade/deficiência 
ggplot(idososerg2010, aes(x=`Pelo menos uma das dificuldades anteriores`, fill = `Pelo menos uma das dificuldades anteriores`)) +
  labs(x = "Idosos sem e com dificuldade/deficiência", y = "Quantidade de Idosos") + 
  geom_bar(show.legend = F) + facet_wrap(.~as.factor(`Vive com cônjuge ou companheiro(a)`)) + 
  scale_fill_manual(values = c("#84D959","#D9574A"))

#Gráfico para variável "Relação de parentesco" (IGNORADO POR NÃO SER MUITO SIGNIFICATIVO)
#ggplot(idososerg2010, aes(x=`Pelo menos uma das dificuldades anteriores`, fill = `Pelo menos uma das dificuldades anteriores`)) +
#  labs(x="Idosos com e sem dificuldade/deficiência", y = "Quantidade de Idosos") + 
#  geom_bar(show.legend = F) + facet_wrap(.~as.factor(`Relação de parentesco com a pessoa responsável pelo domicílio`)) + 
#  scale_fill_manual(values=c("#84D959","#D9574A"))

#Gráfico de barras relacionando o Tipo de unidade doméstica com idosos que têm ou não alguma dificuldade/deficiência
idososerg2010 %>% filter(!is.na(`Tipo de unidade doméstica`)) %>% ggplot(aes(x = `Pelo menos uma das dificuldades anteriores`, fill = `Pelo menos uma das dificuldades anteriores`)) +
  labs(x = "Idosos sem e com dificuldade/deficiência", y = "Quantidade de Idosos") + 
  geom_bar(show.legend = F) + facet_wrap(.~as.factor(`Tipo de unidade doméstica`)) + 
  scale_fill_manual(values = c("#84D959","#D9574A"))

#Gráfico apresentando a porcentagem da relação acima
freq <- idososerg2010 %>% filter(!is.na(`Tipo de unidade doméstica`)) %>%
  group_by(`Tipo de unidade doméstica`, `Pelo menos uma das dificuldades anteriores`) %>%
  summarise(n = n()) %>% 
  mutate(freq = n / sum(n) * 100) %>% 
  ungroup()

ggplot(freq, aes(x = `Pelo menos uma das dificuldades anteriores`, y = freq, fill = `Pelo menos uma das dificuldades anteriores`, label = round(freq, 1))) +
  geom_col(position ='dodge',show.legend = F) +
  facet_wrap(~`Tipo de unidade doméstica`) + 
  geom_text(size = 2, position = position_nudge(x = 0, y = 2)) +
  theme_classic() + scale_fill_manual(values = c("#84D959","#D9574A"))

#Gráfico para variável "Se recebe benefício" (IGNORADO POR NÃO SER MUITO SIGNIFICATIVO) 
#------Fim da comparação com variável "Pelo menos uma das dificuldades anteriores"------#


#------Gráficos comparando com variáveis de dificuldade/deficiência específicas------#

#Gráfico de barras relacionando idosos categorizados por feminino e masculino com e sem dificuldade visual
idososerg2010 %>% filter(`Dificuldade visual` != "Ignorado") %>% ggplot(aes(x = `Dificuldade visual`, fill = `Dificuldade visual`)) +
  labs(x = "Idosos sem e com dificuldade/deficiência visual", y = "Quantidade de Idosos") + 
  geom_bar(show.legend = F) + facet_wrap(.~as.factor(`Sexo`)) + 
  scale_fill_manual(values = c("#84D959","#F59D37", "#D9574A", "#8C0404")) +
  scale_x_discrete(guide = guide_axis(angle = -90))

#Gráfico apresentando a porcentagem da relação acima
freq <- idososerg2010 %>% filter(`Dificuldade visual` != "Ignorado") %>%
  group_by(Sexo, `Dificuldade visual`) %>%
  summarise(n = n()) %>% 
  mutate(freq = n / sum(n) * 100) %>% 
  ungroup()

ggplot(freq, aes(x = `Dificuldade visual`, y = freq, fill = `Dificuldade visual`, label = round(freq, 1))) +
  geom_col(position ='dodge',show.legend = F) +
  facet_wrap(~Sexo) + 
  geom_text(size = 2, position = position_nudge(x = 0, y = 2)) +
  theme_classic() + scale_fill_manual(values = c("#84D959","#F59D37", "#D9574A", "#8C0404")) +
  scale_x_discrete(guide = guide_axis(angle = -90))

#Gráfico de barras relacionando idosos categorizados por feminino e masculino com e sem dificuldade auditiva
idososerg2010 %>% filter(`Dificuldade auditiva` != "Ignorado") %>% ggplot(aes(x = `Dificuldade auditiva`, fill = `Dificuldade auditiva`)) +
  labs(x = "Idosos sem e com dificuldade/deficiência auditiva", y = "Quantidade de Idosos") + 
  geom_bar(show.legend = F) + facet_wrap(.~as.factor(`Sexo`)) + 
  scale_fill_manual(values = c("#84D959","#F59D37", "#D9574A", "#8C0404")) +
  scale_x_discrete(guide = guide_axis(angle = -90))

#Gráfico apresentando a porcentagem da relação acima
freq <- idososerg2010 %>% filter(`Dificuldade auditiva` != "Ignorado") %>%
  group_by(Sexo, `Dificuldade auditiva`) %>%
  summarise(n = n()) %>% 
  mutate(freq = n / sum(n) * 100) %>% 
  ungroup()

ggplot(freq, aes(x = `Dificuldade auditiva`, y = freq, fill = `Dificuldade auditiva`, label = round(freq, 1))) +
  geom_col(position ='dodge',show.legend = F) +
  facet_wrap(~Sexo) + 
  geom_text(size = 2, position = position_nudge(x = 0, y = 2)) +
  theme_classic() + scale_fill_manual(values = c("#84D959","#F59D37", "#D9574A", "#8C0404")) +
  scale_x_discrete(guide = guide_axis(angle = -90))

#Gráfico de barras relacionando idosos categorizados por feminino e masculino com e sem dificuldade motora
idososerg2010 %>% filter(`Dificuldade motora` != "Ignorado") %>% ggplot(aes(x = `Dificuldade motora`, fill = `Dificuldade motora`)) +
  labs(x = "Idosos sem e com dificuldade/deficiência Motora", y = "Quantidade de Idosos") + 
  geom_bar(show.legend = F) + facet_wrap(.~as.factor(`Sexo`)) + 
  scale_fill_manual(values = c("#84D959","#F59D37", "#D9574A", "#8C0404")) +
  scale_x_discrete(guide = guide_axis(angle = -90))

#Gráfico apresentando a porcentagem da relação acima
freq <- idososerg2010 %>% filter(`Dificuldade motora` != "Ignorado") %>%
  group_by(Sexo, `Dificuldade motora`) %>%
  summarise(n = n()) %>% 
  mutate(freq = n / sum(n) * 100) %>% 
  ungroup()

ggplot(freq, aes(x = `Dificuldade motora`, y = freq, fill = `Dificuldade motora`, label = round(freq, 1))) +
  geom_col(position ='dodge',show.legend = F) +
  facet_wrap(~Sexo) + 
  geom_text(size = 2, position = position_nudge(x = 0, y = 2)) +
  theme_classic() + scale_fill_manual(values = c("#84D959","#F59D37", "#D9574A", "#8C0404")) +
  scale_x_discrete(guide = guide_axis(angle = -90))

#Gráfico de barras relacionando idosos categorizados por feminino e masculino com e sem dificuldade mental ou intelectual
idososerg2010 %>% filter(`Dificuldade mental ou intelectual` != "Ignorado") %>% ggplot(aes(x = `Dificuldade mental ou intelectual`, fill = `Dificuldade mental ou intelectual`)) +
  labs(x = "Idosos sem e com dificuldade/deficiência mental/intelectual", y = "Quantidade de Idosos") + 
  geom_bar(show.legend = F) + facet_wrap(.~as.factor(`Sexo`)) + 
  scale_fill_manual(values = c("#84D959","#D9574A")) +
  scale_x_discrete(guide = guide_axis(angle = -90))

#Gráfico apresentando a porcentagem da relação acima
freq <- idososerg2010 %>% filter(`Dificuldade mental ou intelectual` != "Ignorado") %>%
  group_by(Sexo, `Dificuldade mental ou intelectual`) %>%
  summarise(n = n()) %>% 
  mutate(freq = n / sum(n) * 100) %>% 
  ungroup()

ggplot(freq, aes(x = `Dificuldade mental ou intelectual`, y = freq, fill = `Dificuldade mental ou intelectual`, label = round(freq, 1))) +
  geom_col(position ='dodge',show.legend = F) +
  facet_wrap(~Sexo) + 
  geom_text(size = 3, position = position_nudge(x = 0, y = 2)) +
  theme_classic() + scale_fill_manual(values = c("#84D959","#D9574A")) +
  scale_x_discrete(guide = guide_axis(angle = -90))
#------x------x------x------Fim da comparação com variáveis específicas------x------x------x------#

#------x------x------x------Número de idosos em relação a população total------x------x------x------#
numpeserg2010 <- count(serg2010, "Idade")
numidoserg2010 <- count(idososerg2010, "Idade")
proporidoserg2010 <- numidoserg2010[1,2]/numpeserg2010[1,2]
proporidoserg2010 * 100

#------x------x------x------Razão de dependência de Idosos------x------x------x------#
idososdependserg2010 <- serg2010[serg2010["Idade"] >= 65, ]
idososdependserg2010 <- count(idososdependserg2010)
numpessoativaserg2010 <- serg2010[serg2010["Idade"] >= 15 & serg2010["Idade"] < 65, ]
numpessoativaserg2010 <- count(numpessoativaserg2010)

rdiserg2010 <- idososdependserg2010[1,1]/numpessoativaserg2010[1,1]
rdiserg2010 * 100
#------x------x------x------x------Fim do algoritmo \o/------x------x------x------x------#
