
if (!require(dplyr)) install.packages('dplyr');library(dplyr)
library(ggplot2) # Plotting
library(knitr) # kable
#library(GGally) # ggpairs plot
library(ISLR) # Source of Data
library(MASS) # Some Classification Models (LDA, QDA)
library(class) #KNN
library(caret) # Showing Confusion Matrix Data
library(purrr) # Organizing
library(tidyr) # Organize/tidy data
#library(reshape) # Melt data for plotting
library(ape) # Trees
tableCounter = 0
figCounter = 0
#if (!require(SciencesPo)) install.packages('SciencesPo');library(SciencesPo)

dados = read.csv("wiki4HE.csv", na.strings = "?", sep = ';') # leitura dos dados


dim(dados) # dimensão dos dados ( linhas vs Colunas)

head(dados) # as 6 primeiras linhas da base de dados

dados$GENDER <- as.factor( dados$GENDER )
#table(dados$DOMAIN, useNA = "always")
dados$DOMAIN = ifelse(dados$DOMAIN==6, NA,dados$DOMAIN)
dados$DOMAIN <- as.factor( dados$DOMAIN )
dados$PhD <- as.factor( dados$PhD )
dados$YEARSEXP <- as.numeric( dados$YEARSEXP )
dados$UNIVERSITY <- as.factor( dados$UNIVERSITY )
dados$UOC_POSITION <- as.factor( dados$UOC_POSITION )
dados$OTHER_POSITION <- as.factor( dados$OTHER_POSITION )
dados$OTHERSTATUS <- as.factor( dados$OTHERSTATUS )
dados$USERWIKI <- as.factor( dados$USERWIKI )
dados[,11:53] <- lapply( dados[ , 11:53 ],  factor ) # restante das variáveis ( colunas de 11 a 53)

# Definindo o nome das categorias
levels( dados$GENDER ) <- c( "Masculino", "Feminino" ) # sexo
levels( dados$PhD ) <- c("Não","Sim" ) # phd
levels( dados$UNIVERSITY ) <- c("UOC","UPF" ) # 1 = UOC; 2 = UPF
levels( dados$USERWIKI ) <- c("Não","Sim" )#0=No; 1=Yes
levels( dados$OTHER_POSITION ) <- c("Professor", "Associate", "NA" )#1=Professor; 2=Associate; 3=Assistant; 4=Lecturer; 5=Instructor; 6=Adjunct
levels( dados$DOMAIN ) <- c("Arts & Humanities","Sciences","Health Sciences",
                            "Engineering & Architecture", "Law & Politics")
#1=Arts & Humanities; 2=Sciences; 3=Health Sciences; 4=Engineering & Architecture; 5=Law & Politics
levels( dados$UOC_POSITION ) <- c("Professor","Associate", "Assistant", "Lecturer", "Instructor",
                                  "Adjunct", "NA")
# 1=Professor; 2=Associate; 3=Assistant; 4=Lecturer; 5=Instructor; 6=Adjunct


#apply( dados, 2, anyNA ) 

summary(dados)

options(repr.plot.width=10, repr.plot.height=7)
wikifactor = dados # Create separate copy for changing survey items to ordered/factors
wikifactor[,11:ncol(dados)]=lapply(wikifactor[,11:ncol(dados)], ordered)
wikifactor[,c("GENDER","PhD","UNIVERSITY","USERWIKI", "OTHER_POSITION","DOMAIN")] %>%
  keep(is.factor) %>% 
  gather() %>% 
  ggplot(aes(value, fill=value)) +
  facet_wrap(~ key, scales = "free") +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 30, hjust = 0.85),legend.position="none")

options(repr.plot.width=10, repr.plot.height=6)
wikifactor %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value,fill=key)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram(bins=sqrt(nrow(dados))) +
  theme(legend.position="none")

dados %>% dplyr::select(c( GENDER,PhD,UNIVERSITY,USERWIKI, OTHER_POSITION,DOMAIN, AGE, YEARSEXP)) %>% summary()


options(repr.plot.width=10, repr.plot.height=8)
wikifactor[11:ncol(wikifactor)] %>%
  keep(is.factor) %>% 
  gather() %>% 
  ggplot(aes(value,fill=value)) +
  facet_wrap(~ key, scales = "free") +
  geom_bar()+
  theme(legend.position="none")


# definindo as categórias
levels( dados$ENJ1 ) = c( "Discordo Totalmente", "Discordo Parcialmente",
                          "Não concordo, nem discordo",
                          "Concordo Parcialmente", "Concordo Totalmente")


levels( dados$ENJ2 ) = c( "Discordo Totalmente", "Discordo Parcialmente",
                          "Não concordo, nem discordo",
                          "Concordo Parcialmente", "Concordo Totalmente")

mycols = c("#ed0c0c","#f0887d", "#dbd5d5", "#81c9f0","#0c10f2" )
### ENJ1 vs gender ==========

ENJ1_gender <- dados %>% 
  drop_na(ENJ1) %>% # drop 7 observations
  group_by(GENDER, ENJ1) %>%
  summarise(n = n()) %>%
  mutate( freq = (n / sum(n))*100  )  # table of frequency and relative frequency to ENJ1

options(repr.plot.width=13, repr.plot.height=8)
ggplot( ENJ1_gender, aes(x = GENDER, y = freq, fill = ENJ1, label = paste0(round(freq, 1),"%") )) +
  geom_bar(stat = "identity", color = "white" ) +
    geom_text(position = position_stack(vjust = 0.5))+
ggtitle("ENJ1 vs Gênero")+
  labs(fill = "O uso da Wikipedia estimula a curiosidade", y = "Frequência Relativa", x = "Gênero" ) + 
  scale_fill_manual(values = mycols) +
  theme_classic() +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        text = element_text(size=15))

### ENJ1 vs Area ==========

ENJ1_domain <- dados %>% 
  drop_na(ENJ1) %>% # drop 7 observations
  group_by(DOMAIN, ENJ1) %>%
  summarise(n = n()) %>%
  mutate( freq = (n / sum(n))*100  )  # table of frequency and relative frequency to ENJ1

options(repr.plot.width=13, repr.plot.height=8)
ggplot( ENJ1_domain, aes(x = DOMAIN, y = freq, fill = ENJ1, label = paste0(round(freq, 1),"%") ) ) +
  geom_bar(stat = "identity", color = "white" ) +
ggtitle("ENJ1 vs Domain")+
    geom_text(position = position_stack(vjust = 0.5))+
  labs(fill = "O uso da Wikipedia estimula a curiosidade", y = "Frequência Relativa", x = "Area" ) + 
  scale_fill_manual(values = mycols) +
  theme_classic() +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 15),
        text = element_text(size=13))

### ENJ1 vs Phd ==========

ENJ1_phd <- dados %>% 
  drop_na(ENJ1) %>% # drop 7 observations
  group_by(PhD, ENJ1) %>%
  summarise(n = n()) %>%
  mutate( freq = (n / sum(n))*100  )  # table of frequency and relative frequency to ENJ1

options(repr.plot.width=13, repr.plot.height=8)
ggplot( ENJ1_phd, aes(x = PhD, y = freq, fill = ENJ1, label = paste0(round(freq, 1),"%") ) ) +
  geom_bar(stat = "identity", color = "white" ) +
ggtitle("ENJ1 vs PhD")+
    geom_text(position = position_stack(vjust = 0.5))+
  labs(fill = "O uso da Wikipedia estimula a curiosidade", y = "Frequência Relativa", x = "PhD" ) + 
  scale_fill_manual(values = mycols) +
  theme_classic() +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        text = element_text(size=15))

### ENJ1 vs Universidade ==========

ENJ1_uni <- dados %>% 
  drop_na(ENJ1) %>% # drop 7 observations
  group_by(UNIVERSITY, ENJ1) %>%
  summarise(n = n()) %>%
  mutate( freq = (n / sum(n))*100  )  # table of frequency and relative frequency to ENJ1

options(repr.plot.width=13, repr.plot.height=8)
ggplot( ENJ1_uni, aes(x = UNIVERSITY, y = freq, fill = ENJ1, label = paste0(round(freq, 1),"%") ) ) +
  geom_bar(stat = "identity", color = "white" ) +
ggtitle("ENJ1 vs Universidade")+
geom_text(position = position_stack(vjust = 0.5))+
  labs(fill = "O uso da Wikipedia estimula a curiosidade", y = "Frequência Relativa", x = "Universidade" ) + 
  scale_fill_manual(values = mycols) +
  theme_classic() +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 15),
        text = element_text(size=13))

### ENJ1 vs Usuário Wikipédia ==========

ENJ1_user <- dados %>% 
  drop_na(ENJ1) %>% # drop 7 observations
  group_by(USERWIKI, ENJ1) %>%
  summarise(n = n()) %>%
  mutate( freq = (n / sum(n))*100  )  # table of frequency and relative frequency to ENJ1

options(repr.plot.width=13, repr.plot.height=8)
ggplot( ENJ1_user, aes(x = USERWIKI, y = freq, fill = ENJ1, label = paste0(round(freq, 1),"%") )) +
  geom_bar(stat = "identity", color = "white" ) +
ggtitle("ENJ1 vs Usuário Wikipédia")+
    geom_text(position = position_stack(vjust = 0.5))+
  labs(fill = "O uso da Wikipedia estimula a curiosidade", y = "Frequência Relativa", x = "Usuário Wikipédia" ) + 
  scale_fill_manual(values = mycols) +
  theme_classic() +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 15),
        text = element_text(size=13))

### ENJ2 vs Gênero ==========
ENJ2_gender <- dados %>% 
  drop_na(ENJ2) %>% # drop 7 observations
  group_by(GENDER, ENJ2) %>%
  summarise(n = n()) %>%
  mutate( freq = (n / sum(n))*100  )  # table of frequency and relative frequency to ENJ1

options(repr.plot.width=13, repr.plot.height=8)
ggplot( ENJ2_gender, aes(x = GENDER, y = freq, fill = ENJ2, label = paste0(round(freq, 1),"%") ) ) +
  geom_bar(stat = "identity", color = "white" ) +
ggtitle("ENJ2 vs Gênero")+
    geom_text(position = position_stack(vjust = 0.5))+
  labs(fill = "O uso da Wikipédia é interessante?", y = "Frequência Relativa", x = "Gênero" ) + 
  scale_fill_manual(values = mycols) +
  theme_classic() +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        text = element_text(size=15))

### ENJ2 vs Area ==========

ENJ2_domain <- dados %>% 
  drop_na(ENJ2) %>% # drop 7 observations
  group_by(DOMAIN, ENJ2) %>%
  summarise(n = n()) %>%
  mutate( freq = (n / sum(n))*100  )  # table of frequency and relative frequency to ENJ1

options(repr.plot.width=13, repr.plot.height=8)
ggplot( ENJ2_domain, aes(x = DOMAIN, y = freq, fill = ENJ2, label = paste0(round(freq, 1),"%") ) ) +
  geom_bar(stat = "identity", color = "white" ) +
ggtitle("ENJ2 vs Domain")+
    geom_text(position = position_stack(vjust = 0.5))+
  labs(fill = "O uso da Wikipédia é interessante?", y = "Frequência Relativa", x = "Area" ) + 
  scale_fill_manual(values = mycols) +
  theme_classic() +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 15),
        text = element_text(size=13))

### ENJ2 vs Phd ==========

ENJ2_phd <- dados %>% 
  drop_na(ENJ2) %>% # drop 7 observations
  group_by(PhD, ENJ2) %>%
  summarise(n = n()) %>%
  mutate( freq = (n / sum(n))*100  )  # table of frequency and relative frequency to ENJ1

options(repr.plot.width=13, repr.plot.height=8)
ggplot( ENJ2_phd, aes(x = PhD, y = freq, fill = ENJ2, label = paste0(round(freq, 1),"%") ) ) +
  geom_bar(stat = "identity", color = "white" ) +
ggtitle("ENJ2 vs PhD")+
    geom_text(position = position_stack(vjust = 0.5))+
  labs(fill = "O uso da Wikipédia é interessante?", y = "Frequência Relativa", x = "PhD" ) + 
  scale_fill_manual(values = mycols) +
  theme_classic() +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        text = element_text(size=15))

### ENJ2 vs Universidade ==========

ENJ2_uni <- dados %>% 
  drop_na(ENJ2) %>% # drop 7 observations
  group_by(UNIVERSITY, ENJ2) %>%
  summarise(n = n()) %>%
  mutate( freq = (n / sum(n))*100  )  # table of frequency and relative frequency to ENJ1

options(repr.plot.width=13, repr.plot.height=8)
ggplot( ENJ2_uni, aes(x = UNIVERSITY, y = freq, fill = ENJ2, label = paste0(round(freq, 1),"%") ) ) +
  geom_bar(stat = "identity", color = "white" ) +
    geom_text(position = position_stack(vjust = 0.5))+
ggtitle("ENJ2 vs Universidade")+
  labs(fill = "O uso da Wikipédia é interessante?", y = "Frequência Relativa", x = "Universidade" ) + 
  scale_fill_manual(values = mycols) +
  theme_classic() +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 15),
        text = element_text(size=13))

### ENJ2 vs Usuário Wikipédia ==========

ENJ2_user <- dados %>% 
  drop_na(ENJ2) %>% # drop 7 observations
  group_by(USERWIKI, ENJ2) %>%
  summarise(n = n()) %>%
  mutate( freq = (n / sum(n))*100  )  # table of frequency and relative frequency to ENJ1

options(repr.plot.width=13, repr.plot.height=8)
ggplot( ENJ2_user, aes(x = USERWIKI, y = freq, fill = ENJ2, label = paste0(round(freq, 1),"%") )) +
  geom_bar(stat = "identity", color = "white" ) +
ggtitle("ENJ3 vs Usuário Wikipédia")+
    geom_text(position = position_stack(vjust = 0.5))+
  labs(fill = "O uso da Wikipédia é interessante?", y = "Frequência Relativa", x = "Usuário Wikipédia" ) + 
  scale_fill_manual(values = mycols) +
  theme_classic() +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 15),
        text = element_text(size=13))

dados$recomenda = ifelse(dados$Use3=="5"|dados$Use3=="4", "1", "0"  ) # 1 = sim e 0 = não 
dados$recomenda = as.factor(dados$recomenda)
levels(dados$recomenda)=c( "Não","Sim")
table(dados$recomenda)


mod = glm( recomenda ~ AGE + GENDER + DOMAIN + PhD + YEARSEXP + UNIVERSITY + USERWIKI,
           data = dados, family = binomial( link = "logit" ) )
summary(mod)
