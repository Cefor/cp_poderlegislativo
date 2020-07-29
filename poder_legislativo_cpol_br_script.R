library(epiDisplay)
library(stringr)
library(readxl)

Cipol_Qualis_Legis_Planilha_Completa <- read_xlsx("poder_legislativo_cpol_br.xlsx")
#Cipol_Qualis_Legis_Planilha_Completa <- read_rds("poder_legislativo_cpol_br.Rds")

## Análise referente à Tabela 1: Artigos publicados por periódico sobre legislativos
tab1(Cipol_Qualis_Legis_Planilha_Completa$periodico, sort.group = "decreasing")

## Análise referente à Tabela 2: Temáticas dos estudos
temas<-unlist(str_split(Cipol_Qualis_Legis_Planilha_Completa$areaTematica, pattern = ", "))
tab1(temas, sort.group = "decreasing", decimal = 2, graph = F, cum.percent = F)

## Análises referentes à seção 4.1: Legislativo nacional, subnacionais e estrangeiros

# Tipos de legislativos 
legislativos<-unlist(str_split(Cipol_Qualis_Legis_Planilha_Completa$escalaGeografica, pattern = ", "))
tab1(legislativos, sort.group = "decreasing", decimal = 2, graph = F, cum.percent = F)

# Frequência dos trabalhos que abordam o Legislativo federal
tab1(Cipol_Qualis_Legis_Planilha_Completa$federal,
     sort.group = "decreasing", decimal = 2, graph = F, cum.percent = F)

# Temas abordados na Câmara
camara<-Cipol_Qualis_Legis_Planilha_Completa%>%filter(federal=="Câmara")
temas_camara<-unlist(str_split(camara$areaTematica, pattern = ", "))
tab1(temas_camara, sort.group = "decreasing", decimal = 2, graph = F, cum.percent = F)

# Instituições abordados no legislativo subnacional
legislativo_subnacional<-unlist(str_split(Cipol_Qualis_Legis_Planilha_Completa$subnacionais, pattern = ", "))
tab1(legislativo_subnacional, sort.group = "decreasing", decimal = 2, graph = F, cum.percent = F)

# Instituições abordadas no legislativo estrangeiro
legislativo_estrangeiro<-unlist(str_split(Cipol_Qualis_Legis_Planilha_Completa$estrangeiros, pattern = ", "))
tab1(legislativo_estrangeiro, sort.group = "decreasing", decimal = 2, graph = F, cum.percent = F)

# Temas abordados na Câmara
estaduais<-Cipol_Qualis_Legis_Planilha_Completa%>%filter(escalaGeografica=="legislativos estaduais")
temas_camara<-unlist(str_split(camara$areaTematica, pattern = ", "))
tab1(temas_camara, sort.group = "decreasing", decimal = 2, graph = F, cum.percent = F)

## Análises referentes à Tabela 15: Teorias utilizadas
teorias<-unlist(str_split(Cipol_Qualis_Legis_Planilha_Completa$teorias, pattern = ", "))
tab1(teorias, sort.group = "decreasing", decimal = 2, graph = F, cum.percent = F)

## Análises referentes à natureza das pesquisas x metodologias

# Tabela 16: Natureza da pesquisa
tab1(Cipol_Qualis_Legis_Planilha_Completa$tipoPesquisa, sort.group = "decreasing", decimal = 2, graph = F, cum.percent = F)

# Tabela 17: Metodologias utilizadas
quali<-Cipol_Qualis_Legis_Planilha_Completa%>%filter(tipoPesquisa=="Qualitativa")
metodologias<-unlist(str_split(quali$metodologias, pattern = ", "))
tab1(metodologias, sort.group = "decreasing", decimal = 2, graph = F, cum.percent = F)

## Análises à seção 6.1: Tipo de autoria X Colaboração autoral

# Tabela X: Tipo de autoria
tab1(Cipol_Qualis_Legis_Planilha_Completa$tipoAutoria, sort.group = "decreasing", decimal = 2, graph = F, cum.percent = F)

# Criando a variável de área do autor
autor<-gsub(",.*$", "", str_c(Cipol_Qualis_Legis_Planilha_Completa$areaFormacaoAutor,","))
autores<-unlist(str_split(Cipol_Qualis_Legis_Planilha_Completa$areaFormacaoAutor, pattern = ", "))
tab1(autores, sort.group = "decreasing", decimal = 2, graph = F, cum.percent = F)

## Análises da seção 6.2:  Vínculo dos autores e perfis institucionais 

# Analisando o vínculo dos autores com o poder legislativo
tab1(Cipol_Qualis_Legis_Planilha_Completa$vincAutLeg_log, sort.group = "decreasing", decimal = 2, graph = F, cum.percent = F)

# Analisando a qual casa legislativa os autores percentem
tab1(Cipol_Qualis_Legis_Planilha_Completa$vincAutLeg_char, sort.group = "decreasing", decimal = 2, graph = F, cum.percent = F)

# Produção interinstitucional
tab1(Cipol_Qualis_Legis_Planilha_Completa$prodInterInt_log, sort.group = "decreasing", decimal = 2, graph = F, cum.percent = F)
tab1(Cipol_Qualis_Legis_Planilha_Completa$prodInterInt_char, sort.group = "decreasing", decimal = 2, graph = F, cum.percent = F)

# Titulação
titulacao<-unlist(str_split(Cipol_Qualis_Legis_Planilha_Completa$titulacaoAutor, pattern = ", "))
tab1(titulacao, sort.group = "decreasing", decimal = 2, graph = F, cum.percent = F)

# Região Geográfica
regioes<-unlist(str_split(Cipol_Qualis_Legis_Planilha_Completa$regiaoInstituicao, pattern = ", "))
tab1(regioes, sort.group = "decreasing", decimal = 2, graph = F, cum.percent = F)

# Natureza das instituições
nat_instituicoes<-unlist(str_split(Cipol_Qualis_Legis_Planilha_Completa$naturezaInstituicao, pattern = ", "))
tab1(nat_instituicoes, sort.group = "decreasing", decimal = 2, graph = F, cum.percent = F)

# Instituições públicas
publicas<-Cipol_Qualis_Legis_Planilha_Completa%>%filter(grepl("Pública", naturezaInstituicao))
publicas_lista<-unlist(str_split(publicas$nomeInstituicao, pattern = ", "))
publicas_lista<-unlist(str_split(publicas_lista, pattern = "; "))
publicas_lista<-unlist(str_split(publicas_lista, pattern = ": "))
publicas_lista<-unlist(str_split(publicas_lista, pattern = " e "))
publicas_lista<-publicas_lista%>%str_trim()%>%str_to_upper()
publicas_lista<-publicas_lista[publicas_lista %in% 
                                           c("UNIVERSIDADE DE SALAMANCA", "COLUMBIA UNIVERSITY", "MICHIGAN UNIVERSITY","FLORIDA INTERNATIONAL UNIVERSITY","UNIVERSIDADE DE BUENOS AIRES", "SENADO FEDERAL",
                                             "CEBRAP","UNIVERSIDADE VILA VELHA","UNIVERSIDADE DE COIMBRA","FGV","UNIVERSITY OF COLORADO","ILB","MICHIGAN STATE UNIVERSITY","WASHINGTON UNIVERSITY",
                                             "UNIVERSITY OF CALIFORNIA","YALE UNIVERSITY","UNICAMPO","OXFORD UNIVERSITY", "INSTITUTO KELLOG","GERMAN INSTITUE OF GLOBAL STUDIES",
                                             "BERKELEY UNIVERSITY","UNIVERSIDADE NACIONAL DO URUGUAI","HULL UNIVERSITY","UNIVERSITY OF CALGARY","NEW YORK UNIVERSITY",
                                             "HUL UNIVERSITY","CD","CEFOR","PUCMG","FACEL","IUPERJ","ENAP","MINISTÉRIO DO DESENVOLVIMENTO SOCIAL") == FALSE]
tab1(publicas_lista, sort.group = "decreasing", decimal = 2, graph = F, cum.percent = F)

# Instituições de outra natureza
outranatureza<-Cipol_Qualis_Legis_Planilha_Completa%>%filter(grepl("Outra natureza", naturezaInstituicao))
outranatureza_lista<-unlist(str_split(outranatureza$nomeInstituicao, pattern = ", "))
outranatureza_lista<-unlist(str_split(outranatureza_lista, pattern = "; "))
outranatureza_lista<-unlist(str_split(outranatureza_lista, pattern = ": "))
outranatureza_lista<-outranatureza_lista[outranatureza_lista %in% 
                                         c("FGV", "USP", "UNB","UnB","New York University", "Michigan State University",
                                           "Instituto Kellog","HUL UNIVERSITY","GERMAN INSTITUE OF GLOBAL STUDIES") == FALSE]
tab1(outranatureza_lista, sort.group = "decreasing", decimal = 2, graph = F, cum.percent = F)


# Tabela 5: Região geográfica 
regioes<-unlist(str_split(Cipol_Qualis_Legis_Planilha_Completa$regiaoInstituicao, pattern = ", "))
tab1(regioes, sort.group = "decreasing", decimal = 2, graph = F, cum.percent = F)

# Tabela 6: Instituições nacionais públicas
universidades_publicas<-Cipol_Qualis_Legis_Planilha_Completa%>%filter(naturezaInstituicao%in%"Pública")
universidades<-unlist(str_split(universidades_publicas$nomeInstituicao, pattern = c("; ")))
universidades<-str_to_upper(universidades)
tab1(universidades, sort.group = "decreasing", decimal = 2, graph = F, cum.percent = F)

## Seção 6.3: Autoria x sexo
# Predominância dos sexos dos autores
tab1(Cipol_Qualis_Legis_Planilha_Completa$predominio_sexo, sort.group = "decreasing", decimal = 2, graph = F, cum.percent = F)

# Apêndice 2: Artigos sobre temáticas legislativas publicados por ano nos 13 periódicos examinados
anos<-as.data.frame(table(Cipol_Qualis_Legis_Planilha_Completa$ano), stringsAsFactors = F)
ggplot(anos) +
  geom_line(aes(x=Var1, y=Freq, group=1), size=1)+
  labs(title = "Temáticas legislativas",
       x="", 
       y = "")+
  guides(color=guide_legend(title=""))+
  theme_classic()+
  theme(plot.title = element_text(face = "bold", size = 24, hjust = 0.5),
        plot.subtitle = element_text(face = "italic", size = 15, hjust = 0.5),
        plot.caption = element_text(face = "italic", size = 12, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 20),
        legend.text=element_text(size=14),
        legend.title = element_text(size=14),
        axis.text.x = element_text(angle = 45, hjust = 1))



