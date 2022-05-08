# TP CDD version FINAL

# Params: ELEGIR EL TP A ANALIZAR----

# Primer cuatri 2020 ----
# 2020q1_TP1 ----
tp_name <- "2020q1_TP1"
folder_path <-
  'C:\\Users\\Ignac\\Desktop\\Ciencia_de_Datos_para_la_Toma_de_Decisiones\\TP\\EA2_TP1_2020q1'
zip_path <- 
  'C:\\Users\\Ignac\\Desktop\\Ciencia_de_Datos_para_la_Toma_de_Decisiones\\TP\\EA2_TP1_2020q1\\EA2_TP1_2020q1.zip'
ununciado_path <-
  'C:\\Users\\Ignac\\Desktop\\Ciencia_de_Datos_para_la_Toma_de_Decisiones\\TP\\EA2_TP1_2020q1\\EA2_TP1_2020q1_enunciado.pdf'

# 2020q1_TP2 ----
tp_name <- "2020q1_TP2"
folder_path <-
  'C:\\Users\\Ignac\\Desktop\\Ciencia_de_Datos_para_la_Toma_de_Decisiones\\TP\\EA2_TP2_2020q1'
zip_path <- 
  'C:\\Users\\Ignac\\Desktop\\Ciencia_de_Datos_para_la_Toma_de_Decisiones\\TP\\EA2_TP2_2020q1\\EA2_TP2_2020q1.zip'
ununciado_path <-
  'C:\\Users\\Ignac\\Desktop\\Ciencia_de_Datos_para_la_Toma_de_Decisiones\\TP\\EA2_TP2_2020q1\\EA2_TP2_2020q1_enunciado.pdf'

# 2020q1_TP3 ----
tp_name <- "2020q1_TP3"
folder_path <-
  'C:\\Users\\Ignac\\Desktop\\Ciencia_de_Datos_para_la_Toma_de_Decisiones\\TP\\EA2_TP3_2020q1'
zip_path <- 
  'C:\\Users\\Ignac\\Desktop\\Ciencia_de_Datos_para_la_Toma_de_Decisiones\\TP\\EA2_TP3_2020q1\\EA2_TP3_2020q1.zip'
ununciado_path <-
  'C:\\Users\\Ignac\\Desktop\\Ciencia_de_Datos_para_la_Toma_de_Decisiones\\TP\\EA2_TP3_2020q1\\EA2_TP3_2020q1_enunciado.pdf'




# Segundo cuatri 2020 ----
# 2020q2_TP1 (este tiene el alumno "COPIA" para validar)----
tp_name <- "2020q2_TP1"
folder_path <-
  'C:\\Users\\Ignac\\Desktop\\Ciencia_de_Datos_para_la_Toma_de_Decisiones\\TP\\EA2_TP1_2020q2'
zip_path <- 
  'C:\\Users\\Ignac\\Desktop\\Ciencia_de_Datos_para_la_Toma_de_Decisiones\\TP\\EA2_TP1_2020q2\\EA2_TP1_2020q2.zip'
ununciado_path <-
  'C:\\Users\\Ignac\\Desktop\\Ciencia_de_Datos_para_la_Toma_de_Decisiones\\TP\\EA2_TP1_2020q2\\EA2_TP1_2020q2_enunciado.pdf'








# Librerias ----
library(textreuse)
library(tm)
library(pdftools)
library(stringr)
library(dplyr)
library(readr)


# Preprocesamiento ----
# Borro y creo el nuevo directorio
tmp_path <- paste0(getwd(), "/tmp")
tmp_pdf <- paste0(getwd(), "/tmp/pdfs")
tmp_txt <- paste0(getwd(), "/tmp/txt")
unlink(tmp_path, recursive = TRUE)
dir.create(tmp_path)
dir.create(tmp_pdf)
dir.create(tmp_txt)

# Leo el enunciado
reader <- readPDF("pdftools", control = ) # funci?n para leer pdfs
content <- reader(elem = list(uri = ununciado_path),
                  language = "es") #leo el enunciado en pdf y guardo los caracteres y la metadata

#content$content
#help("parse_character")
content$content <- 
  parse_character(content$content, 
                  locale=locale(encoding="UTF-8")) 
#help("str_trim")
lineas_enunc <- 
  read_lines(content$content) %>% str_trim() # remuevo los espacios en blanco del string

fileConn <- file(paste0(tmp_path, "/enunciado.txt"), 
                 encoding = 'UTF-8') # crea una conexi?n a un archivo con el enunciado en formato txt
writeLines(content$content, fileConn) # escribo el texto en la conexi?n creada
close(fileConn) #cierro la conexi?n

# Selecciono el zip y lo decomprimo
unzip(zip_path, exdir = paste0(tmp_pdf))

# defino la funci?n para leer los archivos del directorio (libreria trxUtils de Mariano Bonoli)
read_folder_list <- function(path){
  require(stringr)
  
  files <- list.files(path = path)
  
  files <- files[!str_detect(files, pattern = "\\.")]
  
  if (length(files) == 0){
    return (
      setNames(
        data.frame(matrix(ncol = 4, nrow = 0)),
        c("filename", "extension", "full_filename", "filename_with_path"))
    )
  } else {
    return(
      data.frame(
        filename = str_sub(files, 1, str_locate(files, "\\.")[,1]-1),
        extension = str_sub(files, str_locate(files, "\\.")[,1]+1, 1000),
        full_filename = files,
        filename_with_path = paste0(path, "/", files),
        stringsAsFactors = F
      )
    )
  }
}

# Levanto los txt files y 

#help("str_locate")
#help("str_sub")
folder_list <- read_folder_list(tmp_pdf)
pos_ <- str_locate(folder_list$full_filename, "_")[, 1] # buscamos la posición del primer guion de cada elemento del full_filename
alumnos <- str_sub(folder_list$full_filename, 1, pos_ - 1) # agarra solo el nombre de los alumnos de full_filename
dir.create(paste0(tmp_path, "/_txt_files")) # crea una direccion nueva para los archivos txt
df <- data.frame()
file_list <- vector()

# defino la funcion para leer archivos (libreria trxUtils de Mariano Bonoli)
read_file_list <- function(path, filter_extension = NULL){
  
  require(stringr)
  require(purrr)
  
  if (str_sub(path, -1, -1) == "/")
    path <- str_sub(path, 1, -2)
  if (!file.exists(path)){
    warning(paste0("Folder '", path, "' doesn't exist."))
    return(NULL)
  }
  
  files <- list.files(path = path)
  files <- files[str_detect(files, pattern = "\\.")]
  
  if (is_empty(files))
    return(NULL)
  
  df <- data.frame(
    filename = str_sub(files, 1, str_locate(files, "\\.")[,1]-1),
    extension = str_sub(files, str_locate(files, "\\.")[,1]+1, 1000),
    full_filename = files,
    filename_with_path = paste0(path, "/", files),
    stringsAsFactors = F)
  
  if (!is.null(filter_extension))
    df <- dplyr::filter(df, extension == filter_extension)
  
  df
}

# Leo cada uno de los archivos de en la carpeta temporal de archivos pdfs
for (f in 1:length(folder_list$filename_with_path)){
  filename <- 
    read_file_list(folder_list$filename_with_path[f]) %>%
    # Filtro solo los pdf
    filter(str_sub(full_filename, -3, -1) %in%
             c('pdf', 'PDF', 'Pdf')) %>% 
    # si hay mas de un archivo selecciono el primero, por ejemplo reentregas
    slice(1) 
  
  filename <- filename[str_sub(filename$full_filename, -3, -1) %in% 
                         c('pdf', 'PDF', 'Pdf'),]
  file_list <- c(file_list, filename$filename_with_path)
  content<- 
    reader(elem = list(uri = filename$filename_with_path),
           language = "es")
  content$content <- 
    parse_character(content$content, 
                    locale=locale(encoding="UTF-8"))
  lineas_tp <- 
    read_lines(content$content) %>% str_trim() # se lee en lineas el TP y se quitan los espacios en blanco
  lineas_tp <- lineas_tp[!(lineas_tp %in% lineas_enunc)] # se deja solo lo que NO es enunciado dentro del TP
  fileConn<-file(paste0(tmp_path, "/_txt_files/file", 
                        f, ".txt"),
                 encoding = 'UTF-8')
  writeLines(lineas_tp, fileConn)
  close(fileConn)
  df <- bind_rows(df, data.frame(t(unlist(content$meta))))
}

df <- df %>%
  mutate(id = 1:nrow(df),
         alumno = alumnos,
         file = file_list) %>% 
  select(id, alumno, author, file) 

# hasta aca es el preprocesamiento ----


# tokenizo con ngrams n=5 ----
#help("TextReuseCorpus")
corpus <-
  TextReuseCorpus(dir = paste0(tmp_path, "/_txt_files"), #se leen los archivos txt de la carpeta temporal
                  tokenizer = tokenize_ngrams,
                  lowercase = TRUE, #texto en min?scula para tokenizarlo
                  n = 5,
                  keep_tokens = TRUE,
                  progress = FALSE)


# remover stopwords, y hacer stemming (la librer?a tm NO tiene lematizaci?n)
# help("tm_map") #### los corpus de Text Reuse NO son compatibles con los de tm
# corpus <- tm_map(corpus, removeWords, stopwords("spanish"))


# tokenizo con ngrams n=7 (usamos este) ----
corpus <-
  TextReuseCorpus(dir = paste0(tmp_path, "/_txt_files"),
                  tokenizer = tokenize_ngrams,
                  lowercase = TRUE, #texto en min?scula para tokenizarlo
                  n = 7,
                  keep_tokens = TRUE,
                  progress = FALSE)

# tokenizo con ngrams n=9 ----
corpus <-
  TextReuseCorpus(dir = paste0(tmp_path, "/_txt_files"),
                  tokenizer = tokenize_ngrams,
                  lowercase = TRUE, #texto en min?scula para tokenizarlo
                  n = 9,
                  keep_tokens = TRUE,
                  progress = FALSE)

# tokenizo con oraciones ----
corpus <-
  TextReuseCorpus(dir = paste0(tmp_path, "/_txt_files"),
                  tokenizer = tokenize_sentences,
                  lowercase = TRUE, #texto en min?scula para tokenizarlo
                  keep_tokens = TRUE,
                  progress = FALSE)


# hasta aca tokenizaciones ----

# JACCARD ----
#help("similarity-functions") #info sobre jaccard y ratio of matches (direccional)
comparisons_jaccard <- 
  pairwise_compare(corpus, 
                   jaccard_similarity,
                   progress = FALSE)
candidates_jaccard <-
  pairwise_candidates(comparisons_jaccard)

candidates_jaccard <- candidates_jaccard %>%
  mutate(id1 = as.numeric(str_sub(a, 5, 1000)),
         id2 = as.numeric(str_sub(b, 5, 1000))) %>%
  left_join(df %>% 
              rename(
                alumno1 = alumno,
                author1 = author, 
                file1 = file),
            by = c("id1" = "id")) %>%
  left_join(df %>% 
              rename(
                alumno2 = alumno,
                author2 = author, 
                file2 = file), 
            by = c("id2" = "id")) %>% 
  arrange(desc(score)) %>% 
  select(id1, id2, score,
         alumno1, author1,
         alumno2, author2,
         file1, file2)


# buscamos en que TPs NO coinciden el alumno con el author
df1 <- candidates_jaccard %>% 
  filter(!is.na(alumno1)) %>% 
  group_by(alumno1) %>% 
  mutate(n = n_distinct(author1)) %>% 
  ungroup() %>% 
  filter(n > 1) %>% 
  select(alumno1, author1) %>% 
  distinct() %>% 
  arrange(alumno1)

df2 <- candidates_jaccard %>% 
  filter(!is.na(author1)) %>% 
  group_by(author1) %>% 
  mutate(n = n_distinct(alumno1)) %>% 
  ungroup() %>% 
  filter(n > 1) %>% 
  select(author1, alumno1) %>% 
  distinct() %>% 
  arrange(author1)

# RATIO OF MATCHES (direccional) ----
comparisons_ratio <- 
  pairwise_compare(corpus, 
                   ratio_of_matches, #podria dar indicios de quien se copio de quien
                   directional = TRUE,  #podria servir para saber si varias personas usaron de base algun tp de otro
                   progress = FALSE)
candidates_ratio <-
  pairwise_candidates(comparisons_ratio)

candidates_ratio <- candidates_ratio %>%
  mutate(id1 = as.numeric(str_sub(a, 5, 1000)),
         id2 = as.numeric(str_sub(b, 5, 1000))) %>%
  left_join(df %>% 
              rename(
                alumno1 = alumno,
                author1 = author, 
                file1 = file),
            by = c("id1" = "id")) %>%
  left_join(df %>% 
              rename(
                alumno2 = alumno,
                author2 = author, 
                file2 = file), 
            by = c("id2" = "id")) %>% 
  arrange(desc(score)) %>% 
  select(id1, id2, score,
         alumno1, author1,
         alumno2, author2,
         file1, file2)


# hasta aca calculo de distancias/similaridades entre textos ----

# Clustering jerarquico ----
comparisons_jaccard_DIS <- # input del clustering las DISsimilaridades (distancias) entre individuos
  pairwise_compare(corpus, # uso el corpus tokenizado con n=5 gramas
                   jaccard_dissimilarity, # DISsimilaridad de Jaccard
                   progress = FALSE,
                   directional = T) 

# clustering jerarquico
clusters <- comparisons_jaccard_DIS %>% t() %>% as.dist()  %>%
  hclust(method = "single")

# Dendograma
plot(clusters)

# poniendo un height límite
clust_final_limite <- cutree(clusters,
                      h = quantile(clusters$height, probs = c(0.1))) # como height límite le pongo el fractil del 10% de todos los height)

clust_final_limite


# Multidimensional Scaling ----
library(magrittr)
library(ggpubr)
library(plotly)

#### reduce la dimension a 2 (default) y genera un mapa de 2d

#TPs a identificar (si se quiere observar un par de TPs en especifico)
tp_ident_1 <- "file107"
tp_ident_2 <- "file26"

# MDS clasico (Multidimensional Scaling clasico) (tambien conocido como Principal Coordinates Analysis)
mds_completo <- comparisons_jaccard_DIS %>% t() %>% as.dist()  %>% cmdscale(eig = T) 
eig_mds <- mds_completo$eig # (le pido los autovalores al mds para ver el % de info en el mismo)

# "análisis de confiabilidad" del MDS
sum(abs(eig_mds[1:2]))/sum(abs(eig_mds)) # mantiene un ~4% de la info

# MDS coloreando dos TPs en especifico ----
mds <- mds_completo$points %>% # mediante MDS clasico logra la mejor reduccion a 2 dimensiones (deflault k=2)
  as_tibble() %>% # paso la matriz a un tibble
  mutate(ident = ifelse(rownames(comparisons_jaccard_DIS) %in% c(tp_ident_1, tp_ident_2), 1, 0) %>% 
           factor(),
         archivo = rownames(comparisons_jaccard_DIS))
colnames(mds) <- c("Dim.1", "Dim.2", "ident", "archivo")

# Plot MDS
#help("ggscatter")
mapa <- ggscatter(mds , x = "Dim.1", y = "Dim.2",
                  main = tp_name,
                  xlab = " ",
                  ylab = " ",
                  color = "ident", #los TPs a identificar van en diferente color
                  label = "archivo",
                  size = 1,
                  repel = TRUE) + 
  theme_bw()

#help("ggplotly")
ggplotly(mapa) #interactivo
mapa #estatico

# MDS coloreando los grupos ----
mds_groups <- mds_completo$points %>% # mediante MDS clasico logra la mejor reduccion a 2 dimensiones (deflault k=2)
  as_tibble() %>% # paso la matriz a un tibble
  mutate(Group = clust_final_limite %>% factor(),
         archivo = names(clust_final_limite))
colnames(mds_groups) <- c("Dim.1", "Dim.2", "Group", "archivo")

mds_groups$Group <- as.factor(mds_groups$Group)

cantidad_individuos <- count(mds_groups, Group) # cantidad de individuos en cada grupo (los grupos individuales no son de interes)

# Armo un vector con los nombres (son numeros) de los grupos multiples/NO individuales
grupos_multiples <- c()
for (j in 1:nrow(cantidad_individuos)) {
  if (cantidad_individuos[[j,2]] != as.factor(1)){
    grupos_multiples <- c(grupos_multiples, cantidad_individuos[[j,1]])
  }
}

# Dejo los grupos multiples en el dataframe de grupos y los individuales los considero SIN grupo
for (k in 1:nrow(mds_groups)) {
  if (mds_groups[[k, 3]] %in% grupos_multiples){
  }
  else {
    mds_groups[[k, 3]] <- NA
  }
}

# Plot MDS
#help("ggscatter")
mapa_groups2 <- ggplot(mds_groups,
                       aes(x = Dim.1, y = Dim.2, text = archivo, color = Group)) +
  geom_point()
#mapa_groups <- ggscatter(mds_groups , x = "Dim.1", y = "Dim.2",
#                  main = tp_name,
#                  xlab = " ",
#                  ylab = " ",
#                  color = "Group", #los TPs a identificar van en diferente color
#                  text = "archivo",
#                  size = 3,
#                  repel = TRUE) + 
#  theme_bw()

#help("ggplotly")
ggplotly(mapa_groups2) #interactivo
mapa_groups #estatico


# Pasaje a excel ()----
library(openxlsx)
# Tokenizado con ngrams n=5
list_candidates_ngrams_n5 <- list("Candidatos_Jaccard_n5" = candidates_jaccard,
                                  "Candidatos_Ratio_n5" = candidates_ratio,
                                  "Alumno VS Autor_1" = df1,
                                  "Alumno VS Autor_2" = df2)
write.xlsx(list_candidates_ngrams_n5,
           file = paste0(folder_path, "\\Candidatos_ngrams_n5.xlsx"),
           asTable = TRUE,
           overwrite = TRUE)

# Tokenizado con ngrams n=7
list_candidates_ngrams_n7 <- list("Candidatos_Jaccard_n7" = candidates_jaccard,
                                  "Candidatos_Ratio_n7" = candidates_ratio,
                                  "Alumno VS Autor_1" = df1,
                                  "Alumno VS Autor_2" = df2)
write.xlsx(list_candidates_ngrams_n7,
           file = paste0(folder_path, "\\Candidatos_ngrams_n7.xlsx"),
           asTable = TRUE,
           overwrite = TRUE)

# Tokenizado con ngrams n=9
list_candidates_ngrams_n9 <- list("Candidatos_Jaccard_n9" = candidates_jaccard,
                                  "Candidatos_Ratio_n9" = candidates_ratio,
                                  "Alumno VS Autor_1" = df1,
                                  "Alumno VS Autor_2" = df2)
write.xlsx(list_candidates_ngrams_n9,
           file = paste0(folder_path, "\\Candidatos_ngrams_n9.xlsx"),
           asTable = TRUE,
           overwrite = TRUE)

# Tokenizado con oraciones
list_candidates_sentences <- list("Candidatos_Jaccard_sentences" = candidates_jaccard,
                                  "Candidatos_Ratio_sentences" = candidates_ratio,
                                  "Alumno VS Autor_1" = df1,
                                  "Alumno VS Autor_2" = df2)
write.xlsx(list_candidates_sentences,
           file = paste0(folder_path, "\\Candidatos_ngrams_sentences.xlsx"),
           asTable = TRUE,
           overwrite = TRUE)



