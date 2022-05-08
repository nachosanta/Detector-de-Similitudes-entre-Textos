library(textreuse)
help("jaccard_similarity")

# tokenizacion con n-gramas (pasa a minuscula y quita signos de puntuacion)
a <- tokenize_ngrams("Hola, buenas tardes. Saludos desde Argentina.", lowercase = T, n = 2)
a
b <- tokenize_ngrams("Hola, buenos días. Saludos desde.", lowercase = T, n = 2)
b

jaccard_bag_similarity(a, b) # esta NO la usamos en el TP
length(intersect(a, b))/(length(a)+length(b)) #el problema con esta similaridad es que si el texto es muy grande, achica mucho la similaridad aunque tenga muchas cosas en común

jaccard_similarity(a, b)
length(intersect(a, b))/length(union(a, b)) #el hecho de usar la union en el denominador, corrige en parte el problema anterior (aunque sigue presente que si el texto es muy grande puede opacar las similiradidades)

jaccard_dissimilarity(a, b)
1 - length(intersect(a, b))/length(union(a, b)) #complemento de la similaridad

ratio_of_matches(a, b) # podria dar un indicio de quien se copio de quien pero siempre penaliza a los textos mas cortos dandoles mas score
length(intersect(a, b))/length(b) #cuanto de la intersección conforma b
ratio_of_matches(b, a)
length(intersect(a, b))/length(a) #cuanto de la intersección conforma a


# tokenizacion con oraciones (pasa a minuscula y quita signos de puntuacion) (SON DIFERENTES STRINGS)
c <- tokenize_sentences("Hola, buenas tardes. Saludos desde Argentina.", lowercase = T)
c
d <- tokenize_sentences("Hola, buenas tardes. Saludos desde.", lowercase = T)
d

jaccard_bag_similarity(c, d) # esta NO la usamos en el TP
length(intersect(c, d))/(length(c)+length(d))

jaccard_similarity(c, d)
length(intersect(c, d))/length(union(c, d))

jaccard_dissimilarity(c, d)
1 - length(intersect(c, d))/length(union(c, d))

ratio_of_matches(c, d)
length(intersect(c, d))/length(d) #cuanto de la intersección conforma b
ratio_of_matches(d, c)
length(intersect(c, d))/length(c) #cuanto de la intersección conforma c

