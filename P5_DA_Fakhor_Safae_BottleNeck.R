#Les packages utiliser
library(ggplot2) #facilite la création de graphiques
library(dplyr) #pour le traiteent et la manipulation des données
library(tidyverse)
library(readxl) #pour lire les fichier Excel


#Import des données: La Table erp aprés convertion en fichier csv
erp = read.csv("erp.csv", sep =";")
dim(erp) #dimension de la table erp
str(erp) #Type de chaque colonne
names(erp) #Noms des colonnes

df1 = data.frame(table(erp$product_id)) #crée une table pour vérifier l'unicité de la clé "product_id"
df1[df1$Freq > 1,] #vérification de l'unicité de la clé primaire qui est la colonne "product_id"
#si "df1[df1$Freq>1,] nous donne 0 c'est que la colonne estbien une clé primaire.

#Changement de type des colonnes "product_id" et "stock_quantity".
erp$product_id<-as.character(erp$product_id)
erp$onsale_web<-as.logical(erp$onsale_web)

#Le résumer de la table erp.
summary(erp)
str(erp)

#import de la table Web
web = read_excel("web.xlsx")
dim(web)
str(web)
names(web)
df2 = data.frame(table(web$sku)) #crée une table pour vérifier l'unicité de la clé "sku"
df2[df2$Freq>1,]
#le nombre de doublons qu'on a sur la colonne "sku"
dim(web[duplicated(web$sku),])[1] 

#résumer
str(web)
summary(web)

va_null = web[is.na(web$sku),] #recherche de valeures nulles dans la colonne "sku"
dim(va_null) #28 valeurs manquantes

str(va_null)
head(va_null)

observation = va_null[!is.na(va_null$post_name),] #les lignes ou poste_name n'est pas nul
dim(observation)
observation

#Considérons que les les valeurs manquantes sont due au retrait des produit
#Donc on crée une nouvelle table sans les valeurs NA "web_2"

web_2 = web[!is.na(web$sku),]
dim(web_2)
str(web_2)

#on vérifie l'unicité dans la nouvelle table "web_2"
df3 = data.frame(table(web_2$sku)) 
df3[df3$Freq>1,]
dim(web_2[duplicated(web_2$sku),])[1] #On remarque qu'on a encore des doublons dans la colonne "sku"

#Aprés l'observation des données on remarque que pour certains produits, dans la colonne 'post_type' certaines lignes sont des 'product' et d'autres 'attachement'
#On ne garde alors que les lignes des 'product' puisque les 'attachment' correspond a des lignes ou on a les images du produits.

df_product = web_2[web_2$post_type=='product',]
#on observe la nouvelle table
dim(df_product)
head(df_product)
summary(df_product)

#import de la table "liaison"
liaison = read.csv("liaison.csv", sep =";")
dim(liaison)
str(liaison)
names(liaison)
dim(liaison)

#changement du type de la colonnes 'product_id'
liaison$product_id<-as.character(liaison$product_id)
#observation
str(liaison)
summary(liaison)

#jointure externe : garde toutes les valeurs des clés des deux tables, même si elles n’apparaissent pas dans l’autre table.
erp_liaison= merge(erp,liaison, by ="product_id", all = T)

dim(erp_liaison)
names(erp_liaison)
str(erp_liaison)

#vérificaton si les 'product_id' de "erp" sont abscent dans "liaison"
anti_join(erp,liaison,by="product_id")
#Et inversement
anti_join(liaison,erp,by="product_id")

#aprés lecture du mail de Sylvie on change le nom de la colonne 'sku' dans la table "df_product"
colnames(df_product)[colnames(df_product) == "sku"] <- "id_web"
names(df_product)

#jointure externe
df_final =merge(erp_liaison, df_product, by='id_web', all = T)
dim(df_final)
names(df_final)
str(df_final)
head(df_final)

#On fait les mêmes vérification sur la tabe "df_final"
df_test<- anti_join(erp_liaison,df_product, by='id_web')
dim(df_test)
head(df_test)


#on remarque donc que 111 ligne dans "erp_liaison" n'ont pas de correspandance dans "df_product"
#Alors on crée une nouvelle table où on fait une jointure interne (ne garde que les valeurs de la clé qui sont à la fois dans la première et la seconde table)
df_final_final= merge(erp_liaison,df_product)
dim(df_final_final)

#CA par produit : On crée une nouvelle colonne 'CA'
df_final_final['CA'] = df_final_final['price'] * df_final_final['total_sales']

#pour se faciliter l'observation on crée une nouvelle table où on ne garde que les colonnes concernées 
columns = c('product_id','price', 'total_sales', 'CA')
df_CA = df_final_final[columns]
dim(df_CA)
names(df_CA)
head(df_CA)
str(df_CA)
summary(df_CA)

#Le CA total: on calcul la somme de la colonne 'CA'
CA_total = (sum(df_CA['CA']))
CA_total
print(paste("Le chiffre d'affaire total est de",CA_total, "euros")) 


mn =mean(df_CA$price) # moyenne de la distribution des prix
sd=sd(df_CA$price) #l'écart-type de la distribution des prix

df_CA["z_score"]=0
head(df_CA)

for (i in 1:nrow(df_CA)) {
  x=df_CA$price
  
  df_CA$z_score[i] = (x[i]-mn)/sd
  
}
df_CA
summary(df_CA)


#On calcule l'interquartile

q = quantile(df_CA$price,c(0.25,0.75))
q1=q[1]
q3=q[2]
iqr=q3 - q1
print(paste("Q1 est égale à",q1,", et Q3 est égale à",q3))
print(paste("L'nterquartile est égale à",iqr))


#On calcule les valeurs maximales et minimales.

lower_bd = q1 -(1.5*iqr)
upper_bd = q3 +(1.5*iqr)
print(paste("Valeur minimale :",lower_bd,"Valeur maximale",upper_bd))

#On cherche les outliers.

outliers = df_final_final[(df_final_final['price']>upper_bd) | (df_final_final['price']<lower_bd),]

print(paste("Nombre d'outliers détectés est",nrow(outliers)))
outliers['price']


#Un scatterplot avec les outliers.
instruction=(df_final_final['price']<upper_bd)&(df_final_final['price']>lower_bd)

ggplot(df_final_final,aes(x=1:nrow(df_final_final), y=price, color=instruction))+geom_point()



#La boîte a moustache avec les valeurs aberrantes et les interquartiles.


boxplot(df_CA$price, col = grey(0.8), main = "Prix par produit", ylab = "Prix") 
rug(df_CA$price, side = 2)
abline(h = median(df_CA$price, na.rm = TRUE), col = "navy", lty = 2)
text(1.35, median(df_CA$price, na.rm = TRUE) + 0.15, "Médiane", col = "navy")
Q1 <- quantile(df_CA$price, probs = 0.25, na.rm = TRUE)
abline(h = Q1, col = "darkred")
text(1.35, Q1 + 0.15, "Q1 : premier quartile", col = "darkred", lty = 2)
Q3 <- quantile(df_CA$price, probs = 0.75, na.rm = TRUE)
abline(h = Q3, col = "darkred")
text(1.35, Q3 + 0.15, "Q3 : troisième quartile", col = "darkred", lty = 2)
arrows(x0 = 0.7, y0 = quantile(df_CA$price, probs = 0.75, na.rm = TRUE), x1 = 0.7, y1 = quantile(df_CA$price, probs = 0.25, na.rm = TRUE), length = 0.1, code = 3)
text(0.7, Q1 + (Q3 - Q1) / 2 + 0.15, "h", pos = 2)
mtext("L'écart inter-quartile h contient 50 % des produits", side = 1)
abline(h = Q1 - 1.5 * (Q3 - Q1), col = "darkgreen")
text(1.35, Q1 - 1.5 * (Q3 - Q1) + 0.15, "lower bound", col = "darkgreen", lty = 2)
abline(h = Q3 + 1.5 * (Q3 - Q1), col = "darkgreen")
text(1.35, Q3 + 1.5 * (Q3 - Q1) + 0.15, "upper bound", col = "darkgreen", lty = 2)
