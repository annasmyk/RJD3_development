## pkgdown yaml transfo test

library(stringr)

# paste stringr# paste github page to  notepad++ and save as xml file

# ch_xml_in<-"Data/X13_Formatage_for_GH_ref.xml"
# ch_xml_in<-"Data/TS_Formatage_for_GH_ref.xml"
# ch_xml_in<-"Data/Toolkit_Liste_Fonctions.xml"

ch_xml_in<-"Data/rjd3workspace_Liste_Fonctions.xml"
ch_xml_out<-"Data/Formatage_for_GH_ref_out.xml"




fic_xml1<-readLines(ch_xml_in)
head(fic_xml1)
tail(fic_xml1)
class(fic_xml1)

# supprimer les lignes ne contenant pas de noms de fonctions = pas de ()
pos<-grep("\\()",fic_xml1)
pos
fic_xml2<-fic_xml1[pos]
fic_xml2
# une fonction par ligne 
new_vect<-c(" ")
#unlist(str_split(fic_xml2[1]," ")) |> class()

for (i in seq(1,length(fic_xml2))){
  print(fic_xml2[i])
  new_vect<- c(new_vect,unlist(str_split(fic_xml2[i]," ")))
}
new_vect<-new_vect[-1]
new_vect
length(new_vect)

for (i in seq(1,length(new_vect))){
  print(new_vect[i])
  new_vect[i]<-str_replace(new_vect[i],"\\(","")
  new_vect[i]<-str_replace(new_vect[i],"\\)","")
  str_trim(new_vect[i])
  new_vect[i]<- paste0("- '`",new_vect[i],"`'")
  print(new_vect[i])
}
new_vect
length(new_vect)

writeLines(new_vect,ch_xml_out)

