Voilà le fichier modifié et tu trouveras le code qui m’a permis de modifier le fichier :
  
  library("openxlsx")
library("tidyr")
library("dplyr")

wb <- loadWorkbook("../Downloads/tendances_mensuelles_2012_2024.xlsx")

f <- function(wb, sheet) {
  df <- readWorkbook(xlsxFile = wb, sheet = sheet) |> 
    mutate(
      date = paste(annee_de_deces, sprintf("%02d", mois_deces), "01", sep = "-"), 
      var = paste(classe_age, cause, sexe, sep = "-"), 
      value = tx_stand_par_age
    ) |> 
    pivot_wider(id_cols = date, names_from = var, values_from = value)
  deleteData(wb = wb, sheet = sheet, cols = 1:20, rows = 1:20000, gridExpand = TRUE)
  writeData(wb = wb, sheet = sheet, x = df)
}

f(wb, 2L)
f(wb, 3L)
f(wb, 4L)

saveWorkbook(wb, "../Downloads/tendances_mensuelles_2012_2024_TB.xlsx", overwrite = TRUE)

Tanguy
