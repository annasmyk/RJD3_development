# Doc related

# (faire code structuré)

# comments

### domain to reference and point to results
# Load a Workspace
file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")

jws <- jws_open(file)
compute(jws)
# Select SAProcessing
jsap1 <- jws_sap(jws, 1)

# Select SA-item (as java object)
jsai1 <- jsap_sai(jsap1, 3)

### to be added

read_sai(jsai = jsai1)

s<-read_sai(jsai = jsai1)
s$domainSpec


# workspace and sap refresh

############################## TEST: A real example

library(rjd3workspace)

### AUX

# - 1 refresh specs X13 et TS, voir pour toutes policies
# - idem fonction jsap_refresh

##### complete puis tout
policy = c("FreeParameters", "Complete", "Outliers_StochasticComponent", "Outliers",
           "FixedParameters", "FixedAutoRegressiveParameters", "Fixed")

################## complete
file <- "C:/Users/YWYD5I/Documents/00_RJD3_Developpement/RJD3_development/Workspaces/rjd3workspace_refresh_test_GUI.xml"

jws <- jws_open(file)

# Read workspace
rws <- read_workspace(jws, compute= TRUE)
rws$processing$`SAProcessing-1`$`RF0811`$ts
rws$processing$`SAProcessing-1`$`RF0811`$referenceSpec
rws$processing$`SAProcessing-1`$`RF0811`$estimationSpec
rws$processing$`SAProcessing-1`$`RF0811`$resultSpec
rws$processing$`SAProcessing-1`$`RF0811`$results

rws$processing$`SAProcessing-2`$`RF0811`$referenceSpec # pb outliers is enabled no
rws$processing$`SAProcessing-2`$`RF0811`$estimationSpec
rws$processing$`SAProcessing-2`$`RF0811`$resultSpec # 5 pre speciifed detecté et que 3 declarés
rws$processing$`SAProcessing-2`$`RF0811`$results



jws_refresh(jws, policy = "Complete")

## check refresh in R
# sap 1 ok
rws2 <- read_workspace(jws, compute= TRUE)
rws2$processing$`SAProcessing-1`$`RF0811`$referenceSpec
rws2$processing$`SAProcessing-1`$`RF0811`$estimationSpec
rws2$processing$`SAProcessing-1`$`RF0811`$resultSpec # outliers notés pre specified alors qu'ils ne le sont pas
rws$processing$`SAProcessing-1`$`RF0811`$results

# sap 2
rws2$processing$`SAProcessing-2`$`RF0811`$referenceSpec
rws2$processing$`SAProcessing-2`$`RF0811`$estimationSpec
rws2$processing$`SAProcessing-2`$`RF0811`$resultSpec # result spec pas reestimée ??
rws2$processing$`SAProcessing-2`$`RF0811`$results

# write WS and check in GUI

save_workspace(jws, file.path("C:/Users/YWYD5I/Documents/00_RJD3_Developpement/RJD3_development/Workspaces/rjd3workspace_refresh_test_OUT.xml"), replace=TRUE)


################## "Fixed"
file <- "C:/Users/YWYD5I/Documents/00_RJD3_Developpement/RJD3_development/Workspaces/rjd3workspace_refresh_test_GUI.xml"

jws <- jws_open(file)

# Read workspace
rws <- read_workspace(jws, compute= TRUE)
rws$processing$`SAProcessing-1`$`RF0811`$ts
rws$processing$`SAProcessing-1`$`RF0811`$referenceSpec
rws$processing$`SAProcessing-1`$`RF0811`$estimationSpec
rws$processing$`SAProcessing-1`$`RF0811`$resultSpec
rws$processing$`SAProcessing-1`$`RF0811`$results

rws$processing$`SAProcessing-2`$`RF0811`$referenceSpec # pb outliers is enabled no
rws$processing$`SAProcessing-2`$`RF0811`$estimationSpec
rws$processing$`SAProcessing-2`$`RF0811`$resultSpec # 5 pre speciifed detecté et que 3 declarés
rws$processing$`SAProcessing-2`$`RF0811`$results



jws_refresh(jws, policy = "Fixed")

## check refresh in R
# sap 1 ok
rws2 <- read_workspace(jws, compute= TRUE)
rws2$processing$`SAProcessing-1`$`RF0811`$referenceSpec
rws2$processing$`SAProcessing-1`$`RF0811`$estimationSpec
rws2$processing$`SAProcessing-1`$`RF0811`$resultSpec # outliers notés pre specified alors qu'ils ne le sont pas
rws$processing$`SAProcessing-1`$`RF0811`$results

# sap 2
rws2$processing$`SAProcessing-2`$`RF0811`$referenceSpec
rws2$processing$`SAProcessing-2`$`RF0811`$estimationSpec
rws2$processing$`SAProcessing-2`$`RF0811`$resultSpec # result spec pas reestimée ??
rws2$processing$`SAProcessing-2`$`RF0811`$results ### pb print output pas de (fixed) comme dans X13

################## "FixedParameters"
file <- "C:/Users/YWYD5I/Documents/00_RJD3_Developpement/RJD3_development/Workspaces/rjd3workspace_refresh_test_GUI.xml"

jws <- jws_open(file)

# Read workspace
rws <- read_workspace(jws, compute= TRUE)
rws$processing$`SAProcessing-1`$`RF0811`$ts
rws$processing$`SAProcessing-1`$`RF0811`$referenceSpec
rws$processing$`SAProcessing-1`$`RF0811`$estimationSpec
rws$processing$`SAProcessing-1`$`RF0811`$resultSpec
rws$processing$`SAProcessing-1`$`RF0811`$results

rws$processing$`SAProcessing-2`$`RF0811`$referenceSpec # pb outliers is enabled no
rws$processing$`SAProcessing-2`$`RF0811`$estimationSpec
rws$processing$`SAProcessing-2`$`RF0811`$resultSpec # 5 pre speciifed detecté et que 3 declarés
rws$processing$`SAProcessing-2`$`RF0811`$results



jws_refresh(jws, policy = "FixedParameters") # coeff arima not noted (FIXED) as in "Fixed"

## check refresh in R
# sap 1 ok
rws2 <- read_workspace(jws, compute= TRUE)
rws2$processing$`SAProcessing-1`$`RF0811`$referenceSpec
rws2$processing$`SAProcessing-1`$`RF0811`$estimationSpec
rws2$processing$`SAProcessing-1`$`RF0811`$resultSpec # outliers notés pre specified alors qu'ils ne le sont pas
rws$processing$`SAProcessing-1`$`RF0811`$results

# sap 2
rws2$processing$`SAProcessing-2`$`RF0811`$referenceSpec
rws2$processing$`SAProcessing-2`$`RF0811`$estimationSpec
rws2$processing$`SAProcessing-2`$`RF0811`$resultSpec # result spec pas reestimée ??
rws2$processing$`SAProcessing-2`$`RF0811`$results ### pb print output pas de (fixed) comme dans X13


################## "FixedAutoRegressiveParameters": idem print defaillant dans TS
file <- "C:/Users/YWYD5I/Documents/00_RJD3_Developpement/RJD3_development/Workspaces/rjd3workspace_refresh_test_GUI.xml"

jws <- jws_open(file)

# Read workspace
rws <- read_workspace(jws, compute= TRUE)
rws$processing$`SAProcessing-1`$`RF0811`$ts
rws$processing$`SAProcessing-1`$`RF0811`$referenceSpec
rws$processing$`SAProcessing-1`$`RF0811`$estimationSpec
rws$processing$`SAProcessing-1`$`RF0811`$resultSpec
rws$processing$`SAProcessing-1`$`RF0811`$results

rws$processing$`SAProcessing-2`$`RF0811`$referenceSpec # pb outliers is enabled no
rws$processing$`SAProcessing-2`$`RF0811`$estimationSpec
rws$processing$`SAProcessing-2`$`RF0811`$resultSpec # 5 pre speciifed detecté et que 3 declarés
rws$processing$`SAProcessing-2`$`RF0811`$results



jws_refresh(jws, policy = "FixedAutoRegressiveParameters") # coeff arima not noted (FIXED) as in "Fixed"

## check refresh in R
# sap 1 ok
rws2 <- read_workspace(jws, compute= TRUE)
rws2$processing$`SAProcessing-1`$`RF0811`$referenceSpec
rws2$processing$`SAProcessing-1`$`RF0811`$estimationSpec
rws2$processing$`SAProcessing-1`$`RF0811`$resultSpec # outliers notés pre specified alors qu'ils ne le sont pas
rws$processing$`SAProcessing-1`$`RF0811`$results

# sap 2
rws2$processing$`SAProcessing-2`$`RF0811`$referenceSpec
rws2$processing$`SAProcessing-2`$`RF0811`$estimationSpec
rws2$processing$`SAProcessing-2`$`RF0811`$resultSpec # result spec pas reestimée ??
rws2$processing$`SAProcessing-2`$`RF0811`$results ### pb print output pas de (fixed) comme dans X13


################## "FreeParameters": idem print defaillant dans TS
file <- "C:/Users/YWYD5I/Documents/00_RJD3_Developpement/RJD3_development/Workspaces/rjd3workspace_refresh_test_GUI.xml"

jws <- jws_open(file)

# Read workspace
rws <- read_workspace(jws, compute= TRUE)
rws$processing$`SAProcessing-1`$`RF0811`$ts
rws$processing$`SAProcessing-1`$`RF0811`$referenceSpec
rws$processing$`SAProcessing-1`$`RF0811`$estimationSpec
rws$processing$`SAProcessing-1`$`RF0811`$resultSpec
rws$processing$`SAProcessing-1`$`RF0811`$results

rws$processing$`SAProcessing-2`$`RF0811`$referenceSpec # pb outliers is enabled no
rws$processing$`SAProcessing-2`$`RF0811`$estimationSpec
rws$processing$`SAProcessing-2`$`RF0811`$resultSpec # 5 pre speciifed detecté et que 3 declarés
rws$processing$`SAProcessing-2`$`RF0811`$results



jws_refresh(jws, policy = "FreeParameters") # coeff arima not noted (FIXED) as in "Fixed"

## check refresh in R
# sap 1 ok
rws2 <- read_workspace(jws, compute= TRUE)
rws2$processing$`SAProcessing-1`$`RF0811`$referenceSpec
rws2$processing$`SAProcessing-1`$`RF0811`$estimationSpec
rws2$processing$`SAProcessing-1`$`RF0811`$resultSpec # outliers notés pre specified alors qu'ils ne le sont pas
rws$processing$`SAProcessing-1`$`RF0811`$results

# sap 2
rws2$processing$`SAProcessing-2`$`RF0811`$referenceSpec
rws2$processing$`SAProcessing-2`$`RF0811`$estimationSpec
rws2$processing$`SAProcessing-2`$`RF0811`$resultSpec # result spec pas reestimée ??
rws2$processing$`SAProcessing-2`$`RF0811`$results ### pb print output pas de (fixed) comme dans X13

################## "Outliers": idem print defaillant dans TS ? sans span = whole series re id ?
file <- "C:/Users/YWYD5I/Documents/00_RJD3_Developpement/RJD3_development/Workspaces/rjd3workspace_refresh_test_GUI.xml"

jws <- jws_open(file)


# Read workspace
rws <- read_workspace(jws, compute= TRUE)
rws$processing$`SAProcessing-1`$`RF0811`$ts
rws$processing$`SAProcessing-1`$`RF0811`$referenceSpec
rws$processing$`SAProcessing-1`$`RF0811`$estimationSpec
rws$processing$`SAProcessing-1`$`RF0811`$resultSpec
rws$processing$`SAProcessing-1`$`RF0811`$results

rws$processing$`SAProcessing-2`$`RF0811`$referenceSpec # pb outliers is enabled no
rws$processing$`SAProcessing-2`$`RF0811`$estimationSpec
rws$processing$`SAProcessing-2`$`RF0811`$resultSpec # 5 pre speciifed detecté et que 3 declarés
rws$processing$`SAProcessing-2`$`RF0811`$results


jws_refresh(jws, policy = "Outliers")

## check refresh in R
# sap 1 ok
rws2 <- read_workspace(jws, compute= TRUE)
rws2$processing$`SAProcessing-1`$`RF0811`$referenceSpec
rws2$processing$`SAProcessing-1`$`RF0811`$estimationSpec
rws2$processing$`SAProcessing-1`$`RF0811`$resultSpec # outliers notés pre specified alors qu'ils ne le sont pas
rws$processing$`SAProcessing-1`$`RF0811`$results

# sap 2
rws2$processing$`SAProcessing-2`$`RF0811`$referenceSpec
rws2$processing$`SAProcessing-2`$`RF0811`$estimationSpec
rws2$processing$`SAProcessing-2`$`RF0811`$resultSpec # result spec pas reestimée ??
rws2$processing$`SAProcessing-2`$`RF0811`$results ### pb print output pas de (fixed) comme dans X13

# bilan: x13 outliers pas effacés
# TS effacés

################## "Outliers": idem print defaillant dans TS ? SANS span
file <- "C:/Users/YWYD5I/Documents/00_RJD3_Developpement/RJD3_development/Workspaces/rjd3workspace_refresh_test_GUI.xml"

jws <- jws_open(file)

# dans domain spec 1 LS sur derniere année
# dans estim spec 1 AO sur derniere année

# Read workspace
rws <- read_workspace(jws, compute= TRUE)
# rws$processing$`SAProcessing-1`$`RF0811`$ts
rws$processing$`SAProcessing-1`$`RF0811`$referenceSpec
rws$processing$`SAProcessing-1`$`RF0811`$estimationSpec
rws$processing$`SAProcessing-1`$`RF0811`$resultSpec
rws$processing$`SAProcessing-1`$`RF0811`$results

rws$processing$`SAProcessing-2`$`RF0811`$referenceSpec # pb outliers is enabled no
rws$processing$`SAProcessing-2`$`RF0811`$estimationSpec
rws$processing$`SAProcessing-2`$`RF0811`$resultSpec # 5 pre speciifed detecté et que 3 declarés
rws$processing$`SAProcessing-2`$`RF0811`$results


jws_refresh(jws, policy = "Outliers")

## check refresh in R
# sap 1 ok
rws2 <- read_workspace(jws, compute= TRUE)
rws2$processing$`SAProcessing-1`$`RF0811`$referenceSpec
rws2$processing$`SAProcessing-1`$`RF0811`$estimationSpec
rws2$processing$`SAProcessing-1`$`RF0811`$resultSpec
rws$processing$`SAProcessing-1`$`RF0811`$results

# sap 2
rws2$processing$`SAProcessing-2`$`RF0811`$referenceSpec
rws2$processing$`SAProcessing-2`$`RF0811`$estimationSpec
rws2$processing$`SAProcessing-2`$`RF0811`$resultSpec # result spec pas reestimée ??
rws2$processing$`SAProcessing-2`$`RF0811`$results ### pb print output pas de (fixed) comme dans X13

# bilan: x13 outliers pas effacés
# TS effacés


################## "Outliers": idem print defaillant dans TS ? AVEC span
file <- "C:/Users/YWYD5I/Documents/00_RJD3_Developpement/RJD3_development/Workspaces/rjd3workspace_refresh_test_GUI.xml"

jws <- jws_open(file)

# dans domain spec 1 LS sur derniere année
# dans estim spec 1 AO sur derniere année

# Read workspace
rws <- read_workspace(jws, compute= TRUE)
# rws$processing$`SAProcessing-1`$`RF0811`$ts
rws$processing$`SAProcessing-1`$`RF0811`$referenceSpec
rws$processing$`SAProcessing-1`$`RF0811`$estimationSpec
rws$processing$`SAProcessing-1`$`RF0811`$resultSpec
rws$processing$`SAProcessing-1`$`RF0811`$results

rws$processing$`SAProcessing-2`$`RF0811`$referenceSpec # pb outliers is enabled no
rws$processing$`SAProcessing-2`$`RF0811`$estimationSpec
rws$processing$`SAProcessing-2`$`RF0811`$resultSpec # 5 pre speciifed detecté et que 3 declarés
rws$processing$`SAProcessing-2`$`RF0811`$results


jws_refresh(jws, policy = "Outliers", period=12, start= c(2024, 1))

# spec_x13_ref <- x13_refresh(spec_to_refresh,
#                             reference_spec,
#                             policy = "Current",
#                             period = 12,
#                             start = c(2017, 1),
#                             end = end(y_new)

## check refresh in R
# sap 1 ok
rws2 <- read_workspace(jws, compute= TRUE)
rws2$processing$`SAProcessing-1`$`RF0811`$referenceSpec
rws2$processing$`SAProcessing-1`$`RF0811`$estimationSpec ### error in detection span 2100-01-01
rws2$processing$`SAProcessing-1`$`RF0811`$resultSpec  # pb start: à l'envers ?
rws$processing$`SAProcessing-1`$`RF0811`$results

# sap 2
rws2$processing$`SAProcessing-2`$`RF0811`$referenceSpec
rws2$processing$`SAProcessing-2`$`RF0811`$estimationSpec
rws2$processing$`SAProcessing-2`$`RF0811`$resultSpec # ne garde pas outlier domain spec !!! et pb start
rws2$processing$`SAProcessing-2`$`RF0811`$results ###


################## "Outliers": idem print defaillant dans TS ? AVEC span
file <- "C:/Users/YWYD5I/Documents/00_RJD3_Developpement/RJD3_development/Workspaces/rjd3workspace_refresh_test_GUI.xml"

jws <- jws_open(file)

# dans domain spec 1 LS sur derniere année
# dans estim spec 1 AO sur derniere année

# Read workspace
rws <- read_workspace(jws, compute= TRUE)
# rws$processing$`SAProcessing-1`$`RF0811`$ts
rws$processing$`SAProcessing-1`$`RF0811`$referenceSpec
rws$processing$`SAProcessing-1`$`RF0811`$estimationSpec
rws$processing$`SAProcessing-1`$`RF0811`$resultSpec
rws$processing$`SAProcessing-1`$`RF0811`$results

rws$processing$`SAProcessing-2`$`RF0811`$referenceSpec # pb outliers is enabled no
rws$processing$`SAProcessing-2`$`RF0811`$estimationSpec
rws$processing$`SAProcessing-2`$`RF0811`$resultSpec # 5 pre speciifed detecté et que 3 declarés
rws$processing$`SAProcessing-2`$`RF0811`$results


jws_refresh(jws, policy = "Outliers", period=12, start= c(1996, 1), end =c(2023,12))

# spec_x13_ref <- x13_refresh(spec_to_refresh,
#                             reference_spec,
#                             policy = "Current",
#                             period = 12,
#                             start = c(2017, 1),
#                             end = end(y_new)

## check refresh in R
# sap 1 ok
rws2 <- read_workspace(jws, compute= TRUE)
rws2$processing$`SAProcessing-1`$`RF0811`$referenceSpec
rws2$processing$`SAProcessing-1`$`RF0811`$estimationSpec ### error in detection span 2100-01-01
rws2$processing$`SAProcessing-1`$`RF0811`$resultSpec  # pb start: à l'envers ?
rws$processing$`SAProcessing-1`$`RF0811`$results

# sap 2
rws2$processing$`SAProcessing-2`$`RF0811`$referenceSpec
rws2$processing$`SAProcessing-2`$`RF0811`$estimationSpec
rws2$processing$`SAProcessing-2`$`RF0811`$resultSpec # ne garde pas outlier domain spec !!! et pb start
rws2$processing$`SAProcessing-2`$`RF0811`$results ###





### info= data or none

################## complete NONE : bizarre
file <- "C:/Users/YWYD5I/Documents/00_RJD3_Developpement/RJD3_development/Workspaces/rjd3workspace_refresh_test_GUI.xml"

jws <- jws_open(file)

# Read workspace
rws <- read_workspace(jws, compute= TRUE)
rws$processing$`SAProcessing-1`$`RF0811`$ts
rws$processing$`SAProcessing-1`$`RF0811`$referenceSpec
rws$processing$`SAProcessing-1`$`RF0811`$estimationSpec
rws$processing$`SAProcessing-1`$`RF0811`$resultSpec
rws$processing$`SAProcessing-1`$`RF0811`$results

rws$processing$`SAProcessing-2`$`RF0811`$referenceSpec # pb outliers is enabled no
rws$processing$`SAProcessing-2`$`RF0811`$estimationSpec
rws$processing$`SAProcessing-2`$`RF0811`$resultSpec # 5 pre speciifed detecté et que 3 declarés
rws$processing$`SAProcessing-2`$`RF0811`$results



jws_refresh(jws, policy = "Complete", info = "None")

## check refresh in R
# sap 1 ok
rws2 <- read_workspace(jws, compute= TRUE)
rws2$processing$`SAProcessing-1`$`RF0811`$referenceSpec
rws2$processing$`SAProcessing-1`$`RF0811`$estimationSpec
rws2$processing$`SAProcessing-1`$`RF0811`$resultSpec # outliers notés pre specified alors qu'ils ne le sont pas
rws$processing$`SAProcessing-1`$`RF0811`$results

# sap 2
rws2$processing$`SAProcessing-2`$`RF0811`$referenceSpec
rws2$processing$`SAProcessing-2`$`RF0811`$estimationSpec
rws2$processing$`SAProcessing-2`$`RF0811`$resultSpec # result spec pas reestimée ??
rws2$processing$`SAProcessing-2`$`RF0811`$results

# write WS and check in GUI

save_workspace(jws, file.path("C:/Users/YWYD5I/Documents/00_RJD3_Developpement/RJD3_development/Workspaces/rjd3workspace_refresh_test_OUT.xml"), replace=TRUE)


################## complete data / but here same data
file <- "C:/Users/YWYD5I/Documents/00_RJD3_Developpement/RJD3_development/Workspaces/rjd3workspace_refresh_test_GUI.xml"

jws <- jws_open(file)

# Read workspace
rws <- read_workspace(jws, compute= TRUE)
rws$processing$`SAProcessing-1`$`RF0811`$ts
rws$processing$`SAProcessing-1`$`RF0811`$referenceSpec
rws$processing$`SAProcessing-1`$`RF0811`$estimationSpec
rws$processing$`SAProcessing-1`$`RF0811`$resultSpec
rws$processing$`SAProcessing-1`$`RF0811`$results

rws$processing$`SAProcessing-2`$`RF0811`$referenceSpec # pb outliers is enabled no
rws$processing$`SAProcessing-2`$`RF0811`$estimationSpec
rws$processing$`SAProcessing-2`$`RF0811`$resultSpec # 5 pre speciifed detecté et que 3 declarés
rws$processing$`SAProcessing-2`$`RF0811`$results



jws_refresh(jws, policy = "Complete", info = "Data")

## check refresh in R
# sap 1 ok
rws2 <- read_workspace(jws, compute= TRUE)
rws2$processing$`SAProcessing-1`$`RF0811`$referenceSpec
rws2$processing$`SAProcessing-1`$`RF0811`$estimationSpec
rws2$processing$`SAProcessing-1`$`RF0811`$resultSpec # outliers notés pre specified alors qu'ils ne le sont pas
rws$processing$`SAProcessing-1`$`RF0811`$results

# sap 2
rws2$processing$`SAProcessing-2`$`RF0811`$referenceSpec
rws2$processing$`SAProcessing-2`$`RF0811`$estimationSpec
rws2$processing$`SAProcessing-2`$`RF0811`$resultSpec # result spec pas reestimée ??
rws2$processing$`SAProcessing-2`$`RF0811`$results

## dans x13:
## dans ts: revient à spec ref comme all no difference


# write WS and check in GUI

save_workspace(jws, file.path("C:/Users/YWYD5I/Documents/00_RJD3_Developpement/RJD3_development/Workspaces/rjd3workspace_refresh_test_OUT.xml"), replace=TRUE)



################## "Current": pas ds rjd3workspace
file <- "C:/Users/YWYD5I/Documents/00_RJD3_Developpement/RJD3_development/Workspaces/rjd3workspace_refresh_test_GUI.xml"

jws <- jws_open(file)

# dans domain spec 1 LS sur derniere année
# dans estim spec 1 AO sur derniere année

# Read workspace
rws <- read_workspace(jws, compute= TRUE)
# rws$processing$`SAProcessing-1`$`RF0811`$ts
rws$processing$`SAProcessing-1`$`RF0811`$referenceSpec
rws$processing$`SAProcessing-1`$`RF0811`$estimationSpec
rws$processing$`SAProcessing-1`$`RF0811`$resultSpec
rws$processing$`SAProcessing-1`$`RF0811`$results

rws$processing$`SAProcessing-2`$`RF0811`$referenceSpec # pb outliers is enabled no
rws$processing$`SAProcessing-2`$`RF0811`$estimationSpec
rws$processing$`SAProcessing-2`$`RF0811`$resultSpec # 5 pre speciifed detecté et que 3 declarés
rws$processing$`SAProcessing-2`$`RF0811`$results


jws_refresh(jws, policy = "Current", period=12, start= c(2024, 1))

# spec_x13_ref <- x13_refresh(spec_to_refresh,
#                             reference_spec,
#                             policy = "Current",
#                             period = 12,
#                             start = c(2017, 1),
#                             end = end(y_new)

## check refresh in R
# sap 1 ok
rws2 <- read_workspace(jws, compute= TRUE)
rws2$processing$`SAProcessing-1`$`RF0811`$referenceSpec
rws2$processing$`SAProcessing-1`$`RF0811`$estimationSpec ### error in detection span 2100-01-01
rws2$processing$`SAProcessing-1`$`RF0811`$resultSpec  # pb start: à l'envers ?
rws$processing$`SAProcessing-1`$`RF0811`$results

# sap 2
rws2$processing$`SAProcessing-2`$`RF0811`$referenceSpec
rws2$processing$`SAProcessing-2`$`RF0811`$estimationSpec
rws2$processing$`SAProcessing-2`$`RF0811`$resultSpec # ne garde pas outlier domain spec !!! et pb start
rws2$processing$`SAProcessing-2`$`RF0811`$results ###



#############################################################################
### data refresh
# - in gui
# - in R
################## complete data / but here same data
file <- "C:/Users/YWYD5I/Documents/00_RJD3_Developpement/RJD3_development/Workspaces/rjd3workspace_refresh_test_GUI.xml"

## fichier source prolongé, pas de refresh dans GUI: !!
# old 08/2024
# new 12/2024


jws <- jws_open(file)

# Read workspace
rws <- read_workspace(jws, compute= TRUE)
rws$processing$`SAProcessing-1`$`RF0811`$ts
rws$processing$`SAProcessing-1`$`RF0811`$referenceSpec
rws$processing$`SAProcessing-1`$`RF0811`$estimationSpec
rws$processing$`SAProcessing-1`$`RF0811`$resultSpec
rws$processing$`SAProcessing-1`$`RF0811`$results
tail(rws$processing$`SAProcessing-1`$`RF0811`$results$final$d11final)

rws$processing$`SAProcessing-2`$`RF0811`$referenceSpec # pb outliers is enabled no
rws$processing$`SAProcessing-2`$`RF0811`$estimationSpec
rws$processing$`SAProcessing-2`$`RF0811`$resultSpec # 5 pre speciifed detecté et que 3 declarés
rws$processing$`SAProcessing-2`$`RF0811`$results
tail(rws$processing$`SAProcessing-2`$`RF0811`$results$final$sa)



# jws_refresh(jws, policy = "Complete", info = "Data")
jws_refresh(jws, policy = "Complete")

## check refresh in R
# sap 1 ok
rws2 <- read_workspace(jws, compute= TRUE)
rws2$processing$`SAProcessing-1`$`RF0811`$referenceSpec
rws2$processing$`SAProcessing-1`$`RF0811`$estimationSpec
rws2$processing$`SAProcessing-1`$`RF0811`$resultSpec # outliers notés pre specified alors qu'ils ne le sont pas
rws$processing$`SAProcessing-1`$`RF0811`$results
tail(rws$processing$`SAProcessing-1`$`RF0811`$results$final$d11final)

# sap 2
rws2$processing$`SAProcessing-2`$`RF0811`$referenceSpec
rws2$processing$`SAProcessing-2`$`RF0811`$estimationSpec
rws2$processing$`SAProcessing-2`$`RF0811`$resultSpec # result spec pas reestimée ??
rws2$processing$`SAProcessing-2`$`RF0811`$results
tail(rws$processing$`SAProcessing-2`$`RF0811`$results$final$sa)

## dans x13:
## dans ts: revient à spec ref comme all no difference


# write WS and check in GUI

save_workspace(jws, file.path("C:/Users/YWYD5I/Documents/00_RJD3_Developpement/RJD3_development/Workspaces/rjd3workspace_refresh_test_OUT.xml"), replace=TRUE)


### remarques

## contenus results : final ...series e ?


################################

## T code refesh
library("rjd3toolkit")
library("rjd3workspace")
library("rjd3x13")


# Create the ws
jws <- jws_new()
jsap <- jws_sap_new(jws, "sap1")

ref_spec <- x13_spec("rsa3")
est_spec <- ref_spec |>
    add_outlier(type = "LS", date = "1954-01-01")

add_sa_item(jsap, name = "air", AirPassengers, ref_spec)
set_specification(jsap, 1L, est_spec)

# Refresh without computing first
jws_refresh(jws, policy = "Complete", info = "All")

sai <- read_workspace(jws, compute = TRUE)$processing$sap1$air
waldo::compare(sai$domainSpec, sai$estimationSpec)

## Rien dans domain spec : ne cree rien ? vide ...


# Refresh with computing
jws_compute(jws)
jws_refresh(jws, policy = "Complete", info = "All")

sai <- read_workspace(jws, compute = TRUE)$processing$sap1$air
waldo::compare(sai$domainSpec, sai$estimationSpec)
#> ✔ No differences

# tuile



               : EXAMPLE STRUCTURE

               # Load workspace
               file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")

               jws <- jws_open(file)


               jws_refresh(
                   jws,
                   policy = c("FreeParameters", "Complete", "Outliers_StochasticComponent", "Outliers",
                              "FixedParameters", "FixedAutoRegressiveParameters", "Fixed"),
                   period = 0,
                   start = NULL,
                   end = NULL,
                   info = c("All", "Data", "None")
               )






