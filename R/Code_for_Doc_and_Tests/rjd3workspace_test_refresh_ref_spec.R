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

file <- "C:/Users/YWYD5I/Documents/00_RJD3_Developpement/RJD3_development/Workspaces/rjd3workspace_refresh_test.xml"

jws <- jws_open(file)

# Read workspace
rws <- read_workspace(jws, compute= TRUE)

rws$processing$`SAProcessing-1`$`RF0811 (frozen` ## ISSUE if "auto"....voir T : deatil ?
rws$processing$`SAProcessing-1`$`RF0811`

# Read jsap
jsap1 <- jws_sap(jws,1)
# Select SA-item (as java object)
jsai1 <- jsap_sai(jsap1, 1)
sai1<-read_sai(jsai1)
sai1$referenceSpec
sai1$estimationSpec # ok

jws_ref<-jws_refresh(jws, policy = "Complete") # impossible non ?

jws_refresh(jws, policy = "Complete")


## check refresh in R
# Read jsap
jsap1 <- jws_sap(jws,1)
# Select SA-item (as java object)
jsai1 <- jsap_sai(jsap1, 1)
sai1<-read_sai(jsai1)


sai1$referenceSpec
sai1$estimationSpec # ok





# Read sap
jsap1 <- jws_sap(jws_ref,1)
# Select SA-item (as java object)
jsai1 <- jsap_sai(jsap1, 1)

# read SA-item
read_sai(jsai = jsai1)


# write WS and check in GUI
save_workspace(jws_ref, file.path("C:/Users/YWYD5I/Documents/00_RJD3_Developpement/RJD3_development/Workspaces/rjd3workspace_refresh_test.xml", replace=TRUE)




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






