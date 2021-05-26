
adam = openxlsx::read.xlsx("GitHub/Pinnacle21/Pinnacle21-SDTM-rules.xlsx", sheet = "ADaM")
sdtm = openxlsx::read.xlsx("GitHub/Pinnacle21/Pinnacle21-SDTM-rules.xlsx", sheet = "SDTM")

table(adam$FDA)
table(adam$FDA.Severity)

table(sdtm$FDA)
table(sdtm$FDA.Severity)
