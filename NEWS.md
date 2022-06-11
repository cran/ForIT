---
output:
  pdf_document: default
  html_document: default
---
## ForIT package updated (Version: v2.0.1)

### Major structural changes
- All backstage information (parameters, species, domains,...) is organized as a relational database, see `help(ForIT_DataBase)` for details.  
- Internal data structure has been improved using a list column to store the triangular matrices of the parameters that have different sizes.  
- Functions are now suited for piping.  

### Other changes
- The analysis of prediction error is improved adding the `accuracyPlot` functions-family and the `INFC_CVgrid` function.  
- Introduction of "EPPO codes" to identify species using a well-known standard.  

