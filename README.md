# SDM_2026_2
Este repositorio contiene información, datos y código del curso de modelos de distribución de especie 2026-2 (Doctorado en ciencias biológicas UQ - UTP)

# Requisitos:
- Instalar R
- Instalar Java
- Instalar RStudio
- Acceso a Dropbox
- Crear una cuenta de Github (ESTE REPO SE VOLVERÁ PRIVADO)


# Horarios: Sabados de 8 a 12

> [!AVISO]
> Archivos pesados estarán en Dropbox
  
# Modalidad virtual
>[ Link clase]
>Microsoft Teams meeting
>
>Join:
>https://teams.microsoft.com/meet/265369090130731?p=hyztc072Qw1bp1AFy7
>
>Meeting ID:
>265 369 090 130 731
>
>Passcode:
>JH6SD3TM

# Temas:

## 1.	Introducción a R y GIS (80% Practico, 20% teórico)
- 1.1.	Introducción a R
- 1.2.	Introducción a GIS en R
- 1.3.	Introducción a Machine learning en R
- 1.4.	Github (Reproducibilidad de análisis): [https://github.com/hectornieto/cursoGit/tree/master]
## 2.	Introducción (100% teórico)
- 2.1.	Introducción a los modelos de nicho.
- 2.2.	Conceptos de nicho ecológico.
- 2.3.	Nichos y distribuciones.
- 2.4.	Diagrama BAM
## 3.	Preprocesamiento y modelación (80% Practico, 20% teórico) 
- 3.1.	Limpieza de datos
- 3.2.	Obtención de datos
- 3.3.	Limpieza taxonómica
- 3.4.	Selección de variables
- 3.5.	Selección de algoritmo (tipos de algoritmo: MaxEnt, Random Forest, etc...)
- 3.6.	Ensambles
- 3.7.	Selección de método de background (Target group, environment, aleatorio, ecosystems [números parecidos entre ecosistemas])
- 3.8.	Calibración en ENMEval
- 3.9.	Diferencias en resolución espacial 
## 4.	Evaluación (80% Practico, 20% teórico)
- 4.1.	Desempeño vs significancia estadística.
- 4.2.	Métricas de desempeño y métodos estadísticos de validación.
- 4.3.	¿Qué hacer con pocos datos?
- 4.4.	Evaluación por bloques espaciales (BlockCV)
- 4.5.	Nicho realizado (Limitar a un espacio más realista)
## 5.	Transferencia de modelos de nicho en espacio y tiempo 90% Practico, 10% teórico)
- 5.1.	Conservatismo de nicho
- 5.2.	Transferencia de modelos
- 5.3.	Consideraciones en el contexto de cambio climático


#Paquetes necesarios:
- raster
- terra
- sf
- caret
- ENMEval
- dismo
- enmsdmX
- parallel
- blockCV
- viridis
- usdm
- raster

# Dudas
- ccsosa@uniquindio.edu.co

# Recursos

### Databases:
- [GBIF (Global Biodiversity Information Facility)](https://www.gbif.org/)
- [iDigBio (Integrated Digitized Biocollections)](https://www.idigbio.org/)
- [Plants of the world](https://powo.science.kew.org/)
- [Taxonomic Name Resolution Service](https://tnrs.biendata.org/)
- [TNRS tutorial (Spanish)](https://sib-colombia.github.io/Formacion/LAB/lab02/lab_tnrs.html)
### Predictors
- [Worldclim.org](Worldclim.org) 
- [CHELSA](https://www.chelsa-climate.org/)
- [NASA (earthdata)](https://www.earthdata.nasa.gov/topics/human-dimensions/data-access-tools)
- [ENVIREM (ENVIronmental Rasters for Ecological Modeling)](https://envirem.github.io/)
####GIS shapefiles:
- [GADM](https://gadm.org/)
- [Worldclim](https://www.worldclim.org/data/index.html)
### Data Curation (Recommended):
- [Spatial Data Quality Checks](https://knowledge.base.unocha.org/wiki/spaces/imtoolbox/pages/228622451/Geodata)
- [Biodiversity informatics: managing and applying primary biodiversity data](https://royalsocietypublishing.org/doi/10.1098/rstb.2003.1439)
- [Resolution in species distribution models shapes spatial patterns of plant multifaceted diversity](https://nsojournals.onlinelibrary.wiley.com/doi/full/10.1111/ecog.05973)
- [The influence of spatial errors in species occurrence data used in distribution models](https://besjournals.onlinelibrary.wiley.com/doi/10.1111/j.1365-2664.2007.01408.x)
### Data Curation:
- [Spatial Data Quality Checks](https://knowledge.base.unocha.org/wiki/spaces/imtoolbox/pages/228622451/Geodata)
- [Biodiversity informatics: managing and applying primary biodiversity data](https://royalsocietypublishing.org/doi/10.1098/rstb.2003.1439)
- [Resolution in species distribution models shapes spatial patterns of plant multifaceted diversity](https://nsojournals.onlinelibrary.wiley.com/doi/full/10.1111/ecog.05973)
### Species distribution models:
- [Predictive habitat distribution models in ecology](https://www.sciencedirect.com/science/article/pii/S0304380000003549)
- [Species Distribution Models: Ecological Explanation and Prediction Across Space and TimeWallace: A flexible platform for reproducible modeling of species niches and distributions built for community expansion](https://www.annualreviews.org/content/journals/10.1146/annurev.ecolsys.110308.120159)
- [A standard protocol for reporting species distribution models](https://nsojournals.onlinelibrary.wiley.com/doi/10.1111/ecog.04960)
- [The art of modelling range-shifting species](https://besjournals.onlinelibrary.wiley.com/doi/10.1111/j.2041-210X.2010.00036.x)
- [Wallace: A flexible platform for reproducible modeling of species niches and distributions built for community expansion](https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.12945?medium=article)
- [Including imprecisely georeferenced specimens improves accuracy of species distribution models and estimates of niche breadth](https://onlinelibrary.wiley.com/doi/10.1111/geb.13628)
- [ENMeval: An R package for conducting spatially independent evaluations and estimating optimal model complexity for Maxent ecological niche models](https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.12261)
- [ENMeval 2.0: Redesigned for customizable and reproducible modeling of species’ niches and distributions](https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.13628)
- [Multiple Threshold-Selection Methods Are Needed to Binarise Species Distribution Model Predictions](https://onlinelibrary.wiley.com/doi/full/10.1111/ddi.70019)
- [A new threshold selection method for species distribution models with presence-only data: Extracting the mutation point of the P/E curve by threshold regression](https://onlinelibrary.wiley.com/doi/full/10.1002/ece3.11208)
- [Modelling the potential range of Agrilus planipennis in Europe according to current and future climate conditions](https://www.sciencedirect.com/science/article/pii/S2666719324000669)
- [Choice of threshold alters projections of species range shifts under climate change](https://www.sciencedirect.com/science/article/abs/pii/S0304380011003814)

> [!NOTA]
> Parte del material a usarse ha sido creado para workshops o tutoriales para ONGs