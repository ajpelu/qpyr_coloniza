### Get data from Information System

First, I get two dataframes from Information System (https://linaria.obsnev.es) through SQL senteces. 

* observation data
* transect info 

##### Observation data 
This dataframe contains information about all tree records measured in the field belong to one of the transect set. 

```sql 
SELECT 
  mig_olaps.id, 
  mig_olaps.altitud, 
  mig_olaps.tipo, 
  mig_olaps.subtipo, 
  mig_olaps.localidad, 
  mig_olaps.nombre, 
  mig_olaps.especie, 
  mig_olaps.altura, 
  mig_olaps.eje_mayor, 
  mig_olaps.eje_menor, 
  mig_olaps.diametro_base, 
  mig_olaps.porcentaje_herbivoria, 
  mig_olaps.origen, 
  mig_olaps.edad, 
  mig_olaps.cesped, 
  mig_olaps.tipo_individuo, 
  mig_olaps.edad_interpolada
FROM 
  public.mig_olaps;
```

The data is saved at `./data/migrame_observations.csv`

##### Transect info
This dataframe contains information about location, date, elevation and related information of each transect. 

```sql 
SELECT 
  mig_transectos.id, 
  mig_transectos.nombre, 
  mig_transectos.fecha, 
  mig_transectos.tipo, 
  mig_transectos.altitud, 
  mig_transectos.localidad, 
  mig_transectos.subtipo
FROM 
  public.mig_transectos;
```

The data is saved at `./data/migrame_transect.csv`
