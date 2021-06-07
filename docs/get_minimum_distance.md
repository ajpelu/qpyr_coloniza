### Get minimun distance from edge of crops
Aim: 
* Get minimun distance from centroid of each transect and boundary of each abandoned land. 

#### Steps

* We created a layer of abandoned land crops from `cultivos56_canar.shp` and `cultivos56_sanjuan.shp`
* We uploaded this layer as `aux_ajpelu_cultivos_56.shp`
* We computed the minimun distance among the centroid of each transect and the boundary of each abandoned land crop

```sql 
SELECT 
  t.id AS transect_id, 
  t.nombre AS transect_nombre,
  c.id AS cult_id,
  c.loc,
  c.cultivo,
  c.replicate,
  ST_Distance(st_centroid(t.the_geom),c.geom) AS distance
FROM 
  mig_transectos AS t,
  aux_ajpelu_cultivos_56_lineas AS c
ORDER BY transect_nombre;
```

The output of this query was saved as `./data/min_distance.csv` 