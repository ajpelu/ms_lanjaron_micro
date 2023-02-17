# Data

Log of Data

## Fire perimeter 
- The perimeter of Lanjarón' fire was obtained from [REDIAM](https://portalrediam.cica.es/descargas?path=%2F06_RIESGOS_NATURALES_TECNOLOG%2F03_ACCIDENTES_DESASTRES%2F01_INCENDIOS%2F00_INCENDIOS%2FAreasRecorrFuego)
- Only Lanjarón Fire was selected and exported as `lanjaron_fire_perimeter.shp` (see data/geo folder) The original datum is `EPSG:3042 - ETRS89 / UTM zone 30N (N-E)`

## Plots points
- 6 locations points were sampled: burned (LJQ1, LJQ2, LJQ3) and unburned areas (LJN1, LJN2, LJN3). 
- To get spatial location, we downloaded the original points from GPS device. Then, they were visually corrected by M. Fernández-López, and then finally checked with the [CIR ortophotography of Fire from REDIAM](http://www.ideandalucia.es/catalogo/inspire/srv/api/records/865a890f538ccb8cd7ce0a9d63ffaa3baaaa4977)
- Location points were saved as `lanjaron_sampled_points.shp` (EPSG:4326) 
