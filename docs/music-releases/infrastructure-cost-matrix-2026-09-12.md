# Infraestructura multimedia y costos — 2026-09-12

Precios públicos consultados el 2026-09-12; USD, sin impuestos, soporte ni compute. Revalidar al contratar.

| Alternativa | Storage publicado | Egreso/CDN | Operación y portabilidad | Decisión |
|---|---:|---|---|---|
| Cloudflare R2 Standard | $0.015/GB-mes; 10 GB-mes gratis | egreso Internet gratis; red global Cloudflare | S3-compatible; A $4.50/M, B $0.36/M después del free tier | preferida para derivados hot y entrega LATAM |
| Cloudflare R2 Infrequent Access | $0.01/GB-mes, mínimo 30 días | egreso gratis; retrieval $0.01/GB | S3-compatible, operaciones más caras | candidata para DDEX/archivo no frecuente, no previews hot |
| Backblaze B2 | $6.95/TB-mes, primeros 10 GB gratis | 3× storage gratis; luego $0.01/GB; ilimitado por varios CDN partners | S3-compatible, sin duración mínima; transacciones comunes gratuitas | segunda opción/backup portable |
| AWS S3 | precio por región/clase y por operación | transferencia y CDN se cotizan por región | mayor catálogo, lifecycle y redundancia; riesgo de egreso/costo complejo | no migrar ahora sin benchmark y cotización de región |

Fuentes oficiales:

- https://developers.cloudflare.com/r2/pricing/
- https://www.backblaze.com/cloud-storage/pricing
- https://www.backblaze.com/cloud-storage/transaction-pricing
- https://aws.amazon.com/s3/pricing/

La decisión no reemplaza infraestructura activa: introduce una interfaz SigV4 S3-compatible. Para Ecuador y Latinoamérica se propone medir desde Quito/Guayaquil y al menos São Paulo/Santiago: TTFB, throughput de range requests, tasa de rebuffer, upload multipart y costo por 1.000 reproducciones. Sin esas mediciones no se afirma superioridad regional.

Buckets recomendados: cuarentena con expiración corta; máster/original privado con versioning y retención; derivados privados servidos mediante URL breve y CDN; DDEX privado. Cifrado en reposo administrado por el proveedor, TLS, CORS por origen, rangos habilitados y claves diferentes para API/worker. Evitar lock-in conservando hashes/manifiestos y sin usar URLs de proveedor como identidad.

## Aclaración de compatibilidad — 2026-09-14

R2 no implementa las operaciones S3 de bucket versioning ni las cabeceras S3
Object Lock; no interpretar «S3-compatible» como soporte de esas garantías.
Cloudflare sí ofrece bucket locks propios, que impiden borrar o sobrescribir
objetos durante su retención. Son mecanismos distintos y deben verificarse
con el proveedor seleccionado antes de almacenar másteres de producción.
Fuentes: [matriz S3 de R2](https://developers.cloudflare.com/r2/api/s3/api/)
y [bucket locks](https://developers.cloudflare.com/r2/buckets/bucket-locks/).

El worker actual usa un solo endpoint/principal S3 por despliegue para sus
buckets; no está implementado repartir másteres en B2/S3 y derivados en R2 dentro
de una iteración. Tampoco se ha probado el reintento de PUT contra un objeto
protegido por retención. Por tanto, la preferencia de costo para R2 no autoriza
su uso para originales/DDEX sin diseñar y ensayar backup, recuperación e
idempotencia bajo las políticas reales de retención. No se ha cambiado ningún
proveedor, bucket o política remota.

Otro límite pendiente: el worker promueve originales y sube paquetes con un
solo PUT. R2 documenta un máximo de 5 GiB por carga single-part y S3 de 5 GB por
PUT; el límite interno de 8 GiB no está soportado de extremo a extremo por ese
camino. Antes de producción hay que añadir multipart al worker o acordar y
aplicar un límite inferior coherente en servidor y cliente. No confundir el
multipart inicial de la API con la transferencia posterior del worker.
Fuentes verificadas el 2026-09-14: [límites de R2](https://developers.cloudflare.com/r2/platform/limits/)
y [límites de carga S3](https://aws.amazon.com/s3/faqs/).
