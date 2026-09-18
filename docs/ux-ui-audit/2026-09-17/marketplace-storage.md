# Marketplace y almacenamiento opcional — continuación UX-260917-007

En producción `e1a825b`, negar el getter de almacenamiento sustituye Marketplace
por «No pudimos cargar esta vista» en los tres motores. No aparece como `pageerror`:
lo captura el límite de errores de React. Cuatro regresiones de componente fallan
en el código original: getter, lectura, escritura y borrado denegados.

Se reutilizan los helpers de preferencias opcionales para filtros, medio de pago
preferido e indicadores de carrito. Los filtros continúan en la URL y el estado
de la visita; un fallo del cache no interrumpe la vista ni una respuesta confirmada.
No se guardan credenciales en memoria como sustituto de persistencia requerida.
El contrato de idempotencia de pedidos permanece intacto: si no puede conservar
la clave, no envía el pedido. No se asegura recuperación de un carrito tras cerrar
el navegador cuando éste impide guardar su referencia.

Verificación: 23 pruebas de componente/API; TypeScript, lint y build con cinco
precargas/368229 bytes gzip iniciales. Treinta casos distintos de navegador con
API/PostgreSQL aislados reales: tres motores,320/1280px y almacenamiento normal o
cuatro operaciones denegadas; catálogo poblado, búsqueda, vacío, URL y recarga.
Cero violaciones axe medidas, desbordamientos o excepciones en esos casos. Un caso
inicial contó telemetría externa como POST de checkout; se corrigió el filtro del
harness y se repitió sólo ese caso. Idioma efectivo español; no se afirma cobertura
inglesa, lector manual, pagos reales ni datos de rendimiento de campo.

`MarketplaceStorage.tla` verifica la separación entre cache opcional y clave
durable requerida, con dos intentos acotados. TLC explora ocho estados y comprueba
dos controles negativos con propiedades concretas. El conjunto fijado TLC/Alloy
completo pasa. El modelo abstrae la ejecución de pagos; no certifica handlers
ajenos ni todos los cambios de disponibilidad del almacenamiento.

[Recibo y límites](evidence/marketplace-storage-verification.json).
La continuación está implementada y verificada localmente; revisión, merge y
despliegue siguen pendientes. No requiere migración ni cambio de API. Recuperación:
revertir este incremento web únicamente preservando los demás cambios de main;
no ejecutar rollback de base de datos ni de autenticación por este cambio.

## Production verification — 2026-09-18 17:20 UTC

PR450 exact585003b6e passed all applicable gates and independent approval, then
merged normally as3f0a56c0ded55fa9525dc80c895505ffcb069449. The original CI run
passed259/260 persona cases; WebKit recovery-dialog close timed out on unchanged
auth code. The exact local case passed3/3; one justified retry of only that CI job
passed all260. Failed attempts are not counted as passing evidence.

Cloudflare8f01e9e1 completed; public HTML matches its immutable deployment byte
for byte. Six real production cases passed in Chromium/Firefox/WebKit320/1280
with both storage getters denied: populated catalog, search, empty results, URL
filters and reload, zero page errors, axe violations and horizontal overflow.
The first probe expected Aguilar on page1 although the catalog sorts it later;
that fixture error is excluded. The corrected check starts at the observed QHA
item and finds Aguilar through search. No order/payment was attempted. API remains
e1; no backend deployment was duplicated. See evidence/marketplace-storage-deployment.json.
