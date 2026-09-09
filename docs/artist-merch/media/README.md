# Evidencia visual local

Las capturas de esta carpeta provienen de recorridos locales con datos inequívocamente sintéticos. No son capturas de staging o producción y no demuestran integración con proveedores de pago.

- `ios-discovery.png`: catálogo público en iPhone 16 / iOS 18.3.
- `ios-storefront.png`: tienda sintética y producto publicado.
- `ios-product-checkout-disabled.png`: variante disponible, aviso de piloto y CTA de compra deshabilitado.

El build Release apuntó exclusivamente a `http://127.0.0.1:8080`, servido por `scripts/fixtures/artist-merch-mobile.mjs`. Expo Updates se deshabilitó solamente en el artefacto compilado e instalado para garantizar que la captura correspondiera al bundle local. Los flags versionados y los proveedores permanecieron apagados.
