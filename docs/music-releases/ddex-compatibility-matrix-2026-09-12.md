# Matriz DDEX — selección 2026-09-12, revisión 2026-09-16

Revisión 2026-09-16: se mantiene la combinación de versiones. El adaptador
`tdf-ern432-audio-v5` conserva créditos del snapshot aprobado v2 y la corrección
del deal a gratuidad bajo demanda con fecha de fin. El ZIP es un formato
interno de revisión, **no** un paquete conforme a la coreografía Cloud Storage.
Implementa nombres vinculados al identificador/referencias técnicas según §5.3;
no la estructura de servidor ni los acuses. Ver [fuentes, pruebas y límites actuales](ddex-file-naming.md).
La revisión v5 añade identidad estable de TrackRelease y requisitos compartidos
por operación; ver [continuidad, migración y límites](ddex-operation-lifecycle.md).

La combinación seleccionada es indivisible en runtime:

| Componente | Selección | Fuente oficial |
|---|---|---|
| Mensaje | ERN XML 4.3.2 `NewReleaseMessage` | https://kb.ddex.net/reference-material/standards-specifications/ |
| Release Profile | Audio 2.3.1 para ERN 4.3.1+ | https://ern-rp.ddex.net/ |
| Business Profile | ninguno | https://kb.ddex.net/implementing-each-standard/electronic-release-notification-message-suite-(ern)/ern-4-explained/ern-4-profiles/ |
| AVS | 011 / diccionario AVS versión 11 | https://service.ddex.net/dd/DD-AVS-CURRENT/ |
| Diccionario estructural | DD-ERN-432 | https://service.ddex.net/dd/DD-ERN-432/ |
| XSD | archivo oficial ERN 4.3.2 | https://service.ddex.net/doc/Standards/ERN432/ERN-3305%20-%20ERN%20Part%201%20Definition%20of%20messages%20v4.3.2%20XSD.zip |
| Coreografía objetivo, implementación parcial | Cloud-based Storage 1.8.1; nombres §5.3 implementados en el subconjunto TDF, servidor/entrega pendientes | https://ernccloud.ddex.net/ |
| Alternativa futura | Web Services 1.8 | https://erncws.ddex.net/ |

SHA-256 fijado del ZIP XSD consultado: `bbd5012204ea3dbf08025e58768570650d9f65775b0022f5a93770c0dd411938`. `fetch-ddex-ern432-schema.sh` exige aceptación explícita de la licencia, TLS y ese hash. Si DDEX cambia el archivo, el build falla y obliga a revisar compatibilidad; no lo acepta silenciosamente.

## Reglas implementadas

- Audio cubre single, EP y álbum con SoundRecordings y portada.
- DPID de emisor/receptor debe ser real, sintácticamente válido y asociado a autoridad/evidencia; no hay placeholders.
- Cada grabación necesita ISRC proporcionado y el release UPC/EAN/GRid; la ausencia bloquea con ruta de campo.
- XML, recursos, manifiesto y ZIP se hashean y conservan como assets inmutables de la versión.
- XML se valida localmente con `xmllint --nonet` contra XSD oficial. No se envían datos a validadores externos.
- En ERN 4 no se emite `UpdateIndicator`: una actualización vuelve a enviar una declaración completa y todos los deals activos.
- La retirada usa un `NewReleaseMessage` posterior sin `DealList`; update/takedown exige una exportación inicial validada v5 del mismo producto, remitente y destinatario. No demuestra entrega o aceptación del receptor. Un retiro programado futuro bloquea este mensaje inmediato.
- El adaptador v3 acepta exactamente una regla de escucha completa gratuita a nivel release, territorios por inclusión y sin compra/descarga: `FreeOfChargeModel` + `OnDemandStream`. Incluye fin de vigencia cuando existe e inicia después de disponibilidad/publicación/embargo. No representa derechos de suscripción, radio ni acuerdos comerciales con un DSP. Las configuraciones no representables se bloquean.

Referencias de ciclo de vida:

- https://kb.ddex.net/implementing-each-standard/best-practices-for-all-ddex-standards/guidance-on-message-exchange-protocols-and-choreographies/update-indicator/
- https://kb.ddex.net/implementing-each-standard/best-practices-for-all-ddex-standards/deals-and-commercial-aspects/active-deals-at-time-of-sending-an-ern/
- https://kb.ddex.net/implementing-each-standard/best-practices-for-all-ddex-standards/deals-and-commercial-aspects/takedowns/
- https://kb.ddex.net/implementing-each-standard/best-practices-for-all-ddex-standards/deals-and-commercial-aspects/no-takedown-in-initial-deal/

## Límite contractual

La validez XSD no equivale a aceptación por un DSP. Perfiles particulares, carpetas/nombres negociados, acknowledgements y entrega directa quedan fuera. Antes de habilitar `music_releases.ddex_export` en producción se deben registrar DPID reales, revisar licencia, fijar XSD en el worker y ejecutar fixtures contra el contrato del destinatario.
