# Piloto de activación de creadores de Quito — 2026-08-27

## Decisión y objetivo

Este piloto comprueba si una persona creadora puede pasar de la invitación de campaña a un perfil
profesional publicado y encontrable sin que el moderador opere TDF por ella.

El recorrido principal es:

`invitación → registro → administración del directorio → perfil con imagen → publicación → búsqueda pública`

No se probarán pagos, DDEX, WhatsApp, anuncios, publicación de eventos ni proveedores externos. El
piloto no autoriza desplegar cambios, contactar candidatos, grabar sesiones ni publicar perfiles
reales por cuenta de otra persona.

## Hipótesis y umbral de decisión

La hipótesis es que una persona artista o responsable de un espacio cultural de Quito puede publicar
y volver a encontrar su perfil en 15 minutos o menos, con máximo una indicación neutral y sin que el
moderador tome control del dispositivo.

El piloto de cinco personas se considera validado únicamente si:

- al menos 3 de 5 completan publicación y búsqueda pública en 15 minutos o menos cada una, con
  máximo una indicación neutral por persona contabilizada como éxito;
- las 5 comprenden, antes de pulsar `Publicar`, qué información quedará visible;
- ninguna necesita que el moderador escriba, navegue o publique por ella;
- no aparece un hallazgo de privacidad, seguridad o pérdida de datos de severidad Blocker/Critical;
- al menos una persona vuelve al perfil o comparte voluntariamente su enlace dentro de 48 horas.

Si no se alcanza el umbral, se corrige solo el bloqueo observado de mayor alcance y se repite con
otra cohorte pequeña. No se compensa el resultado enviando más mensajes fríos.

## Fases y compuertas

### Fase A — ensayo aislado con dos personas adultas

Antes de reclutar la cohorte de activación, ejecutar el recorrido con dos personas adultas que den
consentimiento usando un entorno aislado, cuentas desechables y datos ficticios. Esta fase sigue el
protocolo de `docs/persona-testing/human-usability-protocol.md`.

No avanzar si el entorno puede enviar mensajes reales, cobrar, publicar en producción o activar un
proveedor. Tampoco avanzar si la sesión requiere una credencial personal, una red social real o un
dato sensible.

### Fase B — activación acompañada con cinco personas

Avanzar solo cuando:

1. la Fase A no tenga hallazgos Blocker/Critical abiertos;
2. el cambio de imagen del directorio esté fusionado, su despliegue separado haya terminado
   correctamente y la imagen se haya verificado en el entorno exacto que usará la cohorte;
3. el build móvil de vista previa esté disponible si se va a probar continuidad nativa;
4. cada participante acepte crear y, en su caso, publicar su propio perfil;
5. el canal y el texto de invitación hayan sido confirmados por el operador.

La persona participante controla su cuenta, contraseña, contenido y decisión de publicación. El
moderador no observa contraseñas ni conserva correos, teléfonos, handles o enlaces privados en Git.

## Muestra

- Cinco personas adultas de Quito: artistas independientes, productores, gestores o responsables de
  espacios culturales.
- Priorizar conversaciones que ya mostraron interés textual significativo. Una reacción por sí sola
  no equivale a consentimiento ni justifica seguimiento.
- Usar códigos `QAC-01` a `QAC-05` en notas y evidencia sanitizada.
- No reclutar menores ni personas sobre las que exista una relación laboral evaluativa directa.
- Compensación, grabación y conservación de evidencia requieren aprobación específica antes de la
  primera invitación.

## Preparación de la sesión

- Duración: 20–25 minutos más una comprobación asincrónica a las 48 horas.
- Dispositivo: el propio de la persona cuando sea seguro; registrar solo categoría y plataforma.
- Conectividad: registrar `wifi`, `datos móviles` o `intermitente`, sin guardar IP.
- Abrir el enlace de campaña con los parámetros UTM aprobados y redirección actual a `/mi-artista`.
- Tener disponible `/buscar` para la verificación pública y `/mis-clasificados` como ruta actual de
  administración de perfiles del directorio.
- Para Fase A, usar datos e imagen sintéticos y una cuenta desechable por participante.
- Preparar el scorecard sin nombres, handles, emails ni Party IDs.

## Consentimiento verbal mínimo

Leer antes de empezar:

> Queremos observar si TDF se entiende sin ayuda. Evaluamos el producto, no tu habilidad. Puedes
> detenerte o retirar tu participación en cualquier momento. No compartas contraseñas, documentos,
> datos de pago ni información que no quieras hacer pública. Te avisaremos antes de cualquier acción
> de publicación. Hoy no grabaremos ni usaremos citas atribuibles salvo que lo aceptes por separado.

Registrar `sí/no` por separado para participación, grabación, cita textual y publicación. Un `no` a
grabación o cita no impide participar. Un `no` a publicación limita la sesión al entorno aislado.

## Guion del moderador

### Inicio

1. Confirmar consentimiento y dispositivo.
2. Pedir: “Imagina que recibiste esta invitación porque quieres que te encuentren para proyectos en
   Quito. Muéstrame qué harías.”
3. Iniciar el tiempo cuando la persona abra el enlace.

### Tarea principal

La persona debe, sin instrucciones de navegación:

1. registrarse o iniciar sesión;
2. encontrar dónde crear y administrar un perfil profesional público;
3. crear un perfil con nombre público, Quito, descripción breve, imagen y al menos una categoría o
   profesión;
4. revisar qué quedará público;
5. publicar el perfil;
6. abrir `/buscar`, encontrar el perfil y abrir su vista pública;
7. identificar cómo copiaría o compartiría el enlace, sin enviarlo a terceros durante la Fase A.

La transición desde `/mi-artista` hacia `/mis-clasificados` es parte de lo que se mide. No revelar la
ruta salvo que la persona se bloquee y ya se haya registrado el abandono.

### Preguntas neutrales permitidas

- “¿Qué estás buscando?”
- “¿Qué esperas que ocurra si pulsas ahí?”
- “¿Qué información crees que será pública?”
- “¿Qué harías a continuación?”

No explicar la arquitectura, señalar botones ni completar campos. Registrar cada intervención.

### Cierre

Preguntar, sin sugerir respuestas:

- “¿Qué parte fue la menos clara?”
- “¿Qué esperabas encontrar después de registrarte?”
- “¿Qué te haría volver a este perfil?”
- “¿Qué dato no publicarías aquí?”

Confirmar cómo pausar o retirar el perfil y recordar que no existe obligación de compartirlo.

## Medición

Usar `session-scorecard-template.csv`. Registrar hechos observables, no emociones inferidas:

- éxito, parcial o fallo;
- tiempo total y punto de abandono;
- indicaciones neutrales y tomas de control;
- retrocesos, errores visibles y recuperación;
- comprensión de la publicación;
- publicación y hallazgo en búsqueda;
- regreso o compartición voluntaria dentro de 48 horas.

Las citas requieren consentimiento y deben quedar redactadas sin identidad. Capturas, trazas y
grabaciones no se guardan en Git.

## Códigos iniciales de bloqueo

- `ACQ-REDIRECT`: la invitación no lleva a una siguiente acción comprensible.
- `AUTH-ACCOUNT`: registro, inicio de sesión o recuperación impiden continuar.
- `ROLE-CONTEXT`: la persona no entiende qué rol o perfil está administrando.
- `DIR-DISCOVERY`: no encuentra la administración del directorio.
- `DIR-FORM`: no puede completar o guardar el perfil.
- `DIR-MEDIA`: la imagen falta, falla o no aparece donde se espera.
- `DIR-PUBLISH`: no comprende o no puede ejecutar la publicación.
- `DIR-SEARCH`: el perfil publicado no aparece o no se reconoce en búsqueda.
- `TRUST-PRIVACY`: la visibilidad o el uso de datos no quedan claros.
- `NATIVE-HANDOFF`: la transición web/móvil pierde intención o sesión.

Crear un código adicional solo si ninguno describe el hecho observado.

## Regla de priorización posterior

Ordenar hallazgos por número de personas afectadas, imposibilidad de completar, riesgo de privacidad
y cercanía a la activación. Corregir primero una sola causa raíz. Volver a ejecutar el mismo recorrido
antes de ampliar funciones, endpoints o volumen de campaña.

## Borrador de invitación — no enviar sin confirmación

> Hola. Estamos probando si una persona creadora de Quito puede crear y publicar su perfil en TDF
> sin ayuda. La sesión dura unos 20 minutos y evalúa el producto, no a ti. No necesitas compartir
> contraseñas, documentos ni datos de pago, y tú decides si publicas o no. ¿Te interesaría participar
> en una sesión acompañada? Si dices que sí, coordinamos horario y te explicamos el consentimiento
> antes de empezar.

No añadir urgencia, promesa de ingresos, seguimiento automático ni inferencia basada únicamente en
una reacción. Registrar la invitación y cualquier respuesta solo en los sistemas privados autorizados.
