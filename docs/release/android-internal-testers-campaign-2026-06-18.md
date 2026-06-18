# Android Internal Testers Campaign - 2026-06-18

Campaign record: `1` (`TDF App Android Internal Testers - Jun 2026`)
Status: `draft`

## Audience

- Existing parties with email in the local system: 10
- Existing parties with WhatsApp in the local system: 5
- Active registered users with email: 10
- Active registered users with WhatsApp or phone: 3

Do not send to the full audience until Diego approves the test messages.

## Link

Google Play internal testing opt-in URL:

```text
https://play.google.com/apps/testing/com.tdf.records
```

Confirm this against the Play Console tester opt-in link before the full send.

## Email

Subject:

```text
Ayúdanos a probar la app de TDF Records
```

Body lines:

```text
Estamos abriendo la prueba interna de la app Android de TDF Records y nos gustaría que nos ayudes probándola.

Si tienes un teléfono Android, abre este enlace con el mismo correo de Google Play donde recibiste este mensaje:

https://play.google.com/apps/testing/com.tdf.records

Toca "Become a tester" / "Convertirse en tester" y luego instala o actualiza TDF Records desde Google Play.

Cuando la pruebes, respóndenos con cualquier error, captura de pantalla o comentario. También ayuda mucho si nos dices tu modelo de teléfono.

Gracias por apoyar a TDF Records.
```

## WhatsApp

```text
Hola {{name}}, estamos abriendo la prueba interna de la app Android de TDF Records y nos gustaría que nos ayudes probándola.

Si tienes Android, abre este enlace con tu cuenta de Google Play:
https://play.google.com/apps/testing/com.tdf.records

Toca "Become a tester" / "Convertirse en tester" y luego instala o actualiza TDF Records desde Google Play.

Cuando la pruebes, respóndenos con cualquier error, captura o comentario. También ayuda mucho si nos dices tu modelo de teléfono.

Gracias por apoyar a TDF Records.
```

## Approval Gate

1. Send the email copy only to Diego's stored email first.
2. Send the WhatsApp copy only to Diego after a test WhatsApp number is provided or added to Diego's party record.
3. After Diego approves, run the full email broadcast against active registered users.
4. For WhatsApp, send only to records with an existing WhatsApp or primary phone value and keep the message source tied to this draft campaign in the operator notes/logs.
