# Privacidad y recuperación de activación

PARTIAL: corrección y recorridos verificados en aislamiento. No integración ni despliegue de este candidato.

La contraprueba crea una cuenta ficticia, activa artista sin ningún PUT de perfil y observa el marcador del correo en perfil público y búsqueda anónima. La versión corregida exige Cuenta TDF y cero coincidencias del marcador. Nunca utiliza personas ni datos reales. Los primeros dos intentos de contraprueba fallaron por un selector de fixture ubicado en la pantalla incorrecta; la contraprueba válida está en baseline-results.json.

Los resultados positivos ejecutan web79027c996 y API nativa48eb295, Stack/GHC9.10.3, PG16/223migraciones protegidas4b81. Los seis casos de follow y el recorrido de artista+2fans ejecutan la misma fuente. El binario conserva su /version real. Fixtures de Drive, imagen y errores503 están declarados; Google/Apple reales, móviles físicos, clipboard real, humanos y producción no probados. Todas las relaciones sintéticas quedan excluidas de VAR.

run.py tiene modo plan por defecto; rehearsal exige source/expected-source/binario exactos, árbol backend íntegro, hash del ejecutable y puertos libres. Requiere macOS con sandbox-exec, Node22.14.0, PG16 y dependencias locales. No ofrece fallback sin aislamiento. La API se limita a loopback y no puede salir a proveedores. Ejecutar secuencialmente con los otros paquetes, nunca en producción.

Se conserva storage-failure-original.log: ENOSPC antes del navegador. Su PostgreSQL propio se detuvo manualmente; el mismo código pasó tras recuperar espacio. No convertir ese fallo de infraestructura en un fallo de producto ni ocultarlo con los pases posteriores.

AUTH-ENTRY-002 se modifica intencionalmente: el viejo contrato exigía derivar el nombre del correo. Conserva el alta de dos campos y ahora impide esa exposición. Las cuentas históricas y nombres explícitos no se renombran.
