# Tidal Agent (CLI)

CLI que toma un prompt en lenguaje natural y devuelve solo código TidalCycles, listo para enviar al intérprete (por ejemplo, vía `ghci` con Tidal corriendo).

## Uso rápido

```bash
export TIDAL_AGENT_API_KEY="sk-..."
# opcional: export TIDAL_AGENT_API_URL="https://api.openai.com/v1/chat/completions"
# opcional: export TIDAL_AGENT_MODEL="gpt-4o-mini"

# Prompt como argumento
tidal-agent "groove house con bombo a negras y hats rápidos"

# O vía stdin
echo "ambient drone lento con texturas" | tidal-agent
```

El CLI imprime solo el código Tidal filtrado, sin texto extra. Puedes pipearlo directo al socket de Tidal o pegarlo en el editor de tu sesión `ghci`.

## Cómo funciona

- Envia el prompt y un system prompt estricto a un endpoint compatible con OpenAI Chat (configurable con `TIDAL_AGENT_API_URL`/`TIDAL_AGENT_MODEL`).
- Post-procesa la respuesta para quedase solo con líneas que empiezan con `d1`–`d4`, `hush`, `bps`/`cps`, etc. El resto se descarta.
- Limita a pocas líneas para evitar patrones largos o peligrosos.

## Notas

- Requiere Node 18+ (usa `fetch` nativo).
- No trae dependencias adicionales.
- No ejecuta el código Tidal; solo lo imprime. Maneja tu canal de envío a `ghci`/SuperCollider según tu setup.
