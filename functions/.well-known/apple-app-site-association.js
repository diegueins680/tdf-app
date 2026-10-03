export function onRequestGet({ env }) {
  const teamId = String(env.APPLE_TEAM_ID ?? '').trim();
  if (!/^[A-Z0-9]{10}$/.test(teamId)) {
    return Response.json({ error: 'APPLE_TEAM_ID is not configured' }, { status: 503 });
  }
  return Response.json({
    applinks: {
      apps: [],
      details: [{
        appID: `${teamId}.com.tdfrecords.app`,
        components: [
          { '/': '/eventos/*', comment: 'Public event links; the app allowlists attribution parameters.' },
          { '/': '/conversacion/*', comment: 'Stable discussion targets; access is checked by the API.' },
        ],
      }],
    },
  }, {
    headers: { 'Cache-Control': 'public, max-age=3600', 'Content-Type': 'application/json' },
  });
}
