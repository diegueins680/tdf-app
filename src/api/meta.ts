export type VersionInfo = {
  name: string;
  version: string;
  commit?: string;
  buildTime?: string;
};

export async function fetchVersion(apiBase: string): Promise<VersionInfo | null> {
  try {
    const res = await fetch(`${apiBase}/version`, {
      headers: { accept: 'application/json' },
    });
    if (!res.ok) return null;
    return (await res.json()) as VersionInfo;
  } catch {
    return null;
  }
}

