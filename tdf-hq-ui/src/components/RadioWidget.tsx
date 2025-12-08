import { useEffect, useMemo, useRef, useState, useCallback } from 'react';
import { useQuery } from '@tanstack/react-query';
import {
  Box,
  Card,
  CardContent,
  Chip,
  Collapse,
  Divider,
  IconButton,
  LinearProgress,
  Stack,
  TextField,
  Typography,
  Tooltip,
  Button,
  Switch,
  FormControlLabel,
} from '@mui/material';
import PlayArrowIcon from '@mui/icons-material/PlayArrow';
import PauseIcon from '@mui/icons-material/Pause';
import ExpandLessIcon from '@mui/icons-material/ExpandLess';
import ExpandMoreIcon from '@mui/icons-material/ExpandMore';
import RadioIcon from '@mui/icons-material/Radio';
import GraphicEqIcon from '@mui/icons-material/GraphicEq';
import VolumeOffIcon from '@mui/icons-material/VolumeOff';
import ContentCopyIcon from '@mui/icons-material/ContentCopy';
import { generateTidalCode } from '../utils/tidalAgent';
import WarningAmberIcon from '@mui/icons-material/WarningAmber';
import DragIndicatorIcon from '@mui/icons-material/DragIndicator';
import { RadioAPI, type RadioStreamDTO } from '../api/radio';
import RestartAltIcon from '@mui/icons-material/RestartAlt';
import StarIcon from '@mui/icons-material/Star';
import StarBorderIcon from '@mui/icons-material/StarBorder';
import VisibilityOffIcon from '@mui/icons-material/VisibilityOff';
import CloudDownloadIcon from '@mui/icons-material/CloudDownload';
import FiberManualRecordIcon from '@mui/icons-material/FiberManualRecord';
import SkipNextIcon from '@mui/icons-material/SkipNext';
import BoltIcon from '@mui/icons-material/Bolt';
import ShareIcon from '@mui/icons-material/Share';
import PeopleAltIcon from '@mui/icons-material/PeopleAlt';
import PushPinIcon from '@mui/icons-material/PushPin';
import OpenInFullIcon from '@mui/icons-material/OpenInFull';
import { useNavigate } from 'react-router-dom';

interface Prompt {
  text: string;
  author?: string;
  createdAt?: string;
  code?: string;
}

interface LoadStreamDetail {
  streamUrl: string;
  stationName?: string;
  stationId?: string;
}

interface Station {
  id: string;
  name: string;
  streamUrl: string;
  region?: string;
  stationId?: string;
  country?: string;
  genre?: string;
  mood: string;
  prompts: Prompt[];
  source?: 'curated' | 'custom' | 'db' | 'shared';
}

const CURATED_STATIONS: Station[] = [
  {
    id: 'cosmic-cycles',
    name: 'Cosmic Cycles',
    mood: 'Downtempo / Ambient',
    streamUrl: 'https://icecast.radiofrance.fr/fip-midfi.mp3', // reliable FIP stream
    country: 'FR',
    genre: 'Downtempo / Ambient',
    source: 'curated',
    prompts: [
      { text: 'Paisajes sonoros nocturnos con sintes lentos', author: 'Agente', createdAt: '2025-12-01' },
      { text: 'Texturas granulares inspiradas en lluvia en Quito', createdAt: '2025-12-02' },
    ],
  },
  {
    id: 'kexp',
    name: 'KEXP Seattle',
    region: 'US / Alt',
    mood: 'Indie / Live Sessions',
    streamUrl: 'https://kexp-mp3-128.streamguys1.com/kexp128.mp3',
    country: 'US',
    genre: 'Indie / Live Sessions',
    source: 'curated',
    prompts: [],
  },
  {
    id: 'wnyc',
    name: 'WNYC FM',
    region: 'US / News',
    mood: 'News / Talk',
    streamUrl: 'https://fm939.wnyc.org/wnycfm-web',
    country: 'US',
    genre: 'News / Talk',
    source: 'curated',
    prompts: [],
  },
];

function PromptList({ prompts }: { prompts: Prompt[] }) {
  if (prompts.length === 0) {
    return (
      <Typography variant="body2" color="text.secondary">
        No hay prompts aún.
      </Typography>
    );
  }
  return (
    <Stack spacing={1}>
      {prompts.map((p, idx) => (
        <Card key={`${p.text}-${idx}`} variant="outlined">
          <CardContent sx={{ py: 1.25, display: 'flex', flexDirection: 'column', gap: 0.75 }}>
            <Typography variant="body2" fontWeight={600}>
              {p.text}
            </Typography>
            <Typography variant="caption" color="text.secondary">
              {p.author ?? 'Agente Tidal'} {p.createdAt ? `· ${p.createdAt}` : ''}
            </Typography>
            {p.code && (
              <Box
                sx={{
                  bgcolor: 'rgba(148,163,184,0.08)',
                  border: '1px dashed',
                  borderColor: 'divider',
                  borderRadius: 1.5,
                  p: 1,
                  fontFamily: 'monospace',
                  fontSize: 12,
                  whiteSpace: 'pre-line',
                  position: 'relative',
                }}
              >
                {p.code}
                <Box sx={{ position: 'absolute', top: 4, right: 4 }}>
                  <Tooltip title="Copiar instrucciones Tidal">
                    <IconButton
                      size="small"
                      onClick={() => {
                        if (navigator?.clipboard?.writeText) {
                          navigator.clipboard
                            .writeText(p.code ?? '')
                            .catch((err) => console.warn('No se pudo copiar las instrucciones Tidal', err));
                        }
                      }}
                    >
                      <ContentCopyIcon fontSize="small" />
                    </IconButton>
                  </Tooltip>
                </Box>
              </Box>
            )}
          </CardContent>
        </Card>
      ))}
    </Stack>
  );
}

export default function RadioWidget() {
  const navigate = useNavigate();
  const containerRef = useRef<HTMLDivElement | null>(null);
  const [position, setPosition] = useState<{ x: number; y: number }>({ x: 16, y: 16 });
  const [dragging, setDragging] = useState(false);
  const dragState = useRef<{ offsetX: number; offsetY: number; active: boolean }>({
    offsetX: 0,
    offsetY: 0,
    active: false,
  });
  const dragMovedRef = useRef(false);
  const miniContainerRef = useRef<HTMLDivElement | null>(null);
  const [miniPosition, setMiniPosition] = useState<{ x: number; y: number }>(() => {
    if (typeof window === 'undefined') return { x: 12, y: 12 };
    const fromStorage = window.localStorage.getItem('radio-mini-position');
    if (fromStorage) {
      try {
        const parsed = JSON.parse(fromStorage) as { x?: number; y?: number };
        if (typeof parsed.x === 'number' && typeof parsed.y === 'number') {
          return { x: parsed.x, y: parsed.y };
        }
      } catch {
        // ignore parsing errors
      }
    }
    const initialY = Math.max(12, window.innerHeight - 96);
    return { x: 12, y: initialY };
  });
  const [miniDragging, setMiniDragging] = useState(false);
  const miniDragState = useRef<{ offsetX: number; offsetY: number; active: boolean }>({
    offsetX: 0,
    offsetY: 0,
    active: false,
  });
  const miniDragMovedRef = useRef(false);

  const [customStations, setCustomStations] = useState<Station[]>([]);
  const [newStationCountry, setNewStationCountry] = useState('');
  const [newStationGenre, setNewStationGenre] = useState('');
  const [searchCountry, setSearchCountry] = useState('');
  const [searchGenre, setSearchGenre] = useState('');
  const [favoriteKeys, setFavoriteKeys] = useState<string[]>([]);
  const [hiddenKeys, setHiddenKeys] = useState<string[]>([]);
  const [showFavoritesOnly, setShowFavoritesOnly] = useState(false);
  const [showHidden, setShowHidden] = useState(false);
  const [apiError, setApiError] = useState<string | null>(null);
  const [importMessage, setImportMessage] = useState<string | null>(null);
  const [importing, setImporting] = useState(false);
  const [editName, setEditName] = useState('');
  const [editCountry, setEditCountry] = useState('');
  const [editGenre, setEditGenre] = useState('');
  const [lastUpdatedTs, setLastUpdatedTs] = useState<number | null>(null);
  const [autoSkipOnError, setAutoSkipOnError] = useState(false);
  // Start visible by default; only hide when the user explicitly minimizes during the session.
  const [miniBarVisible, setMiniBarVisible] = useState(false);
  const [pinned, setPinned] = useState(() => {
    if (typeof window === 'undefined') return false;
    return window.localStorage.getItem('radio-pinned') === '1';
  });
  const [showCatalogSection, setShowCatalogSection] = useState(true);
  const [showAddSection, setShowAddSection] = useState(false);
  const [shareNotice, setShareNotice] = useState<string | null>(null);
  const controlFadeSx = {
    opacity: 0.65,
    transition: 'opacity 0.2s ease',
    '&:hover': { opacity: 1 },
  } as const;
  const defaultStation = useMemo<Station>(
    () =>
      CURATED_STATIONS[0] ?? {
        id: 'fallback',
        name: 'Radio',
        mood: 'Live',
        streamUrl: '',
        prompts: [],
      },
    [],
  );
  const audioRef = useRef<HTMLAudioElement | null>(null);
  const [expanded, setExpanded] = useState(() => {
    if (typeof window === 'undefined') return false;
    return window.localStorage.getItem('radio-expanded') === '1';
  });
  const [activeId, setActiveId] = useState<string>(defaultStation.id);
  const [isPlaying, setIsPlaying] = useState(false);
  const [muted, setMuted] = useState(false);
  const [playbackWarning, setPlaybackWarning] = useState<string | null>(null);
  const [promptDraft, setPromptDraft] = useState('');
  const promptInputRef = useRef<HTMLInputElement | null>(null);
  const [promptState, setPromptState] = useState<Record<string, Prompt[]>>(() =>
    Object.fromEntries(CURATED_STATIONS.map((s) => [s.id, [...s.prompts]])),
  );
  const keyFor = useCallback((station: Station) => station.streamUrl.toLowerCase(), []);
  const countryQuery = searchCountry.trim();
  const genreQuery = searchGenre.trim();
  useEffect(() => {
    if (typeof window === 'undefined') return;
    try {
      window.localStorage.setItem('radio-expanded', expanded ? '1' : '0');
    } catch {
      // ignore
    }
  }, [expanded]);
  useEffect(() => {
    if (typeof window === 'undefined') return;
    try {
      window.localStorage.setItem('radio-pinned', pinned ? '1' : '0');
    } catch {
      // ignore
    }
  }, [pinned]);

  const radioSearch = useQuery<RadioStreamDTO[], Error>({
    queryKey: ['radio-streams', countryQuery.toLowerCase(), genreQuery.toLowerCase()],
    queryFn: () => RadioAPI.search({ country: countryQuery, genre: genreQuery }),
    staleTime: 60_000,
    gcTime: 5 * 60_000,
    retry: false,
  });
  const refetchStreams = radioSearch.refetch;
  const streamsFetching = radioSearch.isFetching;
  const streamsError = radioSearch.error;
  useEffect(() => {
    if (streamsError) {
      const msg = streamsError instanceof Error ? streamsError.message : 'No se pudo cargar el catálogo de radios.';
      setApiError(msg);
    } else {
      setApiError(null);
    }
  }, [streamsError]);

  useEffect(() => {
    if (radioSearch.data) {
      setLastUpdatedTs(Date.now());
    }
  }, [radioSearch.data]);

  // Hydrate favorites/hidden preferences
  useEffect(() => {
    if (typeof window === 'undefined') return;
    try {
      const favRaw = window.localStorage.getItem('radio-favorites');
      if (favRaw) {
        const parsed = JSON.parse(favRaw);
        if (Array.isArray(parsed)) {
          setFavoriteKeys(parsed.filter((item): item is string => typeof item === 'string'));
        }
      }
      const hidRaw = window.localStorage.getItem('radio-hidden');
      if (hidRaw) {
        const parsed = JSON.parse(hidRaw);
        if (Array.isArray(parsed)) {
          setHiddenKeys(parsed.filter((item): item is string => typeof item === 'string'));
        }
      }
      const favFilter = window.localStorage.getItem('radio-show-favorites');
      if (favFilter) setShowFavoritesOnly(favFilter === '1');
    } catch {
      // ignore
    }
  }, []);

  useEffect(() => {
    if (typeof window === 'undefined') return;
    try {
      window.localStorage.setItem('radio-favorites', JSON.stringify(favoriteKeys));
      window.localStorage.setItem('radio-hidden', JSON.stringify(hiddenKeys));
      window.localStorage.setItem('radio-show-favorites', showFavoritesOnly ? '1' : '0');
    } catch {
      // ignore
    }
  }, [favoriteKeys, hiddenKeys, showFavoritesOnly]);
  const favoriteSet = useMemo(() => new Set(favoriteKeys), [favoriteKeys]);
  const hiddenSet = useMemo(() => new Set(hiddenKeys), [hiddenKeys]);

  const dbStations = useMemo<Station[]>(() => {
    if (!radioSearch.data) return [];
    return radioSearch.data.map((dto) => ({
      id: `db-${dto.rsId}`,
      name: dto.rsName ?? 'Radio',
      streamUrl: dto.rsStreamUrl,
      country: dto.rsCountry ?? undefined,
      region: dto.rsCountry ?? undefined,
      genre: dto.rsGenre ?? undefined,
      mood: dto.rsGenre ?? 'Live',
      prompts: [],
      source: 'db',
    }));
  }, [radioSearch.data]);
  const allStations = useMemo<Station[]>(() => {
    const merged = [...CURATED_STATIONS, ...dbStations, ...customStations];
    const map = new Map<string, Station>();
    merged.forEach((station) => {
      const key = station.streamUrl.toLowerCase();
      if (!map.has(key)) {
        map.set(key, station);
      } else {
        const current = map.get(key)!;
        map.set(key, { ...current, ...station, prompts: current.prompts?.length ? current.prompts : station.prompts });
      }
    });
    return Array.from(map.values());
  }, [customStations, dbStations]);
  const availableStations = allStations;
  const visibleStations = useMemo(
    () =>
      allStations.filter((s) => {
        const key = keyFor(s);
        if (!showHidden && hiddenSet.has(key)) return false;
        if (showFavoritesOnly && !favoriteSet.has(key)) return false;
        return true;
      }),
    [allStations, favoriteSet, hiddenSet, showHidden, showFavoritesOnly, keyFor],
  );
  const sortedVisibleStations = useMemo(() => {
    const arr = [...visibleStations];
    arr.sort((a, b) => {
      const aFav = favoriteSet.has(keyFor(a));
      const bFav = favoriteSet.has(keyFor(b));
      if (aFav !== bFav) return aFav ? -1 : 1;
      return a.name.localeCompare(b.name);
    });
    return arr;
  }, [favoriteSet, keyFor, visibleStations]);
  const hiddenStations = useMemo(
    () => allStations.filter((s) => hiddenSet.has(keyFor(s))),
    [allStations, hiddenSet, keyFor],
  );
  const activeStation = useMemo<Station>(
    () => allStations.find((s) => s.id === activeId) ?? defaultStation,
    [activeId, allStations, defaultStation],
  );
  const countryOptions = useMemo(() => {
    const opts = new Set<string>();
    allStations.forEach((s) => {
      if (s.country) opts.add(s.country);
      if (s.region) opts.add(s.region);
    });
    const cleaned = Array.from(opts).filter((val) => {
      const trimmed = val.trim();
      if (!trimmed) return false;
      if (trimmed.length > 24) return false;
      const lower = trimmed.toLowerCase();
      if (lower.includes('pull request')) return false;
      if (trimmed.startsWith('http')) return false;
      if (/[0-9#]/.test(trimmed)) return false;
      return /^[\p{L}\s\/\-\.'&,]+$/u.test(trimmed);
    });
    return cleaned.slice(0, 16);
  }, [allStations]);
  const genreOptions = useMemo(() => {
    const opts = new Set<string>();
    allStations.forEach((s) => {
      if (s.genre) opts.add(s.genre);
      else if (s.mood) opts.add(s.mood);
    });
    return Array.from(opts).slice(0, 16);
  }, [allStations]);
  const stationPrompts = promptState[activeStation.id] ?? activeStation.prompts;
  useEffect(() => {
    setEditName(activeStation.name);
    setEditCountry(activeStation.country ?? '');
    setEditGenre(activeStation.genre ?? activeStation.mood ?? '');
  }, [activeStation]);
  const toggleFavorite = useCallback(
    (station: Station) => {
      const key = keyFor(station);
      setFavoriteKeys((prev) => (prev.includes(key) ? prev.filter((k) => k !== key) : [...prev, key]));
    },
    [keyFor],
  );
  const hideStation = useCallback(
    (station: Station) => {
      const key = keyFor(station);
      setHiddenKeys((prev) => (prev.includes(key) ? prev : [...prev, key]));
      if (key === keyFor(activeStation)) {
        setActiveId(defaultStation.id);
      }
    },
    [activeStation, defaultStation.id, keyFor],
  );
  const unhideStation = useCallback(
    (station: Station) => {
      const key = keyFor(station);
      setHiddenKeys((prev) => prev.filter((k) => k !== key));
    },
    [keyFor],
  );
  const jumpToNextStation = useCallback(() => {
    if (sortedVisibleStations.length === 0) return;
    const idx = sortedVisibleStations.findIndex((s) => s.id === activeStation.id);
    const safeIdx = idx === -1 ? 0 : idx;
    const next = sortedVisibleStations[(safeIdx + 1) % sortedVisibleStations.length];
    if (!next) return;
    setActiveId(next.id);
    setPlaybackWarning(null);
    setMuted(false);
    setIsPlaying(true);
  }, [activeStation.id, sortedVisibleStations]);
  const isFavoriteActive = useMemo(() => favoriteSet.has(keyFor(activeStation)), [favoriteSet, activeStation, keyFor]);

  const clampPosition = useCallback(
    (x: number, y: number) => {
      if (typeof window === 'undefined') return { x, y };
      const rect = containerRef.current?.getBoundingClientRect();
      const margin = 12;
      const width = rect?.width ?? 300;
      const height = rect?.height ?? 260;
      const maxX = Math.max(margin, window.innerWidth - width - margin);
      const maxY = Math.max(margin, window.innerHeight - height - margin);
      return {
        x: Math.min(Math.max(margin, x), maxX),
        y: Math.min(Math.max(margin, y), maxY),
      };
    },
    [],
  );
  const clampMiniPosition = useCallback(
    (x: number, y: number) => {
      if (typeof window === 'undefined') return { x, y };
      const rect = miniContainerRef.current?.getBoundingClientRect();
      const margin = 12;
      const width = rect?.width ?? 240;
      const height = rect?.height ?? 72;
      const maxX = Math.max(margin, window.innerWidth - width - margin);
      const maxY = Math.max(margin, window.innerHeight - height - margin);
      return {
        x: Math.min(Math.max(margin, x), maxX),
        y: Math.min(Math.max(margin, y), maxY),
      };
    },
    [],
  );

  // Hydrate initial position
  useEffect(() => {
    if (typeof window === 'undefined') return;
    const fromStorage = window.localStorage.getItem('radio-position');
    if (fromStorage) {
      try {
        const parsed = JSON.parse(fromStorage) as { x: number; y: number };
        setPosition(clampPosition(parsed.x, parsed.y));
        return;
      } catch {
        // ignore parsing errors
      }
    }
    const width = window.innerWidth;
    const height = window.innerHeight;
    const initial = clampPosition(width - 320, height - 320);
    setPosition(initial);
  }, [clampPosition]);
  const resetPosition = useCallback(() => {
    if (typeof window === 'undefined') return;
    const width = window.innerWidth;
    const height = window.innerHeight;
    const initial = clampPosition(width - 320, height - 320);
    setPosition(initial);
    try {
      window.localStorage.setItem('radio-position', JSON.stringify(initial));
    } catch {
      // ignore
    }
  }, [clampPosition]);

  // Keep the widget within bounds on resize
  useEffect(() => {
    if (typeof window === 'undefined') return;
    const onResize = () => {
      setPosition((p) => clampPosition(p.x, p.y));
      setMiniPosition((p) => clampMiniPosition(p.x, p.y));
    };
    window.addEventListener('resize', onResize);
    return () => window.removeEventListener('resize', onResize);
  }, [clampMiniPosition, clampPosition]);

  useEffect(() => {
    setMiniPosition((p) => clampMiniPosition(p.x, p.y));
  }, [clampMiniPosition]);

  // Drag handlers
  const isInteractiveTarget = useCallback((target?: HTMLElement | null) => {
    const tag = target?.tagName?.toLowerCase();
    return (
      tag === 'button' ||
      tag === 'a' ||
      tag === 'input' ||
      tag === 'textarea' ||
      tag === 'select' ||
      tag === 'option' ||
      target?.closest('[data-no-drag]')
    );
  }, []);
  const onPointerDown = useCallback(
    (e: React.PointerEvent) => {
      const target = e.target as HTMLElement | null;
      const dragHandle = target?.closest('[data-drag-handle]');
      const isInteractive = isInteractiveTarget(target);
      // Only start drag on the handle or non-interactive areas.
      if (!dragHandle && isInteractive) {
        return;
      }
      e.preventDefault();
      dragMovedRef.current = false;
      dragState.current = { offsetX: e.clientX - position.x, offsetY: e.clientY - position.y, active: true };
      setDragging(true);
      target?.setPointerCapture?.(e.pointerId);
    },
    [isInteractiveTarget, position.x, position.y],
  );
  const onMiniPointerDown = useCallback(
    (e: React.PointerEvent) => {
      const target = e.target as HTMLElement | null;
      if (isInteractiveTarget(target)) return;
      e.preventDefault();
      miniDragMovedRef.current = false;
      miniDragState.current = { offsetX: e.clientX - miniPosition.x, offsetY: e.clientY - miniPosition.y, active: true };
      setMiniDragging(true);
      target?.setPointerCapture?.(e.pointerId);
    },
    [isInteractiveTarget, miniPosition.x, miniPosition.y],
  );

  useEffect(() => {
    if (typeof window === 'undefined') return;
    if (!dragging || !dragState.current.active) return;
    const move = (e: PointerEvent) => {
      if (!dragState.current.active) return;
      dragMovedRef.current = true;
      const next = clampPosition(e.clientX - dragState.current.offsetX, e.clientY - dragState.current.offsetY);
      setPosition(next);
      try {
        window.localStorage.setItem('radio-position', JSON.stringify(next));
      } catch {
        // ignore storage errors
      }
    };
    const up = () => {
      dragState.current.active = false;
      setDragging(false);
    };
    window.addEventListener('pointermove', move);
    window.addEventListener('pointerup', up);
    return () => {
      window.removeEventListener('pointermove', move);
      window.removeEventListener('pointerup', up);
    };
  }, [clampPosition, dragging]);
  useEffect(() => {
    if (typeof window === 'undefined') return;
    if (!miniDragging || !miniDragState.current.active) return;
    const move = (e: PointerEvent) => {
      if (!miniDragState.current.active) return;
      miniDragMovedRef.current = true;
      const next = clampMiniPosition(e.clientX - miniDragState.current.offsetX, e.clientY - miniDragState.current.offsetY);
      setMiniPosition(next);
      try {
        window.localStorage.setItem('radio-mini-position', JSON.stringify(next));
      } catch {
        // ignore storage errors
      }
    };
    const up = () => {
      miniDragState.current.active = false;
      setMiniDragging(false);
    };
    window.addEventListener('pointermove', move);
    window.addEventListener('pointerup', up);
    return () => {
      window.removeEventListener('pointermove', move);
      window.removeEventListener('pointerup', up);
    };
  }, [clampMiniPosition, miniDragging]);

  // Hydrate prompts and radio settings from localStorage to keep user submissions/settings across reloads.
  useEffect(() => {
    if (typeof window === 'undefined') return;
    try {
      const rawStations = window.localStorage.getItem('radio-stations');
      if (rawStations) {
        const parsedStations: Station[] = JSON.parse(rawStations);
        setCustomStations(
          parsedStations.map((s) => ({
            ...s,
            source: s.source ?? 'custom',
            mood: s.mood ?? s.genre ?? 'Custom',
          })),
        );
      }
      const raw = window.localStorage.getItem('radio-prompts');
      if (!raw) return;
      const saved: Record<string, Prompt[]> = JSON.parse(raw);
      setPromptState((prev) => ({ ...prev, ...saved }));
    } catch {
      // ignore parse errors
    }
    try {
      const rawSettings = window.localStorage.getItem('radio-settings');
      if (rawSettings) {
        const parsed = JSON.parse(rawSettings) as { stationId?: string; playOnLoad?: boolean; muted?: boolean };
        if (parsed.stationId) setActiveId(parsed.stationId);
        if (parsed.muted !== undefined) setMuted(parsed.muted);
        if (parsed.playOnLoad) setIsPlaying(true);
      }
    } catch {
      // ignore parse errors
    }
  }, []);

  // Persist prompts and settings to localStorage when they change.
  useEffect(() => {
    if (typeof window === 'undefined') return;
    try {
      window.localStorage.setItem('radio-prompts', JSON.stringify(promptState));
    } catch {
      // ignore quota errors
    }
  }, [promptState]);

  useEffect(() => {
    if (typeof window === 'undefined') return;
    try {
      const settings = {
        stationId: activeStation.id,
        playOnLoad: isPlaying,
        muted,
      };
      window.localStorage.setItem('radio-settings', JSON.stringify(settings));
      window.localStorage.setItem('radio-stations', JSON.stringify(customStations));
    } catch {
      // ignore
    }
  }, [activeStation.id, isPlaying, muted, customStations]);

  useEffect(() => {
    const audio = audioRef.current;
    if (!audio) return;
    if (!activeStation.streamUrl) {
      setIsPlaying(false);
      setPlaybackWarning('La emisora no tiene un stream disponible. Elige otra estación.');
      audio.removeAttribute('src');
      audio.load();
      return;
    }
    audio.src = activeStation.streamUrl;
    audio.muted = muted;
    if (isPlaying) {
      void audio.play().catch(() => setIsPlaying(false));
    }
  }, [activeStation.streamUrl, isPlaying, muted]);

  useEffect(() => {
    const audio = audioRef.current;
    if (!audio) return;
    let clearTimer: number | undefined;
    const handleError = () => {
      setIsPlaying(false);
      const fallbackToDefault = activeStation.id !== defaultStation.id;
      setPlaybackWarning(
        fallbackToDefault
          ? `No pudimos reproducir ${activeStation.name || 'esta emisora'}. Cambiamos a ${defaultStation.name}.`
          : 'No pudimos reproducir esta emisora. Revisa el stream o prueba con otra.',
      );
      if (fallbackToDefault) {
        setActiveId(defaultStation.id);
        setTimeout(() => setIsPlaying(true), 160);
      }
    };
    const handleCanPlay = () => {
      if (clearTimer) {
        window.clearTimeout(clearTimer);
      }
      if (!playbackWarning) return;
      clearTimer = window.setTimeout(() => setPlaybackWarning(null), 2400);
    };
    audio.addEventListener('error', handleError);
    audio.addEventListener('canplay', handleCanPlay);
    return () => {
      if (clearTimer) {
        window.clearTimeout(clearTimer);
      }
      audio.removeEventListener('error', handleError);
      audio.removeEventListener('canplay', handleCanPlay);
    };
  }, [activeStation.id, activeStation.name, defaultStation.id, defaultStation.name, playbackWarning]);

  // Publish presence so other perfiles can see current stream.
  useEffect(() => {
    if (!isPlaying || !activeStation.streamUrl) {
      void RadioAPI.clearPresence().catch(() => undefined);
      return undefined;
    }
    const timer = setTimeout(() => {
      void RadioAPI.setPresence({
        rpuStreamUrl: activeStation.streamUrl,
        rpuStationName: activeStation.name,
        rpuStationId: activeStation.id,
      }).catch(() => {
        // ignore presence errors
      });
    }, 180);
    return () => {
      clearTimeout(timer);
    };
  }, [activeStation.id, activeStation.name, activeStation.streamUrl, isPlaying]);

  useEffect(() => {
    const handleLoadStream = (event: Event) => {
      const detail = (event as CustomEvent<LoadStreamDetail>).detail;
      if (!detail?.streamUrl) return;
      setPlaybackWarning(null);
      const existing = availableStations.find((s) => s.streamUrl === detail.streamUrl);
      if (existing) {
        setActiveId(existing.id);
      } else {
        const id = detail.stationId ?? `shared-${Math.random().toString(36).slice(2, 8)}`;
        const station: Station = {
          id,
          stationId: detail.stationId,
          name: detail.stationName?.trim() ?? 'Stream compartido',
          streamUrl: detail.streamUrl,
          mood: 'Compartido',
          prompts: [],
          source: 'shared',
        };
        setCustomStations((prev) => {
          if (prev.some((s) => s.streamUrl === station.streamUrl)) return prev;
          return [...prev, station];
        });
        setActiveId(id);
      }
      setExpanded(true);
      setMuted(false);
      setIsPlaying(true);
    };
    window.addEventListener('tdf-radio-load-stream', handleLoadStream as EventListener);
    return () => window.removeEventListener('tdf-radio-load-stream', handleLoadStream as EventListener);
  }, [availableStations]);

  const togglePlay = () => {
    const audio = audioRef.current;
    if (!audio) return;
    if (isPlaying) {
      audio.pause();
      setIsPlaying(false);
      setPlaybackWarning(null);
    } else {
      if (muted) setMuted(false);
      void audio
        .play()
        .then(() => {
          setPlaybackWarning(null);
          setIsPlaying(true);
        })
        .catch(() => {
          setIsPlaying(false);
          setPlaybackWarning('No se pudo reproducir este stream. Intenta con otra estación o revisa la URL.');
          if (autoSkipOnError) {
            setTimeout(() => jumpToNextStation(), 200);
          }
        });
    }
  };

  const promptsWithCode = useMemo(() => {
    return stationPrompts.map((p) => ({
      ...p,
      code: p.code ?? generateTidalCode(p.text, activeStation.mood).code,
    }));
  }, [activeStation, stationPrompts]);

  const handleShare = useCallback(async () => {
    if (!activeStation.streamUrl) return;
    const message = `Escuchando ${activeStation.name} (${activeStation.mood || 'Radio'})\n${activeStation.streamUrl}`;
    try {
      if (navigator.share) {
        await navigator.share({
          title: activeStation.name,
          text: message,
          url: activeStation.streamUrl,
        });
        setShareNotice('Enviado para compartir');
      } else if (navigator.clipboard?.writeText) {
        await navigator.clipboard.writeText(message);
        setShareNotice('Copiado al portapapeles');
      }
      setTimeout(() => setShareNotice(null), 2200);
    } catch {
      setShareNotice('No se pudo compartir. Intenta de nuevo.');
      setTimeout(() => setShareNotice(null), 2200);
    }
  }, [activeStation]);

  const [newStationName, setNewStationName] = useState('');
  const [newStationUrl, setNewStationUrl] = useState('');
  const normalizeField = useCallback((value?: string | null) => {
    const trimmed = (value ?? '').trim();
    return trimmed === '' ? undefined : trimmed;
  }, []);
  const clearFilters = useCallback(() => {
    setSearchCountry('');
    setSearchGenre('');
    void refetchStreams();
  }, [refetchStreams]);
  const handleImportClick = useCallback(() => {
    void (async () => {
      setImporting(true);
      setImportMessage(null);
      setApiError(null);
      try {
        const res = await RadioAPI.importSources({ rirLimit: 800 });
        setImportMessage(`Importadas ${res.rirInserted} nuevas, ${res.rirUpdated} actualizadas de ${res.rirProcessed} streams.`);
        void refetchStreams();
        setLastUpdatedTs(Date.now());
      } catch (err) {
        const msg = err instanceof Error ? err.message : 'No se pudo importar el catálogo.';
        setImportMessage(msg);
      } finally {
        setImporting(false);
      }
    })();
  }, [refetchStreams]);
  const persistActiveStream = useCallback(
    async (url: string, name?: string, country?: string, genre?: string) => {
      try {
        const payload = {
          rsuStreamUrl: url,
          rsuName: normalizeField(name),
          rsuCountry: normalizeField(country),
          rsuGenre: normalizeField(genre),
        };
        const saved = await RadioAPI.upsertActive(payload);
        void refetchStreams();
        setApiError(null);
        return saved;
      } catch (err) {
        console.warn('No se pudo guardar metadata del stream', err);
        const msg = err instanceof Error ? err.message : 'No se pudo guardar metadata del stream.';
        setApiError(msg);
        return null;
      }
    },
    [normalizeField, refetchStreams],
  );
  const addCustomStation = () => {
    const name = newStationName.trim();
    const url = newStationUrl.trim();
    if (!name || !url) {
      setTestResult('Ingresa nombre y URL de la radio.');
      return;
    }
    const id = `custom-${Math.random().toString(36).slice(2, 8)}`;
    const country = normalizeField(newStationCountry);
    const genre = normalizeField(newStationGenre);
    const station: Station = {
      id,
      name,
      streamUrl: url,
      country,
      region: country,
      genre,
      mood: genre ?? 'Custom',
      prompts: [],
      source: 'custom',
    };
    setCustomStations((prev) => [...prev, station]);
    setPromptState((prev) => ({ ...prev, [id]: [] }));
    setActiveId(id);
    setPlaybackWarning(null);
    setNewStationName('');
    setNewStationUrl('');
    setNewStationCountry('');
    setNewStationGenre('');
  };

  const removeCustomStation = (id: string) => {
    setCustomStations((prev) => prev.filter((s) => s.id !== id));
    setPromptState((prev) => {
      const next = { ...prev };
      delete next[id];
      return next;
    });
    if (activeId === id) {
      setActiveId(defaultStation.id);
    }
  };

  const [testResult, setTestResult] = useState<string | null>(null);
  const testStream = async () => {
    const url = newStationUrl.trim();
    if (!url) {
      setTestResult('Ingresa una URL para probar.');
      return;
    }
    setPlaybackWarning(null);
    const controller = new AbortController();
    const timeout = setTimeout(() => controller.abort(), 6000);
    try {
      const res = await fetch(url, { method: 'HEAD', signal: controller.signal });
      if (res.ok) {
        const saved = await persistActiveStream(url, newStationName, newStationCountry, newStationGenre);
        setTestResult(saved ? 'Stream disponible ✅ · guardado en catálogo' : 'Stream disponible ✅');
      } else {
        setTestResult('No pudimos validar el stream. Verifica la URL.');
      }
    } catch {
      setTestResult('No pudimos validar el stream. Verifica la URL.');
    } finally {
      clearTimeout(timeout);
    }
  };

  const handleAddPrompt = () => {
    const value = promptDraft.trim();
    if (!value) return;
    const newPrompt: Prompt = {
      text: value,
      author: 'Tú',
      createdAt: new Date().toISOString().slice(0, 10),
      code: generateTidalCode(value, activeStation.mood).code,
    };
    setPromptState((prev) => ({
      ...prev,
      [activeStation.id]: [newPrompt, ...(prev[activeStation.id] ?? [])],
    }));
    setPromptDraft('');
  };

  const openAndFocusPrompt = useCallback(() => {
    setExpanded(true);
    // focus after collapse anim completes
    setTimeout(() => {
      promptInputRef.current?.focus();
    }, 120);
  }, []);

  return (
    <>
      {!miniBarVisible && (
        <Box
          sx={{
            position: 'fixed',
            left: position.x,
            top: position.y,
            zIndex: 1400,
            width: expanded ? { xs: '95%', sm: 420 } : { xs: 220, sm: 260 },
            cursor: pinned ? 'default' : dragging ? 'grabbing' : 'grab',
            touchAction: 'none',
            userSelect: 'none',
          }}
          ref={containerRef}
          onPointerDown={pinned ? undefined : onPointerDown}
          tabIndex={0}
          onKeyDown={(e) => {
            const step = e.shiftKey ? 20 : 10;
            const tag = (e.target as HTMLElement | null)?.tagName?.toLowerCase();
            if (['input', 'textarea'].includes(tag ?? '')) return;
            if (['ArrowUp', 'ArrowDown', 'ArrowLeft', 'ArrowRight'].includes(e.key)) {
              e.preventDefault();
              setPosition((p) => {
                const next = clampPosition(
                  e.key === 'ArrowLeft' ? p.x - step : e.key === 'ArrowRight' ? p.x + step : p.x,
                  e.key === 'ArrowUp' ? p.y - step : e.key === 'ArrowDown' ? p.y + step : p.y,
                );
                try {
                  window.localStorage.setItem('radio-position', JSON.stringify(next));
                } catch {
                  // ignore
                }
                return next;
              });
              return;
            }
            if (e.key.toLowerCase() === 'f') {
              e.preventDefault();
              setShowFavoritesOnly((v) => !v);
            }
            if (e.key.toLowerCase() === 'r') {
              e.preventDefault();
              resetPosition();
            }
            if (e.key.toLowerCase() === 'n') {
              e.preventDefault();
              jumpToNextStation();
            }
          }}
        >
      <Card
        elevation={6}
        sx={{ borderRadius: 3, overflow: 'hidden', cursor: dragging ? 'grabbing' : 'grab' }}
      >
        <Box
          data-drag-handle
          sx={{
            display: 'flex',
            alignItems: 'center',
            gap: 1,
            px: 1.5,
            py: 0.75,
            bgcolor: dragging ? 'action.selected' : 'action.hover',
            borderBottom: '1px dashed',
            borderColor: 'divider',
            cursor: dragging ? 'grabbing' : 'grab',
            userSelect: 'none',
          }}
        >
          <DragIndicatorIcon fontSize="small" color="action" />
          <Typography variant="caption" color="text.secondary" fontWeight={600}>
            {pinned ? 'Fijado' : 'Arrastra para mover'}
          </Typography>
          <Box sx={{ flex: 1 }} />
          <Tooltip title={pinned ? 'Permitir mover' : 'Fijar posición'}>
            <IconButton
              size="small"
              onClick={() => setPinned((v) => !v)}
              data-no-drag
              color={pinned ? 'primary' : 'inherit'}
            >
              <PushPinIcon fontSize="small" sx={{ transform: pinned ? 'rotate(25deg)' : 'none' }} />
            </IconButton>
          </Tooltip>
          <Tooltip title="Ocultar radio y mostrar mini barra">
            <IconButton size="small" onClick={() => setMiniBarVisible(true)} data-no-drag>
              <VisibilityOffIcon fontSize="small" />
            </IconButton>
          </Tooltip>
          <Tooltip title="Volver a la esquina">
            <IconButton size="small" onClick={resetPosition} data-no-drag>
              <RestartAltIcon fontSize="small" />
            </IconButton>
          </Tooltip>
        </Box>
        <CardContent
          sx={{ p: 2, cursor: 'pointer' }}
          onClick={(e) => {
            if (dragging || dragMovedRef.current) {
              dragMovedRef.current = false;
              return;
            }
            // Avoid toggling when clicking control buttons
            const tag = (e.target as HTMLElement | null)?.tagName?.toLowerCase();
            if (tag === 'button' || tag === 'svg' || tag === 'path') return;
            openAndFocusPrompt();
          }}
        >
          <Stack direction="row" alignItems="center" spacing={1}>
            <RadioIcon color="secondary" />
            <Box flex={1} minWidth={0}>
              <Typography variant="subtitle1" fontWeight={800} noWrap>
                Radio Inteligente
              </Typography>
              <Typography variant="caption" color="text.secondary" noWrap>
                {activeStation.name} · {activeStation.mood}
              </Typography>
            </Box>
            <Tooltip title={isPlaying ? 'Pausar' : 'Reproducir'}>
              <IconButton onClick={togglePlay} color="primary" data-no-drag>
                {isPlaying ? <PauseIcon /> : <PlayArrowIcon />}
              </IconButton>
            </Tooltip>
            <Tooltip title={isFavoriteActive ? 'Quitar de favoritos' : 'Marcar favorito'}>
              <IconButton
                onClick={() => toggleFavorite(activeStation)}
                color={isFavoriteActive ? 'warning' : 'inherit'}
                data-no-drag
                sx={controlFadeSx}
              >
                {isFavoriteActive ? <StarIcon /> : <StarBorderIcon />}
              </IconButton>
            </Tooltip>
            <Tooltip title={muted ? 'Quitar silencio' : 'Silenciar'}>
              <IconButton onClick={() => setMuted((m) => !m)} color="inherit" data-no-drag sx={controlFadeSx}>
                {muted ? <VolumeOffIcon /> : <GraphicEqIcon />}
              </IconButton>
            </Tooltip>
            <Tooltip title={autoSkipOnError ? 'Desactivar auto-skip al fallar' : 'Saltar al siguiente si falla'}>
              <IconButton
                onClick={() => setAutoSkipOnError((v) => !v)}
                color={autoSkipOnError ? 'success' : 'inherit'}
                data-no-drag
                sx={controlFadeSx}
              >
                <BoltIcon />
              </IconButton>
            </Tooltip>
            <IconButton
              onClick={() => {
                setExpanded((p) => !p);
                if (!expanded) {
                  setTimeout(() => promptInputRef.current?.focus(), 120);
                }
              }}
              color="inherit"
              data-no-drag
              sx={controlFadeSx}
            >
              {expanded ? <ExpandMoreIcon /> : <ExpandLessIcon />}
            </IconButton>
          </Stack>
          <Collapse in={!expanded}>
            <Stack direction="row" spacing={1} mt={1} alignItems="center">
              <GraphicEqIcon fontSize="small" color="secondary" />
              <Typography variant="caption" color="text.secondary">
                Generada en vivo con prompts de usuarios
              </Typography>
            </Stack>
            <Typography variant="caption" color="text.secondary" display="block" mt={0.5}>
              Reproduciendo: {activeStation.name}
            </Typography>
            <Stack direction="row" spacing={1} mt={1} alignItems="center">
              <Button
                variant="outlined"
                size="small"
                startIcon={<ShareIcon fontSize="small" />}
                onClick={() => {
                  void handleShare();
                }}
                data-no-drag
              >
                Compartir stream
              </Button>
              <Button
                variant="text"
                size="small"
                startIcon={<PeopleAltIcon fontSize="small" />}
                onClick={() => navigate('/social')}
                data-no-drag
              >
                Conexiones
              </Button>
              {shareNotice && (
                <Chip label={shareNotice} size="small" color="info" variant="outlined" />
              )}
            </Stack>
          </Collapse>
          {playbackWarning && (
            <Stack direction="row" spacing={1} alignItems="center" sx={{ mt: 1 }} data-no-drag>
              <WarningAmberIcon fontSize="small" color="warning" />
              <Typography variant="caption" sx={{ color: 'warning.main' }}>
                {playbackWarning}
              </Typography>
              {sortedVisibleStations.length > 1 && (
                <Button size="small" variant="text" onClick={jumpToNextStation} data-no-drag>
                  Probar siguiente
                </Button>
              )}
            </Stack>
          )}
        </CardContent>
        <Collapse in={expanded}>
          <Divider />
          <CardContent sx={{ p: 2 }}>
              <Stack spacing={1.5}>
                <Stack direction={{ xs: 'column', sm: 'row' }} alignItems={{ xs: 'flex-start', sm: 'center' }} spacing={1}>
                  <Typography variant="body2" color="text.secondary">
                    Estaciones del mundo (usa el widget para escuchar):
                  </Typography>
                  <Box flex={1} />
                  <Stack direction="row" spacing={1}>
                    <Tooltip title={showCatalogSection ? 'Ocultar catálogo' : 'Mostrar catálogo'}>
                      <Button
                        size="small"
                        variant="text"
                        startIcon={showCatalogSection ? <ExpandLessIcon /> : <ExpandMoreIcon />}
                        onClick={() => setShowCatalogSection((v) => !v)}
                        data-no-drag
                      >
                        Catálogo
                      </Button>
                    </Tooltip>
                    <Tooltip title={showAddSection ? 'Ocultar agregar/editar' : 'Mostrar agregar/editar'}>
                      <Button
                        size="small"
                        variant="text"
                        startIcon={showAddSection ? <ExpandLessIcon /> : <ExpandMoreIcon />}
                        onClick={() => setShowAddSection((v) => !v)}
                        data-no-drag
                      >
                        Editar/añadir
                      </Button>
                    </Tooltip>
                  </Stack>
                </Stack>

              <Collapse in={showCatalogSection} timeout="auto">
                <Stack spacing={1.5}>
                  <Stack spacing={1}>
                    <Typography variant="caption" color="text.secondary">
                      Busca por país o género en el catálogo guardado:
                    </Typography>
                    <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} alignItems="center">
                      <TextField
                        size="small"
                        label="País"
                        value={searchCountry}
                        onChange={(e) => setSearchCountry(e.target.value)}
                        placeholder="Ecuador, US, MX..."
                        fullWidth
                        inputProps={{ list: 'radio-country-options' }}
                      />
                      <TextField
                        size="small"
                        label="Género"
                        value={searchGenre}
                        onChange={(e) => setSearchGenre(e.target.value)}
                        placeholder="Ambient, Dembow, Jazz..."
                        fullWidth
                        inputProps={{ list: 'radio-genre-options' }}
                      />
                      <Stack direction="row" spacing={1} alignItems="center" sx={{ minWidth: { sm: 220 } }}>
                        <Button
                          variant="contained"
                          size="small"
                          onClick={() => {
                            void refetchStreams();
                          }}
                          disabled={streamsFetching}
                          data-no-drag
                        >
                          {streamsFetching ? 'Buscando...' : 'Buscar'}
                        </Button>
                        {(countryQuery || genreQuery) && (
                          <Button variant="text" size="small" onClick={clearFilters} data-no-drag>
                            Limpiar
                          </Button>
                        )}
                      </Stack>
                    </Stack>
                    <Stack direction={{ xs: 'column', sm: 'row' }} spacing={2} alignItems="center">
                      <FormControlLabel
                        control={
                          <Switch
                            checked={showFavoritesOnly}
                            onChange={() => setShowFavoritesOnly((v) => !v)}
                            color="warning"
                            size="small"
                          />
                        }
                        label="Solo favoritos"
                      />
                      <Button
                        variant="contained"
                        color="secondary"
                        size="small"
                        startIcon={<CloudDownloadIcon fontSize="small" />}
                        onClick={handleImportClick}
                        disabled={importing}
                        data-no-drag
                      >
                        {importing ? 'Importando...' : 'Importar'}
                      </Button>
                      {lastUpdatedTs && (
                        <Typography variant="caption" color="text.secondary">
                          Última actualización: {new Date(lastUpdatedTs).toLocaleTimeString()}
                        </Typography>
                      )}
                    </Stack>
                    {(countryOptions.length > 0 || genreOptions.length > 0) && (
                      <Stack spacing={0.5}>
                        <Typography variant="caption" color="text.secondary">
                          Sugerencias rápidas:
                        </Typography>
                        {countryOptions.length > 0 && (
                          <>
                            <datalist id="radio-country-options">
                              {countryOptions.map((c) => (
                                <option key={`country-opt-${c}`} value={c} />
                              ))}
                            </datalist>
                            <Stack direction="row" spacing={1} flexWrap="wrap" alignItems="center">
                              <Typography variant="caption" color="text.secondary">
                                País:
                              </Typography>
                              {countryOptions.map((c) => (
                                <Chip key={`country-${c}`} size="small" label={c} onClick={() => setSearchCountry(c)} />
                              ))}
                            </Stack>
                          </>
                        )}
                        {genreOptions.length > 0 && (
                          <>
                            <datalist id="radio-genre-options">
                              {genreOptions.map((g) => (
                                <option key={`genre-opt-${g}`} value={g} />
                              ))}
                            </datalist>
                            <Stack direction="row" spacing={1} flexWrap="wrap" alignItems="center">
                              <Typography variant="caption" color="text.secondary">
                                Género:
                              </Typography>
                              {genreOptions.map((g) => (
                                <Chip key={`genre-${g}`} size="small" label={g} onClick={() => setSearchGenre(g)} />
                              ))}
                            </Stack>
                          </>
                        )}
                      </Stack>
                    )}
                    {streamsFetching && (
                      <LinearProgress variant="indeterminate" sx={{ height: 4, borderRadius: 999, maxWidth: 320 }} />
                    )}
                    {streamsError && (
                      <Typography variant="caption" color="error">
                        No se pudo cargar el catálogo de radios.
                      </Typography>
                    )}
                    {apiError && (
                      <Typography variant="caption" color="error">
                        {apiError}
                      </Typography>
                    )}
                    {importMessage && (
                      <Typography variant="caption" color={importMessage.includes('Importadas') ? 'success.main' : 'error'}>
                        {importMessage}
                      </Typography>
                    )}
                  </Stack>
                  <Stack spacing={1}>
                    <Stack direction="row" spacing={1} flexWrap="wrap" alignItems="center">
                      <Typography variant="caption" color="text.secondary">
                        {streamsFetching
                          ? 'Buscando estaciones...'
                          : `Catálogo: ${sortedVisibleStations.length} coincidencias${countryQuery || genreQuery ? ' (filtrado)' : ''}`}
                      </Typography>
                      {(countryQuery || genreQuery) && sortedVisibleStations.length === 0 ? (
                        <Typography variant="caption" color="text.secondary">
                          Sin resultados con estos filtros.
                        </Typography>
                      ) : null}
                      {hiddenStations.length > 0 && (
                        <Tooltip title="Mostrar/ocultar estaciones que marcaste como ocultas">
                          <Button
                            size="small"
                            variant={showHidden ? 'contained' : 'outlined'}
                            startIcon={<VisibilityOffIcon fontSize="small" />}
                            onClick={() => setShowHidden((v) => !v)}
                            data-no-drag
                          >
                            {showHidden ? 'Ocultar ocultos' : `Ver ocultos (${hiddenStations.length})`}
                          </Button>
                        </Tooltip>
                      )}
                    </Stack>
                    <Stack direction="row" spacing={1} flexWrap="wrap">
                      {sortedVisibleStations.map((station) => (
                        <Chip
                          key={station.id}
                          label={
                            station.country || station.region || station.genre
                              ? `${station.name} · ${station.country ?? station.region ?? station.genre}`
                              : station.name
                          }
                          color={station.id === activeId ? 'primary' : 'default'}
                          onClick={() => {
                            setPlaybackWarning(null);
                            setActiveId(station.id);
                          }}
                          variant={station.id === activeId ? 'filled' : 'outlined'}
                          onDelete={
                            station.id.startsWith('custom-')
                              ? () => removeCustomStation(station.id)
                              : () => hideStation(station)
                          }
                          deleteIcon={
                            station.id.startsWith('custom-') ? undefined : <VisibilityOffIcon fontSize="small" />
                          }
                          icon={
                            station.id === activeId ? (
                              <FiberManualRecordIcon fontSize="small" color="success" />
                            ) : favoriteSet.has(keyFor(station)) ? (
                              <StarIcon fontSize="small" />
                            ) : undefined
                          }
                        />
                      ))}
                    </Stack>
                    {showHidden && hiddenStations.length > 0 && (
                      <Stack spacing={0.5}>
                        <Typography variant="caption" color="text.secondary">
                          Ocultas ({hiddenStations.length})
                        </Typography>
                        <Stack direction="row" spacing={1} flexWrap="wrap">
                          {hiddenStations.map((station) => (
                            <Chip
                              key={`hidden-${station.id}`}
                              label={
                                station.country || station.region || station.genre
                                  ? `${station.name} · ${station.country ?? station.region ?? station.genre}`
                                  : station.name
                              }
                              variant="outlined"
                              onDelete={() => unhideStation(station)}
                              deleteIcon={<RestartAltIcon fontSize="small" />}
                            />
                          ))}
                        </Stack>
                      </Stack>
                    )}
                  </Stack>
                </Stack>
              </Collapse>

              <Collapse in={showAddSection} timeout="auto">
                <Stack spacing={1}>
                  <Typography variant="caption" color="text.secondary">
                    Pega una URL de radio (ej. onlineradiobox.com) y guárdala para escucharla aquí.
                  </Typography>
                  <Typography variant="caption" color="text.secondary">
                    También puedes editar la metadata de la estación activa y guardarla en el catálogo.
                  </Typography>
                  <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
                    <TextField
                      size="small"
                      label="Nombre"
                      value={editName}
                      onChange={(e) => setEditName(e.target.value)}
                      fullWidth
                    />
                    <TextField
                      size="small"
                      label="País"
                      value={editCountry}
                      onChange={(e) => setEditCountry(e.target.value)}
                      fullWidth
                      inputProps={{ list: 'radio-country-options' }}
                    />
                    <TextField
                      size="small"
                      label="Género"
                      value={editGenre}
                      onChange={(e) => setEditGenre(e.target.value)}
                      fullWidth
                      inputProps={{ list: 'radio-genre-options' }}
                    />
                    <Button
                      variant="outlined"
                      onClick={() => {
                        void persistActiveStream(activeStation.streamUrl, editName, editCountry, editGenre);
                      }}
                      data-no-drag
                    >
                      Guardar metadata
                    </Button>
                  </Stack>
                  <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
                    <TextField
                      size="small"
                      label="Nombre de la radio"
                      value={newStationName}
                      onChange={(e) => setNewStationName(e.target.value)}
                      fullWidth
                    />
                    <TextField
                      size="small"
                      label="URL del stream"
                      value={newStationUrl}
                      onChange={(e) => setNewStationUrl(e.target.value)}
                      placeholder="https://"
                      fullWidth
                    />
                    <TextField
                      size="small"
                      label="País"
                      value={newStationCountry}
                      onChange={(e) => setNewStationCountry(e.target.value)}
                      placeholder="Ecuador, México, UK..."
                      fullWidth
                    />
                    <TextField
                      size="small"
                      label="Género"
                      value={newStationGenre}
                      onChange={(e) => setNewStationGenre(e.target.value)}
                      placeholder="Afro, Ambient, Indie..."
                      fullWidth
                    />
                    <Stack direction={{ xs: 'row', sm: 'column' }} spacing={1} sx={{ minWidth: { sm: 160 } }}>
                      <Button
                        variant="outlined"
                        onClick={() => {
                          void testStream();
                        }}
                        data-no-drag
                      >
                        Probar stream
                      </Button>
                      <Button
                        variant="contained"
                        onClick={addCustomStation}
                        disabled={!newStationName.trim() || !newStationUrl.trim()}
                        data-no-drag
                      >
                        Guardar
                      </Button>
                    </Stack>
                  </Stack>
                  {testResult && (
                    <Stack direction="row" spacing={1} alignItems="center">
                      <WarningAmberIcon fontSize="small" color={testResult.includes('✅') ? 'success' : 'warning'} />
                      <Typography variant="caption" color="text.secondary">
                        {testResult}
                      </Typography>
                    </Stack>
                  )}
                  <LinearProgress
                    variant="indeterminate"
                    sx={{ height: 6, borderRadius: 999, bgcolor: 'rgba(148,163,184,0.2)' }}
                  />
                  <Typography variant="caption" color="text.secondary">
                    Streams de muestra; agrega tus prompts para escuchar nuevas variaciones.
                  </Typography>
                  <Typography variant="subtitle2">Prompts en uso</Typography>
                  <PromptList prompts={promptsWithCode} />
                  <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
                    <TextField
                      fullWidth
                      size="small"
                      placeholder="Escribe tu prompt (ej. dembow con quenas y delay granular)"
                      value={promptDraft}
                      onChange={(e) => setPromptDraft(e.target.value)}
                      inputRef={promptInputRef}
                    />
                    <Button variant="contained" onClick={handleAddPrompt} disabled={!promptDraft.trim()} data-no-drag>
                      Generar Tidal
                    </Button>
                  </Stack>
                  <Box>
                    <Typography variant="caption" color="text.secondary">
                      Tip: estos streams se generan con un agente que convierte prompts en código Tidal Cycles y los publica en tiempo real.
                    </Typography>
                  </Box>
                </Stack>
              </Collapse>
            </Stack>
          </CardContent>
        </Collapse>
      </Card>
    </Box>
      )}
      {/* Audio is programmatically controlled; captions are not available for live streams. */}
      {/* eslint-disable-next-line jsx-a11y/media-has-caption */}
      <audio ref={audioRef} preload="none" aria-hidden="true" />
      {miniBarVisible && (
        <Box
          sx={{
            position: 'fixed',
            left: miniPosition.x,
            top: miniPosition.y,
            zIndex: 1400,
            bgcolor: 'background.paper',
            boxShadow: 6,
            borderRadius: 2,
            px: 1,
            py: 0.5,
            display: 'flex',
            alignItems: 'center',
            gap: 0.75,
            border: '1px solid',
            borderColor: 'divider',
            cursor: miniDragging ? 'grabbing' : 'grab',
            touchAction: 'none',
            userSelect: 'none',
            minWidth: 220,
          }}
          ref={miniContainerRef}
          onPointerDown={onMiniPointerDown}
          onClick={(e) => {
            if (miniDragging || miniDragMovedRef.current) {
              miniDragMovedRef.current = false;
              return;
            }
            const tag = (e.target as HTMLElement | null)?.tagName?.toLowerCase();
            if (tag === 'button' || tag === 'svg' || tag === 'path') return;
            setMiniBarVisible(false);
            setExpanded(true);
          }}
        >
          <DragIndicatorIcon fontSize="small" color="action" />
          <Tooltip title={isPlaying ? 'Pausar' : 'Reproducir'}>
            <IconButton size="small" onClick={togglePlay} data-no-drag>
              {isPlaying ? <PauseIcon fontSize="small" /> : <PlayArrowIcon fontSize="small" />}
            </IconButton>
          </Tooltip>
          <Tooltip title="Saltar al siguiente">
            <IconButton size="small" onClick={jumpToNextStation} data-no-drag>
              <SkipNextIcon fontSize="small" />
            </IconButton>
          </Tooltip>
          <Tooltip title="Mostrar radio">
            <IconButton
              size="small"
              onClick={() => {
                setMiniBarVisible(false);
                setExpanded(true);
              }}
              data-no-drag
            >
              <OpenInFullIcon fontSize="small" />
            </IconButton>
          </Tooltip>
          <Box sx={{ minWidth: 0, maxWidth: 220 }}>
            <Typography variant="caption" fontWeight={700} noWrap>
              {activeStation.name}
            </Typography>
            <Typography variant="caption" color="text.secondary" noWrap>
              {activeStation.genre ?? activeStation.mood}
            </Typography>
          </Box>
        </Box>
      )}
    </>
  );
}
