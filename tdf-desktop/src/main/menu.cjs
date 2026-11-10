'use strict';
// Native application menu. Playback entries mirror the hosted player's state
// and send commands back to the same playback session.
const { Menu, shell } = require('electron');
const { describeForMenu } = require('./playbackState.cjs');

function buildMenu({ app, channel, playback, updater, actions }) {
  const isMac = process.platform === 'darwin';
  const view = describeForMenu(playback);
  const updateLabel = (() => {
    switch (updater.status) {
      case 'disabled': return updater.reason === 'unsigned-test-build' ? 'Actualizaciones desactivadas (compilación de prueba sin firma)'
        : updater.reason === 'development-run' ? 'Actualizaciones desactivadas (ejecución de desarrollo)' : 'Actualizaciones desactivadas';
      case 'checking': return 'Buscando actualizaciones…';
      case 'available': return `Descargar la versión ${updater.version ?? ''}`.trim();
      case 'downloading': return `Descargando actualización… ${updater.percent ?? 0}%`;
      case 'downloaded': return `Reiniciar para instalar ${updater.version ?? ''}`.trim();
      case 'error': return 'Reintentar búsqueda de actualizaciones';
      default: return 'Buscar actualizaciones…';
    }
  })();
  const updateItem = {
    label: updateLabel,
    enabled: updater.status !== 'disabled' && updater.status !== 'checking' && updater.status !== 'downloading',
    click: () => actions.onUpdateAction(),
  };
  const aboutItem = { label: 'Acerca de TDF Records', click: () => actions.showAbout() };

  const template = [
    ...(isMac ? [{
      label: app.name,
      submenu: [
        aboutItem,
        updateItem,
        { type: 'separator' },
        { role: 'services' },
        { type: 'separator' },
        { role: 'hide' }, { role: 'hideOthers' }, { role: 'unhide' },
        { type: 'separator' },
        { role: 'quit' },
      ],
    }] : []),
    {
      label: 'Archivo',
      submenu: [
        { label: 'Ir al inicio', accelerator: 'CmdOrCtrl+Shift+H', click: () => actions.navigate(channel.startPath) },
        { label: 'Abrir en el navegador', click: () => actions.openCurrentInBrowser() },
        { type: 'separator' },
        { label: 'Cerrar sesión y borrar datos locales…', click: () => actions.clearLocalData() },
        { type: 'separator' },
        isMac ? { role: 'close' } : { role: 'quit' },
      ],
    },
    {
      label: 'Edición',
      submenu: [
        { role: 'undo' }, { role: 'redo' }, { type: 'separator' },
        { role: 'cut' }, { role: 'copy' }, { role: 'paste' },
        ...(isMac ? [{ role: 'pasteAndMatchStyle' }, { role: 'delete' }, { role: 'selectAll' }] : [{ role: 'delete' }, { type: 'separator' }, { role: 'selectAll' }]),
      ],
    },
    {
      label: 'Ver',
      submenu: [
        { label: 'Recargar', accelerator: 'CmdOrCtrl+R', click: () => actions.reload() },
        ...(app.isPackaged ? [] : [{ role: 'toggleDevTools' }]),
        { type: 'separator' },
        { role: 'resetZoom' }, { role: 'zoomIn' }, { role: 'zoomOut' },
        { type: 'separator' },
        { role: 'togglefullscreen' },
      ],
    },
    {
      label: 'Reproducción',
      submenu: [
        { label: view.nowPlaying, enabled: false },
        { type: 'separator' },
        { label: view.toggleLabel, enabled: view.toggleEnabled, accelerator: 'MediaPlayPause', click: () => actions.playback('toggle') },
        { label: 'Siguiente', enabled: view.nextEnabled, accelerator: 'MediaNextTrack', click: () => actions.playback('next') },
        { label: 'Anterior', enabled: view.previousEnabled, accelerator: 'MediaPreviousTrack', click: () => actions.playback('previous') },
      ],
    },
    { label: 'Ventana', role: 'windowMenu' },
    {
      label: 'Ayuda',
      role: 'help',
      submenu: [
        ...(isMac ? [] : [aboutItem, updateItem, { type: 'separator' }]),
        { label: 'Sitio web de TDF Records', click: () => { void shell.openExternal(channel.appOrigin); } },
        { label: 'Abrir carpeta de diagnóstico', click: () => actions.openLogs() },
      ],
    },
  ];
  return Menu.buildFromTemplate(template);
}

module.exports = { buildMenu };
