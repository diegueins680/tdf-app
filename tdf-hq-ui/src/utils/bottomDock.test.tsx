import { act, useRef } from 'react';
import { createRoot } from 'react-dom/client';
import {
  BOTTOM_DOCK_OFFSET,
  GLOBAL_PLAYER_HEIGHT_VAR,
  RADIO_BAR_HEIGHT_VAR,
  useDockedBarHeight,
} from './bottomDock';

function Bar({ cssVar, active, height }: { cssVar: string; active: boolean; height: number }) {
  const ref = useRef<HTMLDivElement | null>(null);
  useDockedBarHeight(cssVar, ref, active);
  return (
    <div
      ref={(node) => {
        ref.current = node;
        if (node) node.getBoundingClientRect = () => ({ height } as DOMRect);
      }}
    />
  );
}

describe('bottom dock reservations', () => {
  beforeAll(() => {
    (globalThis as unknown as { IS_REACT_ACT_ENVIRONMENT?: boolean }).IS_REACT_ACT_ENVIRONMENT = true;
  });

  it('stacks published bar heights and restores the body only after the last bar leaves', async () => {
    document.body.style.paddingBottom = '3px';
    const container = document.createElement('div');
    document.body.appendChild(container);
    const root = createRoot(container);
    const render = (player: boolean, radio: boolean) => act(async () => {
      root.render(
        <>
          <Bar cssVar={GLOBAL_PLAYER_HEIGHT_VAR} active={player} height={72.2} />
          <Bar cssVar={RADIO_BAR_HEIGHT_VAR} active={radio} height={60} />
        </>,
      );
    });
    const style = document.documentElement.style;

    await render(true, true);
    expect(style.getPropertyValue(GLOBAL_PLAYER_HEIGHT_VAR)).toBe('73px');
    expect(style.getPropertyValue(RADIO_BAR_HEIGHT_VAR)).toBe('60px');
    expect(document.body.style.paddingBottom).toBe(BOTTOM_DOCK_OFFSET);

    await render(false, true);
    expect(style.getPropertyValue(GLOBAL_PLAYER_HEIGHT_VAR)).toBe('');
    expect(document.body.style.paddingBottom).toBe(BOTTOM_DOCK_OFFSET);

    await render(false, false);
    expect(style.getPropertyValue(RADIO_BAR_HEIGHT_VAR)).toBe('');
    expect(document.body.style.paddingBottom).toBe('3px');

    await act(async () => root.unmount());
    container.remove();
    document.body.style.paddingBottom = '';
  });
});
