import i18n from '../i18n';
import { jest } from '@jest/globals';
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { MemoryRouter } from 'react-router-dom';
import { CART_META_KEY, CART_OPEN_EVENT, writeCartMeta } from '../features/marketplace/cartSummary';

jest.unstable_mockModule('../session/SessionContext', () => ({
  useSession: () => ({ session: null, loading: false, login: jest.fn(), logout: jest.fn(), setApiToken: jest.fn() }),
}));

jest.unstable_mockModule('./BrandLogo', () => ({
  default: () => <span>TDF Records</span>,
}));

const { default: PublicBranding } = await import('./PublicBranding');

const flushPromises = () => new Promise<void>((resolve) => setTimeout(resolve, 0));

const render = async (route: string) => {
  const container = document.createElement('div');
  document.body.appendChild(container);
  let root: Root | null = createRoot(container);
  await act(async () => {
    root?.render(
      <MemoryRouter initialEntries={[route]}>
        <PublicBranding>
          <div>Contenido</div>
        </PublicBranding>
      </MemoryRouter>,
    );
    await flushPromises();
  });
  return {
    container,
    cleanup: async () => {
      await act(async () => {
        root?.unmount();
        await flushPromises();
      });
      root = null;
      container.remove();
    },
  };
};

const cartButton = (container: HTMLElement) =>
  container.querySelector<HTMLElement>('header [data-testid="marketplace-cart-button"]');

const badgeText = (container: HTMLElement) => {
  const badge = container.querySelector('header .MuiBadge-badge');
  if (!badge || badge.classList.contains('MuiBadge-invisible')) return null;
  return badge.textContent;
};

describe('MarketplaceCartButton in the public header', () => {
  const originalInnerWidth = window.innerWidth;

  beforeAll(() => {
    (globalThis as unknown as { IS_REACT_ACT_ENVIRONMENT?: boolean }).IS_REACT_ACT_ENVIRONMENT = true;
  });

  beforeEach(async () => {
    await i18n.changeLanguage('es');
    window.localStorage.clear();
    window.sessionStorage.clear();
  });

  afterEach(() => {
    Object.defineProperty(window, 'innerWidth', { configurable: true, value: originalInnerWidth });
  });

  it('shows the cart icon without a badge on /marketplace when the cart is empty', async () => {
    const view = await render('/marketplace');
    try {
      const button = cartButton(view.container);
      expect(button).not.toBeNull();
      expect(button?.getAttribute('aria-label')).toBe('Carrito, sin productos');
      expect(badgeText(view.container)).toBeNull();
    } finally {
      await view.cleanup();
    }
  });

  it('renders the stored count and updates as soon as the cart event fires', async () => {
    window.localStorage.setItem(CART_META_KEY, JSON.stringify({ cartId: 'cart-1', count: 1, updatedAt: 1 }));
    const view = await render('/marketplace');
    try {
      expect(cartButton(view.container)?.getAttribute('aria-label')).toBe('Carrito, 1 producto');
      expect(badgeText(view.container)).toBe('1');

      await act(async () => {
        writeCartMeta({ cartId: 'cart-1', count: 3, updatedAt: 2 });
        await flushPromises();
      });
      expect(cartButton(view.container)?.getAttribute('aria-label')).toBe('Carrito, 3 productos');
      expect(badgeText(view.container)).toBe('3');

      await act(async () => {
        writeCartMeta(null);
        await flushPromises();
      });
      expect(cartButton(view.container)).not.toBeNull();
      expect(badgeText(view.container)).toBeNull();
    } finally {
      await view.cleanup();
    }
  });

  it('follows changes made in another tab', async () => {
    const view = await render('/marketplace');
    try {
      await act(async () => {
        window.localStorage.setItem(CART_META_KEY, JSON.stringify({ cartId: 'cart-1', count: 2 }));
        window.dispatchEvent(new StorageEvent('storage', { key: CART_META_KEY }));
        await flushPromises();
      });
      expect(badgeText(view.container)).toBe('2');
    } finally {
      await view.cleanup();
    }
  });

  it('asks the marketplace page to open its cart drawer when clicked on /marketplace', async () => {
    const opened = jest.fn();
    window.addEventListener(CART_OPEN_EVENT, opened);
    const view = await render('/marketplace');
    try {
      await act(async () => {
        cartButton(view.container)?.click();
        await flushPromises();
      });
      expect(opened).toHaveBeenCalledTimes(1);
    } finally {
      window.removeEventListener(CART_OPEN_EVENT, opened);
      await view.cleanup();
    }
  });

  it('stays in the top bar (not the overflow menu) at a 360px phone width', async () => {
    Object.defineProperty(window, 'innerWidth', { configurable: true, value: 360 });
    window.localStorage.setItem(CART_META_KEY, JSON.stringify({ cartId: 'cart-1', count: 1 }));
    const view = await render('/marketplace');
    try {
      const button = cartButton(view.container);
      expect(button).not.toBeNull();
      // The desktop nav is the only header region hidden on xs.
      expect(button?.closest('nav')).toBeNull();
      expect(button?.closest('[role="menu"]')).toBeNull();
      expect(document.querySelector('[role="menu"]')).toBeNull();
    } finally {
      await view.cleanup();
    }
  });

  it('links returning visitors on other pages to the cart only when it has items', async () => {
    const empty = await render('/records');
    try {
      expect(cartButton(empty.container)).toBeNull();
    } finally {
      await empty.cleanup();
    }

    window.localStorage.setItem(CART_META_KEY, JSON.stringify({ cartId: 'cart-1', count: 2 }));
    const withItems = await render('/records');
    try {
      const link = cartButton(withItems.container);
      expect(link?.tagName).toBe('A');
      expect(link?.getAttribute('href')).toBe('/marketplace#carrito');
      expect(link?.getAttribute('aria-label')).toBe('Carrito, 2 productos');
    } finally {
      await withItems.cleanup();
    }
  });
});
