import type { NavigateOptions, To } from 'react-router';

// This application uses React Router's declarative BrowserRouter. In that mode,
// navigate() is synchronous; the Promise return is reserved for data/framework
// routers. Keep the type aligned with the configured router so promise lint
// rules continue to catch real async mistakes.
declare module 'react-router' {
  interface NavigateFunction {
    (to: To, options?: NavigateOptions): void;
    (delta: number): void;
  }
}

export {};
