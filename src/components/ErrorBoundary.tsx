import { Component, ReactNode } from 'react';

export default class ErrorBoundary extends Component<{children: ReactNode}, {hasError: boolean}> {
  constructor(props:any){ super(props); this.state = { hasError: false }; }
  static getDerivedStateFromError(){ return { hasError: true }; }
  componentDidCatch(err:any){ console.error('UI error boundary caught:', err); }
  render(){ return this.state.hasError ? <div style={{padding:16}}>Algo salió mal. Revisa la consola.</div> : this.props.children; }
}
