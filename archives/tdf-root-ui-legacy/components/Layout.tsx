import { useId, useState } from 'react';
import { Outlet } from 'react-router-dom';
import Header from './tdf/Header';
import Footer from './tdf/Footer';
import SideNav from './SideNav';
import AboutDialog from './AboutDialog';
import { useAuth } from '../auth/AuthProvider';

export default function Layout() {
  const { user, logout } = useAuth();
  const [isNavCollapsed, setNavCollapsed] = useState(true);
  const navId = useId();
  const [aboutOpen, setAboutOpen] = useState(false);

  const toggleNav = () => {
    setNavCollapsed(prev => !prev);
  };

  const shellClassName = [
    'app-shell',
    isNavCollapsed ? 'app-shell--nav-collapsed' : 'app-shell--nav-expanded',
  ].join(' ');

  return (
    <>
      <Header
        items={[]}
        username={user?.username}
        onLogout={logout}
        onShowAbout={() => setAboutOpen(true)}
      />
      <div className={shellClassName}>
        <SideNav id={navId} collapsed={isNavCollapsed} onToggle={toggleNav} />
        <main className="app-shell__content">
          <button
            type="button"
            className="side-nav-toggle"
            onClick={toggleNav}
            aria-expanded={!isNavCollapsed}
            aria-controls={navId}
          >
            <span className="side-nav-toggle__icon" aria-hidden="true">
              {isNavCollapsed ? '☰' : '✕'}
            </span>
            <span className="side-nav-toggle__label">
              {isNavCollapsed ? 'Abrir menú' : 'Cerrar menú'}
            </span>
          </button>
          <div className="container stack-lg">
            <Outlet />
          </div>
        </main>
      </div>
      <Footer />
      <AboutDialog open={aboutOpen} onClose={() => setAboutOpen(false)} />
    </>
  );
}
