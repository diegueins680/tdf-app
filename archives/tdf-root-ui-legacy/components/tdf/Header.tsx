import { useState } from 'react'
import { Link, NavLink } from 'react-router-dom'
import { Logo } from '../Logo'

type HeaderNavItem = {
  to: string
  label: string
  disabled?: boolean
  tooltip?: string
}

type HeaderProps = {
  items: HeaderNavItem[]
  username?: string
  onLogout?: () => void
  onShowAbout?: () => void
}

export default function Header({ items, username, onLogout, onShowAbout }: HeaderProps) {
  const [logoReady, setLogoReady] = useState(false)

  const navClass = ({ isActive }: { isActive: boolean }) => `nav-link${isActive ? ' is-active' : ''}`

  const brandClass = logoReady ? 'brand has-logo' : 'brand is-fallback'

  return (
    <header className="site-header">
      <div className="container navbar">
        <Link to="/" className={brandClass} aria-label="TDF Records home">
          <Logo
            className="brand-logo"
            alt="TDF Records"
            tone="dark"
            onLoad={() => setLogoReady(true)}
          />
          <span className="brand-title">TDF Records</span>
        </Link>
        <nav className="nav-links" aria-label="Primary">
          {items.map(item => (
            item.disabled ? (
              <span key={item.to} className="nav-link is-disabled" title={item.tooltip} aria-disabled="true">
                {item.label}
              </span>
            ) : (
              <NavLink key={item.to} to={item.to} className={navClass} title={item.tooltip ?? undefined}>
                {item.label}
              </NavLink>
            )
          ))}
        </nav>
        <div className="nav-actions">
          <a
            className="nav-link nav-link--docs"
            href="/docs"
            target="_blank"
            rel="noreferrer"
          >
            Docs
          </a>
          {onShowAbout && (
            <button className="btn btn-outline" onClick={onShowAbout} type="button">
              Acerca de
            </button>
          )}
          {username && (
            <NavLink to="/account/password" className={navClass}>
              Seguridad
            </NavLink>
          )}
          {username && <span className="user-id" aria-label="Signed in user">{username}</span>}
          {onLogout && (
            <button className="btn" onClick={onLogout} type="button">
              Salir
            </button>
          )}
        </div>
      </div>
    </header>
  )
}
