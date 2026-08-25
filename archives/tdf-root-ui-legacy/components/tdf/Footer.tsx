import { Logo } from '../Logo';

export default function Footer(){
  return (
    <footer className="site-footer">
      <div className="container cluster" style={{justifyContent:"space-between"}}>
        <div className="cluster">
          <Logo className="brand-logo" alt="TDF Records" tone="dark" />
          <span>&copy; {new Date().getFullYear()} TDF Records</span>
        </div>
        <div className="cluster">
          <a href="https://instagram.com" target="_blank" rel="noreferrer">Instagram</a>
          <a href="https://bandcamp.com" target="_blank" rel="noreferrer">Bandcamp</a>
          <a href="https://youtube.com" target="_blank" rel="noreferrer">YouTube</a>
        </div>
      </div>
    </footer>
  );
}
