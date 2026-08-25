type PlaceholderProps = {
  title: string;
  description?: string;
};

export default function Placeholder({ title, description }: PlaceholderProps) {
  return (
    <section className="placeholder">
      <header className="placeholder__header">
        <h1>{title}</h1>
      </header>
      <p className="placeholder__body">
        {description ?? 'Esta sección está en construcción. Vuelve pronto para más funcionalidades.'}
      </p>
    </section>
  );
}
