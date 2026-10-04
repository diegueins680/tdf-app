import { Routes } from 'react-router-dom';

import { renderProtectedRoutes } from './protectedRoutes';
import { renderPublicRoutes } from './publicRoutes';

export default function AppRoutes() {
  return (
    <Routes>
      {renderPublicRoutes()}
      {renderProtectedRoutes()}
    </Routes>
  );
}
