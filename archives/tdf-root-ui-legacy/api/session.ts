import { post } from './client';
import type { LoginRequest, LoginResponse } from './types';

export const Session = {
  login: (body: LoginRequest) => post<LoginResponse>('/login', body),
};
