import { post } from './client';

export type SignupPayload = {
  firstName: string;
  lastName: string;
  email: string;
  phone?: string;
  password?: string;
  googleIdToken?: string;
  marketingOptIn?: boolean;
};

export type ForgotPasswordPayload = {
  email: string;
};

export type ResetPasswordPayload = {
  token: string;
  password: string;
};

export type ChangePasswordPayload = {
  username: string;
  currentPassword: string;
  newPassword: string;
};

export const AuthApi = {
  signup: (body: SignupPayload) => post('/v1/signup', body),
  requestPasswordReset: (body: ForgotPasswordPayload) => post('/v1/password-reset', body),
  resetPassword: (body: ResetPasswordPayload) => post('/v1/password-reset/confirm', body),
  changePassword: (body: ChangePasswordPayload) => post('/v1/password/change', body),
};
