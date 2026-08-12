import { z } from 'zod';

export const emailSchema = z.string().email('Correo electrónico inválido');
export const passwordSchema = z.string().min(8, 'Mínimo 8 caracteres');
export const requiredString = (field: string) => z.string().min(1, `${field} es obligatorio`);
export const optionalString = z.string().optional().or(z.literal(''));

export const partySchema = z.object({
  name: requiredString('Nombre'),
  email: emailSchema.optional().or(z.literal('')),
  phone: optionalString,
});

export const feedbackSchema = z.object({
  title: requiredString('Título'),
  description: requiredString('Descripción'),
  consent: z.boolean().refine(v => v === true, 'Debes aceptar los términos'),
});
