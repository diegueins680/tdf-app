import { get, post, patch } from './client';
import type { RoomCreate, RoomDTO, RoomUpdate } from './types';

export const Rooms = {
  list: () => get<RoomDTO[]>('/rooms'),
  create: (body: RoomCreate) => post<RoomDTO>('/rooms', body),
  update: (id: string, body: RoomUpdate) => patch<RoomDTO>(`/rooms/${id}`, body),
};
