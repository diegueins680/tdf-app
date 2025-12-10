import { get, post, put } from './client';
import type { RoomDTO, RoomCreate, RoomUpdate } from './types';

export const Rooms = {
  list: () => get<RoomDTO[]>('/rooms'),
  listPublic: () => get<RoomDTO[]>('/rooms/public'),
  create: (body: RoomCreate) => post<RoomDTO>('/rooms', body),
  update: (roomId: string, body: RoomUpdate) => put<RoomDTO>(`/rooms/${roomId}`, body),
};
