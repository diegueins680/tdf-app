const pad2 = (value: number): string => String(value).padStart(2, '0');

export const toLocalDateInputValue = (date: Date = new Date()): string => {
  if (Number.isNaN(date.getTime())) return '';
  return `${date.getFullYear()}-${pad2(date.getMonth() + 1)}-${pad2(date.getDate())}`;
};
