export type CsvCell = string | number | boolean | null | undefined;

// Encode user-influenced strings as text when their prefix could be interpreted
// as a spreadsheet formula. Preserve typed numbers, including negative cents.
// This is an export encoding policy, not a guarantee about every CSV consumer.
export const serializeCsvCell = (value: CsvCell): string => {
  let text = String(value ?? '');
  if (typeof value === 'string'
    && /^(?:\s*[=+\-@\uFF1D\uFF0B\uFF0D\uFF20]|\s*[\t\r\n])/u.test(value)) {
    text = `'${text}`;
  }
  return `"${text.replace(/"/g, '""')}"`;
};
