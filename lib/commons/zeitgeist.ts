export function formatDate(date: string | Date): string {
  if (date instanceof String) {
    date = new Date(`${date}T00:00:00Z`);
  }

  return (date as Date)
    .toLocaleDateString('en-US', {
      day: 'numeric',
      month: 'long',
      year: 'numeric',
      timeZone: 'UTC',
    })
    .toString();
}
