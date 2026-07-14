/**
 * Opens the Pagefind search modal from our own trigger button, since
 * <pagefind-modal-trigger> renders its own immutable markup and can't be
 * restyled (see https://pagefind.app/docs/custom-components/).
 */
export function setupSearchTrigger() {
  const triggerButton = document.querySelector(
    "[data-search-trigger]",
  ) as HTMLButtonElement;
  const modal = document.getElementById("search-modal") as
    (HTMLElement & { open: () => void }) | null;

  if (!triggerButton || !modal) {
    return;
  }

  triggerButton.addEventListener("click", () => {
    modal.open();
  });

  // Preserve the Cmd/Ctrl+K shortcut that Pagefind's own trigger provided.
  document.addEventListener("keydown", (event) => {
    if ((event.metaKey || event.ctrlKey) && event.key.toLowerCase() === "k") {
      event.preventDefault();
      modal.open();
    }
  });
}
