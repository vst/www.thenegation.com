export function setupSearch() {
  const searchButton = document.getElementById("search-button");
  const searchOverlay = document.getElementById("search-overlay");
  const searchClose = document.getElementById("search-close");

  function openSearch() {
    if (!searchOverlay) {
      return;
    }

    searchOverlay.setAttribute("aria-hidden", "false");

    document.body.style.overflow = "hidden";
    // Focus on search input after Pagefind loads
    setTimeout(() => {
      const searchInput = searchOverlay.querySelector(
        'input[type="text"]',
      ) as HTMLInputElement;
      searchInput?.focus();
    }, 100);
  }

  function closeSearch() {
    searchOverlay?.setAttribute("aria-hidden", "true");
    document.body.style.overflow = "";
  }

  searchButton?.addEventListener("click", openSearch);
  searchClose?.addEventListener("click", closeSearch);

  // Close on overlay click (but not dialog click)
  searchOverlay?.addEventListener("click", function (e) {
    if (e.target === searchOverlay) {
      closeSearch();
    }
  });

  // Close on Escape key
  document.addEventListener("keydown", function (e) {
    if (
      e.key === "Escape" &&
      searchOverlay?.getAttribute("aria-hidden") === "false"
    ) {
      closeSearch();
    }
  });

  // Optional: Keyboard shortcut (Ctrl+K or Cmd+K)
  document.addEventListener("keydown", function (e) {
    if ((e.ctrlKey || e.metaKey) && e.key === "k") {
      e.preventDefault();
      openSearch();
    }
  });

  // Setup Pagefind UI:
  window.addEventListener("DOMContentLoaded", () => {
    // @ts-expect-error
    new PagefindUI({
      element: "#search",
      showSubResults: true,
      showImages: false,
    });
  });
}
