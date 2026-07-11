/**
 * Dark mode theme management
 */
export function setupTheme() {
  const STORAGE_KEY = "darkMode";
  const toggleButton = document.querySelector(
    "[data-theme-toggle]",
  ) as HTMLButtonElement;

  if (!toggleButton) {
    return;
  }

  /**
   * Get initial dark mode state from localStorage or system preference
   */
  function getInitialDarkMode(): boolean {
    const stored = localStorage.getItem(STORAGE_KEY);
    if (stored !== null) {
      return stored === "true";
    }
    return window.matchMedia("(prefers-color-scheme: dark)").matches;
  }

  /**
   * Apply dark mode class to the root element and update button aria-checked
   */
  function applyTheme(isDark: boolean) {
    // Update aria-checked attribute for setting the toggle state:
    toggleButton.setAttribute("aria-checked", String(isDark));

    // Add or remove the dark class on the root element. This must be the
    // root element (not body) because @catppuccin/tailwindcss scopes its
    // palette override to `:root`, which our `dark` variant only matches
    // when `.dark` is on `:root` itself or an ancestor of it:
    document.documentElement.classList.toggle("dark", isDark);

    // @catppuccin/tailwindcss's automatic light/dark flavor switch is
    // broken by a known upstream bug (a Sass compilation quirk nests
    // `:root` a second time inside its `@variant dark` block, producing
    // a selector that can never match — see
    // https://github.com/catppuccin/tailwindcss/issues/37). As a
    // workaround, force the mocha flavor directly via its scoped class
    // instead of relying on the `dark` variant to flip it; latte remains
    // the plain `:root` default for light mode.
    document.documentElement.classList.toggle("mocha", isDark);

    // Set the data-pf-theme for pagefind component theme:
    document
      .querySelector("[data-pf-theme]")
      ?.setAttribute("data-pf-theme", isDark ? "dark" : "light");
  }

  /**
   * Toggle dark mode and persist to localStorage
   */
  function toggleTheme() {
    const isDark = !document.documentElement.classList.contains("dark");
    applyTheme(isDark);
    localStorage.setItem(STORAGE_KEY, String(isDark));
  }

  // Initialize theme on page load
  const initialDarkMode = getInitialDarkMode();
  applyTheme(initialDarkMode);

  // Listen for toggle button clicks
  toggleButton.addEventListener("click", toggleTheme);

  // Listen for system preference changes
  window
    .matchMedia("(prefers-color-scheme: dark)")
    .addEventListener("change", (e) => {
      // Only sync if user hasn't manually set preference
      if (localStorage.getItem(STORAGE_KEY) === null) {
        applyTheme(e.matches);
      }
    });
}
