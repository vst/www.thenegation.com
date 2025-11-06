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
   * Apply dark mode class to body and update button aria-checked
   */
  function applyTheme(isDark: boolean) {
    if (isDark) {
      document.body.classList.add("dark");
    } else {
      document.body.classList.remove("dark");
    }
    toggleButton.setAttribute("aria-checked", String(isDark));
  }

  /**
   * Toggle dark mode and persist to localStorage
   */
  function toggleTheme() {
    const isDark = !document.body.classList.contains("dark");
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
