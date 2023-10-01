/**
 * Returns user's theme preference.
 *
 * @returns 'light' | 'dark'
 */
function getThemePreference() {
  // Get the theme preference from the local storage:
  const theme = window.localStorage.getItem("theme");

  // Return as per user preference:
  return theme === "dark" ||
    (theme == null && window.matchMedia("(prefers-color-scheme: dark)").matches)
    ? "dark"
    : "light";
}

/**
 * Set's user's theme preference.
 *
 * @param theme null | undefined | 'light' | 'dark'
 */
function setThemePreference(theme) {
  if (theme == null) {
    window.localStorage.removeItem("theme");
  } else {
    window.localStorage.setItem("theme", theme);
  }
}

/**
 * Switches the theme.
 *
 * @param theme 'light' | 'dark'
 */
function switchTheme(theme) {
  if (theme === "dark") {
    document.documentElement.classList.add("dark");
    setThemePreference("dark");
  } else {
    document.documentElement.classList.remove("dark");
    setThemePreference("light");
  }
}

// Automatically switch theme:
switchTheme(getThemePreference());
