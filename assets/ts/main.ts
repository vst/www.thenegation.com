import { setupCodeCopy } from "./-code-copy";
import { setupSearch } from "./-search";
import { setupTheme } from "./-theme";

document.addEventListener("DOMContentLoaded", () => {
  setupTheme();
  setupCodeCopy();
  setupSearch();
});
