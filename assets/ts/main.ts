import { setupCodeCopy } from "./-code-copy";
import { setupNavToggle } from "./-nav-toggle";
import { setupSearchTrigger } from "./-search-trigger";
import { setupTheme } from "./-theme";

document.addEventListener("DOMContentLoaded", () => {
  setupTheme();
  setupCodeCopy();
  setupNavToggle();
  setupSearchTrigger();
});
