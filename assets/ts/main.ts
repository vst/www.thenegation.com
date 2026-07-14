import { setupCodeCopy } from "./-code-copy";
import { setupNavToggle } from "./-nav-toggle";
import { setupTheme } from "./-theme";

document.addEventListener("DOMContentLoaded", () => {
  setupTheme();
  setupCodeCopy();
  setupNavToggle();
});
