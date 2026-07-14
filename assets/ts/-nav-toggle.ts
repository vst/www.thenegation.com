/**
 * Mobile hamburger toggle for the primary navigation links.
 */
export function setupNavToggle() {
  const toggleButton = document.querySelector(
    "[data-nav-toggle]",
  ) as HTMLButtonElement;
  const menu = document.getElementById("primary-navigation");
  const icon = document.querySelector("[data-nav-toggle-icon]");

  if (!toggleButton || !menu) {
    return;
  }

  function setOpen(isOpen: boolean) {
    toggleButton.setAttribute("aria-expanded", String(isOpen));
    menu?.classList.toggle("is-open", isOpen);
    icon?.classList.toggle("bi-list", !isOpen);
    icon?.classList.toggle("bi-x-lg", isOpen);
  }

  toggleButton.addEventListener("click", () => {
    const isOpen = toggleButton.getAttribute("aria-expanded") === "true";
    setOpen(!isOpen);
  });
}
