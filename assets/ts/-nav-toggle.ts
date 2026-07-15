/**
 * Mobile hamburger toggle for the primary navigation links.
 */
export function setupNavToggle() {
  const toggleButton = document.querySelector(
    "[data-nav-toggle]",
  ) as HTMLButtonElement;
  const menu = document.getElementById("primary-navigation");
  const menuIcon = document.querySelector("[data-nav-menu-icon]");
  const closeIcon = document.querySelector("[data-nav-close-icon]");

  if (!toggleButton || !menu) {
    return;
  }

  function setOpen(isOpen: boolean) {
    toggleButton.setAttribute("aria-expanded", String(isOpen));
    menu?.classList.toggle("is-open", isOpen);
    menuIcon?.classList.toggle("hidden", isOpen);
    closeIcon?.classList.toggle("hidden", !isOpen);
  }

  toggleButton.addEventListener("click", () => {
    const isOpen = toggleButton.getAttribute("aria-expanded") === "true";
    setOpen(!isOpen);
  });
}
