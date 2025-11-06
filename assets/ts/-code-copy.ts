export function setupCodeCopy() {
  document.querySelectorAll("pre code").forEach((block) => {
    if (!(block && block.textContent)) {
      return;
    }

    const content = block.textContent.trim();

    const isLineNumbers = content
      .split("\n")
      .every((line) => /^\d+$/.test(line.trim()));

    if (!isLineNumbers && !block.classList.contains("copy-added")) {
      block.classList.add("copy-added");

      const button = document.createElement("button");
      button.className = "copy-button";
      button.textContent = "Copy";

      button.addEventListener("click", () => {
        navigator.clipboard.writeText(content).then(() => {
          button.textContent = "Copied!";
          setTimeout(() => {
            button.textContent = "Copy";
          }, 2000);
        });
      });

      block.parentNode?.insertBefore(button, block);
    }
  });
}
