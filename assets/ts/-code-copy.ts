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

      const pre = block.parentElement;
      if (!pre || !pre.parentNode) {
        return;
      }

      const wrapper = document.createElement("div");
      wrapper.className = "code-block";
      pre.parentNode.insertBefore(wrapper, pre);
      wrapper.append(pre);
      wrapper.append(button);

      button.addEventListener("click", () => {
        navigator.clipboard.writeText(content).then(() => {
          button.textContent = "Copied!";
          setTimeout(() => {
            button.textContent = "Copy";
          }, 2000);
        });
      });
    }
  });
}
