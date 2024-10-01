// Add copy-to-clipboard button to code blocks:
document.querySelectorAll("pre").forEach((pre) => {
  pre.querySelectorAll("code").forEach((e) => {
    const button = document.createElement("button");
    button.innerText = "Copy code";
    button.addEventListener("click", () => {
      let text = "";

      e.querySelectorAll("span").forEach((listing) => {
        text += listing.innerText;
      });

      navigator.clipboard.writeText(text);
      console.log(text);
    });

    const buttonContainer = document.createElement("div");
    buttonContainer.style.display = "inline-block";
    buttonContainer.style.marginTop = "12px";
    buttonContainer.style.padding = "2px 6px";
    buttonContainer.style.border = "1px solid #919498";
    buttonContainer.style.borderRadius = "6px";
    buttonContainer.style.backgroundColor = "#17181e";
    buttonContainer.style.fontSize = "10px";
    buttonContainer.appendChild(button);

    pre.appendChild(buttonContainer);
  });
});
