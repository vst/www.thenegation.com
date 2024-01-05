const plugin = require("tailwindcss/plugin");
const defaultTheme = require("tailwindcss/defaultTheme");

/** @type {import('tailwindcss').Config} */
module.exports = {
  content: [
    "./templates/**/*.html",
    "./themes/**/*.html",
    "./themes/**/*.html",
  ],
  theme: {
    extend: {
      fontFamily: {
        sans: ["Inter var", ...defaultTheme.fontFamily.sans],
      },
    },
  },
  darkMode: "class",
  plugins: [
    require("@tailwindcss/typography"),
    // See:
    //
    // 1. https://tailwindcss.com/docs/adding-custom-styles#writing-plugins
    // 2. https://tailwindcss.com/docs/plugins
    // 3. https://tailwindcss.com/docs/configuration#core-plugins
    plugin(function ({ addBase, addComponents, addUtilities, theme }) {
      addBase({
        html: {
          fontSize: "1.125rem",
          lineHeight: "1.75rem",
        },
      });
      addComponents({
        ".label": {
          backgroundColor: theme("colors.gray.300"),
          color: theme("colors.gray.800"),
          borderRadius: theme("borderRadius.sm"),
          paddingTop: theme("spacing.1"),
          paddingBottom: theme("spacing.1"),
          paddingLeft: theme("spacing.2"),
          paddingRight: theme("spacing.2"),
          fontSize: theme("fontSize.xs"),
        },
      });
      addUtilities({});
    }),
  ],
};
