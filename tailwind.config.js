/** @type {import('tailwindcss').Config} */
module.exports = {
  content: ["./dist/*.html"],
  theme: {
    fontFamily: {
      "sans": ["Cooper Hewitt", "sans-serif"],
    }
  },
  plugins: [require("@tailwindcss/typography"), require("daisyui")],
}

