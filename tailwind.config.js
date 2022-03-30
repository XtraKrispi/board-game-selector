const plugin = require('tailwindcss/plugin')

module.exports = {
  content: ["./src/**/*.elm", "./public/**/*.html"],
  theme: {
    extend: {},
  },
  plugins: [],
  variants: {
    extend: {
      display: ["group-hover"]
    }
  }
}