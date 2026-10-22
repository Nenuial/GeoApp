// Playwright: the built site (_build/site, `make render`) served at the root, checked at phone, projector
// and full-HD sizes. `make test` from the project folder.
import { defineConfig, devices } from "@playwright/test";

const PORT = 4322;

export default defineConfig({
  testDir: ".",
  outputDir: "test-results",
  fullyParallel: true,
  reporter: [["list"]],
  use: { baseURL: `http://localhost:${PORT}` },
  webServer: {
    command: `node ../tools/serve.mjs ${PORT}`,
    port: PORT,
    reuseExistingServer: true,
  },
  projects: [
    { name: "phone", use: { ...devices["iPhone 13"], browserName: "chromium" } },
    { name: "phone-landscape", use: { ...devices["iPhone 13 landscape"], browserName: "chromium" } },
    { name: "beamer", use: { viewport: { width: 1280, height: 800 } } },
    { name: "hd", use: { viewport: { width: 1920, height: 1080 } } },
  ],
});
