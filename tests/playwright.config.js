// Playwright: the built site (_build/site, `make render`) served at the root, checked at phone, projector
// and full-HD sizes. `make test` from the project folder; GEOAPP_URL=<address> checks another server instead
// (the Docker image, or https://app.geoviews.ch after a release).
import { defineConfig, devices } from "@playwright/test";

const PORT = 4322;
const URL = process.env.GEOAPP_URL;

export default defineConfig({
  testDir: ".",
  outputDir: "test-results",
  fullyParallel: true,
  reporter: [["list"]],
  use: { baseURL: URL || `http://localhost:${PORT}` },
  webServer: URL ? undefined : {
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
