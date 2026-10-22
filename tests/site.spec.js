// Every page of the built site, in both themes: no script error, no sideways scrolling, every sim mounted;
// then the modes of a tool page (presets, ?embed, ?present) and the links between the languages.
import { test, expect } from "@playwright/test";
import { readdirSync, statSync, existsSync } from "node:fs";
import { join, relative } from "node:path";

const site = new URL("../_build/site/", import.meta.url).pathname;
const pages = [];
(function walk(dir) {
  for (const name of readdirSync(dir)) {
    const p = join(dir, name);
    if (statSync(p).isDirectory()) { if (name !== "site_libs") walk(p); }
    else if (name.endsWith(".html")) pages.push("/" + relative(site, p));
  }
})(site);

// Quarto keeps the theme choice in localStorage: the dark theme (listed first, the default) is "alternate"
const THEMES = { dark: "alternate", light: "default" };

for (const [theme, value] of Object.entries(THEMES)) {
  test.describe(`${theme} theme`, () => {
    test.beforeEach(async ({ page }) => {
      await page.addInitScript(v => { try { localStorage.setItem("quarto-color-scheme", v); } catch (e) {} }, value);
    });
    for (const path of pages) {
      test(path, async ({ page }) => {
        const errors = [];
        page.on("pageerror", e => errors.push(e.message));
        page.on("console", m => { if (m.type() === "error") errors.push(m.text()); });
        await page.goto(path, { waitUntil: "load" });
        await expect(page.locator("body")).toHaveClass(theme === "dark" ? /quarto-dark/ : /quarto-light/);
        // no sideways scrolling
        const overflow = await page.evaluate(() => document.scrollingElement.scrollWidth - window.innerWidth);
        expect(overflow, "page wider than the screen").toBeLessThanOrEqual(1);
        // every sim mounted, with something drawn
        for (const sim of await page.locator(".geores[data-sim]").all()) {
          await expect(sim).toHaveAttribute("data-mounted", "");
          expect(await sim.locator("canvas, svg").count()).toBeGreaterThan(0);
        }
        if (path.startsWith("/fr/")) expect(await page.getAttribute("html", "lang")).toMatch(/^fr/);
        expect(errors, errors.join("\n")).toEqual([]);
      });
    }
  });
}

test.describe("tool page", () => {
  const url = "/climatology/climogram.html";

  test("presets from the address", async ({ page }) => {
    await page.goto(url);
    const second = await page.locator(".geores-controls select.a option").nth(3).getAttribute("value");
    await page.goto(`${url}?station=${second}`);
    await expect(page.locator(".geores-controls select.a")).toHaveValue(second);
  });

  test("toolbar under the tool", async ({ page }) => {
    await page.goto(url);
    await expect(page.locator(".app-toolbar button")).toHaveCount(3);
  });

  test("?embed shows the tool alone", async ({ page }) => {
    await page.goto(`${url}?embed`);
    await expect(page.locator("#quarto-header")).toBeHidden();
    await expect(page.locator(".app-embed-wrap .geores[data-sim]")).toBeVisible();
    await expect(page.locator(".app-embed-link")).toHaveAttribute("href", /climogram\.html$/);
  });

  test("?present fills the screen, Escape leaves", async ({ page }) => {
    await page.goto(`${url}?present`);
    await expect(page.locator(".geores.geores-expanded")).toBeVisible();
    await expect(page.locator("html")).toHaveClass(/app-present/);
    await page.keyboard.press("Escape");
    await expect(page.locator(".geores.geores-expanded")).toHaveCount(0);
    await expect(page.locator("html")).not.toHaveClass(/app-present/);
  });

  test("full screen stays on top of the page when the sim redraws (map-scales zoom)", async ({ page }) => {
    await page.goto("/cartography/map-scales.html");
    await page.locator(".app-toolbar button").first().click();
    await page.locator(".geores-controls button.scx-out").click();
    await page.waitForTimeout(500);
    const top = await page.evaluate(() => document.elementFromPoint(innerWidth / 2, 20).closest(".geores-expanded") !== null);
    expect(top, "the enlarged sim covers the navbar").toBe(true);
  });

  test("key P toggles present mode", async ({ page }) => {
    await page.goto(url);
    await page.locator("h1").click();
    await page.keyboard.press("p");
    await expect(page.locator(".geores.geores-expanded")).toBeVisible();
  });
});

test.describe("languages", () => {
  test("English page links to its French twin", async ({ page }) => {
    await page.goto("/climatology/climogram.html");
    await expect(page.locator('#languages-links a[href$="/fr/climatology/climogram.html"]')).toHaveCount(1);
  });

  test("French sims are in French", async ({ page }) => {
    await page.goto("/fr/climatology/climogram.html");
    await expect(page.locator(".geores-controls")).toContainText("Comparer");
  });

  test("the OC link of the English site leads to the French OC page", async ({ page }) => {
    await page.goto("/index.html");
    const href = await page.locator('a.nav-link[href*="/fr/oc/"]').first().getAttribute("href");
    const res = await page.request.get(new URL(href, page.url()).toString());
    expect(res.status()).toBe(200);
  });

  test("OC pages exist only in French", () => {
    expect(existsSync(join(site, "fr/oc/russie/index.html"))).toBe(true);
    expect(existsSync(join(site, "oc"))).toBe(false);
  });
});

test.describe("pyramids", () => {
  const url = "/demography/pyramids.html";

  test("the address sets the place and the year, and follows the controls", async ({ page }) => {
    await page.goto(`${url}?c=JPN&y=2000`);
    await expect(page.locator(".app-pyramids select.place")).toHaveValue("JPN");
    await expect(page.locator(".app-pyramids .year-out")).toHaveText("2000");
    await expect(page.locator(".app-svg .app-title")).toContainText("2000");
    await page.locator(".app-pyramids select.place").selectOption("NER");
    await expect(page).toHaveURL(/c=NER/);
    await page.locator(".app-pyramids input.rel").check();
    await expect(page).toHaveURL(/rel=1/);
    expect(await page.locator(".app-svg rect.app-men").count()).toBe(21);
  });

  test("one bar per year of age, an outline to compare", async ({ page }) => {
    await page.goto(`${url}?c=CHE&y=2024&cmp=IND`);
    expect(await page.locator(".app-svg rect.app-men").count()).toBe(101);
    await expect(page.locator(".app-svg path.app-outline")).toHaveCount(2);
  });

  test("play moves the years on", async ({ page }) => {
    await page.goto(`${url}?y=1950`);
    await page.locator(".app-pyramids button.play").click();
    await expect(page.locator(".app-pyramids .year-out")).not.toHaveText("1950");
  });

  test("French labels and the French name of the place", async ({ page }) => {
    await page.goto(`/fr${url}?c=CHE`);
    await expect(page.locator(".app-svg .app-title")).toContainText("Suisse");
    await expect(page.locator(".app-pyramids .geores-controls")).toContainText("Lieu");
  });
});

test.describe("population explorer", () => {
  const url = "/demography/explorer.html";

  test("the map colours the countries by class", async ({ page }) => {
    await page.goto(`${url}?ind=tfr&y=2024`);
    await expect(page.locator(".app-map path.app-land").first()).toBeVisible();
    const colours = await page.locator(".app-map path.app-land").evaluateAll(ps => new Set(ps.map(p => p.style.fill).filter(Boolean)).size);
    expect(colours).toBeGreaterThan(4);
  });

  test("a country on the map opens its curves, and the curves link to its pyramid", async ({ page }) => {
    await page.goto(`${url}?ind=growth`);
    await page.locator(".app-map path.app-land").nth(30).click();
    await expect(page).toHaveURL(/view=chart&c=[A-Z]{3}/);
    await expect(page.locator(".app-chart path.app-line")).toHaveCount(3);
    await expect(page.locator(".app-foot a[href*='pyramids.html?c=']")).toHaveCount(1);
  });

  test("the table sorts and filters", async ({ page }) => {
    await page.goto(`${url}?ind=lex&view=table`);
    await page.locator(".app-search").fill("Switz");
    await expect(page.locator(".app-table tbody tr")).toHaveCount(1);
    await expect(page.locator(".app-table tbody tr")).toContainText("Switzerland");
  });
});

test.describe("demograph", () => {
  const url = "/demography/demograph.html";

  test("a population from the address, with its rates and the gap shaded", async ({ page }) => {
    await page.goto(`${url}?p=SWE`);
    await expect(page.locator(".app-demograph select.pop")).toHaveValue("SWE");
    await expect(page.locator(".app-head")).toContainText("1751");
    await expect(page.locator(".app-chart path.app-line")).toHaveCount(3);
    expect(await page.locator(".app-chart path.app-inc").count()).toBeGreaterThan(0);
  });

  test("a whole country continues with the UN to 2100; a part of a country cannot", async ({ page }) => {
    await page.goto(`${url}?p=CHE&ext=1`);
    await expect(page.locator(".app-head")).toContainText("2100");
    expect(await page.locator(".app-chart path.app-un").count()).toBe(3);
    await page.locator(".app-demograph select.pop").selectOption("DEUTE");
    await expect(page.locator(".app-demograph input.ext")).toBeDisabled();
  });
});

test.describe("climate explorer", () => {
  const url = "/climatology/climates.html";

  test("thousands of stations; a station and a second one from the address", async ({ page }) => {
    await page.goto(`${url}?s=ch-ber&c=wmo-00082331`);
    expect(await page.locator(".app-dots circle").count()).toBeGreaterThan(4000);
    await expect(page.locator(".app-climogram")).toContainText("Bern");
    await expect(page.locator(".app-climogram")).toContainText("Manaus");
    await expect(page.locator(".app-rings circle")).toHaveCount(2);
  });

  test("the quiz hides the name and the code, the answer shows them", async ({ page }) => {
    await page.goto(`${url}?s=wmo-00062366&quiz=1`);
    await expect(page.locator(".app-climogram")).not.toContainText("Cairo");
    await expect(page.locator(".app-rings circle")).toHaveCount(0);
    await page.locator(".app-climates button.reveal").click();
    await expect(page.locator(".app-climogram")).toContainText("Cairo");
  });

  test("a click on the map picks the nearest station", async ({ page }) => {
    await page.goto(url);
    const dot = page.locator(".app-dots circle[data-id='wmo-00027612']");
    await dot.click({ force: true });
    await expect(page).toHaveURL(/s=wmo-00027612/);
    await expect(page.locator(".app-climogram")).toContainText(/Mosc/);
  });
});
