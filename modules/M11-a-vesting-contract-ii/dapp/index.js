import unocss from "https://esm.sh/@unocss/runtime"
import presetUno from "https://esm.sh/@unocss/preset-uno"
import { presetDaisy } from "https://esm.sh/unocss-preset-daisy";

const mainPromise = import("./output/Main/index.js");

unocss({
  defaults: {
    presets: [
      presetUno(),
      presetDaisy({
        styled: true,
        themes: ["light", "dark"],
      }),
    ],
    shortcuts: [{
      "Storybook-logo": "py-8 px-4 flex place-content-center text-2xl text-inherit uppercase no-underline",
      "Storybook-nav": "navbar bg-base-100",
      "Storybook-nav-section": "navbar-center",
      "Storybook-nav-list": "flex place-content-center space-x-4",
      "Storybook-link": "inline-flex flex-shrink-0 cursor-pointer select-none flex-wrap items-center justify-center border-transparent text-center transition duration-200 ease-in-out rounded-btn h-12 px-4 text-sm min-h-12 font-semibold uppercase no-underline border-neutral bg-neutral text-neutral-content",
      "is-active": "border-primary bg-primary text-primary-content",
      "Storybook-main": "mt-8 px-4 container mx-auto",
    }],
  },
});

mainPromise.then(({ main }) => main());
