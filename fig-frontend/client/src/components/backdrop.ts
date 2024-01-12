import { html, css, LitElement, unsafeCSS } from "lit";
import { customElement } from "lit/decorators.js";

import * as Config from "../config";

import blueprint from "../assets/blueprint.jpg";

@customElement("fig-backdrop")
export class Backdrop extends LitElement {
  static styles = css`
#backdrop {
  z-index: -2;
  position: absolute;
  left: 0px;
  top: 0px;
  width: 100vw;
  height: 100vh;
  background-image: url("${unsafeCSS(new URL(blueprint, Config.SCRIPT_URL))}");
}
#blur {
  z-index: -1;
  backdrop-filter: blur(3px);
  position: absolute;
  left: 0px;
  top: 0px;
  width: 100vw;
  height: 100vh;
  background-size: 100px 100px;
  background-position: -20px -20px;
  background-image:
      linear-gradient(to right, lightgrey 1px, transparent 1px),
      linear-gradient(to bottom, lightgrey 1px, transparent 1px);
}
`;
  render() {
    return html`
<div id="backdrop"></div>
<div id="blur"></div>
`;
  }
}
