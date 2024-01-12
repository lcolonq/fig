import { MobxLitElement } from "@adobe/lit-mobx";
import { html, css } from "lit";
import { customElement } from "lit/decorators.js";

import * as State from "../state";

@customElement("fig-gizmo")
export class Gizmo extends MobxLitElement {
  private global = State.global;

  static style = css`
`;

  static get(id: string): Gizmo | null {
    const e = document.getElementById(id);
    if (e instanceof Gizmo) return e;
    return null;
  }

  constructor() {
    super();
  }

  render() {
    console.log("render", this.global.gizmo_active);
    if (this.global.gizmo_active) {
      return html`
<fig-window>
<h1>hi</h1>
</fig-window>
`;
    }
  }
}
